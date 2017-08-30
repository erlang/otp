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
 * Description: Mutex, rwmutex and condition variable implementation
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define ETHR_INLINE_MTX_FUNC_NAME_(X) X ## __
#define ETHR_MUTEX_IMPL__
#define ETHR_TRY_INLINE_FUNCS

#include <limits.h>
#include "ethread.h"
#include "ethr_internal.h"

#define ETHR_SPIN_WITH_WAITERS 1

#define ETHR_MTX_MAX_FLGS_SPIN 10

#ifdef ETHR_USE_OWN_RWMTX_IMPL__
static int default_rwmtx_main_spincount;
static int default_rwmtx_aux_spincount;
#endif
#ifdef ETHR_USE_OWN_MTX_IMPL__
static int default_mtx_main_spincount;
static int default_mtx_aux_spincount;
static int default_cnd_main_spincount;
static int default_cnd_aux_spincount;
#endif

static int no_spin;

#ifndef ETHR_USE_OWN_RWMTX_IMPL__
static pthread_rwlockattr_t write_pref_attr_data;
static pthread_rwlockattr_t *write_pref_attr;
#endif

#if defined(ETHR_MTX_Q_LOCK_SPINLOCK__)
#  define ETHR_MTX_QLOCK_INIT ethr_spinlock_init
#  define ETHR_MTX_QLOCK_DESTROY ethr_spinlock_destroy
#  define ETHR_MTX_Q_LOCK ethr_spin_lock
#  define ETHR_MTX_Q_UNLOCK ethr_spin_unlock
#elif defined(ETHR_MTX_Q_LOCK_PTHREAD_MUTEX__)
#  define ETHR_MTX_QLOCK_INIT(QL) pthread_mutex_init((QL), NULL)
#  define ETHR_MTX_QLOCK_DESTROY pthread_mutex_destroy
#  define ETHR_MTX_Q_LOCK(L)					\
do {								\
    int res__ = pthread_mutex_lock(L);				\
    if (res__ != 0)						\
	ETHR_FATAL_ERROR__(res__);				\
} while (0)
#  define ETHR_MTX_Q_UNLOCK(L)					\
do {								\
    int res__ = pthread_mutex_unlock(L);			\
    if (res__ != 0)						\
	ETHR_FATAL_ERROR__(res__);				\
} while (0)
#elif defined(ETHR_MTX_Q_LOCK_CRITICAL_SECTION__)
#  define ETHR_MTX_QLOCK_INIT(QL) (InitializeCriticalSection((QL)), 0)
#  define ETHR_MTX_QLOCK_DESTROY(QL) (DeleteCriticalSection((QL)), 0)
#  define ETHR_MTX_Q_LOCK(QL) EnterCriticalSection((QL))
#  define ETHR_MTX_Q_UNLOCK(QL) LeaveCriticalSection((QL))
#endif

int
ethr_mutex_lib_init(int cpu_conf)
{
    int res = 0;

    no_spin = cpu_conf == 1;

#ifdef ETHR_USE_OWN_MTX_IMPL__
    default_mtx_main_spincount = ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_BASE;
    default_mtx_aux_spincount = ETHR_MTX_DEFAULT_AUX_SPINCOUNT;
    default_cnd_main_spincount = ETHR_CND_DEFAULT_MAIN_SPINCOUNT;
    default_cnd_aux_spincount = ETHR_CND_DEFAULT_AUX_SPINCOUNT;
#endif

#ifdef ETHR_USE_OWN_RWMTX_IMPL__

    default_rwmtx_main_spincount = ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_BASE;
    default_rwmtx_aux_spincount = ETHR_RWMTX_DEFAULT_AUX_SPINCOUNT;

#else

#if defined(ETHR_HAVE_PTHREAD_RWLOCKATTR_SETKIND_NP) \
    && defined(ETHR_HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
    res = pthread_rwlockattr_init(&write_pref_attr_data);
    if (res != 0)
	return res;
    res = pthread_rwlockattr_setkind_np(
	&write_pref_attr_data,
	PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
    write_pref_attr = &write_pref_attr_data;
#else
    write_pref_attr = NULL;
#endif

#endif

    return res;
}

#ifdef ETHR_USE_OWN_RWMTX_IMPL__

#ifdef ETHR_ATOMIC_HAVE_INC_DEC_INSTRUCTIONS
#if 0 /*
       * When inc and dec are real atomic instructions as on x86, the
       * ETHR_RLOCK_WITH_INC_DEC implementations performs better with
       * lots of read locks compared to the cmpxchg based implementation.
       * It, however, performs worse with lots of mixed reads and writes.
       * It could be used for rwlocks that are known to be read locked
       * much, but the readers array based implementation outperforms it
       * by far. Therefore, it has been disabled, and will probably be
       * removed some time in the future.
       */
#  define ETHR_RLOCK_WITH_INC_DEC
#endif
#endif

static int reader_groups_array_size = 0;
static int main_threads_array_size = 0;

#endif

int
ethr_mutex_lib_late_init(int no_reader_groups, int no_main_threads)
{

#ifdef ETHR_USE_OWN_MTX_IMPL__
    default_mtx_main_spincount += (no_main_threads
				   * ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_INC);
    if (default_mtx_main_spincount > ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_MAX)
	default_mtx_main_spincount = ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_MAX;
#endif

#ifdef ETHR_USE_OWN_RWMTX_IMPL__

    default_rwmtx_main_spincount += (no_main_threads
				     * ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_INC);
    if (default_rwmtx_main_spincount > ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_MAX)
	default_rwmtx_main_spincount = ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_MAX;

    reader_groups_array_size = (no_reader_groups <= 1
				? 1
				: no_reader_groups + 1);
    main_threads_array_size = (no_main_threads <= 1
			       ? 1
			       : no_main_threads + 1);
#endif
    return 0;
}

int
ethr_rwmutex_set_reader_group(int ix)
{
#ifdef ETHR_USE_OWN_RWMTX_IMPL__
    ethr_ts_event *tse;

    if (ix < 0 || reader_groups_array_size <= ix)
	return EINVAL;

    tse = ethr_get_ts_event();

    if ((tse->iflgs & ETHR_TS_EV_ETHREAD) == 0) {
	ethr_leave_ts_event(tse);
	return EINVAL;
    }

    tse->rgix = ix;
    
    ethr_leave_ts_event(tse);
#endif
    return 0;
}

#if defined(ETHR_MTX_HARD_DEBUG_Q) || defined(ETHR_MTX_HARD_DEBUG_WSQ)
static void hard_debug_chk_q__(struct ethr_mutex_base_ *, int);
#define ETHR_RWMTX_HARD_DEBUG_CHK_Q(RWMTX) hard_debug_chk_q__(&(RWMTX)->mtxb,1)
#define ETHR_MTX_HARD_DEBUG_CHK_Q(MTX) hard_debug_chk_q__(&(MTX)->mtxb, 0)
#else
#define ETHR_RWMTX_HARD_DEBUG_CHK_Q(RWMTX)
#define ETHR_MTX_HARD_DEBUG_CHK_Q(MTX)
#endif

#ifdef ETHR_USE_OWN_RWMTX_IMPL__
static void
rwmutex_transfer_read_lock(ethr_rwmutex *rwmtx,
			   ethr_sint32_t initial,
			   int q_locked);
static void
rwmutex_unlock_wake(ethr_rwmutex *rwmtx,
		    int have_w,
		    ethr_sint32_t initial,
		    int transfer_read_lock);
static int
rwmutex_try_complete_runlock(ethr_rwmutex *rwmtx,
			     ethr_sint32_t initial,
			     ethr_ts_event *tse,
			     int start_next_ix,
			     int check_before_try,
			     int try_write_lock);
#endif

/* -- Utilities used by multiple implementations -- */

#if defined(ETHR_USE_OWN_RWMTX_IMPL__) || defined(ETHR_USE_OWN_MTX_IMPL__) \
    || defined(ETHR_WIN32_THREADS)

static ETHR_INLINE void
enqueue(ethr_ts_event **queue,
	ethr_ts_event *tse_start,
	ethr_ts_event *tse_end)
{
    if (!*queue) {
	*queue = tse_start;
	tse_start->prev = tse_end;
	tse_end->next = tse_start;
    }
    else {
	tse_end->next = *queue;
	tse_start->prev = (*queue)->prev;
	(*queue)->prev->next = tse_start;
	(*queue)->prev = tse_end;
    }
}


static ETHR_INLINE void
dequeue(ethr_ts_event **queue,
	ethr_ts_event *tse_start,
	ethr_ts_event *tse_end)
{
    if (tse_start->prev == tse_end) {
	ETHR_ASSERT(*queue == tse_start && tse_end->next == tse_start);
	*queue = NULL;
    }
    else {
	if (*queue == tse_start)
	    *queue = tse_end->next;
	tse_end->next->prev = tse_start->prev;
	tse_start->prev->next = tse_end->next;
    }
}

#endif

#if defined(ETHR_USE_OWN_RWMTX_IMPL__) || defined(ETHR_USE_OWN_MTX_IMPL__)

static ETHR_INLINE void
insert(ethr_ts_event *tse_pred, ethr_ts_event *tse)
{
    tse->next = tse_pred->next;
    tse->prev = tse_pred;
    tse_pred->next->prev = tse;
    tse_pred->next = tse;
}

static ETHR_INLINE void
rwmutex_freqread_wtng_rdrs_inc(ethr_rwmutex *rwmtx, ethr_ts_event *tse)
{
    int ix = (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ
	      ? tse->rgix
	      : tse->mtix);
    rwmtx->tdata.ra[ix].data.waiting_readers++;
}

static ETHR_INLINE void
rwmutex_freqread_rdrs_add(ethr_rwmutex *rwmtx,
			  ethr_rwmutex_type type,
			  int ix,
			  int inc)
{
    if (type == ETHR_RWMUTEX_TYPE_FREQUENT_READ || ix == 0)
	ethr_atomic32_add(&rwmtx->tdata.ra[ix].data.readers, inc);
    else {
	ETHR_ASSERT(type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ);
	ETHR_ASSERT(ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers) == 0);
	ETHR_ASSERT(inc == 1);
	ethr_atomic32_set(&rwmtx->tdata.ra[ix].data.readers, (ethr_sint32_t) 1);
    }
}

static ETHR_INLINE void
rwmutex_freqread_rdrs_inc(ethr_rwmutex *rwmtx, ethr_ts_event *tse)
{
    int ix;
    if (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ) {
	ix = tse->rgix;
    atomic_inc:
	ethr_atomic32_inc(&rwmtx->tdata.ra[ix].data.readers);
    }
    else {
	ix = tse->mtix;
	if (ix == 0)
	    goto atomic_inc;
	ETHR_ASSERT(rwmtx->type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ);
	ETHR_ASSERT(ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers) == 0);
	ethr_atomic32_set(&rwmtx->tdata.ra[ix].data.readers, (ethr_sint32_t) 1);
    }
}

#if 0 /* Not used */

static ETHR_INLINE void
rwmutex_freqread_rdrs_dec(ethr_rwmutex *rwmtx, ethr_ts_event *tse)
{
    int ix;
    if (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ) {
	ix = tse->rgix;
    atomic_dec:
	ethr_atomic32_dec(&rwmtx->tdata.ra[ix].data.readers);
    }
    else {
	ix = tse->mtix;
	if (ix == 0)
	    goto atomic_dec;
	ETHR_ASSERT(rwmtx->type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ);
	ETHR_ASSERT(ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers) == 1);
	ethr_atomic32_set(&rwmtx->tdata.ra[ix].data.readers, (ethr_sint32_t) 0);
    }
}

#endif

static ETHR_INLINE ethr_sint32_t
rwmutex_freqread_rdrs_dec_read(ethr_rwmutex *rwmtx, ethr_ts_event *tse)
{
    int ix;
    if (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ) {
	ix = tse->rgix;
    atomic_dec_read:
	return ethr_atomic32_dec_read(&rwmtx->tdata.ra[ix].data.readers);
    }
    else {
	ix = tse->mtix;
	if (ix == 0)
	    goto atomic_dec_read;
	ETHR_ASSERT(rwmtx->type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ);
	ETHR_ASSERT(ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers) == 1);
	ethr_atomic32_set(&rwmtx->tdata.ra[ix].data.readers, (ethr_sint32_t) 0);
	return (ethr_sint32_t) 0;
    }
}

static ETHR_INLINE ethr_sint32_t
rwmutex_freqread_rdrs_dec_read_relb(ethr_rwmutex *rwmtx, ethr_ts_event *tse)
{
    int ix;
    if (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ) {
	ix = tse->rgix;
    atomic_dec_read:
	return ethr_atomic32_dec_read_relb(&rwmtx->tdata.ra[ix].data.readers);
    }
    else {
	ix = tse->mtix;
	if (ix == 0)
	    goto atomic_dec_read;
	ETHR_ASSERT(rwmtx->type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ);
	ETHR_ASSERT(ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers) == 1);
	ethr_atomic32_set_relb(&rwmtx->tdata.ra[ix].data.readers,
			     (ethr_sint32_t) 0);
	return (ethr_sint32_t) 0;
    }
}

static ETHR_INLINE ethr_sint32_t
rwmutex_freqread_rdrs_read(ethr_rwmutex *rwmtx, int ix)
{
    ethr_sint32_t res = ethr_atomic32_read(&rwmtx->tdata.ra[ix].data.readers);
#ifdef ETHR_DEBUG
    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
	ETHR_ASSERT(res >= 0);
	break;
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ:
	ETHR_ASSERT(ix == 0 ? res >= 0 : (res == 0 || res == 1));
	break;
    default:
	ETHR_ASSERT(0);
	break;
    }
#endif
    return res;
}

static void
event_wait(struct ethr_mutex_base_ *mtxb,
	   ethr_ts_event *tse,
	   int spincount,
	   ethr_sint32_t type,
	   int is_rwmtx,
	   int is_freq_read)
{
    int locked = 0;
    ethr_sint32_t act;
    int need_try_complete_runlock = 0;
    int transfer_read_lock = 0;

    /* Need to enqueue and wait... */

    tse->uflgs = type;
    ethr_atomic32_set(&tse->uaflgs, type);

    ETHR_MTX_Q_LOCK(&mtxb->qlck);
    locked = 1;

#ifdef ETHR_MTX_HARD_DEBUG_Q
    hard_debug_chk_q__(mtxb, is_rwmtx);
#endif

    act = ethr_atomic32_read(&mtxb->flgs);

    if (act & type) {

	/* Wait bit already there; enqueue... */

	ETHR_ASSERT(mtxb->q);
	if (type == ETHR_RWMTX_W_WAIT_FLG__) {
	    enqueue(&mtxb->q, tse, tse);
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
	    mtxb->ws++;
#endif
	}
	else {
	    ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	    ETHR_ASSERT(is_rwmtx);
	    ETHR_ASSERT(rwmtx->rq_end);
	    insert(rwmtx->rq_end, tse);
	    rwmtx->rq_end = tse;
	    if (is_freq_read)
		rwmutex_freqread_wtng_rdrs_inc(rwmtx, tse);
	    else
		rwmtx->tdata.rs++;
	}
    }
    else {

	/* Set wait bit */

	while (1) {
	    ethr_sint32_t new, exp = act;
	    need_try_complete_runlock = 0;
	    transfer_read_lock = 0;

	    if (type == ETHR_RWMTX_W_WAIT_FLG__) {
		if (is_freq_read && act == ETHR_RWMTX_R_FLG__)
		    need_try_complete_runlock = 1;
		if (act != 0)
		    new = act | ETHR_RWMTX_W_WAIT_FLG__;
		else
		    new = ETHR_RWMTX_W_FLG__; /* Try to get it */
	    }
	    else {
		ETHR_ASSERT(is_rwmtx);

		if (!is_freq_read) {
		    if (act & (ETHR_RWMTX_W_FLG__| ETHR_RWMTX_W_WAIT_FLG__))
			new = act | ETHR_RWMTX_R_WAIT_FLG__;
		    else
			new = act + 1; /* Try to get it */
		}
		else {
		    new = act | ETHR_RWMTX_R_WAIT_FLG__;
		    if ((act & (ETHR_RWMTX_W_FLG__
				| ETHR_RWMTX_W_WAIT_FLG__)) == 0) {
			/* Transfer read lock to this thread. */
			transfer_read_lock = 1;
		    }
		}
	    }

	    act = ethr_atomic32_cmpxchg_acqb(&mtxb->flgs, new, exp);
	    if (exp == act) {
		if (new & type) {
		    act = new;
		    break;
		}
		else {
		    /* Got it */
		    goto done;
		}
	    }
	}

	/* Enqueue */

	if (type == ETHR_RWMTX_R_WAIT_FLG__) {
	    ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	    ETHR_ASSERT(is_rwmtx);
	    ETHR_ASSERT(!rwmtx->rq_end);
	    rwmtx->rq_end =  tse;
	    if (is_freq_read)
		rwmutex_freqread_wtng_rdrs_inc(rwmtx, tse);
	    else
		rwmtx->tdata.rs++;
	}
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
	else {
	    mtxb->ws++;
	}
#endif

	enqueue(&mtxb->q, tse, tse);
    }

#ifdef ETHR_MTX_HARD_DEBUG_Q
    hard_debug_chk_q__(mtxb, is_rwmtx);
#endif

    /* Wait */
    locked = 0;

    ETHR_ASSERT(!(transfer_read_lock && need_try_complete_runlock));

    if (transfer_read_lock) {
	ETHR_ASSERT(((ethr_rwmutex *) mtxb)->type
		    != ETHR_RWMUTEX_TYPE_NORMAL);
	/*
	 * We are the only one in the queue and we are not write
	 * locked; rwmutex_transfer_read_lock() will:
	 * - transfer a read lock to us (since we're first in q)
	 * - unlock the Q-lock
	 */
	rwmutex_transfer_read_lock(((ethr_rwmutex *) mtxb), act, 1);
    }
    else {
	ETHR_MTX_Q_UNLOCK(&mtxb->qlck);

	if (need_try_complete_runlock) {
	    ETHR_ASSERT(((ethr_rwmutex *) mtxb)->type
			!= ETHR_RWMUTEX_TYPE_NORMAL);
	    /*
	     * We were the only one in queue when we enqueued, and it
	     * was seemingly read locked. We need to try to complete a
	     * runlock otherwise we might be hanging forever. If the
	     * runlock could be completed we will be dequeued and
	     * woken by ourselves.
	     */
	    rwmutex_try_complete_runlock((ethr_rwmutex *) mtxb,
					 act, tse, 0, 1, 0);
	}
    }

    while (1) {
	ethr_event_reset(&tse->event);

	act = ethr_atomic32_read_acqb(&tse->uaflgs);
	if (!act)
	    goto done; /* Got it */

	ETHR_ASSERT(act == type);
	ethr_event_swait(&tse->event, spincount);
	/* swait result: 0 || EINTR */

	act = ethr_atomic32_read_acqb(&tse->uaflgs);
	if (!act)
	    goto done; /* Got it */
    }

 done:
    if (locked)
	ETHR_MTX_Q_UNLOCK(&mtxb->qlck);
}

static void
wake_writer(struct ethr_mutex_base_ *mtxb, int is_rwmtx)
{
    ethr_ts_event *tse;

    tse = mtxb->q;
    ETHR_ASSERT(tse);
    dequeue(&mtxb->q, tse, tse);

    ETHR_ASSERT(tse->uflgs == ETHR_RWMTX_W_WAIT_FLG__);
    ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs) == ETHR_RWMTX_W_WAIT_FLG__);
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
    mtxb->ws--;
#endif
#if defined(ETHR_MTX_HARD_DEBUG_Q) || defined(ETHR_MTX_HARD_DEBUG_WSQ)
    hard_debug_chk_q__(mtxb, is_rwmtx);
#endif

    ETHR_MTX_Q_UNLOCK(&mtxb->qlck);

    ethr_atomic32_set(&tse->uaflgs, 0);
    ethr_event_set(&tse->event);
}

static ETHR_INLINE int
initial_spincount(struct ethr_mutex_base_ *mtxb)
{
    return (mtxb->aux_scnt < ETHR_MTX_MAX_FLGS_SPIN
	    ? mtxb->aux_scnt
	    : ETHR_MTX_MAX_FLGS_SPIN);
}

static ETHR_INLINE int
update_spincount(struct ethr_mutex_base_ *mtxb,
		 ethr_ts_event *tse,
		 int *scnt_state,
		 int *scnt)
{
    int state = *scnt_state;
    if (state <= 0) {
	/* Here state is max spincount to do on event negated */
	*scnt = -state;
    }
    else {
	/* Here state is initial spincount made on flags */
	*scnt = ((tse->iflgs & ETHR_TS_EV_MAIN_THR)
		 ? mtxb->main_scnt
		 : mtxb->aux_scnt);
	if (*scnt <= state)
	    *scnt = 0;
	else {
	    if (*scnt <= ETHR_MTX_MAX_FLGS_SPIN)
		*scnt_state = 0; /* No spin on event */
	    else {
		 /* Spin on event after... */
		*scnt_state = -1*(*scnt - ETHR_MTX_MAX_FLGS_SPIN);
		/* ... we have spun on flags */
		*scnt = ETHR_MTX_MAX_FLGS_SPIN;
	    }
	    *scnt -= state;
	    return 0;
	}
    }
    return 1;
}

int check_readers_array(ethr_rwmutex *rwmtx,
			int start_rix,
			int length);

static ETHR_INLINE void
write_lock_wait(struct ethr_mutex_base_ *mtxb,
		ethr_sint32_t initial,
		int is_rwmtx,
		int is_freq_read)
{
    ethr_sint32_t act = initial;
    int scnt, start_scnt;
    ethr_ts_event *tse = NULL;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
    int res;

    ETHR_ASSERT(!is_freq_read || is_rwmtx);

    start_scnt = scnt = initial_spincount(mtxb);

    /*
     * Spin trying to write lock for a while. If unsuccessful,
     * wait on event.
     */

    while (1) {
	while (act != 0) {

	    if (is_freq_read && act == ETHR_RWMTX_R_FLG__) {
		ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
		scnt--;
		if (!tse)
		    tse = ethr_get_ts_event();
		res = rwmutex_try_complete_runlock(rwmtx, act,
						   tse, 0, 0,
						   1);
		if (res != EBUSY)
		    goto done; /* Got it */
		if (scnt <= 0)
		    goto chk_spin;
		if (--until_yield == 0) {
		    until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
		    (void) ETHR_YIELD();
		}
	    }

	    if (scnt <= 0) {
	    chk_spin:
		scnt = 0;

		if (!tse)
		    tse = ethr_get_ts_event();
		if (update_spincount(mtxb, tse, &start_scnt, &scnt)) {
		    event_wait(mtxb, tse, scnt, ETHR_RWMTX_W_WAIT_FLG__,
			       is_rwmtx, is_freq_read);
		    goto done;
		}
	    }
	    ETHR_SPIN_BODY;
	    if (--until_yield == 0) {
		until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
		(void) ETHR_YIELD();
	    }
	    act = ethr_atomic32_read(&mtxb->flgs);
	    scnt--;
	}

	act = ethr_atomic32_cmpxchg_acqb(&mtxb->flgs,
					 ETHR_RWMTX_W_FLG__,
					 0);
	if (act == 0)
	    goto done; /* Got it */
    }

 done:
    if (tse)
	ethr_leave_ts_event(tse);
}

static int
mtxb_init(struct ethr_mutex_base_ *mtxb,
	  int def_main_scnt,
	  int main_scnt,
	  int def_aux_scnt,
	  int aux_scnt)
{
    ETHR_MTX_HARD_DEBUG_LFS_INIT(mtxb);
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
    mtxb->ws = 0;
#endif
    ETHR_MTX_CHK_EXCL_INIT(mtxb);
    if (no_spin) {
	mtxb->main_scnt = 0;
	mtxb->aux_scnt = 0;
    }
    else {
	if (main_scnt > SHRT_MAX)
	    mtxb->main_scnt = SHRT_MAX;
	else if (main_scnt < 0)
	    mtxb->main_scnt = def_main_scnt;
	else
	    mtxb->main_scnt = (short) main_scnt;
	if (aux_scnt > SHRT_MAX)
	    mtxb->aux_scnt = SHRT_MAX;
	else if (aux_scnt < 0)
	    mtxb->aux_scnt = def_aux_scnt;
	else
	    mtxb->aux_scnt = (short) aux_scnt;
	if (mtxb->main_scnt < mtxb->aux_scnt)
	    mtxb->main_scnt = mtxb->aux_scnt;

    }
    mtxb->q = NULL;
    ethr_atomic32_init(&mtxb->flgs, 0);
    return ETHR_MTX_QLOCK_INIT(&mtxb->qlck);
}

static int
mtxb_destroy(struct ethr_mutex_base_ *mtxb)
{
    ethr_sint32_t act;
    ETHR_MTX_Q_LOCK(&mtxb->qlck);
    act = ethr_atomic32_read(&mtxb->flgs);
    ETHR_MTX_Q_UNLOCK(&mtxb->qlck);
    if (act != 0)
	return EINVAL;
    return ETHR_MTX_QLOCK_DESTROY(&mtxb->qlck);
}


#endif /* ETHR_USE_OWN_RWMTX_IMPL__ || ETHR_USE_OWN_MTX_IMPL__ */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Mutex and condition variable implementation                               *
\*                                                                           */

#ifdef ETHR_USE_OWN_MTX_IMPL__

/* -- Mutex ---------------------------------------------------------------- */

int
ethr_mutex_init_opt(ethr_mutex *mtx, ethr_mutex_opt *opt)
{
    int res;
#if ETHR_XCHK
    if (!mtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#endif
    ETHR_MTX_HARD_DEBUG_FENCE_INIT(mtx);
    res = mtxb_init(&mtx->mtxb,
		    default_mtx_main_spincount,
		    opt ? opt->main_spincount : -1,
		    default_mtx_aux_spincount,
		    opt ? opt->aux_spincount : -1);
#if ETHR_XCHK
    if (res != 0)
	mtx->initialized = 0;
#endif
    return res;
}

int
ethr_mutex_init(ethr_mutex *mtx)
{
    return ethr_mutex_init_opt(mtx, NULL);
}

int
ethr_mutex_destroy(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!mtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = 0;
#endif
    return mtxb_destroy(&mtx->mtxb);
}

void
ethr_mutex_lock_wait__(ethr_mutex *mtx, ethr_sint32_t initial)
{
    write_lock_wait(&mtx->mtxb, initial, 0, 0);
}

void
ethr_mutex_unlock_wake__(ethr_mutex *mtx, ethr_sint32_t initial)
{
    ethr_ts_event *tse;

    ETHR_MTX_Q_LOCK(&mtx->mtxb.qlck);
    tse = mtx->mtxb.q;

    ETHR_ASSERT(tse);
    ETHR_ASSERT(ethr_atomic32_read(&mtx->mtxb.flgs)
		== (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__));
    ETHR_ASSERT(initial & ETHR_RWMTX_W_WAIT_FLG__);
    ETHR_MTX_HARD_DEBUG_CHK_Q(mtx);

    /*
     * If we have multiple waiters, there is no need to modify
     * mtxb->flgs; otherwise, we need to clear the write wait bit...
     */
    if (tse->next == mtx->mtxb.q)
	ethr_atomic32_set(&mtx->mtxb.flgs, ETHR_RWMTX_W_FLG__);

    wake_writer(&mtx->mtxb, 0);
}

/* -- Condition variables -------------------------------------------------- */

static void
enqueue_mtx(ethr_mutex *mtx, ethr_ts_event *tse_start, ethr_ts_event *tse_end)
{
    ethr_sint32_t act;

    /*
     * `ethr_cond_signal()' and `ethr_cond_broadcast()' end up here. If `mtx'
     * is not currently locked by current thread, we almost certainly have a
     * hard to debug race condition. There might however be some (strange)
     * use for it. POSIX also allow a call to `pthread_cond_signal' or
     * `pthread_cond_broadcast' even though the the associated mutex isn't
     * locked by the caller. Therefore, we also allow this kind of strange
     * usage, but optimize for the case where the mutex is locked by the
     * calling thread.
     */

    ETHR_MTX_Q_LOCK(&mtx->mtxb.qlck);

    ETHR_MTX_HARD_DEBUG_CHK_Q(mtx);

#ifdef ETHR_MTX_HARD_DEBUG_WSQ
    {
	int dbg_nws__ = 0;
	ethr_ts_event *dbg_tse__;
	for (dbg_tse__ = tse_start;
	     dbg_tse__ != tse_end;
	     dbg_tse__ = dbg_tse__->next)
	    dbg_nws__++;
	mtx->mtxb.ws += dbg_nws__ + 1;
    }
#endif

    act = ethr_atomic32_read(&mtx->mtxb.flgs);
    ETHR_ASSERT(act == 0
		|| act == ETHR_RWMTX_W_FLG__
		|| act == (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__));
    if (act & ETHR_RWMTX_W_FLG__) {
	/* The normal sane case */
	if (!(act & ETHR_RWMTX_W_WAIT_FLG__)) {
	    ETHR_ASSERT(!mtx->mtxb.q);
	    act = ethr_atomic32_cmpxchg(&mtx->mtxb.flgs,
					(ETHR_RWMTX_W_FLG__
					 | ETHR_RWMTX_W_WAIT_FLG__),
					ETHR_RWMTX_W_FLG__);
	    if (act != ETHR_RWMTX_W_FLG__) {
		/*
		 * Sigh... this wasn't so sane after all since, the mutex was
		 * obviously not locked by the current thread....
		 */
		ETHR_ASSERT(act == 0);
		goto mtx_unlocked;
	    }
	}

#ifdef ETHR_DEBUG
	if (act & ETHR_RWMTX_W_WAIT_FLG__)
	    ETHR_ASSERT(mtx->mtxb.q);
	else
	    ETHR_ASSERT(!mtx->mtxb.q);
#endif

	enqueue(&mtx->mtxb.q, tse_start, tse_end);

	ETHR_MTX_HARD_DEBUG_CHK_Q(mtx);
	ETHR_MTX_Q_UNLOCK(&mtx->mtxb.qlck);

    }
    else {
	int multi;
    mtx_unlocked:
	/* Sigh... mutex isn't locked... */

	multi = tse_start != tse_end;

	while (1) {
	    ethr_sint32_t new, exp = act;

	    if (multi || (act & ETHR_RWMTX_W_FLG__))
		new = ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__;
	    else
		new = ETHR_RWMTX_W_FLG__;

	    act = ethr_atomic32_cmpxchg(&mtx->mtxb.flgs, new, exp);
	    if (exp == act) {
		ETHR_ASSERT(!mtx->mtxb.q);
		if (act & ETHR_RWMTX_W_FLG__) {
		    enqueue(&mtx->mtxb.q, tse_start, tse_end);

		    ETHR_MTX_HARD_DEBUG_CHK_Q(mtx);
		    ETHR_MTX_Q_UNLOCK(&mtx->mtxb.qlck);

		}
		else {
		    ETHR_ASSERT(!mtx->mtxb.q);
		    /*
		     * Acquired the mutex on behalf of the
		     * first thread in the queue; wake
		     * it and enqueue the rest...
		     */
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
		    mtx->mtxb.ws--;
#endif
		    if (multi) {
			enqueue(&mtx->mtxb.q, tse_start->next, tse_end);
			ETHR_ASSERT(mtx->mtxb.q);
		    }

		    ETHR_MTX_HARD_DEBUG_CHK_Q(mtx);
		    ETHR_MTX_Q_UNLOCK(&mtx->mtxb.qlck);

		    ethr_atomic32_set(&tse_start->uaflgs, 0);
		    ethr_event_set(&tse_start->event);
		}
		break;
	    }
	}
    }
}

int
ethr_cond_init_opt(ethr_cond *cnd, ethr_cond_opt *opt)
{
#if ETHR_XCHK
    if (!cnd) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = ETHR_COND_INITIALIZED;
#endif
    ETHR_MTX_HARD_DEBUG_FENCE_INIT(cnd);
    cnd->q = NULL;
    if (no_spin) {
	cnd->main_scnt = 0;
	cnd->aux_scnt = 0;
    }
    else {
	if (!opt || opt->main_spincount < 0)
	    cnd->main_scnt = default_cnd_main_spincount;
	else if (opt->main_spincount > SHRT_MAX)
	    cnd->main_scnt = SHRT_MAX;
	else
	    cnd->main_scnt = (short) opt->main_spincount;
	if (!opt || opt->aux_spincount < 0)
	    cnd->aux_scnt = default_cnd_aux_spincount;
	else if (opt->aux_spincount > SHRT_MAX)
	    cnd->aux_scnt = SHRT_MAX;
	else
	    cnd->aux_scnt = (short) opt->aux_spincount;
	if (cnd->main_scnt < cnd->aux_scnt)
	    cnd->main_scnt = cnd->aux_scnt;
    }
    ETHR_MTX_QLOCK_INIT(&cnd->qlck);
    return 0;
}

int
ethr_cond_init(ethr_cond *cnd)
{
    return ethr_cond_init_opt(cnd, NULL);
}

int
ethr_cond_destroy(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!cnd || cnd->initialized != ETHR_COND_INITIALIZED) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = 0;
#endif
    return ETHR_MTX_QLOCK_DESTROY(&cnd->qlck);
}

void
ethr_cond_signal(ethr_cond *cnd)
{
    ethr_ts_event *tse;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);

    ETHR_MTX_Q_LOCK(&cnd->qlck);

    tse = cnd->q;

    if (!tse) {
	ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
	ETHR_MTX_Q_UNLOCK(&cnd->qlck);
    }
    else {
	ethr_mutex *mtx = (ethr_mutex *) tse->udata;

	ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
	ETHR_ASSERT(tse->uflgs == ETHR_RWMTX_W_WAIT_FLG__);
	ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs) == ETHR_CND_WAIT_FLG__);

	ethr_atomic32_set(&tse->uaflgs, ETHR_RWMTX_W_WAIT_FLG__);

	dequeue(&cnd->q, tse, tse);

	ETHR_MTX_Q_UNLOCK(&cnd->qlck);

	tse->next = tse->prev = NULL;

	enqueue_mtx(mtx, tse, tse);

	ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
	ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    }
}

void
ethr_cond_broadcast(ethr_cond *cnd)
{
    int got_all;
    ethr_ts_event *tse;
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
    do {
	got_all = 1;

	ETHR_MTX_Q_LOCK(&cnd->qlck);

	tse = cnd->q;

	if (!tse) {
	    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
	    ETHR_MTX_Q_UNLOCK(&cnd->qlck);
	}
	else {
	    ethr_mutex *mtx = (ethr_mutex *) tse->udata;
	    ethr_ts_event *tse_tmp, *tse_end;

	    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
	    tse_end = cnd->q->prev;

	    tse_tmp = tse;

	    do {

		if (mtx == (ethr_mutex *) tse_tmp->udata) {
		    /* The normal case */

		    ETHR_ASSERT(tse_tmp->uflgs == ETHR_RWMTX_W_WAIT_FLG__);
		    ETHR_ASSERT(ethr_atomic32_read(&tse_tmp->uaflgs)
				== ETHR_CND_WAIT_FLG__);

		    ethr_atomic32_set(&tse_tmp->uaflgs,
				      ETHR_RWMTX_W_WAIT_FLG__);
		}
		else {
		    /* Should be very unusual */
		    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
		    tse_end = tse_tmp->prev;
		    got_all = 0;
		    break;
		}

		tse_tmp = tse_tmp->next;

	    } while (tse_tmp != cnd->q);

	    dequeue(&cnd->q, tse, tse_end);

	    ETHR_MTX_Q_UNLOCK(&cnd->qlck);

	    enqueue_mtx(mtx, tse, tse_end);
	}

    } while (!got_all);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
}

int
ethr_cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
    int woken;
    int scnt;
    void *udata = NULL;
    ethr_ts_event *tse;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);
    ETHR_ASSERT(mtx);
    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

    tse = ethr_get_ts_event();

    scnt = ((tse->iflgs & ETHR_TS_EV_MAIN_THR)
	    ? cnd->main_scnt
	    : cnd->aux_scnt);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);

    udata = tse->udata; /* Got to restore udata before returning */
    tse->udata = (void *) mtx;

    tse->uflgs = ETHR_RWMTX_W_WAIT_FLG__; /* Prep for mutex lock op */
    ethr_atomic32_set(&tse->uaflgs, ETHR_CND_WAIT_FLG__);

    ETHR_MTX_Q_LOCK(&cnd->qlck);

    enqueue(&cnd->q, tse, tse);

    ETHR_MTX_Q_UNLOCK(&cnd->qlck);

    ethr_mutex_unlock(mtx);

    /* Wait */
    woken = 0;
    while (1) {
	ethr_sint32_t act;

	ethr_event_reset(&tse->event);

	act = ethr_atomic32_read_acqb(&tse->uaflgs);
	if (!act)
	    break; /* Mtx locked */

	/* First time, got EINTR, or spurious wakeup... */

	ETHR_ASSERT(act == ETHR_CND_WAIT_FLG__
		    || act == ETHR_RWMTX_W_WAIT_FLG__);

	if (woken) {
	    /*
	     * If act == ETHR_RWMTX_W_WAIT_FLG__, we have already been enqueued
	     * on the mutex; continue wait until locked...
	     */
	    if (act == ETHR_CND_WAIT_FLG__) {
		ETHR_MTX_Q_LOCK(&cnd->qlck);
		act = ethr_atomic32_read(&tse->uaflgs);
		ETHR_ASSERT(act == ETHR_CND_WAIT_FLG__
			    || act == ETHR_RWMTX_W_WAIT_FLG__);
		/*
		 * If act == ETHR_RWMTX_W_WAIT_FLG__, we have already
		 * enqueued on the mutex; continue wait until locked...
		 */
		if (act == ETHR_CND_WAIT_FLG__)
		    dequeue(&cnd->q, tse, tse);

		ETHR_MTX_Q_UNLOCK(&cnd->qlck);

		if (act == ETHR_CND_WAIT_FLG__) {
		    tse->udata = udata;
		    ethr_leave_ts_event(tse);
		    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
		    ethr_mutex_lock(mtx);
		    return EINTR;
		}
	    }
	    ETHR_ASSERT(act == ETHR_RWMTX_W_WAIT_FLG__);
	}
	ethr_event_swait(&tse->event, scnt);
	/* swait result: 0 || EINTR */
	woken = 1;
    }

    ETHR_MTX_HARD_DEBUG_LFS_RWLOCK(&mtx->mtxb);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(cnd);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_CHK_EXCL_SET_EXCL(&mtx->mtxb);
    tse->udata = udata;
    ethr_leave_ts_event(tse);
    return 0;
}

#elif defined(ETHR_PTHREADS) && !defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)
/* -- pthread mutex and condition variables -------------------------------- */

int
ethr_mutex_init(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (!mtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#endif
    return pthread_mutex_init(&mtx->pt_mtx, NULL);
}

int
ethr_mutex_init_opt(ethr_mutex *mtx, ethr_mutex_opt *opt)
{
    return ethr_mutex_init(mtx);
}

int
ethr_mutex_destroy(ethr_mutex *mtx)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!mtx || mtx->initialized != ETHR_MUTEX_INITIALIZED) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
#if ETHR_XCHK
    mtx->initialized = 0;
#endif
    return pthread_mutex_destroy(&mtx->pt_mtx);
}

int
ethr_cond_init(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (!cnd) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = ETHR_COND_INITIALIZED;
#endif
    return pthread_cond_init(&cnd->pt_cnd, NULL);
}

int
ethr_cond_init_opt(ethr_cond *cnd, ethr_cond_opt *opt)
{
    return ethr_cond_init(cnd);
}

int
ethr_cond_destroy(ethr_cond *cnd)
{
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!cnd || cnd->initialized != ETHR_COND_INITIALIZED) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = 0;
#endif
    return pthread_cond_destroy(&cnd->pt_cnd);
}

void
ethr_cond_signal(ethr_cond *cnd)
{
    int res;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);

    res = pthread_cond_signal(&cnd->pt_cnd);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

void
ethr_cond_broadcast(ethr_cond *cnd)
{
    int res;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);

    res = pthread_cond_broadcast(&cnd->pt_cnd);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

int
ethr_cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
    int res;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(cnd);
    ETHR_ASSERT(cnd->initialized == ETHR_COND_INITIALIZED);
    ETHR_ASSERT(mtx);
    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

    res = pthread_cond_wait(&cnd->pt_cnd, &mtx->pt_mtx);
    if (res != 0 && res != EINTR)
	ETHR_FATAL_ERROR__(res);
    return res;
}

#elif defined(ETHR_WIN32_THREADS) || defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)

/*
 * As of Vista/Server, 2008 Windows has condition variables that can be
 * used with critical sections. However, we need to be able to run on
 * older Windows versions too, so we need to implement condition variables
 * ourselves.
 */

#ifdef ETHR_DBG_WIN_MTX_WITH_PTHREADS
/*
 * For debugging of this implementation on POSIX platforms...
 */

#define ethr_win_get_errno__() EINVAL
#if defined(__GNUC__)
#define __forceinline __inline__
#else
#define __forceinline
#endif

static int
InitializeCriticalSectionAndSpinCount(CRITICAL_SECTION *cs, int sc)
{
    return 0 == pthread_mutex_init((pthread_mutex_t *) cs, NULL);
}

static void DeleteCriticalSection(CRITICAL_SECTION *cs)
{
    int res = pthread_mutex_destroy((pthread_mutex_t *) cs);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

int TryEnterCriticalSection(CRITICAL_SECTION *cs)
{
    int res;
    res = pthread_mutex_trylock((pthread_mutex_t *) cs);
    if (res != 0 && res != EBUSY)
	ETHR_FATAL_ERROR__(res);
    return res == 0;
}

void EnterCriticalSection(CRITICAL_SECTION *cs)
{
    int res = pthread_mutex_lock((pthread_mutex_t *) cs);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

void LeaveCriticalSection(CRITICAL_SECTION *cs)
{
    int res = pthread_mutex_unlock((pthread_mutex_t *) cs);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

#endif

#define ETHR_CND_WAIT__ ((ethr_sint32_t) 0x11dead11)
#define ETHR_CND_WAKEUP__ ((ethr_sint32_t) 0x11beef11)

static ETHR_FORCE_INLINE void
cond_wakeup(ethr_ts_event *tse)
{
    ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs) == ETHR_CND_WAIT__);

    ethr_atomic32_set_relb(&tse->uaflgs, ETHR_CND_WAKEUP__);
    ethr_event_set(&tse->event);
}

void
ethr_mutex_cond_wakeup__(ethr_mutex *mtx)
{
    /*
     * Called by ethr_mutex_unlock() when we have
     * cond signal/broadcast wakeups waiting to
     * be completed.
     */
    ethr_ts_event *tse;

    if (!mtx->posix_compliant) {
	tse = mtx->wakeups;
	dequeue(&mtx->wakeups, tse, tse);
    }
    else {
	ethr_spin_lock(&mtx->lock);
	tse = mtx->wakeups;
	if (tse)
	  dequeue(&mtx->wakeups, tse, tse);
	if (!mtx->wakeups)
	  ethr_atomic32_set_relb(&mtx->have_wakeups, 0);
	ethr_spin_unlock(&mtx->lock);
    }

    LeaveCriticalSection(&mtx->cs);

    ETHR_ASSERT(tse || mtx->posix_compliant);

    /*
     * We delay actual condition variable wakeup until
     * this point when we have left the critical section.
     * This in order to avoid that the other thread is
     * woken and then right away have to go to sleep
     * waiting for the critical section that we are in.
     *
     * We also only wake one thread at a time even if
     * there are multiple threads waiting to be woken.
     * Otherwise all but one will be woken and then right
     * away have to go to sleep on the critical section.
     * Since each wakeup is guaranteed to generate at
     * least one lock/unlock sequence on this mutex, all
     * threads will eventually be woken.
     */

    if (tse)
	cond_wakeup(tse);
}

int
ethr_mutex_init_opt(ethr_mutex *mtx, ethr_mutex_opt *opt)
{
    int spincount;
#if ETHR_XCHK
    if (!mtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    mtx->initialized = ETHR_MUTEX_INITIALIZED;
#endif

    spincount = opt ? opt->aux_spincount : 0;
    if (spincount < 0)
	spincount = 0;

    if (!InitializeCriticalSectionAndSpinCount(&mtx->cs, spincount)) {
#if ETHR_XCHK
	mtx->initialized = 0;
#endif
	return ethr_win_get_errno__();
    }

    mtx->posix_compliant = opt ? opt->posix_compliant : 0;
    mtx->wakeups = NULL;
    if (mtx->posix_compliant) {
	ethr_atomic32_init(&mtx->locked, 0);
	ethr_atomic32_init(&mtx->have_wakeups, 0);
	ethr_spinlock_init(&mtx->lock);
    }
    return 0;
}

int
ethr_mutex_init(ethr_mutex *mtx)
{
    return ethr_mutex_init_opt(mtx, NULL);
}

int
ethr_mutex_destroy(ethr_mutex *mtx)
{
    DeleteCriticalSection(&mtx->cs);
    if (mtx->posix_compliant)
	return ethr_spinlock_destroy(&mtx->lock);
    else
	return 0;
}

int
ethr_cond_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
    void *udata;
    ethr_ts_event *tse = ethr_get_ts_event();
    int spincount;

    udata = tse->udata;
    tse->udata = (void *) mtx;
    ethr_atomic32_set_relb(&tse->uaflgs, ETHR_CND_WAIT__);

    EnterCriticalSection(&cnd->cs);
    enqueue(&cnd->waiters, tse, tse);
    LeaveCriticalSection(&cnd->cs);

    ethr_mutex_unlock(mtx);

    spincount = cnd->spincount;

    while (ethr_atomic32_read_acqb(&tse->uaflgs) != ETHR_CND_WAKEUP__) {
	ethr_event_reset(&tse->event);
	if (ethr_atomic32_read_acqb(&tse->uaflgs) == ETHR_CND_WAKEUP__)
	    break;
	ethr_event_swait(&tse->event, spincount);
	spincount = 0;
    }

    tse->udata = udata;
    ethr_leave_ts_event(tse);

    ethr_mutex_lock(mtx);

    return 0;
}

static ETHR_FORCE_INLINE void
posix_compliant_mtx_enqueue(ethr_mutex *mtx,
			    ethr_ts_event *tse_start,
			    ethr_ts_event *tse_end)
{
    ethr_ts_event *tse_wakeup = NULL; /* Avoid erroneous compiler warning... */
    /*
     * The associated mutex might not be locked, so we need to
     * check if it is. If locked, enqueue for wakeup at unlock;
     * otherwise, wakeup the first one now and enqueue the rest.
     */
    if (tse_start == tse_end && !ethr_atomic32_read(&mtx->locked)) {
	tse_wakeup = tse_start;
    wakeup:
	cond_wakeup(tse_wakeup);
    }
    else {
	int need_wakeup;
	ethr_spin_lock(&mtx->lock);
	if (!mtx->wakeups)
	    ethr_atomic32_set_mb(&mtx->have_wakeups, 1);
	need_wakeup = !ethr_atomic32_read(&mtx->locked);
	if (need_wakeup) {
	    if (tse_start == tse_end) {
		if (!mtx->wakeups)
		    ethr_atomic32_set_relb(&mtx->have_wakeups, 0);
		ethr_spin_unlock(&mtx->lock);
		tse_wakeup = tse_start;
		goto wakeup;
	    }
	    tse_wakeup = tse_start;
	    tse_start = tse_start->next;
	}
	enqueue(&mtx->wakeups, tse_start, tse_end);
	ethr_spin_unlock(&mtx->lock);
	if (need_wakeup)
	    goto wakeup;
    }
}

static ETHR_FORCE_INLINE void
enqueue_cond_wakeups(ethr_ts_event *queue, int posix_compliant)
{
    if (queue) {
	int more;
	ethr_ts_event *q = queue;

	/*
	 * Waiters may be using different mutexes...
	 */

	do {
	    ethr_mutex *mtx;
	    ethr_ts_event *tse, *tse_start, *tse_end;

	    more = 0;
	    tse_start = q;
	    mtx = (ethr_mutex *) tse_start->udata;

	    ETHR_ASSERT(posix_compliant
			? mtx->posix_compliant
			: !mtx->posix_compliant);

	    ETHR_ASSERT(ethr_atomic32_read(&tse_start->uaflgs)
			== ETHR_CND_WAIT__);
	    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

	    tse_end = tse_start->prev;

	    for (tse = tse_start->next; tse != tse_start; tse = tse->next) {

		ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs)
			    == ETHR_CND_WAIT__);

		if (mtx != (ethr_mutex *) tse->udata) {
		    tse_end = tse->prev;
		    dequeue(&q, tse_start, tse_end);
		    more = 1;
		    break;
		}
	    }

	    if (posix_compliant)
		posix_compliant_mtx_enqueue(mtx, tse_start, tse_end);
	    else
		enqueue(&mtx->wakeups, tse_start, tse_end);

	} while (more);
    }
}

void
ethr_cond_broadcast(ethr_cond *cnd)
{
    ethr_ts_event *waiters;

    EnterCriticalSection(&cnd->cs);
    waiters = cnd->waiters;
    cnd->waiters = NULL;
    LeaveCriticalSection(&cnd->cs);

    if (cnd->posix_compliant)
	enqueue_cond_wakeups(waiters, 1);
    else
	enqueue_cond_wakeups(waiters, 0);
}

void
ethr_cond_signal(ethr_cond *cnd)
{
    ethr_mutex *mtx;
    ethr_ts_event *tse;

    EnterCriticalSection(&cnd->cs);
    tse = cnd->waiters;
    if (tse)
      dequeue(&cnd->waiters, tse, tse);
    LeaveCriticalSection(&cnd->cs);

    if (tse) {
	mtx = (ethr_mutex *) tse->udata;

	ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs) == ETHR_CND_WAIT__);
	ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);
	ETHR_ASSERT(cnd->posix_compliant
		    ? mtx->posix_compliant
		    : !mtx->posix_compliant);

	if (cnd->posix_compliant)
	    posix_compliant_mtx_enqueue(mtx, tse, tse);
	else
	    enqueue(&mtx->wakeups, tse, tse);
    }
}

int
ethr_cond_init_opt(ethr_cond *cnd, ethr_cond_opt *opt)
{
    int spincount;

#if ETHR_XCHK
    if (!cnd) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    cnd->initialized = ETHR_COND_INITIALIZED;
#endif

    spincount = opt ? opt->aux_spincount : 0;
    if (spincount < 0)
	spincount = 0;

    if (!InitializeCriticalSectionAndSpinCount(&cnd->cs, spincount)) {
#if ETHR_XCHK
	cnd->initialized = 0;
#endif
	return ethr_win_get_errno__();
    }

    cnd->posix_compliant = opt ? opt->posix_compliant : 0;
    cnd->waiters = NULL;
    cnd->spincount = spincount;
    return 0;
}

int
ethr_cond_init(ethr_cond *cnd)
{
    return ethr_cond_init_opt(cnd, NULL);
}

int
ethr_cond_destroy(ethr_cond *cnd)
{
    DeleteCriticalSection(&cnd->cs);
    return 0;
}

#else
#error "No mutex implementation found"
#endif

/* -- Exported symbols of inline functions --------------------------------- */

int
ethr_mutex_trylock(ethr_mutex *mtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(mtx);
    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

    return ethr_mutex_trylock__(mtx);
}

void
ethr_mutex_lock(ethr_mutex *mtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(mtx);
    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

    ethr_mutex_lock__(mtx);
}

void
ethr_mutex_unlock(ethr_mutex *mtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(mtx);
    ETHR_ASSERT(mtx->initialized == ETHR_MUTEX_INITIALIZED);

    ethr_mutex_unlock__(mtx);
}


#ifdef ETHR_USE_OWN_RWMTX_IMPL__
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Read/Write Mutex                                                          *
\*                                                                           */

static void
wake_readers(ethr_rwmutex *rwmtx, int rs)
{
    ethr_ts_event *tse;
#ifdef ETHR_DEBUG
    int drs = 0;
#endif

    tse = rwmtx->mtxb.q;
    ETHR_ASSERT(tse);
    ETHR_ASSERT(rwmtx->rq_end);
    dequeue(&rwmtx->mtxb.q, tse, rwmtx->rq_end);
    rwmtx->rq_end->next = NULL;
    rwmtx->rq_end = NULL;

    ETHR_ASSERT(!rwmtx->mtxb.q
		|| (ethr_atomic32_read(&rwmtx->mtxb.q->uaflgs)
		    == ETHR_RWMTX_W_WAIT_FLG__));

    ETHR_RWMTX_HARD_DEBUG_CHK_Q(rwmtx);
    ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);

    while (tse) {
	ethr_ts_event *tse_next;

#ifdef ETHR_DEBUG
	ETHR_ASSERT(tse->uflgs == ETHR_RWMTX_R_WAIT_FLG__);
	ETHR_ASSERT(ethr_atomic32_read(&tse->uaflgs)
		    == ETHR_RWMTX_R_WAIT_FLG__);
	drs++;
#endif

	tse_next = tse->next; /* we aren't allowed to read tse->next
				 after we have reset uaflgs */

	ethr_atomic32_set(&tse->uaflgs, 0);
	ethr_event_set(&tse->event);
	tse = tse_next;
    }

    ETHR_ASSERT(rs == drs);
}

static ETHR_INLINE int
is_w_waiter(ethr_ts_event *tse)
{
    ETHR_ASSERT(tse->uflgs == ETHR_RWMTX_W_WAIT_FLG__
		|| tse->uflgs == ETHR_RWMTX_R_WAIT_FLG__);
    return tse->uflgs == ETHR_RWMTX_W_WAIT_FLG__;
}

static ETHR_INLINE int
multiple_w_waiters(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(rwmtx->mtxb.q);
    ETHR_ASSERT(rwmtx->mtxb.q->uflgs == ETHR_RWMTX_W_WAIT_FLG__);

    if (!rwmtx->rq_end)
	return rwmtx->mtxb.q->next != rwmtx->mtxb.q;
    else {
	ETHR_ASSERT(rwmtx->mtxb.q->next != rwmtx->mtxb.q);
	if (rwmtx->mtxb.q->next->uflgs == ETHR_RWMTX_W_WAIT_FLG__)
	    return 1;
	ETHR_ASSERT(rwmtx->rq_end->next == rwmtx->mtxb.q
		    || rwmtx->rq_end->next->uflgs == ETHR_RWMTX_W_WAIT_FLG__);
	return rwmtx->rq_end->next != rwmtx->mtxb.q;
    }
}

int check_readers_array(ethr_rwmutex *rwmtx,
			int start_rix,
			int length)
{
    int ix = start_rix;

    ETHR_MEMORY_BARRIER;

    do {
	ethr_sint32_t act = rwmutex_freqread_rdrs_read(rwmtx, ix);
	if (act != 0)
	    return EBUSY;
	ix++;
	if (ix == length)
	    ix = 0;
    } while (ix != start_rix);

    return 0;
}

static void
rwmutex_freqread_rdrs_dec_chk_wakeup(ethr_rwmutex *rwmtx,
				     ethr_ts_event *tse,
				     ethr_sint32_t initial)
{
    ethr_sint32_t act = initial;

    if ((act & (ETHR_RWMTX_W_FLG__|
		ETHR_RWMTX_R_ABRT_UNLCK_FLG__)) == 0) {
	if ((act & ETHR_RWMTX_WAIT_FLGS__) == 0) {
	    if (act & ETHR_RWMTX_R_PEND_UNLCK_MASK__) {
		/*
		 * We *need* to try to complete the runlock.
		 * A writer that just enqueued (not seen by us
		 * in flag field) may depend on someone else
		 * completing the runlock. We just took over
		 * that responsibilty since we modified reader
		 * groups.
		 */
		rwmutex_try_complete_runlock(rwmtx, act, tse, 1, 0, 0);
	    }
	}
	else if ((act & ETHR_RWMTX_WAIT_FLGS__) == ETHR_RWMTX_R_WAIT_FLG__)
	    rwmutex_transfer_read_lock(rwmtx, act, 0);
	else if ((act & ETHR_RWMTX_WAIT_FLGS__) == ETHR_RWMTX_W_WAIT_FLG__)
	    rwmutex_try_complete_runlock(rwmtx, act, tse, 1, 0, 0);
	else {
	    /*
	     * Don't know if we got readers or writers
	     * first in queue; need to peek
	     */
	    ETHR_MTX_Q_LOCK(&rwmtx->mtxb.qlck);
	    if (!rwmtx->mtxb.q)
		ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);
	    else if (is_w_waiter(rwmtx->mtxb.q)) {
		act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
		ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);
		if ((act & ETHR_RWMTX_W_FLG__) == 0)
		    rwmutex_try_complete_runlock(rwmtx, act, tse, 1, 0, 0);
	    }
	    else {
		/*
		 * rwmutex_transfer_read_lock() will
		 * unlock Q lock.
		 */
		act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
		if (act & ETHR_RWMTX_W_FLG__)
		    ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);
		else
		    rwmutex_transfer_read_lock(rwmtx, act, 1);
	    }
	}
    }
}

static void
rwmutex_freqread_restore_failed_tryrlock(ethr_rwmutex *rwmtx,
					 ethr_ts_event *tse)
{
    ethr_sint32_t act;
    /*
     * Restore failed increment
     */
    act = rwmutex_freqread_rdrs_dec_read(rwmtx, tse);

    ETHR_MEMORY_BARRIER;

    if (act == 0) {
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	rwmutex_freqread_rdrs_dec_chk_wakeup(rwmtx, tse, act);
    }
}

static int
rwmutex_try_complete_runlock(ethr_rwmutex *rwmtx,
			     ethr_sint32_t initial,
			     ethr_ts_event *tse,
			     int start_next_ix,
			     int check_before_try,
			     int try_write_lock)
{
    ethr_ts_event *tse_tmp;
    ethr_sint32_t act = initial;
    int six, res, length;

    ETHR_ASSERT((act & ETHR_RWMTX_W_FLG__) == 0);

    if (act & ETHR_RWMTX_R_ABRT_UNLCK_FLG__)
	return try_write_lock ? EBUSY : 0;

    tse_tmp = tse;
    if (!tse_tmp)
	tse_tmp = ethr_get_ts_event();

    if ((act & ETHR_RWMTX_WAIT_FLGS__) && (act & ~ETHR_RWMTX_WAIT_FLGS__) == 0)
	goto check_waiters;

    if (rwmtx->type == ETHR_RWMUTEX_TYPE_FREQUENT_READ) {
	length = reader_groups_array_size;
	six = tse_tmp->rgix;
    }
    else {
	length = main_threads_array_size;
	six = tse_tmp->mtix;
    }
    if (start_next_ix) {
	six++;
	if (six >= length)
	    six = 0;
    }

    if (!tse)
	ethr_leave_ts_event(tse_tmp);

    if (check_before_try) {
	res = check_readers_array(rwmtx, six, length);

	ETHR_MEMORY_BARRIER;

	if (res == EBUSY)
	    return try_write_lock ? EBUSY : 0;
    }

 restart:

    while (1) {
	ethr_sint32_t exp = act;
	ethr_sint32_t new = act+1;

	ETHR_ASSERT((act & ETHR_RWMTX_R_ABRT_UNLCK_FLG__) == 0);

	ETHR_ASSERT((act & ETHR_RWMTX_R_PEND_UNLCK_MASK__)
		    < ETHR_RWMTX_R_PEND_UNLCK_MASK__);

	act = ethr_atomic32_cmpxchg(&rwmtx->mtxb.flgs, new, exp);
	if (exp == act) {
	    act = new;
	    break;
	}

	if (!try_write_lock) {
	    if (act == 0 || (act & (ETHR_RWMTX_W_FLG__
				    | ETHR_RWMTX_R_ABRT_UNLCK_FLG__)))
		return 0;
	    if ((act & ETHR_RWMTX_WAIT_FLGS__) == 0) {
		if ((act & ETHR_RWMTX_R_FLG__) == 0)
		    return 0;
	    }
	    else if ((act & ETHR_RWMTX_R_FLG__) == 0) {
		if (act & ETHR_RWMTX_R_PEND_UNLCK_MASK__)
		    return 0;
		goto check_waiters;
	    }
	}
	else {
	    if (act == 0)
		goto tryrwlock;
	    if (act & (ETHR_RWMTX_W_FLG__
		       | ETHR_RWMTX_R_ABRT_UNLCK_FLG__))
		return EBUSY;
	}
    }

    res = check_readers_array(rwmtx, six, length);

    ETHR_MEMORY_BARRIER;

    ETHR_ASSERT((act & ETHR_RWMTX_W_FLG__) == 0);

    while (1) {
	int finished_abort = 0;
	ethr_sint32_t exp = act;
	ethr_sint32_t new = act;

	new--;
	if (act & ETHR_RWMTX_R_ABRT_UNLCK_FLG__) {
	    if ((new & ETHR_RWMTX_R_PEND_UNLCK_MASK__) == 0) {
		new &= ~ETHR_RWMTX_R_ABRT_UNLCK_FLG__;
		finished_abort = 1;
	    }
	    ETHR_ASSERT(act & ETHR_RWMTX_R_FLG__);
	}
	else if ((act & ETHR_RWMTX_R_FLG__) && res != EBUSY) {
	    new &= ~ETHR_RWMTX_R_FLG__;
	}

	ETHR_ASSERT(act & ETHR_RWMTX_R_PEND_UNLCK_MASK__);

	act = ethr_atomic32_cmpxchg(&rwmtx->mtxb.flgs, new, exp);
	if (exp == act) {
	    act = new;
	    if (act & ETHR_RWMTX_W_FLG__)
		return try_write_lock ? EBUSY : 0;
	    if (finished_abort && (act & ETHR_RWMTX_WAIT_FLGS__))
		goto restart;
	    if (act & (ETHR_RWMTX_R_FLG__
		       | ETHR_RWMTX_R_ABRT_UNLCK_FLG__
		       | ETHR_RWMTX_R_PEND_UNLCK_MASK__))
		return try_write_lock ? EBUSY : 0;
	    /* Read unlock completed */
	    break;
	}
    }

    /*
     * Read unlock completed, but we have to check if
     * threads have to be woken (or if we should try
     * to write lock it).
     */

    if (act & ETHR_RWMTX_WAIT_FLGS__) {
    check_waiters:
	rwmutex_unlock_wake(rwmtx, 0, act, 0);
	return try_write_lock ? EBUSY : 0;
    }

    if (!try_write_lock)
	return 0;

 tryrwlock:
    /* Try to write lock it */

    act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs,
				     ETHR_RWMTX_W_FLG__,
				     0);
    return act == 0 ? 0 : EBUSY;
}

#ifdef ETHR_RLOCK_WITH_INC_DEC

static ETHR_INLINE void
rwmutex_incdec_restore_failed_tryrlock(ethr_rwmutex *rwmtx)
{
    ethr_sint32_t act;
    /*
     * Restore failed increment
     */
    act = ethr_atomic32_dec_read(&rwmtx->mtxb.flgs);
    if ((act & ETHR_RWMTX_WAIT_FLGS__)
	&& (act & ~ETHR_RWMTX_WAIT_FLGS__) == 0) {
	rwmutex_unlock_wake(rwmtx, 0, act, 0);
    }
}

#endif

static void
rwmutex_normal_rlock_wait(ethr_rwmutex *rwmtx, ethr_sint32_t initial)
{
    ethr_sint32_t act = initial, exp;
    int scnt, start_scnt;
    ethr_ts_event *tse = NULL;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;

    start_scnt = scnt = initial_spincount(&rwmtx->mtxb);

    /*
     * Spin trying to read lock for a while. If unsuccessful,
     * wait on event.
     */

    while (1) {

#ifdef ETHR_RLOCK_WITH_INC_DEC
	rwmutex_incdec_restore_failed_tryrlock(rwmtx);
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
#endif

	while (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) {
	    if (scnt <= 0) {
		tse = ethr_get_ts_event();
		if (update_spincount(&rwmtx->mtxb, tse, &start_scnt, &scnt)) {
		    event_wait(&rwmtx->mtxb, tse, scnt,
			       ETHR_RWMTX_R_WAIT_FLG__, 1, 0);
		    goto done;
		}
	    }
	    ETHR_SPIN_BODY;
	    if (--until_yield == 0) {
		until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
		(void) ETHR_YIELD();
	    }
	    act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	    scnt--;
	}
	exp = act;

#ifdef ETHR_RLOCK_WITH_INC_DEC
	act = ethr_atomic32_inc_read(&rwmtx->mtxb.flgs);
	if ((act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) == 0)
	    goto done; /* Got it */
#else
	act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs, exp+1, exp);
	if (act == exp)
	    goto done; /* Got it */
#endif
    }

 done:
    if (tse)
	ethr_leave_ts_event(tse);
}

static void
rwmutex_freqread_rlock_wait(ethr_rwmutex *rwmtx,
			    ethr_ts_event *tse);

static int
rwmutex_freqread_rlock(ethr_rwmutex *rwmtx, ethr_ts_event *tse, int trylock)
{
    int res = 0;
    ethr_sint32_t act;

    rwmutex_freqread_rdrs_inc(rwmtx, tse);

    ETHR_MEMORY_BARRIER;

    act = ethr_atomic32_read_acqb(&rwmtx->mtxb.flgs);

    if (act != ETHR_RWMTX_R_FLG__) {
	int wake_other_readers;

	while (1) {
	    ethr_sint32_t exp, new;

	    wake_other_readers = 0;

	    if (act == 0)
		new = act | ETHR_RWMTX_R_FLG__;
	    else if (act == ETHR_RWMTX_R_FLG__)
		break; /* Got it */
	    else if (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) {
		rwmutex_freqread_restore_failed_tryrlock(rwmtx, tse);
		if (trylock)
		    res = EBUSY;
		else
		    rwmutex_freqread_rlock_wait(rwmtx, tse);
		break;
	    }
	    else if (act & ETHR_RWMTX_R_ABRT_UNLCK_FLG__) {
		if ((act & ETHR_RWMTX_R_FLG__) == 0)
		    ETHR_FATAL_ERROR__(EFAULT);
		/*
		 * An aborted runlock, not write locked, and no write
		 * waiters, i.e., we got it...
		 */
		if (act & ETHR_RWMTX_R_WAIT_FLG__)
		    wake_other_readers = 1;
		break;
	    }
	    else {
		new = act | ETHR_RWMTX_R_FLG__;
		if (act & ETHR_RWMTX_R_PEND_UNLCK_MASK__) {
		    /*
		     * Someone is doing tryrwlock (no writer and no
		     * write waiters); we will try to abort that...
		     */
		    new |= ETHR_RWMTX_R_ABRT_UNLCK_FLG__;
		}

		if (act & ETHR_RWMTX_R_WAIT_FLG__)
		    wake_other_readers = 1;
	    }

	    exp = act;
	    act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs, new, exp);
	    if (act == exp)
		break;
	}

	if (wake_other_readers)
	    rwmutex_transfer_read_lock(rwmtx, act, 0);
    }

    return res;
}

static void
rwmutex_freqread_rlock_wait(ethr_rwmutex *rwmtx,
			    ethr_ts_event *tse)
{
    ethr_sint32_t act;
    int scnt, start_scnt;
    int until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;

    start_scnt = scnt = initial_spincount(&rwmtx->mtxb);

    /*
     * Spin trying to read lock for a while. If unsuccessful,
     * wait on event.
     */

    while (1) {

	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);

	while (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) {
	    if (scnt <= 0) {
		if (update_spincount(&rwmtx->mtxb, tse, &start_scnt, &scnt)) {
		    event_wait(&rwmtx->mtxb, tse, scnt,
			       ETHR_RWMTX_R_WAIT_FLG__, 1, 1);
		    return; /* Got it */
		}
	    }
	    ETHR_SPIN_BODY;
	    if (--until_yield == 0) {
		until_yield = ETHR_YIELD_AFTER_BUSY_LOOPS;
		(void) ETHR_YIELD();
	    }
	    act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	    scnt--;
	}

	if (rwmutex_freqread_rlock(rwmtx, tse, 1) != EBUSY)
	    break; /* Got it */
    }
}

static void
rwmutex_normal_rwlock_wait(ethr_rwmutex *rwmtx, ethr_sint32_t initial)
{
    write_lock_wait(&rwmtx->mtxb, initial, 1, 0);
}

static void
rwmutex_freqread_rwlock_wait(ethr_rwmutex *rwmtx, ethr_sint32_t initial)
{
    write_lock_wait(&rwmtx->mtxb, initial, 1, 1);
}

static ETHR_INLINE void
rwlock_wake_set_flags(ethr_rwmutex *rwmtx,
		      ethr_sint32_t new_initial,
		      ethr_sint32_t act_initial)
{
    ethr_sint32_t act, act_mask;
    int chk_abrt_flg;

    ETHR_MEMORY_BARRIER;

    if (rwmtx->type != ETHR_RWMUTEX_TYPE_NORMAL) {
	/* r pend unlock mask may vary and must be retained */
	act_mask = ETHR_RWMTX_R_PEND_UNLCK_MASK__;
	if (new_initial & ETHR_RWMTX_R_FLG__)
	    chk_abrt_flg = 1;
	else
	    chk_abrt_flg = 0;
    }
    else {
#ifdef ETHR_RLOCK_WITH_INC_DEC
	/* rs mask may vary and must be retained */
	act_mask = ETHR_RWMTX_RS_MASK__;
	chk_abrt_flg = 0;
#else
	/* rs mask always zero */
	ETHR_ASSERT((act_initial & ETHR_RWMTX_RS_MASK__) == 0);
	ethr_atomic32_set(&rwmtx->mtxb.flgs, new_initial);
	return;
#endif
    }

    act = act_initial;
    while (1) {
	ethr_sint32_t exp = act;
	ethr_sint32_t new = new_initial + (act & act_mask);
	if (chk_abrt_flg && (act & act_mask))
	    new |= ETHR_RWMTX_R_ABRT_UNLCK_FLG__;
	act = ethr_atomic32_cmpxchg(&rwmtx->mtxb.flgs, new, exp);
	if (act == exp)
	    break;
	exp = act;
    }
}

#ifdef ETHR_DEBUG

static void
dbg_unlock_wake(ethr_rwmutex *rwmtx,
		int have_w,
		ethr_ts_event *tse)
{
    ethr_sint32_t exp, act, imask;

    exp = have_w ? ETHR_RWMTX_W_FLG__ : 0;

    if (rwmtx->type != ETHR_RWMUTEX_TYPE_NORMAL)
	imask = ETHR_RWMTX_R_PEND_UNLCK_MASK__|ETHR_RWMTX_R_ABRT_UNLCK_FLG__;
    else {
#ifdef ETHR_RLOCK_WITH_INC_DEC
	imask = ETHR_RWMTX_RS_MASK__;
#else
	imask = 0;
#endif
    }

    ETHR_ASSERT(tse);

    if (is_w_waiter(tse)) {

	exp |= ETHR_RWMTX_W_WAIT_FLG__;
	if (rwmtx->rq_end) {
	    exp |= ETHR_RWMTX_R_WAIT_FLG__;
	}
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	ETHR_ASSERT((exp & ~imask) == (act & ~imask));

	ETHR_RWMTX_HARD_DEBUG_CHK_Q(rwmtx);

    }
    else {

	exp |= ETHR_RWMTX_R_WAIT_FLG__;
	if (rwmtx->rq_end->next != rwmtx->mtxb.q)
	    exp |= ETHR_RWMTX_W_WAIT_FLG__;
	else if (exp == ETHR_RWMTX_R_WAIT_FLG__) {
	    if (!have_w) {
		if (rwmtx->type != ETHR_RWMUTEX_TYPE_NORMAL)
		    imask |= ETHR_RWMTX_R_FLG__;
		else
		    imask |= ETHR_RWMTX_RS_MASK__;
	    }
	}
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	ETHR_ASSERT((exp & ~imask) == (act & ~imask));

	ETHR_RWMTX_HARD_DEBUG_CHK_Q(rwmtx);

    }
}

#endif

static void
rwmutex_transfer_read_lock(ethr_rwmutex *rwmtx,
			   ethr_sint32_t initial,
			   int q_locked)
{
    ethr_sint32_t act = initial;

    if (!q_locked) {
	ethr_ts_event *tse;
	ETHR_ASSERT(initial & ETHR_RWMTX_R_WAIT_FLG__);
	ETHR_ASSERT((initial & ETHR_RWMTX_W_FLG__) == 0);
	ETHR_MTX_Q_LOCK(&rwmtx->mtxb.qlck);

	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	tse = rwmtx->mtxb.q;
	if ((act & ETHR_RWMTX_W_FLG__) || !tse || is_w_waiter(tse)) {
	    /* Someone else woke the readers up... */
	    ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);
	    return;
	}
    }

    rwmutex_unlock_wake(rwmtx, 0, initial, 1);
}

static void
rwmutex_unlock_wake(ethr_rwmutex *rwmtx, int have_w, ethr_sint32_t initial,
		    int transfer_read_lock)
{
    ethr_sint32_t new, act = initial;
    ethr_ts_event *tse;

    if (transfer_read_lock) {
	/*
	 * - Q already locked
	 * - Got R waiters first in Q
	 * - Not W locked
	 */
	tse = rwmtx->mtxb.q;

	ETHR_ASSERT(act & ETHR_RWMTX_R_WAIT_FLG__);
	ETHR_ASSERT((act & (ETHR_RWMTX_W_FLG__)) == 0);
	ETHR_ASSERT(tse && !is_w_waiter(tse));
    }
    else {

	if ((act & ETHR_RWMTX_WAIT_FLGS__) == 0) {
	    if (!have_w)
		return;
	    else {
		while ((act & ETHR_RWMTX_WAIT_FLGS__) == 0) {
		    ethr_sint32_t exp = act;
		    new = exp & ~ETHR_RWMTX_W_FLG__;
		    act = ethr_atomic32_cmpxchg(&rwmtx->mtxb.flgs, new, exp);
		    if (act == exp)
			return;
		}
	    }
	}

	ETHR_MTX_Q_LOCK(&rwmtx->mtxb.qlck);
	tse = rwmtx->mtxb.q;

	if (!have_w) {
	    if (!tse) {
#ifdef ETHR_DEBUG
		act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
		ETHR_ASSERT((act & ETHR_RWMTX_WAIT_FLGS__) == 0);
#endif
		goto already_served;
	    }
	    act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	    if (act == (ETHR_RWMTX_R_WAIT_FLG__|ETHR_RWMTX_R_FLG__)) {
		ETHR_ASSERT(tse && !is_w_waiter(tse));
	    }
	    else if (act & ~ETHR_RWMTX_WAIT_FLGS__) {
	    already_served:
		ETHR_MTX_Q_UNLOCK(&rwmtx->mtxb.qlck);
		return;
	    }
	}
    }

#ifdef ETHR_DEBUG
    dbg_unlock_wake(rwmtx, have_w, tse);
#endif

    if (is_w_waiter(tse)) {

	if (!have_w) {
	    act = ethr_atomic32_read_bor(&rwmtx->mtxb.flgs,
				       ETHR_RWMTX_W_FLG__);
	    ETHR_ASSERT((act & ~(ETHR_RWMTX_WAIT_FLGS__
				 | (rwmtx->type == ETHR_RWMUTEX_TYPE_NORMAL
				    ? 0
				    : ETHR_RWMTX_R_PEND_UNLCK_MASK__))) == 0);
	    ETHR_ASSERT(act & ETHR_RWMTX_W_WAIT_FLG__);
	    act |= ETHR_RWMTX_W_FLG__;
	}

	/*
	 * If we have multiple write waiters, there
	 * is no need to modify mtxb->flgs; otherwise,
	 * we need to clear the write wait bit...
	 */
	if (!multiple_w_waiters(rwmtx)) {
	    new = ETHR_RWMTX_W_FLG__;
	    if (tse->next != rwmtx->mtxb.q) {
		ETHR_ASSERT(tse->next->uflgs == ETHR_RWMTX_R_WAIT_FLG__);
		new |= ETHR_RWMTX_R_WAIT_FLG__;
	    }

	    rwlock_wake_set_flags(rwmtx, new, act);
	}

	wake_writer(&rwmtx->mtxb, 1);
    }
    else {
	int rs;

	if (rwmtx->type == ETHR_RWMUTEX_TYPE_NORMAL) {
	    rs = rwmtx->tdata.rs;
	    new = (ethr_sint32_t) rs;
	    rwmtx->tdata.rs = 0;
	}
	else {
	    ethr_rwmutex_type type = rwmtx->type;
	    int length = (type == ETHR_RWMUTEX_TYPE_FREQUENT_READ
			  ? reader_groups_array_size
			  : main_threads_array_size);
	    int ix;

	    rs = 0;
	    for (ix = 0; ix < length; ix++) {
		int wrs = rwmtx->tdata.ra[ix].data.waiting_readers;
		rwmtx->tdata.ra[ix].data.waiting_readers = 0;
		ETHR_ASSERT(wrs >= 0);
		if (wrs) {
		    rs += wrs;
		    rwmutex_freqread_rdrs_add(rwmtx, type, ix, wrs);
		}
	    }

	    new = ETHR_RWMTX_R_FLG__;
	}

	if (rwmtx->rq_end->next != rwmtx->mtxb.q)
	    new |= ETHR_RWMTX_W_WAIT_FLG__;

	rwlock_wake_set_flags(rwmtx, new, act);

	wake_readers(rwmtx, rs);
    }
}

static ethr_rwmtx_readers_array__ *
alloc_readers_array(int length, ethr_rwmutex_lived lived)
{
    ethr_rwmtx_readers_array__ *ra;
    size_t sz;
    void *mem;

    sz = sizeof(ethr_rwmtx_readers_array__) * (length + 1);

    switch (lived) {
    case ETHR_RWMUTEX_LONG_LIVED:
	mem = ethr_mem__.ll.alloc(sz);
	break;
    case ETHR_RWMUTEX_SHORT_LIVED:
	mem = ethr_mem__.sl.alloc(sz);
	break;
    default:
	mem = ethr_mem__.std.alloc(sz);
	break;
    }
    if (!mem)
	return NULL;

    if ((((ethr_uint_t) mem) & ETHR_CACHE_LINE_MASK) == 0) {
	ra = (ethr_rwmtx_readers_array__ *) mem;
	ra->data.byte_offset = 0;
    }
    else {
	ra = ((ethr_rwmtx_readers_array__ *)
	      ((((ethr_uint_t) mem) & ~ETHR_CACHE_LINE_MASK)
	       + ETHR_CACHE_LINE_SIZE));
	ra->data.byte_offset = (int) ((ethr_uint_t) ra
				      - (ethr_uint_t) mem);
    }
    ra->data.lived = lived;
    return ra;
}

static void
free_readers_array(ethr_rwmtx_readers_array__ *ra)
{
    void *ptr = (void *) (((char *) ra) - ra->data.byte_offset);
    switch (ra->data.lived) {
    case ETHR_RWMUTEX_LONG_LIVED:
	ethr_mem__.ll.free(ptr);
	break;
    case ETHR_RWMUTEX_SHORT_LIVED:
	ethr_mem__.sl.free(ptr);
	break;
    default:
	ethr_mem__.std.free(ptr);
	break;
    }
}

int
ethr_rwmutex_init_opt(ethr_rwmutex *rwmtx, ethr_rwmutex_opt *opt)
{
    int res;
    ethr_rwmtx_readers_array__ *ra = NULL;
#if ETHR_XCHK
    if (ethr_not_completely_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!rwmtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    rwmtx->initialized = ETHR_RWMUTEX_INITIALIZED;
#endif
    ETHR_MTX_HARD_DEBUG_FENCE_INIT(rwmtx);
    rwmtx->rq_end = NULL;
    rwmtx->type = opt ? opt->type : ETHR_RWMUTEX_TYPE_NORMAL;
    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
	if (main_threads_array_size <= reader_groups_array_size) {
	    /* No point using reader groups... */
	    rwmtx->type = ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ;
	}
	/* Fall through */
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ: {
	int length;

	length = (rwmtx->type == ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ
		  ? main_threads_array_size
		  : reader_groups_array_size);

	if (length == 1) {
	    /* No point using a frequent reader type... */
	    rwmtx->type = ETHR_RWMUTEX_TYPE_NORMAL;
	}
	else {
	    int ix;
	    ra = alloc_readers_array(length,
				     (opt
				      ? opt->lived
				      : ETHR_RWMUTEX_UNKNOWN_LIVED));
	    if (!ra) {
		res = ENOMEM;
		goto error;
	    }

	    rwmtx->tdata.ra = ra;

	    for (ix = 0; ix < length; ix++) {
		ethr_atomic32_init(&rwmtx->tdata.ra[ix].data.readers, 0);
		rwmtx->tdata.ra[ix].data.waiting_readers = 0;
	    }
	    break;
	}
    }
    case ETHR_RWMUTEX_TYPE_NORMAL:
	rwmtx->tdata.rs = 0;
	break;
    default:
	res = EINVAL;
	goto error;
    }
    res = mtxb_init(&rwmtx->mtxb,
		    default_rwmtx_main_spincount,
		    opt ? opt->main_spincount : -1,
		    default_rwmtx_aux_spincount,
		    opt ? opt->aux_spincount : -1);
    if (res == 0)
	return 0;

 error:

    if (ra)
	free_readers_array(ra);

#if ETHR_XCHK
    rwmtx->initialized = 0;
#endif
    return res;
}

int
ethr_rwmutex_init(ethr_rwmutex *rwmtx)
{
    return ethr_rwmutex_init_opt(rwmtx, NULL);
}

int
ethr_rwmutex_destroy(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
    if (rwmtx->type != ETHR_RWMUTEX_TYPE_NORMAL) {
	ethr_sint32_t act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	if (act == ETHR_RWMTX_R_FLG__)
	    rwmutex_try_complete_runlock(rwmtx, act, NULL, 0, 0, 0);
    }
    res = mtxb_destroy(&rwmtx->mtxb);
    if (res != 0)
	return res;
    if (rwmtx->type != ETHR_RWMUTEX_TYPE_NORMAL)
	free_readers_array(rwmtx->tdata.ra);
#if ETHR_XCHK
    rwmtx->initialized = 0;
#endif
    return 0;
}

#define ETHR_MAX_TRYRLOCK_TRIES 5

int
ethr_rwmutex_tryrlock(ethr_rwmutex *rwmtx)
{
    int res = 0;
    ethr_sint32_t act;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL: {
#ifdef ETHR_RLOCK_WITH_INC_DEC
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	if (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__))
	    res = EBUSY;
	else {
	    act = ethr_atomic32_inc_read_acqb(&rwmtx->mtxb.flgs);
	    if (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) {
		rwmutex_incdec_restore_failed_tryrlock(rwmtx);
		res = EBUSY;
	    }
	}
#else
	ethr_sint32_t exp = 0;
	int tries = 0;

	while (1) {
	    act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs, exp+1, exp);
	    if (act == exp) {
		res = 0;
		break;
	    }
	    if (tries > ETHR_MAX_TRYRLOCK_TRIES
		|| (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__))) {
		res = EBUSY;
		break;
	    }
	    tries++;
	    exp = act;
	}
#endif
	break;
    }

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ: {
	ethr_ts_event *tse = ethr_get_ts_event();
	res = rwmutex_freqread_rlock(rwmtx, tse, 1);
	ethr_leave_ts_event(tse);
	break;
    }
    }

#ifdef ETHR_MTX_CHK_EXCL
    if (res == 0) {
	ETHR_MTX_CHK_EXCL_SET_NON_EXCL(&rwmtx->mtxb);
	ETHR_MTX_CHK_EXCL_IS_NOT_EXCL(&rwmtx->mtxb);
    }
#endif

    ETHR_MTX_HARD_DEBUG_LFS_TRYRLOCK(&rwmtx->mtxb, res);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
    return res;
}

void
ethr_rwmutex_rlock(ethr_rwmutex *rwmtx)
{
    ethr_sint32_t act;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL: {
#ifdef ETHR_RLOCK_WITH_INC_DEC
	act = ethr_atomic32_inc_read_acqb(&rwmtx->mtxb.flgs);
	if (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__))
	    rwmutex_normal_rlock_wait(rwmtx, act);
#else
	ethr_sint32_t exp = 0;

	while (1) {
	    act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs, exp+1, exp);
	    if (act == exp)
		break;

	    if (act & (ETHR_RWMTX_W_FLG__|ETHR_RWMTX_W_WAIT_FLG__)) {
		rwmutex_normal_rlock_wait(rwmtx, act);
		break;
	    }
	    exp = act;
	}
#endif
	break;
    }

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ: {
	ethr_ts_event *tse = ethr_get_ts_event();
	rwmutex_freqread_rlock(rwmtx, tse, 0);
	ethr_leave_ts_event(tse);
	break;
    }
    }

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
    ETHR_MTX_CHK_EXCL_SET_NON_EXCL(&rwmtx->mtxb);
    ETHR_MTX_CHK_EXCL_IS_NOT_EXCL(&rwmtx->mtxb);
    ETHR_MTX_HARD_DEBUG_LFS_RLOCK(&rwmtx->mtxb);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
}

void
ethr_rwmutex_runlock(ethr_rwmutex *rwmtx)
{
    ethr_sint32_t act;

    ETHR_MTX_CHK_EXCL_IS_NOT_EXCL(&rwmtx->mtxb);
    ETHR_MTX_CHK_EXCL_UNSET_NON_EXCL(&rwmtx->mtxb);
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
    ETHR_MTX_HARD_DEBUG_LFS_RUNLOCK(&rwmtx->mtxb);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL:
	act = ethr_atomic32_dec_read_relb(&rwmtx->mtxb.flgs);
	if ((act & ETHR_RWMTX_WAIT_FLGS__)
	    && (act & ~ETHR_RWMTX_WAIT_FLGS__) == 0) {
	    ETHR_ASSERT((act & ETHR_RWMTX_W_FLG__) == 0);
	    rwmutex_unlock_wake(rwmtx, 0, act, 0);
	}
	break;

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ: {
	ethr_ts_event *tse = ethr_get_ts_event();

	act = rwmutex_freqread_rdrs_dec_read_relb(rwmtx, tse);

	ETHR_ASSERT(act >= 0);

	ETHR_MEMORY_BARRIER;

	if (act == 0) {
	    act = ethr_atomic32_read(&rwmtx->mtxb.flgs);
	    if (act != ETHR_RWMTX_R_FLG__)
		rwmutex_freqread_rdrs_dec_chk_wakeup(rwmtx, tse, act);
	}

	ethr_leave_ts_event(tse);
	break;
    }
    }

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
}

int
ethr_rwmutex_tryrwlock(ethr_rwmutex *rwmtx)
{
    int res = 0;
    ethr_sint32_t act;

    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL:
	act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs,
					 ETHR_RWMTX_W_FLG__, 0);
	if (act != 0)
	    res = EBUSY;
	break;

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ:

	res = 0;
	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);

	do {

	    if (act == 0)
		act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs,
						 ETHR_RWMTX_W_FLG__, 0);
	    else if (act == ETHR_RWMTX_R_FLG__) {
		res = rwmutex_try_complete_runlock(rwmtx, act, NULL,
						   0, 1, 1);
		break;
	    }
	    else {
		res = EBUSY;
		break;
	    }

	} while (act != 0);

	break;
    }

#ifdef ETHR_MTX_CHK_EXCL
    if (res == 0) {
	ETHR_MTX_CHK_EXCL_SET_EXCL(&rwmtx->mtxb);
	ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(&rwmtx->mtxb);
    }
#endif

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
    ETHR_MTX_HARD_DEBUG_LFS_TRYRWLOCK(&rwmtx->mtxb, res);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    return res;
}

void
ethr_rwmutex_rwlock(ethr_rwmutex *rwmtx)
{
    ethr_sint32_t act;
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL:
	act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs,
					 ETHR_RWMTX_W_FLG__, 0);
	if (act != 0)
	    rwmutex_normal_rwlock_wait(rwmtx, act);
	break;

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ:

	act = ethr_atomic32_read(&rwmtx->mtxb.flgs);

	do {

	    if (act != 0) {
		rwmutex_freqread_rwlock_wait(rwmtx, act);
		break;
	    }

	    act = ethr_atomic32_cmpxchg_acqb(&rwmtx->mtxb.flgs,
					     ETHR_RWMTX_W_FLG__, 0);

	} while (act != 0);

	break;
    }

    ETHR_MTX_CHK_EXCL_SET_EXCL(&rwmtx->mtxb);
    ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(&rwmtx->mtxb);
    ETHR_MTX_HARD_DEBUG_LFS_RWLOCK(&rwmtx->mtxb);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

}

void
ethr_rwmutex_rwunlock(ethr_rwmutex *rwmtx)
{
    ethr_sint32_t act;
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
    ETHR_MTX_HARD_DEBUG_LFS_RWUNLOCK(&rwmtx->mtxb);

    ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(&rwmtx->mtxb);
    ETHR_MTX_CHK_EXCL_UNSET_EXCL(&rwmtx->mtxb);

    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);

    switch (rwmtx->type) {
    case ETHR_RWMUTEX_TYPE_NORMAL:
	act = ethr_atomic32_cmpxchg_relb(&rwmtx->mtxb.flgs,
					 0, ETHR_RWMTX_W_FLG__);
	if (act != ETHR_RWMTX_W_FLG__)
	    rwmutex_unlock_wake(rwmtx, 1, act, 0);
	break;

    case ETHR_RWMUTEX_TYPE_FREQUENT_READ:
    case ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ:
	act = ethr_atomic32_cmpxchg_relb(&rwmtx->mtxb.flgs, 0,
					 ETHR_RWMTX_W_FLG__);
	if (act != ETHR_RWMTX_W_FLG__)
	    rwmutex_unlock_wake(rwmtx, 1, act, 0);
	break;
    }

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(rwmtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(rwmtx);
}

#else
/* -- pthread read/write mutex --------------------------------------------- */

int
ethr_rwmutex_init(ethr_rwmutex *rwmtx)
{
#if ETHR_XCHK
    if (!rwmtx) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
    rwmtx->initialized = ETHR_RWMUTEX_INITIALIZED;
#endif
    return pthread_rwlock_init(&rwmtx->pt_rwlock, write_pref_attr);
}

int
ethr_rwmutex_init_opt(ethr_rwmutex *rwmtx, ethr_rwmutex_opt *opt)
{
    return ethr_rwmutex_init(rwmtx);
}

int
ethr_rwmutex_destroy(ethr_rwmutex *rwmtx)
{
    int res;
#if ETHR_XCHK
    if (ethr_not_inited__) {
	ETHR_ASSERT(0);
	return EACCES;
    }
    if (!rwmtx || rwmtx->initialized != ETHR_RWMUTEX_INITIALIZED) {
	ETHR_ASSERT(0);
	return EINVAL;
    }
#endif
    res = pthread_rwlock_destroy(&rwmtx->pt_rwlock);
#if ETHR_XCHK
    rwmtx->initialized = 0;
#endif
    return res;
}

/* -- Exported symbols of inline functions --------------------------------- */

int
ethr_rwmutex_tryrlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    return ethr_rwmutex_tryrlock__(rwmtx);
}

void
ethr_rwmutex_rlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ethr_rwmutex_rlock__(rwmtx);
}

void
ethr_rwmutex_runlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ethr_rwmutex_runlock__(rwmtx);
}

int
ethr_rwmutex_tryrwlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    return ethr_rwmutex_tryrwlock__(rwmtx);
}

void
ethr_rwmutex_rwlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    return ethr_rwmutex_rwlock__(rwmtx);
}

void
ethr_rwmutex_rwunlock(ethr_rwmutex *rwmtx)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(rwmtx);
    ETHR_ASSERT(rwmtx->initialized == ETHR_RWMUTEX_INITIALIZED);

    ethr_rwmutex_rwunlock__(rwmtx);
}

#endif /* pthread */


#if defined(ETHR_USE_OWN_RWMTX_IMPL__) || defined(ETHR_USE_OWN_MTX_IMPL__)

#ifdef ETHR_MTX_HARD_DEBUG_Q
static void
hard_debug_chk_q__(struct ethr_mutex_base_ *mtxb, int is_rwmtx)
{
    int res;
    ethr_sint32_t flgs = ethr_atomic32_read(&mtxb->flgs);

    ETHR_MTX_HARD_ASSERT(res == 0);

    ETHR_MTX_HARD_ASSERT(!(flgs & ETHR_RWMTX_R_WAIT_FLG__) || is_rwmtx);

    if (!(flgs & ETHR_RWMTX_WAIT_FLGS__)) {
	ETHR_MTX_HARD_ASSERT(!mtxb->q);
	if (is_rwmtx) {
	    ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	    ETHR_MTX_HARD_ASSERT(!rwmtx->rq_end);
	    ETHR_MTX_HARD_ASSERT(!rwmtx->rs);
	}
    }
    else {
	ethr_ts_event *tse;
	int ws = 0, rs = 0, rsf = 0, ref = 0;

	ETHR_MTX_HARD_ASSERT(mtxb->q);

	tse = mtxb->q;

	do {
	    ethr_sint32_t type;

	    ETHR_MTX_HARD_ASSERT(tse->next->prev == tse);
	    ETHR_MTX_HARD_ASSERT(tse->prev->next == tse);

	    type = ethr_atomic32_read(&tse->uaflgs);
	    ETHR_MTX_HARD_ASSERT(type == tse->uflgs);
	    switch (type) {
	    case ETHR_RWMTX_W_WAIT_FLG__:
		ws++;
		break;
	    case ETHR_RWMTX_R_WAIT_FLG__: {
		ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
		ETHR_MTX_HARD_ASSERT(is_rwmtx);
		if (!rsf)
		    rsf = 1;
		ETHR_MTX_HARD_ASSERT(!ref);
		if (rwmtx->rq_end == tse) {
		    ETHR_MTX_HARD_ASSERT(
			tse->next == rwmtx->mtxb.q
			|| tse->next->uflgs == ETHR_RWMTX_W_WAIT_FLG__);
		    ref = 1;
		}
		rs++;
		break;
	    }
	    default:
		ETHR_MTX_HARD_ASSERT(! "invalid wait type found");
	    }

	    tse = tse->next;
	} while (tse != mtxb->q);

	if (is_rwmtx) {
	    ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	    ETHR_MTX_HARD_ASSERT(rs == rwmtx->rs);
	}

#ifdef ETHR_MTX_HARD_DEBUG_WSQ
	ETHR_MTX_HARD_ASSERT(ws == mtxb->ws);
#endif

	if (flgs & ETHR_RWMTX_W_WAIT_FLG__)
	    ETHR_MTX_HARD_ASSERT(ws);
	else
	    ETHR_MTX_HARD_ASSERT(!ws);

	if (flgs & ETHR_RWMTX_R_WAIT_FLG__) {
	    ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	    ETHR_MTX_HARD_ASSERT(is_rwmtx);
	    ETHR_MTX_HARD_ASSERT(rwmtx->rq_end);
	    ETHR_MTX_HARD_ASSERT(rsf);
	    ETHR_MTX_HARD_ASSERT(ref);
	    ETHR_MTX_HARD_ASSERT(rs);
	}
	else {
	    if (is_rwmtx) {
		ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
		ETHR_MTX_HARD_ASSERT(!rwmtx->rq_end);
	    }
	    ETHR_MTX_HARD_ASSERT(!rsf);
	    ETHR_MTX_HARD_ASSERT(!ref);
	    ETHR_MTX_HARD_ASSERT(!rs);
	}
    }
}

#elif defined(ETHR_MTX_HARD_DEBUG_WSQ)

static void
hard_debug_chk_q__(struct ethr_mutex_base_ *mtxb, int is_rwmtx)
{
    int ws = 0;
    int rs = 0;

    if (mtxb->q) {
	ethr_ts_event *tse = mtxb->q;
	do {
	    switch (tse->uflgs) {
	    case ETHR_RWMTX_W_WAIT_FLG__:
		ws++;
		break;
	    case ETHR_RWMTX_R_WAIT_FLG__:
		rs++;
		break;
	    default:
		ETHR_MTX_HARD_ASSERT(0);
		break;
	    }
	    tse = tse->next;
	} while (tse != mtxb->q);
    }

    ETHR_MTX_HARD_ASSERT(mtxb->ws == ws);
    if (is_rwmtx) {
	ethr_rwmutex *rwmtx = (ethr_rwmutex *) mtxb;
	ETHR_MTX_HARD_ASSERT(rwmtx->rs == rs);
    }
}

#endif

#endif
