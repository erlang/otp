/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
 * Description: Thread progress information. Used by lock free algorithms
 *              to determine when all involved threads are guaranteed to
 *              have passed a specific point of execution.
 *
 *              Usage instructions below.
 *
 * Author: 	Rickard Green
 */

/*
 * ------ Usage instructions -----------------------------------------------
 *
 * This module keeps track of the progress of a set of managed threads. Only
 * threads that behave well can be allowed to be managed. A managed thread
 * should update its thread progress frequently. Currently only scheduler
 * threads, the system-message-dispatcher threads, and the aux-thread are
 * managed threads. We typically do not want any async threads as managed
 * threads since they cannot guarantee a frequent update of thread progress,
 * since they execute user implemented driver code that is assumed to be
 * time consuming.
 *
 * erts_thr_progress_current() returns the global current thread progress
 * value of managed threads. I.e., the latest progress value that all
 * managed threads have reached. Thread progress values are opaque.
 *
 * erts_thr_progress_has_reached(VAL) returns a value != 0 if current
 * global thread progress has reached or passed VAL.
 *
 * erts_thr_progress_later() returns a thread progress value in the future
 * which no managed thread have yet reached.
 *
 * All threads issue a full memory barrier when reaching a new thread
 * progress value. They only reach new thread progress values in specific
 * controlled states when calling erts_thr_progress_update(). Schedulers
 * call erts_thr_progress_update() in between execution of processes,
 * when going to sleep and when waking up.
 *
 * Sleeping managed threads are considered to have reached next thread
 * progress value immediately. They are not woken and do therefore not
 * issue any memory barriers when reaching a new thread progress value.
 * A sleeping thread do however immediately issue a memory barrier upon
 * wakeup.
 *
 * Both managed and registered unmanaged threads may request wakeup when
 * the global thread progress reach a certain value using
 * erts_thr_progress_wakeup().
 *
 * Note that thread progress values are opaque, and that you are only
 * allowed to use thread progress values retrieved from this API!
 *
 * -------------------------------------------------------------------------
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "erl_thr_progress.h"
#include "global.h"


#define ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE 0

#ifdef DEBUG
#undef ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE
#define ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE 1
#endif

#define ERTS_THR_PRGR_PRINT_LEADER 0
#define ERTS_THR_PRGR_PRINT_VAL 0
#define ERTS_THR_PRGR_PRINT_BLOCKERS 0

#define ERTS_THR_PRGR_FTL_ERR_BLCK_POLL_INTERVAL 100

#define ERTS_THR_PRGR_LFLG_BLOCK	((erts_aint32_t) (1U << 31))
#define ERTS_THR_PRGR_LFLG_NO_LEADER	((erts_aint32_t) (1U << 30))
#define ERTS_THR_PRGR_LFLG_WAITING_UM	((erts_aint32_t) (1U << 29))
#define ERTS_THR_PRGR_LFLG_ACTIVE_MASK	(~(ERTS_THR_PRGR_LFLG_NO_LEADER	\
					   | ERTS_THR_PRGR_LFLG_BLOCK	\
					   | ERTS_THR_PRGR_LFLG_WAITING_UM))

#define ERTS_THR_PRGR_LFLGS_ACTIVE(LFLGS)				\
    ((LFLGS) & ERTS_THR_PRGR_LFLG_ACTIVE_MASK)

/*
 * We use a 64-bit value for thread progress. By this wrapping of
 * the thread progress will more or less never occur.
 *
 * On 32-bit systems we therefore need a double word atomic.
 */
#undef read_acqb
#define read_acqb erts_thr_prgr_read_acqb__
#undef read_nob
#define read_nob erts_thr_prgr_read_nob__

static ERTS_INLINE void
set_mb(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic64_set_mb(atmc, (erts_aint64_t) val);
}

static ERTS_INLINE void
set_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic64_set_nob(atmc, (erts_aint64_t) val);
}

static ERTS_INLINE void
init_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic64_init_nob(atmc, (erts_aint64_t) val);
}

/* #define ERTS_THR_PROGRESS_STATE_DEBUG */

#ifdef ERTS_THR_PROGRESS_STATE_DEBUG

#ifdef __GNUC__
#warning "Thread progress state debug is on"
#endif

#define ERTS_THR_PROGRESS_STATE_DEBUG_LEADER	((erts_aint32_t) (1U << 0))
#define ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE	((erts_aint32_t) (1U << 1))

#define ERTS_THR_PROGRESS_STATE_DEBUG_INIT(ID)						\
    erts_atomic32_init_nob(&intrnl->thr[(ID)].data.state_debug,				\
			   ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE)

#define ERTS_THR_PROGRESS_STATE_DEBUG_SET_ACTIVE(ID, ON)				\
do {											\
    erts_aint32_t state_debug__;							\
    state_debug__ = erts_atomic32_read_nob(&intrnl->thr[(ID)].data.state_debug);	\
    if ((ON))										\
	state_debug__ |= ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE;				\
    else										\
	state_debug__ &= ~ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE;				\
    erts_atomic32_set_nob(&intrnl->thr[(ID)].data.state_debug, state_debug__);		\
} while (0)

#define ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(ID, ON)				\
do {											\
    erts_aint32_t state_debug__;							\
    state_debug__ = erts_atomic32_read_nob(&intrnl->thr[(ID)].data.state_debug);	\
    if ((ON))										\
	state_debug__ |= ERTS_THR_PROGRESS_STATE_DEBUG_LEADER;				\
    else										\
	state_debug__ &= ~ERTS_THR_PROGRESS_STATE_DEBUG_LEADER;				\
    erts_atomic32_set_nob(&intrnl->thr[(ID)].data.state_debug, state_debug__);		\
} while (0)

#else

#define ERTS_THR_PROGRESS_STATE_DEBUG_INIT(ID)
#define ERTS_THR_PROGRESS_STATE_DEBUG_SET_ACTIVE(ID, ON)
#define ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(ID, ON)

#endif /* ERTS_THR_PROGRESS_STATE_DEBUG */

#define ERTS_THR_PRGR_BLCKR_INVALID        ((erts_aint32_t) (~0U))
#define ERTS_THR_PRGR_BLCKR_UNMANAGED      ((erts_aint32_t) (1U << 31))

#define ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING  ((erts_aint32_t) (1U << 31))

#define ERTS_THR_PRGR_BM_BITS 32
#define ERTS_THR_PRGR_BM_SHIFT 5
#define ERTS_THR_PRGR_BM_MASK 0x1f

#define ERTS_THR_PRGR_WAKEUP_DATA_MASK (ERTS_THR_PRGR_WAKEUP_DATA_SIZE - 1)

#define ERTS_THR_PRGR_WAKEUP_IX(V) \
    ((int) ((V) & ERTS_THR_PRGR_WAKEUP_DATA_MASK))

typedef struct {
    erts_atomic32_t len;
    int id[1];
} ErtsThrPrgrManagedWakeupData;

typedef struct {
    erts_atomic32_t len;
    int high_sz;
    int low_sz;
    erts_atomic32_t *high;
    erts_atomic32_t *low;
} ErtsThrPrgrUnmanagedWakeupData;

typedef struct {
    erts_atomic32_t lflgs;
    erts_atomic32_t block_count;
    erts_atomic_t blocker_event;
    erts_atomic32_t pref_wakeup_used;
    erts_atomic32_t managed_count;
    erts_atomic32_t managed_id;
    erts_atomic32_t unmanaged_id;    
    int chk_next_ix;
    struct {
	int waiting;
	erts_atomic32_t current;
    } umrefc_ix;
} ErtsThrPrgrMiscData;

typedef struct {
    ERTS_THR_PRGR_ATOMIC current;
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
    erts_atomic32_t state_debug;
#endif
} ErtsThrPrgrElement;

typedef union {
    ErtsThrPrgrElement data;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsThrPrgrElement))];
} ErtsThrPrgrArray;

typedef union {
    erts_atomic_t refc;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_atomic_t))];
} ErtsThrPrgrUnmanagedRefc;

typedef struct {
    union {
	ErtsThrPrgrMiscData data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(
		sizeof(ErtsThrPrgrMiscData))];
    } misc;
    ErtsThrPrgrUnmanagedRefc umrefc[2];
    ErtsThrPrgrArray *thr;
    struct {
	int no;
	ErtsThrPrgrCallbacks *callbacks;
	ErtsThrPrgrManagedWakeupData *data[ERTS_THR_PRGR_WAKEUP_DATA_SIZE];
    } managed;
    struct {
	int no;
	ErtsThrPrgrCallbacks *callbacks;
	ErtsThrPrgrUnmanagedWakeupData *data[ERTS_THR_PRGR_WAKEUP_DATA_SIZE];
    } unmanaged;
} ErtsThrPrgrInternalData;

static ErtsThrPrgrInternalData *intrnl;

ErtsThrPrgr erts_thr_prgr__;

erts_tsd_key_t erts_thr_prgr_data_key__;

static void handle_wakeup_requests(ErtsThrPrgrVal current);
static int got_sched_wakeups(void);
static erts_aint32_t block_thread(ErtsThrPrgrData *tpd);

static ERTS_INLINE void
wakeup_managed(int id)
{
    ErtsThrPrgrCallbacks *cbp = &intrnl->managed.callbacks[id];
    ASSERT(0 <= id && id < intrnl->managed.no);
    cbp->wakeup(cbp->arg);
}


static ERTS_INLINE void
wakeup_unmanaged(int id)
{
    ErtsThrPrgrCallbacks *cbp = &intrnl->unmanaged.callbacks[id];
    ASSERT(0 <= id && id < intrnl->unmanaged.no);
    cbp->wakeup(cbp->arg);
}

static ERTS_INLINE ErtsThrPrgrData *
perhaps_thr_prgr_data(ErtsSchedulerData *esdp)
{
    if (esdp)
	return &esdp->thr_progress_data;
    else
	return erts_tsd_get(erts_thr_prgr_data_key__);
}

static ERTS_INLINE ErtsThrPrgrData *
thr_prgr_data(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(esdp);
    ASSERT(tpd);
    return tpd;
}

static void
init_tmp_thr_prgr_data(ErtsThrPrgrData *tpd)
{
    tpd->id = -1;
    tpd->is_managed = 0;
    tpd->is_blocking = 0;
    tpd->is_temporary = 1;
#ifdef ERTS_ENABLE_LOCK_CHECK
    tpd->is_delaying = 0;
#endif
    erts_tsd_set(erts_thr_prgr_data_key__, (void *) tpd);
}

static ERTS_INLINE ErtsThrPrgrData *
tmp_thr_prgr_data(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(esdp);

    if (!tpd) {
        /*
         * We only allocate the part up to the wakeup_request field which is
         * the first field only used by registered threads
         */
        size_t alloc_size = offsetof(ErtsThrPrgrData, wakeup_request);

        /* We may land here as a result of unmanaged_delay being called from
         * the lock counting module, which in turn might be called from within
         * the allocator, so we use plain malloc to avoid deadlocks. */
        tpd =
#ifdef ERTS_ENABLE_LOCK_COUNT
            malloc(alloc_size);
#else
            erts_alloc(ERTS_ALC_T_T_THR_PRGR_DATA, alloc_size);
#endif

        init_tmp_thr_prgr_data(tpd);
    }

    return tpd;
}

static ERTS_INLINE void
return_tmp_thr_prgr_data(ErtsThrPrgrData *tpd)
{
    if (tpd->is_temporary) {
        erts_tsd_set(erts_thr_prgr_data_key__, NULL);

#ifdef ERTS_ENABLE_LOCK_COUNT
        free(tpd);
#else
        erts_free(ERTS_ALC_T_T_THR_PRGR_DATA, tpd);
#endif
    }
}

static ERTS_INLINE int
block_count_dec(void)
{
    erts_aint32_t block_count;
    block_count = erts_atomic32_dec_read_mb(&intrnl->misc.data.block_count);
    if (block_count == 0) {
	erts_tse_t *event;
	event = ((erts_tse_t*)
		 erts_atomic_read_nob(&intrnl->misc.data.blocker_event));
	if (event)
	    erts_tse_set(event);
	return 1;
    }

    return (block_count & ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING) == 0;
}

static ERTS_INLINE int
block_count_inc(void)
{
    erts_aint32_t block_count;
    block_count = erts_atomic32_inc_read_mb(&intrnl->misc.data.block_count);
    return (block_count & ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING) == 0;
}


void
erts_thr_progress_pre_init(void)
{
    intrnl = NULL;
    erts_tsd_key_create(&erts_thr_prgr_data_key__,
			"erts_thr_prgr_data_key");
    init_nob(&erts_thr_prgr__.current, ERTS_THR_PRGR_VAL_FIRST);
}

void
erts_thr_progress_init(int no_schedulers, int managed, int unmanaged)
{
    int i, j, um_low, um_high;
    char *ptr;
    size_t cb_sz, intrnl_sz, thr_arr_sz, m_wakeup_size, um_wakeup_size,
	tot_size;

    intrnl_sz = sizeof(ErtsThrPrgrInternalData);
    intrnl_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(intrnl_sz);

    cb_sz = sizeof(ErtsThrPrgrCallbacks)*(managed+unmanaged);
    cb_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(cb_sz);

    thr_arr_sz = sizeof(ErtsThrPrgrArray)*managed;
    ASSERT(thr_arr_sz == ERTS_ALC_CACHE_LINE_ALIGN_SIZE(thr_arr_sz));

    m_wakeup_size = sizeof(ErtsThrPrgrManagedWakeupData);
    m_wakeup_size += (managed - 1)*sizeof(int);
    m_wakeup_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(m_wakeup_size);

    um_low = (unmanaged - 1)/ERTS_THR_PRGR_BM_BITS + 1;
    um_high = (um_low - 1)/ERTS_THR_PRGR_BM_BITS + 1;

    um_wakeup_size = sizeof(ErtsThrPrgrUnmanagedWakeupData);
    um_wakeup_size += (um_high + um_low)*sizeof(erts_atomic32_t);
    um_wakeup_size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(um_wakeup_size);

    tot_size = intrnl_sz;
    tot_size += cb_sz;
    tot_size += thr_arr_sz;
    tot_size += m_wakeup_size*ERTS_THR_PRGR_WAKEUP_DATA_SIZE;
    tot_size += um_wakeup_size*ERTS_THR_PRGR_WAKEUP_DATA_SIZE;

    ptr = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_THR_PRGR_IDATA,
					     tot_size);

    intrnl = (ErtsThrPrgrInternalData *) ptr;
    ptr += intrnl_sz;

    erts_atomic32_init_nob(&intrnl->misc.data.lflgs,
			   ERTS_THR_PRGR_LFLG_NO_LEADER);
    erts_atomic32_init_nob(&intrnl->misc.data.block_count,
			   (ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING
			    | (erts_aint32_t) managed));
    erts_atomic_init_nob(&intrnl->misc.data.blocker_event, ERTS_AINT_NULL);
    erts_atomic32_init_nob(&intrnl->misc.data.pref_wakeup_used, 0);
    erts_atomic32_init_nob(&intrnl->misc.data.managed_count, 0);
    erts_atomic32_init_nob(&intrnl->misc.data.managed_id, no_schedulers);
    erts_atomic32_init_nob(&intrnl->misc.data.unmanaged_id, -1);
    intrnl->misc.data.chk_next_ix = 0;
    intrnl->misc.data.umrefc_ix.waiting = -1;
    erts_atomic32_init_nob(&intrnl->misc.data.umrefc_ix.current, 0);

    erts_atomic_init_nob(&intrnl->umrefc[0].refc, (erts_aint_t) 0);
    erts_atomic_init_nob(&intrnl->umrefc[1].refc, (erts_aint_t) 0);

    intrnl->thr = (ErtsThrPrgrArray *) ptr;
    ptr += thr_arr_sz;
    for (i = 0; i < managed; i++)
	init_nob(&intrnl->thr[i].data.current, 0);

    intrnl->managed.callbacks = (ErtsThrPrgrCallbacks *) ptr;
    intrnl->unmanaged.callbacks = &intrnl->managed.callbacks[managed];
    ptr += cb_sz;

    intrnl->managed.no = managed;
    for (i = 0; i < managed; i++) {
	intrnl->managed.callbacks[i].arg = NULL;
	intrnl->managed.callbacks[i].wakeup = NULL;
    }

    intrnl->unmanaged.no = unmanaged;
    for (i = 0; i < unmanaged; i++) {
	intrnl->unmanaged.callbacks[i].arg = NULL;
	intrnl->unmanaged.callbacks[i].wakeup = NULL;
    }

    for (i = 0; i < ERTS_THR_PRGR_WAKEUP_DATA_SIZE; i++) {
	intrnl->managed.data[i] = (ErtsThrPrgrManagedWakeupData *) ptr;
	erts_atomic32_init_nob(&intrnl->managed.data[i]->len, 0);
	ptr += m_wakeup_size;
    }

    for (i = 0; i < ERTS_THR_PRGR_WAKEUP_DATA_SIZE; i++) {
	erts_atomic32_t *bm;
	intrnl->unmanaged.data[i] = (ErtsThrPrgrUnmanagedWakeupData *) ptr;
	erts_atomic32_init_nob(&intrnl->unmanaged.data[i]->len, 0);
	bm = (erts_atomic32_t *) (ptr + sizeof(ErtsThrPrgrUnmanagedWakeupData));
	intrnl->unmanaged.data[i]->high = bm;
	intrnl->unmanaged.data[i]->high_sz = um_high;
	for (j = 0; j < um_high; j++)
	    erts_atomic32_init_nob(&intrnl->unmanaged.data[i]->high[j], 0);
	intrnl->unmanaged.data[i]->low
	    = &intrnl->unmanaged.data[i]->high[um_high];	
	intrnl->unmanaged.data[i]->low_sz = um_low;
	for (j = 0; j < um_low; j++)
	    erts_atomic32_init_nob(&intrnl->unmanaged.data[i]->low[j], 0);
	ptr += um_wakeup_size;
    }
    ERTS_THR_MEMORY_BARRIER;
}

static void
init_wakeup_request_array(ErtsThrPrgrVal *w)
{
    int i;
    ErtsThrPrgrVal current;

    current = read_acqb(&erts_thr_prgr__.current);
    for (i = 0; i < ERTS_THR_PRGR_WAKEUP_DATA_SIZE; i++) {
	w[i] = current - ((ErtsThrPrgrVal) (ERTS_THR_PRGR_WAKEUP_DATA_SIZE + i));
	if (w[i] > current)
	    w[i]--;
    }
}

void
erts_thr_progress_register_unmanaged_thread(ErtsThrPrgrCallbacks *callbacks)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(NULL);
    int is_blocking = 0;

    if (tpd) {
	if (!tpd->is_temporary)
	    erts_exit(ERTS_ABORT_EXIT,
		     "%s:%d:%s(): Double register of thread\n",
		     __FILE__, __LINE__, __func__);
	is_blocking = tpd->is_blocking;
	return_tmp_thr_prgr_data(tpd);
    }

    /*
     * We only allocate the part up to the leader field
     * which is the first field only used by managed threads
     */
    tpd = erts_alloc(ERTS_ALC_T_THR_PRGR_DATA,
		     offsetof(ErtsThrPrgrData, leader));
    tpd->id = (int) erts_atomic32_inc_read_nob(&intrnl->misc.data.unmanaged_id);
    tpd->is_managed = 0;
    tpd->is_blocking = is_blocking;
    tpd->is_temporary = 0;
#ifdef ERTS_ENABLE_LOCK_CHECK
    tpd->is_delaying = 0;
#endif
    ASSERT(tpd->id >= 0);
    if (tpd->id >= intrnl->unmanaged.no)
	erts_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Too many unmanaged registered threads\n",
		 __FILE__, __LINE__, __func__);

    init_wakeup_request_array(&tpd->wakeup_request[0]);
    erts_tsd_set(erts_thr_prgr_data_key__, (void *) tpd);

    ASSERT(callbacks->wakeup);

    intrnl->unmanaged.callbacks[tpd->id] = *callbacks;
}


void
erts_thr_progress_register_managed_thread(ErtsSchedulerData *esdp,
					  ErtsThrPrgrCallbacks *callbacks,
					  int pref_wakeup)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(NULL);
    int is_blocking = 0, managed;

    if (tpd) {
	if (!tpd->is_temporary)
	    erts_exit(ERTS_ABORT_EXIT,
		     "%s:%d:%s(): Double register of thread\n",
		     __FILE__, __LINE__, __func__);
	is_blocking = tpd->is_blocking;
	return_tmp_thr_prgr_data(tpd);
    }

    if (esdp)
	tpd = &esdp->thr_progress_data;
    else
	tpd = erts_alloc(ERTS_ALC_T_THR_PRGR_DATA, sizeof(ErtsThrPrgrData));

    if (pref_wakeup
	&& !erts_atomic32_xchg_nob(&intrnl->misc.data.pref_wakeup_used, 1))
	tpd->id = 0;
    else if (esdp)
	tpd->id = (int) esdp->no;
    else
	tpd->id = erts_atomic32_inc_read_nob(&intrnl->misc.data.managed_id);
    ASSERT(tpd->id >= 0);
    if (tpd->id >= intrnl->managed.no)
	erts_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Too many managed registered threads\n",
		 __FILE__, __LINE__, __func__);

    tpd->is_managed = 1;
    tpd->is_blocking = is_blocking;
    tpd->is_temporary = 0;
#ifdef ERTS_ENABLE_LOCK_CHECK
    tpd->is_delaying = 1;
#endif

    init_wakeup_request_array(&tpd->wakeup_request[0]);

    ERTS_THR_PROGRESS_STATE_DEBUG_INIT(tpd->id);

    tpd->leader = 0;
    tpd->active = 1;
    tpd->confirmed = 0;
    tpd->leader_state.current = ERTS_THR_PRGR_VAL_WAITING;
    erts_tsd_set(erts_thr_prgr_data_key__, (void *) tpd);

    erts_atomic32_inc_nob(&intrnl->misc.data.lflgs);

    ASSERT(callbacks->wakeup);
    ASSERT(callbacks->prepare_wait);
    ASSERT(callbacks->wait);
    ASSERT(callbacks->finalize_wait);

    intrnl->managed.callbacks[tpd->id] = *callbacks;

    callbacks->prepare_wait(callbacks->arg);
    managed = erts_atomic32_inc_read_relb(&intrnl->misc.data.managed_count);
    if (managed != intrnl->managed.no) {
	/* Wait until all managed threads have registered... */
	do {
	    callbacks->wait(callbacks->arg);
	    callbacks->prepare_wait(callbacks->arg);
	    managed = erts_atomic32_read_acqb(&intrnl->misc.data.managed_count);
	} while (managed != intrnl->managed.no);
    }
    else {
	int id;
	/* All managed threads have registered; lets go... */
	for (id = 0; id < managed; id++)
	    if (id != tpd->id)
		wakeup_managed(id);
    }
    callbacks->finalize_wait(callbacks->arg);
}

static ERTS_INLINE int
leader_update(ErtsThrPrgrData *tpd)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0);
#endif
    if (!tpd->leader) {
	/* Probably need to block... */
	block_thread(tpd);
    }
    else {
	ErtsThrPrgrVal current;
	int ix, chk_next_ix, umrefc_ix, my_ix, no_managed, waiting_unmanaged;
	erts_aint32_t lflgs;
	ErtsThrPrgrVal next;
	erts_aint_t refc;

	my_ix = tpd->id;

	if (tpd->leader_state.current == ERTS_THR_PRGR_VAL_WAITING) {
	    /* Took over as leader from another thread */
	    tpd->leader_state.current = read_nob(&erts_thr_prgr__.current);
	    tpd->leader_state.next = tpd->leader_state.current;
	    tpd->leader_state.next++;
	    if (tpd->leader_state.next == ERTS_THR_PRGR_VAL_WAITING)
		tpd->leader_state.next = 0;
	    tpd->leader_state.chk_next_ix = intrnl->misc.data.chk_next_ix;
	    tpd->leader_state.umrefc_ix.waiting = intrnl->misc.data.umrefc_ix.waiting;
	    tpd->leader_state.umrefc_ix.current =
		(int) erts_atomic32_read_nob(&intrnl->misc.data.umrefc_ix.current);

	    if (tpd->confirmed == tpd->leader_state.current) {
		ErtsThrPrgrVal val = tpd->leader_state.current + 1;
		if (val == ERTS_THR_PRGR_VAL_WAITING)
		    val = 0;
		tpd->confirmed = val;
		set_mb(&intrnl->thr[my_ix].data.current, val);
	    }
	}


	next = tpd->leader_state.next;

	waiting_unmanaged = 0;
	umrefc_ix = -1; /* Shut up annoying warning */

	chk_next_ix = tpd->leader_state.chk_next_ix;
	no_managed = intrnl->managed.no;
	ASSERT(0 <= chk_next_ix && chk_next_ix <= no_managed);
	/* Check manged threads */
	if (chk_next_ix < no_managed) {
	    for (ix = chk_next_ix; ix < no_managed; ix++) {
		ErtsThrPrgrVal tmp;
		if (ix == my_ix)
		    continue;
		tmp = read_nob(&intrnl->thr[ix].data.current);
		if (tmp != next && tmp != ERTS_THR_PRGR_VAL_WAITING) {
		    tpd->leader_state.chk_next_ix = ix;
		    ASSERT(erts_thr_progress_has_passed__(next, tmp));
		    goto done;
		}
	    }
	}

	/* Check unmanged threads */
	waiting_unmanaged = tpd->leader_state.umrefc_ix.waiting != -1;
	umrefc_ix = (waiting_unmanaged
		     ? tpd->leader_state.umrefc_ix.waiting
		     : tpd->leader_state.umrefc_ix.current);
	refc = erts_atomic_read_nob(&intrnl->umrefc[umrefc_ix].refc);
	ASSERT(refc >= 0);
	if (refc != 0) {
	    int new_umrefc_ix;

	    if (waiting_unmanaged)
		goto done;

	    new_umrefc_ix = (umrefc_ix + 1) & 0x1;
	    tpd->leader_state.umrefc_ix.waiting = umrefc_ix;
	    tpd->leader_state.chk_next_ix = no_managed;
	    erts_atomic32_set_nob(&intrnl->misc.data.umrefc_ix.current,
				  (erts_aint32_t) new_umrefc_ix);
	    tpd->leader_state.umrefc_ix.current = new_umrefc_ix;
	    ETHR_MEMBAR(ETHR_StoreLoad);
	    refc = erts_atomic_read_nob(&intrnl->umrefc[umrefc_ix].refc);
	    ASSERT(refc >= 0);
	    waiting_unmanaged = 1;
	    if (refc != 0)
		goto done;
	}

	/* Make progress */
	current = next;

	next++;
	if (next == ERTS_THR_PRGR_VAL_WAITING)
	    next = 0;

	set_nob(&intrnl->thr[my_ix].data.current, next);
	set_mb(&erts_thr_prgr__.current, current);
	tpd->confirmed = next;
	tpd->leader_state.next = next;
	tpd->leader_state.current = current;

#if ERTS_THR_PRGR_PRINT_VAL
	if (current % 1000 == 0)
	    erts_fprintf(stderr, "%b64u\n", current);
#endif
	handle_wakeup_requests(current);

	if (waiting_unmanaged) {
	    waiting_unmanaged = 0;
	    tpd->leader_state.umrefc_ix.waiting = -1;
	    erts_atomic32_read_band_nob(&intrnl->misc.data.lflgs,
					~ERTS_THR_PRGR_LFLG_WAITING_UM);
	}
	tpd->leader_state.chk_next_ix = 0;

    done:

	if (tpd->active) {
	    lflgs = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);
	    if (lflgs & ERTS_THR_PRGR_LFLG_BLOCK)
		(void) block_thread(tpd);
	}
	else {
	    int force_wakeup_check = 0;
	    erts_aint32_t set_flags = ERTS_THR_PRGR_LFLG_NO_LEADER;
	    tpd->leader = 0;
	    tpd->leader_state.current = ERTS_THR_PRGR_VAL_WAITING;
#if ERTS_THR_PRGR_PRINT_LEADER
	    erts_fprintf(stderr, "L <- %d\n", tpd->id);
#endif

	    ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(tpd->id, 0);

	    intrnl->misc.data.umrefc_ix.waiting
		= tpd->leader_state.umrefc_ix.waiting;
	    if (waiting_unmanaged)
		set_flags |= ERTS_THR_PRGR_LFLG_WAITING_UM;

	    lflgs = erts_atomic32_read_bor_relb(&intrnl->misc.data.lflgs,
						set_flags);
	    lflgs |= set_flags;
	    if (lflgs & ERTS_THR_PRGR_LFLG_BLOCK)
		lflgs = block_thread(tpd);

	    if (waiting_unmanaged) {
		/* Need to check umrefc again */
		ETHR_MEMBAR(ETHR_StoreLoad);
		refc = erts_atomic_read_nob(&intrnl->umrefc[umrefc_ix].refc);
		if (refc == 0) {
		    /* Need to force wakeup check */
		    force_wakeup_check = 1;
		}
	    }

	    if ((force_wakeup_check
		 || ((lflgs & (ERTS_THR_PRGR_LFLG_NO_LEADER
			       | ERTS_THR_PRGR_LFLG_WAITING_UM
			       | ERTS_THR_PRGR_LFLG_ACTIVE_MASK))
		     == ERTS_THR_PRGR_LFLG_NO_LEADER))
		&& got_sched_wakeups()) {
		/* Someone need to make progress */
		wakeup_managed(0);
	    }
	}
    }

    return tpd->leader;
}

static int
update(ErtsThrPrgrData *tpd)
{
    int res;
    ErtsThrPrgrVal val;

    if (tpd->leader)
	res = 1;
    else {
	erts_aint32_t lflgs;
	res = 0;
	val = read_acqb(&erts_thr_prgr__.current);
	if (tpd->confirmed == val) {
	    val++;
	    if (val == ERTS_THR_PRGR_VAL_WAITING)
		val = 0;
	    tpd->confirmed = val;
	    set_mb(&intrnl->thr[tpd->id].data.current, val);
	}

	lflgs = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);
	if (lflgs & ERTS_THR_PRGR_LFLG_BLOCK)
	    res = 1; /* Need to block in leader_update() */

	if ((lflgs & ERTS_THR_PRGR_LFLG_NO_LEADER)
	    && (tpd->active || ERTS_THR_PRGR_LFLGS_ACTIVE(lflgs) == 0)) {
	    /* Try to take over leadership... */
	    erts_aint32_t olflgs;
	    olflgs = erts_atomic32_read_band_acqb(
		&intrnl->misc.data.lflgs,
		~ERTS_THR_PRGR_LFLG_NO_LEADER);
	    if (olflgs & ERTS_THR_PRGR_LFLG_NO_LEADER) {
		tpd->leader = 1;
#if ERTS_THR_PRGR_PRINT_LEADER
		erts_fprintf(stderr, "L -> %d\n", tpd->id);
#endif
		ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(tpd->id, 1);
	    }
	}
	res |= tpd->leader;
    }
    return res;
}

int
erts_thr_progress_update(ErtsSchedulerData *esdp)
{
    return update(thr_prgr_data(esdp));
}


int
erts_thr_progress_leader_update(ErtsSchedulerData *esdp)
{
    return leader_update(thr_prgr_data(esdp));
}

void
erts_thr_progress_prepare_wait(ErtsSchedulerData *esdp)
{
    erts_aint32_t lflgs;
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0);
#endif

    block_count_dec();

    tpd->confirmed = ERTS_THR_PRGR_VAL_WAITING;
    set_mb(&intrnl->thr[tpd->id].data.current, ERTS_THR_PRGR_VAL_WAITING);

    lflgs = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);

    if ((lflgs & (ERTS_THR_PRGR_LFLG_NO_LEADER
		  | ERTS_THR_PRGR_LFLG_WAITING_UM
		  | ERTS_THR_PRGR_LFLG_ACTIVE_MASK))
	== ERTS_THR_PRGR_LFLG_NO_LEADER 
	&& got_sched_wakeups()) {
	/* Someone need to make progress */
	wakeup_managed(0);
    }
}

void
erts_thr_progress_finalize_wait(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);
    ErtsThrPrgrVal current, val;

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0);
#endif

    /*
     * We aren't allowed to continue until our thread
     * progress is past global current.
     */
    val = current = read_acqb(&erts_thr_prgr__.current);
    while (1) {
	val++;
	if (val == ERTS_THR_PRGR_VAL_WAITING)
	    val = 0;
	tpd->confirmed = val;
	set_mb(&intrnl->thr[tpd->id].data.current, val);
	val = read_acqb(&erts_thr_prgr__.current);
	if (current == val)
	    break;
	current = val;
    }
    if (block_count_inc())
	block_thread(tpd);
    if (update(tpd))
	leader_update(tpd);
}

void
erts_thr_progress_active(ErtsSchedulerData *esdp, int on)
{
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0);
#endif

    ERTS_THR_PROGRESS_STATE_DEBUG_SET_ACTIVE(tpd->id, on);

    if (on) {
	ASSERT(!tpd->active);
	tpd->active = 1;
	erts_atomic32_inc_nob(&intrnl->misc.data.lflgs);
    }
    else {
	ASSERT(tpd->active);
	tpd->active = 0;
	erts_atomic32_dec_nob(&intrnl->misc.data.lflgs);
	if (update(tpd))
	    leader_update(tpd);
    }

#ifdef DEBUG
    {
	erts_aint32_t n = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);
	n &= ERTS_THR_PRGR_LFLG_ACTIVE_MASK;
	ASSERT(tpd->active <= n && n <= intrnl->managed.no);
    }
#endif

}

static ERTS_INLINE void
unmanaged_continue(ErtsThrPrgrDelayHandle handle)
{
    int umrefc_ix = (int) handle;
    erts_aint_t refc;

    ASSERT(umrefc_ix == 0 || umrefc_ix == 1);
    refc = erts_atomic_dec_read_relb(&intrnl->umrefc[umrefc_ix].refc);
    ASSERT(refc >= 0);
    if (refc == 0) {
	erts_aint_t lflgs;
	ERTS_THR_READ_MEMORY_BARRIER;
	lflgs = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);
	if ((lflgs & (ERTS_THR_PRGR_LFLG_NO_LEADER
		      | ERTS_THR_PRGR_LFLG_WAITING_UM
		      | ERTS_THR_PRGR_LFLG_ACTIVE_MASK))
	    == (ERTS_THR_PRGR_LFLG_NO_LEADER|ERTS_THR_PRGR_LFLG_WAITING_UM)
	    && got_sched_wakeups()) {
	    /* Others waiting for us... */
	    wakeup_managed(0);
	}
    }
}

void
erts_thr_progress_unmanaged_continue__(ErtsThrPrgrDelayHandle handle)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(NULL);
    ERTS_LC_ASSERT(tpd && tpd->is_delaying);
    tpd->is_delaying--;
    ASSERT(tpd->is_delaying >= 0);
    if (!tpd->is_delaying)
	return_tmp_thr_prgr_data(tpd);
#endif
    ASSERT(!erts_thr_progress_is_managed_thread());

    unmanaged_continue(handle);
}

ErtsThrPrgrDelayHandle
erts_thr_progress_unmanaged_delay__(void)
{
    int umrefc_ix;
    ASSERT(!erts_thr_progress_is_managed_thread());
    umrefc_ix = (int) erts_atomic32_read_acqb(&intrnl->misc.data.umrefc_ix.current);
    while (1) {
	int tmp_ix;
	erts_atomic_inc_acqb(&intrnl->umrefc[umrefc_ix].refc);
	tmp_ix = (int) erts_atomic32_read_acqb(&intrnl->misc.data.umrefc_ix.current);
	if (tmp_ix == umrefc_ix)
	    break;
	unmanaged_continue(umrefc_ix);
	umrefc_ix = tmp_ix;
    }
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	ErtsThrPrgrData *tpd = tmp_thr_prgr_data(NULL);
	tpd->is_delaying++;
    }
#endif
    return (ErtsThrPrgrDelayHandle) umrefc_ix;
}

static ERTS_INLINE int
has_reached_wakeup(ErtsThrPrgrVal wakeup)
{
    /*
     * Exactly the same as erts_thr_progress_has_reached(), but
     * also verify valid wakeup requests in debug mode.
     */
    ErtsThrPrgrVal current;

    current = read_acqb(&erts_thr_prgr__.current);

#if ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE
    {
	ErtsThrPrgrVal limit;
	/*
	 * erts_thr_progress_later() returns values which are
	 * equal to 'current + 2', or 'current + 3'. That is, users
	 * should never get a hold of values larger than that.
	 *
	 * That is, valid values are values less than 'current + 4'.
	 *
	 * Values larger than this won't work with the wakeup
	 * algorithm.
	 */

	limit = current + 4;
	if (limit == ERTS_THR_PRGR_VAL_WAITING)
	    limit = 0;
	else if (limit < current) /* Wrapped */
	    limit += 1;

	if (!erts_thr_progress_has_passed__(limit, wakeup))
	    erts_exit(ERTS_ABORT_EXIT,
		     "Invalid wakeup request value found:"
		     " current=%b64u, wakeup=%b64u, limit=%b64u",
		     current, wakeup, limit);
    }
#endif

    if (current == wakeup)
	return 1;
    return erts_thr_progress_has_passed__(current, wakeup);
}

static void
request_wakeup_managed(ErtsThrPrgrData *tpd, ErtsThrPrgrVal value)
{
    ErtsThrPrgrManagedWakeupData *mwd;
    int ix, wix;

    /*
     * Only managed threads that aren't in waiting state
     * are allowed to call this function.
     */

    ASSERT(tpd->is_managed);
    ASSERT(tpd->confirmed != ERTS_THR_PRGR_VAL_WAITING);

    if (has_reached_wakeup(value)) {
	wakeup_managed(tpd->id);
	return;
    }

    wix = ERTS_THR_PRGR_WAKEUP_IX(value);
    if (tpd->wakeup_request[wix] == value)
	return; /* Already got a request registered */

    ASSERT(erts_thr_progress_has_passed__(value,
					  tpd->wakeup_request[wix]));


    if (tpd->confirmed == value) {
	/*
	 * We have already confirmed this value. We need to request
	 * wakeup for a value later than our latest confirmed value in
	 * order to prevent progress from reaching the requested value
	 * while we are writing the request.
	 *
	 * It is ok to move the wakeup request forward since the only
	 * guarantee we make (and can make) is that the thread will be
	 * woken some time *after* the requested value has been reached.
	 */
	value++;
	if (value == ERTS_THR_PRGR_VAL_WAITING)
	    value = 0;

	wix = ERTS_THR_PRGR_WAKEUP_IX(value);
	if (tpd->wakeup_request[wix] == value)
	    return; /* Already got a request registered */

	ASSERT(erts_thr_progress_has_passed__(value,
					      tpd->wakeup_request[wix]));
    }

    tpd->wakeup_request[wix] = value;

    mwd = intrnl->managed.data[wix];

    ix = erts_atomic32_inc_read_nob(&mwd->len) - 1;
#if ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE
    if (ix >= intrnl->managed.no)
	erts_exit(ERTS_ABORT_EXIT, "Internal error: Too many wakeup requests\n");
#endif
    mwd->id[ix] = tpd->id;

    ASSERT(!erts_thr_progress_has_reached(value));

    /*
     * This thread is guarranteed to issue a full memory barrier:
     * - after the request has been written, but
     * - before the global thread progress reach the (possibly
     *   increased) requested wakeup value.
     */
}

static void
request_wakeup_unmanaged(ErtsThrPrgrData *tpd, ErtsThrPrgrVal value)
{
    int wix, ix, id, bit;
    ErtsThrPrgrUnmanagedWakeupData *umwd;

    ASSERT(!tpd->is_managed);

    /*
     * Thread progress *can* reach and pass our requested value while
     * we are writing the request.
     */

    if (has_reached_wakeup(value)) {
	wakeup_unmanaged(tpd->id);
	return;
    }

    wix = ERTS_THR_PRGR_WAKEUP_IX(value);

    if (tpd->wakeup_request[wix] == value)
	return; /* Already got a request registered */

    ASSERT(erts_thr_progress_has_passed__(value,
					  tpd->wakeup_request[wix]));

    umwd = intrnl->unmanaged.data[wix];

    id = tpd->id;

    bit = id & ERTS_THR_PRGR_BM_MASK;
    ix = id >> ERTS_THR_PRGR_BM_SHIFT;
    ASSERT(0 <= ix && ix < umwd->low_sz);
    erts_atomic32_read_bor_nob(&umwd->low[ix], 1 << bit);

    bit = ix & ERTS_THR_PRGR_BM_MASK;
    ix >>= ERTS_THR_PRGR_BM_SHIFT;
    ASSERT(0 <= ix && ix < umwd->high_sz);
    erts_atomic32_read_bor_nob(&umwd->high[ix], 1 << bit);

    erts_atomic32_inc_mb(&umwd->len);

    if (erts_thr_progress_has_reached(value))
	wakeup_unmanaged(tpd->id);
    else
	tpd->wakeup_request[wix] = value;
}

void
erts_thr_progress_wakeup(ErtsSchedulerData *esdp,
			 ErtsThrPrgrVal value)
{
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);
    ASSERT(!tpd->is_temporary);
    if (tpd->is_managed)
	request_wakeup_managed(tpd, value);
    else
	request_wakeup_unmanaged(tpd, value);
}

static void
wakeup_unmanaged_threads(ErtsThrPrgrUnmanagedWakeupData *umwd)
{
    int hix;
    for (hix = 0; hix < umwd->high_sz; hix++) {
	erts_aint32_t hmask = erts_atomic32_read_nob(&umwd->high[hix]);
	if (hmask) {
	    int hbase = hix << ERTS_THR_PRGR_BM_SHIFT;
	    int hbit;
	    for (hbit = 0; hbit < ERTS_THR_PRGR_BM_BITS; hbit++) {
		if (hmask & (1U << hbit)) {
		    erts_aint_t lmask;
		    int lix = hbase + hbit;
		    ASSERT(0 <= lix && lix < umwd->low_sz);
		    lmask = erts_atomic32_read_nob(&umwd->low[lix]);
		    if (lmask) {
			int lbase = lix << ERTS_THR_PRGR_BM_SHIFT;
			int lbit;
			for (lbit = 0; lbit < ERTS_THR_PRGR_BM_BITS; lbit++) {
			    if (lmask & (1U << lbit)) {
				int id = lbase + lbit;
				wakeup_unmanaged(id);
			    }
			}
			erts_atomic32_set_nob(&umwd->low[lix], 0);
		    }
		}
	    }
	    erts_atomic32_set_nob(&umwd->high[hix], 0);
	}
    }
}


static void
handle_wakeup_requests(ErtsThrPrgrVal current)
{
    ErtsThrPrgrManagedWakeupData *mwd;
    ErtsThrPrgrUnmanagedWakeupData *umwd;
    int wix, len, i;

    wix = ERTS_THR_PRGR_WAKEUP_IX(current);

    mwd = intrnl->managed.data[wix];
    len = erts_atomic32_read_nob(&mwd->len);
    ASSERT(len >= 0);
    if (len) {
	for (i = 0; i < len; i++)
	    wakeup_managed(mwd->id[i]);
	erts_atomic32_set_nob(&mwd->len, 0);
    }

    umwd = intrnl->unmanaged.data[wix];
    len = erts_atomic32_read_nob(&umwd->len);
    ASSERT(len >= 0);
    if (len) {
	wakeup_unmanaged_threads(umwd);
	erts_atomic32_set_nob(&umwd->len, 0);
    }

}

static int
got_sched_wakeups(void)
{
    int wix;

    ERTS_THR_MEMORY_BARRIER;

    for (wix = 0; wix < ERTS_THR_PRGR_WAKEUP_DATA_SIZE; wix++) {
 	ErtsThrPrgrManagedWakeupData **mwd = intrnl->managed.data;
	if (erts_atomic32_read_nob(&mwd[wix]->len))
	    return 1;
    }
    for (wix = 0; wix < ERTS_THR_PRGR_WAKEUP_DATA_SIZE; wix++) {
 	ErtsThrPrgrUnmanagedWakeupData **umwd = intrnl->unmanaged.data;
	if (erts_atomic32_read_nob(&umwd[wix]->len))
	    return 1;
    }
    return 0;
}

static erts_aint32_t
block_thread(ErtsThrPrgrData *tpd)
{
    erts_aint32_t lflgs;
    ErtsThrPrgrCallbacks *cbp = &intrnl->managed.callbacks[tpd->id];

    do {
	block_count_dec();

	while (1) {
	    cbp->prepare_wait(cbp->arg);
	    lflgs = erts_atomic32_read_nob(&intrnl->misc.data.lflgs);
	    if (lflgs & ERTS_THR_PRGR_LFLG_BLOCK)
		cbp->wait(cbp->arg);
	    else
		break;
	}

    } while (block_count_inc());

    cbp->finalize_wait(cbp->arg);

    return lflgs;
}

static erts_aint32_t
thr_progress_block(ErtsThrPrgrData *tpd, int wait)
{
    erts_tse_t *event = NULL; /* Remove erroneous warning... sigh... */
    erts_aint32_t lflgs, bc;

    if (tpd->is_blocking++)
	return (erts_aint32_t) 0;

    while (1) {
	lflgs = erts_atomic32_read_bor_nob(&intrnl->misc.data.lflgs,
					   ERTS_THR_PRGR_LFLG_BLOCK);
	if (lflgs & ERTS_THR_PRGR_LFLG_BLOCK)
	    block_thread(tpd);
	else
	    break;
    }

#if ERTS_THR_PRGR_PRINT_BLOCKERS
    erts_fprintf(stderr, "block(%d)\n", tpd->id);
#endif

    ASSERT(ERTS_AINT_NULL
	   == erts_atomic_read_nob(&intrnl->misc.data.blocker_event));

    if (wait) {
	event = erts_tse_fetch();
	erts_tse_reset(event);
	erts_atomic_set_nob(&intrnl->misc.data.blocker_event,
			    (erts_aint_t) event);
    }
    if (tpd->is_managed)
	erts_atomic32_dec_nob(&intrnl->misc.data.block_count);
    bc = erts_atomic32_read_band_mb(&intrnl->misc.data.block_count,
				    ~ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING);
    bc &= ~ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING;
    if (wait) {
	while (bc != 0) {
	    erts_tse_wait(event);
	    erts_tse_reset(event);
	    bc = erts_atomic32_read_acqb(&intrnl->misc.data.block_count);
	}
    }
    return bc;

}

void
erts_thr_progress_block(void)
{
    thr_progress_block(tmp_thr_prgr_data(NULL), 1);
}

int
erts_thr_progress_fatal_error_block(ErtsThrPrgrData *tmp_tpd_bufp)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(NULL);

    if (!tpd) {
	/*
	 * We stack allocate since failure to allocate memory may
	 * have caused the problem in the first place. This is ok
	 * since we never complete an unblock after a fatal error
	 * block.
	 */
	tpd = tmp_tpd_bufp;
	init_tmp_thr_prgr_data(tpd);
    }

    /* Returns number of threads that have not yes been blocked */
    return thr_progress_block(tpd, 0);
}

void
erts_thr_progress_fatal_error_wait(SWord timeout) {
    erts_aint32_t bc;
    SWord time_left = timeout;
    ErtsMonotonicTime timeout_time;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    /*
     * Counting poll intervals may give us a too long timeout
     * if cpu is busy. We use timeout time to try to prevent
     * this. In case we havn't got time correction this may
     * however fail too...
     */
    timeout_time = erts_get_monotonic_time(esdp);
    timeout_time += ERTS_MSEC_TO_MONOTONIC((ErtsMonotonicTime) timeout);

    while (1) {
	if (erts_milli_sleep(ERTS_THR_PRGR_FTL_ERR_BLCK_POLL_INTERVAL) == 0)
	    time_left -= ERTS_THR_PRGR_FTL_ERR_BLCK_POLL_INTERVAL;
	bc = erts_atomic32_read_acqb(&intrnl->misc.data.block_count);
	if (bc == 0)
	    break; /* Succefully blocked all managed threads */
	if (time_left <= 0)
	    break; /* Timeout */
	if (timeout_time <= erts_get_monotonic_time(esdp))
	    break; /* Timeout */
    }
}

void
erts_thr_progress_unblock(void)
{
    erts_tse_t *event;
    int id, break_id, sz, wakeup;
    ErtsThrPrgrData *tpd = thr_prgr_data(NULL);

    ASSERT(tpd->is_blocking);
    if (--tpd->is_blocking)
	return;

    sz = intrnl->managed.no;

    wakeup = 1;
    if (!tpd->is_managed)
	id = break_id = tpd->id < 0 ? 0 : tpd->id % sz;
    else {
	break_id = tpd->id;
	id = break_id + 1;
	if (id >= sz)
	    id = 0;
	if (id == break_id)
	    wakeup = 0;
	erts_atomic32_inc_nob(&intrnl->misc.data.block_count);
    }

    event = ((erts_tse_t *)
	     erts_atomic_read_nob(&intrnl->misc.data.blocker_event));
    ASSERT(event);
    erts_atomic_set_nob(&intrnl->misc.data.blocker_event, ERTS_AINT_NULL);

    erts_atomic32_read_bor_relb(&intrnl->misc.data.block_count,
				ERTS_THR_PRGR_BC_FLG_NOT_BLOCKING);
#if ERTS_THR_PRGR_PRINT_BLOCKERS
    erts_fprintf(stderr, "unblock(%d)\n", tpd->id);
#endif
    erts_atomic32_read_band_mb(&intrnl->misc.data.lflgs,
			       ~ERTS_THR_PRGR_LFLG_BLOCK);

    if (wakeup) {
	do {
	    ErtsThrPrgrVal tmp;
	    tmp = read_nob(&intrnl->thr[id].data.current);
	    if (tmp != ERTS_THR_PRGR_VAL_WAITING)
		wakeup_managed(id);
	    if (++id >= sz)
		id = 0;
	} while (id != break_id);
    }

    return_tmp_thr_prgr_data(tpd);
    erts_tse_return(event);
}

int
erts_thr_progress_is_blocking(void)
{
    ErtsThrPrgrData *tpd = perhaps_thr_prgr_data(NULL);
    return tpd && tpd->is_blocking;
}

void erts_thr_progress_dbg_print_state(void)
{
    int id;
    int sz = intrnl->managed.no;

    erts_fprintf(stderr, "--- thread progress ---\n");
    erts_fprintf(stderr,"current=%b64u\n", erts_thr_progress_current());
    for (id = 0; id < sz; id++) {
	ErtsThrPrgrVal current = read_nob(&intrnl->thr[id].data.current);
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
	erts_aint32_t state_debug;
	char *active, *leader;

	state_debug = erts_atomic32_read_nob(&intrnl->thr[id].data.state_debug);
	active = (state_debug & ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE
		  ? "true"
		  : "false");
	leader = (state_debug & ERTS_THR_PROGRESS_STATE_DEBUG_LEADER
		  ? "true"
		  : "false");
#endif
	if (current == ERTS_THR_PRGR_VAL_WAITING)
	    erts_fprintf(stderr,
			 "  id=%d, current=WAITING"
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
			 ", active=%s, leader=%s"
#endif
			 "\n", id
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
			 , active, leader
#endif
		);
	else
	    erts_fprintf(stderr,
			 "  id=%d, current=%b64u"
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
			 ", active=%s, leader=%s"
#endif
			 "\n", id, current
#ifdef ERTS_THR_PROGRESS_STATE_DEBUG
			 , active, leader
#endif
		);
    }
    erts_fprintf(stderr, "-----------------------\n");
    

}

