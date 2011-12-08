/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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
 * threads and the aux_thread are managed threads. We typically do not want
 * any async threads as managed threads since they cannot guarantee a
 * frequent update of thread progress, since they execute user implemented
 * driver code.
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

#ifdef ERTS_SMP

#define ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE 0

#ifdef DEBUG
#undef ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE
#define ERTS_THR_PRGR_DBG_CHK_WAKEUP_REQUEST_VALUE 1
#endif

#define ERTS_THR_PRGR_PRINT_LEADER 0
#define ERTS_THR_PRGR_PRINT_VAL 0

#define ERTS_THR_PRGR_LFLG_NO_LEADER (((erts_aint32_t) 1) << 31)
#define ERTS_THR_PRGR_LFLG_ACTIVE_MASK (~ERTS_THR_PRGR_LFLG_NO_LEADER)

#define ERTS_THR_PRGR_LFLGS_ACTIVE(LFLGS) \
    ((LFLGS) & ERTS_THR_PRGR_LFLG_ACTIVE_MASK)

#define ERTS_THR_PRGR_LFLGS_ALL_WAITING(LFLGS) \
    (((LFLGS) & (ERTS_THR_PRGR_LFLG_NO_LEADER \
		 |ERTS_THR_PRGR_LFLG_ACTIVE_MASK)) \
     == ERTS_THR_PRGR_LFLG_NO_LEADER)

/*
 * We use a 64-bit value for thread progress. By this wrapping of
 * the thread progress will more or less never occur.
 *
 * On 32-bit systems we therefore need a double word atomic.
 */

#define read_acqb erts_thr_prgr_read_acqb__

#ifdef ARCH_64

static ERTS_INLINE void
set_mb(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic_set_mb(atmc, val);
}

static ERTS_INLINE void
set_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic_set_nob(atmc, val);
}

static ERTS_INLINE ErtsThrPrgrVal
read_nob(ERTS_THR_PRGR_ATOMIC *atmc)
{
    return (ErtsThrPrgrVal) erts_atomic_read_nob(atmc);
}

static ERTS_INLINE void
init_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    erts_atomic_init_nob(atmc, val);
}

#else

#undef dw_sint_to_val
#define dw_sint_to_val erts_thr_prgr_dw_sint_to_val__

static void
val_to_dw_sint(ethr_dw_sint_t *dw_sint, ErtsThrPrgrVal val)
{
#ifdef ETHR_SU_DW_NAINT_T__
    dw_sint->dw_sint = (ETHR_SU_DW_NAINT_T__) val;
#else
    dw_sint->sint[ETHR_DW_SINT_LOW_WORD]
	= (ethr_sint_t) (val & 0xffffffff);
    dw_sint->sint[ETHR_DW_SINT_HIGH_WORD]
	= (ethr_sint_t) ((val >> 32) & 0xffffffff);
#endif
}

static ERTS_INLINE void
set_mb(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    ethr_dw_sint_t dw_sint;
    val_to_dw_sint(&dw_sint, val);
    erts_dw_atomic_set_mb(atmc, &dw_sint);
}

static ERTS_INLINE void
set_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    ethr_dw_sint_t dw_sint;
    val_to_dw_sint(&dw_sint, val);
    erts_dw_atomic_set_nob(atmc, &dw_sint);
}

static ERTS_INLINE ErtsThrPrgrVal
read_nob(ERTS_THR_PRGR_ATOMIC *atmc)
{
    ethr_dw_sint_t dw_sint;
    erts_dw_atomic_read_nob(atmc, &dw_sint);
    return erts_thr_prgr_dw_sint_to_val__(&dw_sint);
}

static ERTS_INLINE void
init_nob(ERTS_THR_PRGR_ATOMIC *atmc, ErtsThrPrgrVal val)
{
    ethr_dw_sint_t dw_sint;
    val_to_dw_sint(&dw_sint, val);
    erts_dw_atomic_init_nob(atmc, &dw_sint);
}

#endif

/* #define ERTS_THR_PROGRESS_STATE_DEBUG */

#ifdef ERTS_THR_PROGRESS_STATE_DEBUG

#ifdef __GNUC__
#warning "Thread progress state debug is on"
#endif

#define ERTS_THR_PROGRESS_STATE_DEBUG_LEADER	(((erts_aint32_t) 1) << 0)
#define ERTS_THR_PROGRESS_STATE_DEBUG_ACTIVE	(((erts_aint32_t) 1) << 1)

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

    erts_atomic32_t pref_wakeup_used;
    erts_atomic32_t managed_id;
    erts_atomic32_t unmanaged_id;    
} ErtsThrPrgrMiscVolatile;

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

typedef struct {
    union {
	ErtsThrPrgrMiscVolatile tile;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(
		sizeof(ErtsThrPrgrMiscVolatile))];
    } vola;
    ErtsThrPrgrArray *thr;
    struct {
	int no;
	ErtsThrPrgrWakeupCallback *callback;
	ErtsThrPrgrManagedWakeupData *data[ERTS_THR_PRGR_WAKEUP_DATA_SIZE];
    } managed;
    struct {
	int no;
	ErtsThrPrgrWakeupCallback *callback;
	ErtsThrPrgrUnmanagedWakeupData *data[ERTS_THR_PRGR_WAKEUP_DATA_SIZE];
    } unmanaged;
} ErtsThrPrgrInternalData;

static ErtsThrPrgrInternalData *intrnl;

ErtsThrPrgr erts_thr_prgr__;

erts_tsd_key_t erts_thr_prgr_data_key__;

static void handle_wakeup_requests(ErtsThrPrgrVal current);
static int got_sched_wakeups(void);

static ERTS_INLINE void
wakeup_managed(int id)
{
    ErtsThrPrgrWakeupCallback *wdp = &intrnl->managed.callback[id];
    ASSERT(0 <= id && id < intrnl->managed.no);
    wdp->wakeup(wdp->arg);
}


static ERTS_INLINE void
wakeup_unmanaged(int id)
{
    ErtsThrPrgrWakeupCallback *wdp = &intrnl->unmanaged.callback[id];
    ASSERT(0 <= id && id < intrnl->unmanaged.no);
    wdp->wakeup(wdp->arg);
}

static ERTS_INLINE ErtsThrPrgrData *
thr_prgr_data(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd;
    if (esdp)
	tpd = &esdp->thr_progress_data;
    else
	tpd = erts_tsd_get(erts_thr_prgr_data_key__);
    ASSERT(tpd);
    return tpd;
}

void
erts_thr_progress_pre_init(void)
{
    intrnl = NULL;
    erts_tsd_key_create(&erts_thr_prgr_data_key__);
    init_nob(&erts_thr_prgr__.current, 0);
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

    cb_sz = sizeof(ErtsThrPrgrWakeupCallback)*(managed+unmanaged);
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

    erts_atomic32_init_nob(&intrnl->vola.tile.lflgs,
			   ERTS_THR_PRGR_LFLG_NO_LEADER);
    erts_atomic32_init_nob(&intrnl->vola.tile.pref_wakeup_used, 0);
    erts_atomic32_init_nob(&intrnl->vola.tile.managed_id, no_schedulers);
    erts_atomic32_init_nob(&intrnl->vola.tile.unmanaged_id, -1);

    intrnl->thr = (ErtsThrPrgrArray *) ptr;
    ptr += thr_arr_sz;
    for (i = 0; i < managed; i++)
	init_nob(&intrnl->thr[i].data.current, 0);

    intrnl->managed.callback = (ErtsThrPrgrWakeupCallback *) ptr;
    intrnl->unmanaged.callback = &intrnl->managed.callback[managed];
    ptr += cb_sz;

    intrnl->managed.no = managed;
    for (i = 0; i < managed; i++) {
	intrnl->managed.callback[i].arg = NULL;
	intrnl->managed.callback[i].wakeup = NULL;
    }

    intrnl->unmanaged.no = unmanaged;
    for (i = 0; i < unmanaged; i++) {
	intrnl->unmanaged.callback[i].arg = NULL;
	intrnl->unmanaged.callback[i].wakeup = NULL;
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
erts_thr_progress_register_unmanaged_thread(ErtsThrPrgrWakeupCallback *callback)
{
    ErtsThrPrgrData *tpd;
    if (erts_tsd_get(erts_thr_prgr_data_key__))
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Double register of thread\n",
		 __FILE__, __LINE__, __func__);
    /*
     * We only allocate the part up to the leader field
     * which is the first field only used by managed threads
     */
    tpd = erts_alloc(ERTS_ALC_T_THR_PRGR_DATA,
		     offsetof(ErtsThrPrgrData, leader));
    tpd->id = (int) erts_atomic32_inc_read_nob(&intrnl->vola.tile.unmanaged_id);
    tpd->is_managed = 0;
    ASSERT(tpd->id >= 0);
    if (tpd->id >= intrnl->unmanaged.no)
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Too many unmanaged registered threads\n",
		 __FILE__, __LINE__, __func__);

    init_wakeup_request_array(&tpd->wakeup_request[0]);
    erts_tsd_set(erts_thr_prgr_data_key__, (void *) tpd);

    intrnl->unmanaged.callback[tpd->id] = *callback;
}


void
erts_thr_progress_register_managed_thread(ErtsSchedulerData *esdp,
					  ErtsThrPrgrWakeupCallback *callback,
					  int pref_wakeup)
{
    ErtsThrPrgrData *tpd;
    if (erts_tsd_get(erts_thr_prgr_data_key__))
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Double register of thread\n",
		 __FILE__, __LINE__, __func__);
    if (esdp)
	tpd = &esdp->thr_progress_data;
    else
	tpd = erts_alloc(ERTS_ALC_T_THR_PRGR_DATA, sizeof(ErtsThrPrgrData));

    if (pref_wakeup
	&& !erts_atomic32_xchg_nob(&intrnl->vola.tile.pref_wakeup_used, 1))
	tpd->id = 0;
    else if (esdp)
	tpd->id = (int) esdp->no;
    else
	tpd->id = erts_atomic32_inc_read_nob(&intrnl->vola.tile.managed_id);
    ASSERT(tpd->id >= 0);
    if (tpd->id >= intrnl->managed.no)
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:%s(): Too many managed registered threads\n",
		 __FILE__, __LINE__, __func__);

    tpd->is_managed = 1;

    init_wakeup_request_array(&tpd->wakeup_request[0]);

    ERTS_THR_PROGRESS_STATE_DEBUG_INIT(tpd->id);

    tpd->leader = 0;
    tpd->active = 1;
    tpd->previous.local = 0;
    tpd->previous.current = ERTS_THR_PRGR_VAL_WAITING;
    erts_tsd_set(erts_thr_prgr_data_key__, (void *) tpd);

    erts_atomic32_inc_nob(&intrnl->vola.tile.lflgs);
    intrnl->managed.callback[tpd->id] = *callback;
}

static ERTS_INLINE int
leader_update(ErtsThrPrgrData *tpd)
{
    if (tpd->leader) {
	erts_aint32_t lflgs;
	ErtsThrPrgrVal next;
	int ix, sz, make_progress;

	if (tpd->previous.current == ERTS_THR_PRGR_VAL_WAITING) {
	    /* Took over as leader from another thread */
	    tpd->previous.current = read_acqb(&erts_thr_prgr__.current);
	    tpd->previous.next = tpd->previous.current;
	    tpd->previous.next++;
	    if (tpd->previous.next == ERTS_THR_PRGR_VAL_WAITING)
		tpd->previous.next = 0;
	}

	if (tpd->previous.local == tpd->previous.current) {
	    ErtsThrPrgrVal val = tpd->previous.current + 1;
	    if (val == ERTS_THR_PRGR_VAL_WAITING)
		val = 0;
	    tpd->previous.local = val;
	    set_mb(&intrnl->thr[tpd->id].data.current, val);
	}

	next = tpd->previous.next;

	make_progress = 1;
	sz = intrnl->managed.no;
	for (ix = 0; ix < sz; ix++) {
	    ErtsThrPrgrVal tmp;
	    tmp = read_nob(&intrnl->thr[ix].data.current);
	    if (tmp != next && tmp != ERTS_THR_PRGR_VAL_WAITING) {
		make_progress = 0;
		ASSERT(erts_thr_progress_has_passed__(next, tmp));
		break;
	    }
	}

	if (make_progress) {
	    ErtsThrPrgrVal current = next;

	    next++;
	    if (next == ERTS_THR_PRGR_VAL_WAITING)
		next = 0;

	    set_nob(&intrnl->thr[tpd->id].data.current, next);
	    set_mb(&erts_thr_prgr__.current, current);
	    tpd->previous.local = next;
	    tpd->previous.next = next;
	    tpd->previous.current = current;

#if ERTS_THR_PRGR_PRINT_VAL
	    if (current % 1000 == 0)
		erts_fprintf(stderr, "%b64u\n", current);
#endif
	    handle_wakeup_requests(current);
	}

	if (!tpd->active) {
	    tpd->leader = 0;
	    tpd->previous.current = ERTS_THR_PRGR_VAL_WAITING;
#if ERTS_THR_PRGR_PRINT_LEADER
	    erts_fprintf(stderr, "L <- %d\n", tpd->id);
#endif
	    ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(tpd->id, 0);

	    lflgs = erts_atomic32_read_bor_relb(&intrnl->vola.tile.lflgs,
						ERTS_THR_PRGR_LFLG_NO_LEADER);
	    if (ERTS_THR_PRGR_LFLGS_ACTIVE(lflgs) == 0 && got_sched_wakeups())
		wakeup_managed(0);
	}
    }

    return tpd->leader;
}

static int
update(ErtsThrPrgrData *tpd)
{
    ErtsThrPrgrVal val;

    if (!tpd->leader) {
	erts_aint32_t lflgs;
	val = read_acqb(&erts_thr_prgr__.current);
	if (tpd->previous.local == val) {
	    val++;
	    if (val == ERTS_THR_PRGR_VAL_WAITING)
		val = 0;
	    tpd->previous.local = val;
	    set_mb(&intrnl->thr[tpd->id].data.current, val);
	}

	lflgs = erts_atomic32_read_nob(&intrnl->vola.tile.lflgs);
	if ((lflgs & ERTS_THR_PRGR_LFLG_NO_LEADER)
	    && (tpd->active || ERTS_THR_PRGR_LFLGS_ACTIVE(lflgs) == 0)) {
	    /* Try to take over leadership... */
	    erts_aint32_t olflgs;
	    olflgs = erts_atomic32_read_band_acqb(
		&intrnl->vola.tile.lflgs,
		~ERTS_THR_PRGR_LFLG_NO_LEADER);
	    if (olflgs & ERTS_THR_PRGR_LFLG_NO_LEADER) {
		tpd->leader = 1;
#if ERTS_THR_PRGR_PRINT_LEADER
		erts_fprintf(stderr, "L -> %d\n", tpd->id);
#endif
		ERTS_THR_PROGRESS_STATE_DEBUG_SET_LEADER(tpd->id, 1);
	    }
	}
    }
    return tpd->leader;
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

    tpd->previous.local = ERTS_THR_PRGR_VAL_WAITING;
    set_mb(&intrnl->thr[tpd->id].data.current, ERTS_THR_PRGR_VAL_WAITING);

    lflgs = erts_atomic32_read_nob(&intrnl->vola.tile.lflgs);
    if (ERTS_THR_PRGR_LFLGS_ALL_WAITING(lflgs) && got_sched_wakeups())
	wakeup_managed(0); /* Someone need to make progress */
}

void
erts_thr_progress_finalize_wait(ErtsSchedulerData *esdp)
{
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);
    ErtsThrPrgrVal current, val;

    /*
     * We aren't allowed to continue until our thread
     * progress is past global current.
     */
    val = current = read_acqb(&erts_thr_prgr__.current);
    while (1) {
	val++;
	if (val == ERTS_THR_PRGR_VAL_WAITING)
	    val = 0;
	tpd->previous.local = val;
	set_mb(&intrnl->thr[tpd->id].data.current, val);
	val = read_acqb(&erts_thr_prgr__.current);
	if (current == val)
	    break;
	current = val;
    }
    if (update(tpd))
	leader_update(tpd);
}

void
erts_thr_progress_active(ErtsSchedulerData *esdp, int on)
{
    ErtsThrPrgrData *tpd = thr_prgr_data(esdp);

    ERTS_THR_PROGRESS_STATE_DEBUG_SET_ACTIVE(tpd->id, on);

    if (on) {
	ASSERT(!tpd->active);
	tpd->active = 1;
	erts_atomic32_inc_nob(&intrnl->vola.tile.lflgs);
    }
    else {
	ASSERT(tpd->active);
	tpd->active = 0;
	erts_atomic32_dec_nob(&intrnl->vola.tile.lflgs);

	if (update(tpd))
	    leader_update(tpd);
    }

#ifdef DEBUG
    {
	erts_aint32_t n = erts_atomic32_read_nob(&intrnl->vola.tile.lflgs);
	n &= ERTS_THR_PRGR_LFLG_ACTIVE_MASK;
	ASSERT(tpd->active <= n && n <= intrnl->managed.no);
    }
#endif

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
	 * equal to 'current + 2'. That is, users should never
	 * get a hold of values larger than that.
	 *
	 * That is, valid values are values less than 'current + 3'.
	 *
	 * Values larger than this won't work with the wakeup
	 * algorithm.
	 */

	limit = current + 3;
	if (limit == ERTS_THR_PRGR_VAL_WAITING)
	    limit = 0;
	else if (limit < current) /* Wrapped */
	    limit + 1;

	if (!erts_thr_progress_has_passed__(limit, wakeup))
	    erl_exit(ERTS_ABORT_EXIT,
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
    ASSERT(tpd->previous.local != ERTS_THR_PRGR_VAL_WAITING);

    if (has_reached_wakeup(value)) {
	wakeup_managed(tpd->id);
	return;
    }

    wix = ERTS_THR_PRGR_WAKEUP_IX(value);
    if (tpd->wakeup_request[wix] == value)
	return; /* Already got a request registered */

    ASSERT(erts_thr_progress_has_passed__(value,
					  tpd->wakeup_request[wix]));


    if (tpd->previous.local == value) {
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
	erl_exit(ERTS_ABORT_EXIT, "Internal error: Too many wakeup requests\n");
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
    ASSERT(tpd);
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
		if (hmask & (1 << hbit)) {
		    erts_aint_t lmask;
		    int lix = hbase + hbit;
		    ASSERT(0 <= lix && lix < umwd->low_sz);
		    lmask = erts_atomic32_read_nob(&umwd->low[lix]);
		    if (lmask) {
			int lbase = lix << ERTS_THR_PRGR_BM_SHIFT;
			int lbit;
			for (lbit = 0; lbit < ERTS_THR_PRGR_BM_BITS; lbit++) {
			    if (lmask & (1 << lbit)) {
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

#endif
