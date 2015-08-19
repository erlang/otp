/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2013. All Rights Reserved.
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
 * Description: Lock-free queue for communication between threads.
 *
 *              Currently only a many-to-one version has been,
 *              implemented, i.e., many threads can enqueue but
 *              only one thread can dequeue at a time. It doesn't
 *              have to be the same thread dequeuing every time, but
 *              synchronization so that only one thread dequeues
 *              at a time has to be provided by other means.
 *
 *              When/If the need for a many-to-many queue arises,
 *              this implementation can relatively easy be extended
 *              to support that too.
 *
 *              Usage instructions below.
 *
 * Author: 	Rickard Green
 */

/*
 * ------ Usage instructions -----------------------------------------------
 *
 * Dequeuing generates garbage that needs to be cleaned up.
 * erts_thr_q_dequeue() automatically cleans, but garbage may have to be
 * cleaned up also when the queue is empty. This is done by calling
 * erts_thr_q_clean(). In the SMP case thread progress may have to be made
 * before cleaning can continue. If so, erts_thr_q_need_thr_progress() in
 * combination with erts_thr_progress_wakeup() can be used in order to
 * request a wakeup at appropriate time.
 *
 * Enqueuing implies memory allocation and dequeuing implies memory
 * deallocation. Memory allocation can be moved to another more suitable
 * thread using  erts_thr_q_prepare_enqueue() together with
 * erts_thr_q_enqueue_prepared() instead of using erts_thr_q_enqueue().
 * Memory deallocation can can be moved to another more suitable thread by
 * disabling auto_finalize_dequeue when initializing the queue and then use
 * erts_thr_q_get_finalize_dequeue_data() together
 * erts_thr_q_finalize_dequeue() after dequeuing or cleaning.
 *
 * Ending the life of the queue using either erts_thr_q_destroy()
 * or erts_thr_q_finalize() impies cleaning the queue. Both functions
 * return the cleaning result and may have to be called multiple times
 * until the queue is clean. Once one of these functions have been called
 * enqueuing is not allowed. This has to be synchronized by the user.
 * If auto_finalize_dequeue has been disabled, the finalize dequeue
 * functionality has to be called after ending the life of the queue just
 * as when dequeuing or cleaning on a queue that is alive.
 *
 * -------------------------------------------------------------------------
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_thr_queue.h"

#if defined(DEBUG)
#define ERTS_THR_Q_DBG_CHK_DATA 1
#else
#define ERTS_THR_Q_DBG_CHK_DATA 0
#endif

#define ERTS_THR_Q_MAX_CLEAN_REACHED_HEAD_COUNT 100
#define ERTS_THR_Q_MAX_SCHED_CLEAN_OPS 50
#define ERTS_THR_Q_MAX_DEQUEUE_CLEAN_OPS 3

#define ERTS_THR_Q_MAX_FINI_DEQ_OPS 50

#ifdef ERTS_SMP
ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(sl_element,
				 ErtsThrQElement_t,
				 1000,
				 ERTS_ALC_T_THR_Q_EL_SL)
#else

static void
init_sl_element_alloc(void)
{
}

static ErtsThrQElement_t *
sl_element_alloc(void)
{
    return erts_alloc(ERTS_ALC_T_THR_Q_EL_SL,
		      sizeof(ErtsThrQElement_t));
}

static void
sl_element_free(ErtsThrQElement_t *p)
{
    erts_free(ERTS_ALC_T_THR_Q_EL_SL, p);
}

#endif

#define ErtsThrQDirtyReadEl(A) \
    ((ErtsThrQElement_t *) erts_atomic_read_dirty((A)))
#define ErtsThrQDirtySetEl(A, V) \
    erts_atomic_set_dirty((A), (erts_aint_t) (V))

typedef union {
    ErtsThrQ_t q;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsThrQ_t))];
} ErtsAlignedThrQ_t;

void
erts_thr_q_init(void)
{
    init_sl_element_alloc();
}

static void noop_callback(void *arg) { }

void
erts_thr_q_initialize(ErtsThrQ_t *q, ErtsThrQInit_t *qi)
{
#ifndef USE_THREADS
    q->init = *qi;
    if (!q->init.notify)
	q->init.notify = noop_callback;
    q->first = NULL;
    q->last = NULL;
    q->q.blk = NULL;
#else
    erts_atomic_init_nob(&q->tail.data.marker.next, ERTS_AINT_NULL);
    q->tail.data.marker.data.ptr = NULL;
    erts_atomic_init_nob(&q->tail.data.last,
			 (erts_aint_t) &q->tail.data.marker);
    erts_atomic_init_nob(&q->tail.data.um_refc[0], 0);
    erts_atomic_init_nob(&q->tail.data.um_refc[1], 0);
    erts_atomic32_init_nob(&q->tail.data.um_refc_ix, 0);
    q->tail.data.live = qi->live.objects;
    q->tail.data.arg = qi->arg;
    q->tail.data.notify = qi->notify;
    if (!q->tail.data.notify)
	q->tail.data.notify = noop_callback;

    erts_atomic_init_nob(&q->head.head, (erts_aint_t) &q->tail.data.marker);
    q->head.live = qi->live.objects;
    q->head.first = &q->tail.data.marker;
    q->head.unref_end = &q->tail.data.marker;
    q->head.clean_reached_head_count = 0;
    q->head.deq_fini.automatic = qi->auto_finalize_dequeue;
    q->head.deq_fini.start = NULL;
    q->head.deq_fini.end = NULL;
#ifdef ERTS_SMP
    q->head.next.thr_progress = erts_thr_progress_current();
    q->head.next.thr_progress_reached = 1;
#endif
    q->head.next.um_refc_ix = 1;
    q->head.next.unref_end = &q->tail.data.marker;
    q->head.used_marker = 1;
    q->head.arg = qi->arg;
    q->head.notify = q->tail.data.notify;
    q->q.finalizing = 0;
    q->q.live = qi->live.queue;
    q->q.blk = NULL;
#endif
}

ErtsThrQCleanState_t
erts_thr_q_finalize(ErtsThrQ_t *q)
{
#ifdef USE_THREADS
    q->q.finalizing = 1;
#endif
    while (erts_thr_q_dequeue(q));
    return erts_thr_q_clean(q);
}

ErtsThrQ_t *
erts_thr_q_create(ErtsThrQInit_t *qi)
{
    ErtsAlcType_t atype;
    ErtsThrQ_t *q, *qblk;
    UWord qw;

    switch (qi->live.queue) {
    case ERTS_THR_Q_LIVE_SHORT:
	atype = ERTS_ALC_T_THR_Q_SL;
	break;
    case ERTS_THR_Q_LIVE_LONG:
	atype = ERTS_ALC_T_THR_Q_LL;
	break;
    default:
	atype = ERTS_ALC_T_THR_Q;
	break;
    }

    qw = (UWord) erts_alloc(atype,
			    sizeof(ErtsThrQ_t) + (ERTS_CACHE_LINE_SIZE-1));
    qblk = (ErtsThrQ_t *) qw;
    if (qw & ERTS_CACHE_LINE_MASK)
	qw = (qw & ~ERTS_CACHE_LINE_MASK) + ERTS_CACHE_LINE_SIZE;
    ASSERT((qw & ERTS_CACHE_LINE_MASK) == 0);
    q = (ErtsThrQ_t *) qw;
    erts_thr_q_initialize(q, qi);
    q->q.blk = qblk;
    return q;
}

ErtsThrQCleanState_t
erts_thr_q_destroy(ErtsThrQ_t *q)
{
    if (!q->q.blk)
	erts_exit(ERTS_ABORT_EXIT,
		 "Trying to destroy not created thread queue\n");
    return erts_thr_q_finalize(q);
}

#ifdef USE_THREADS

static void
destroy(ErtsThrQ_t *q)
{
    ErtsAlcType_t atype;
    switch (q->q.live) {
    case ERTS_THR_Q_LIVE_SHORT:
	atype = ERTS_ALC_T_THR_Q_SL;
	break;
    case ERTS_THR_Q_LIVE_LONG:
	atype = ERTS_ALC_T_THR_Q_LL;
	break;
    default:
	atype = ERTS_ALC_T_THR_Q;
	break;
    }
    erts_free(atype, q->q.blk);
}

#endif

static ERTS_INLINE ErtsThrQElement_t *
element_live_alloc(ErtsThrQLive_t live)
{
    switch (live) {
    case ERTS_THR_Q_LIVE_SHORT:
	return sl_element_alloc();
    default:
	return (ErtsThrQElement_t *) erts_alloc(ERTS_ALC_T_THR_Q_EL,
						sizeof(ErtsThrQElement_t));
    }
}

static ERTS_INLINE ErtsThrQElement_t *
element_alloc(ErtsThrQ_t *q)
{
    ErtsThrQLive_t live;
#ifdef USE_THREADS
    live = q->tail.data.live;
#else
    live = q->init.live.objects;
#endif
    return element_live_alloc(live);
}

static ERTS_INLINE void
element_live_free(ErtsThrQLive_t live, ErtsThrQElement_t *el)
{
    switch (live) {
    case ERTS_THR_Q_LIVE_SHORT:
	sl_element_free(el);
	break;
    default:
	erts_free(ERTS_ALC_T_THR_Q_EL, el);
    }
}

static ERTS_INLINE void
element_free(ErtsThrQ_t *q, ErtsThrQElement_t *el)
{
    ErtsThrQLive_t live;
#ifdef USE_THREADS
    live = q->head.live;
#else
    live = q->init.live.objects;
#endif
    element_live_free(live, el);
}

#ifdef USE_THREADS

static ERTS_INLINE ErtsThrQElement_t *
enqueue_managed(ErtsThrQ_t *q, ErtsThrQElement_t *this)
{
    erts_aint_t ilast, itmp;

    erts_atomic_init_nob(&this->next, ERTS_AINT_NULL);
    /* Enqueue at end of list... */

    ilast = erts_atomic_read_nob(&q->tail.data.last);
    while (1) {
	ErtsThrQElement_t *last = (ErtsThrQElement_t *) ilast;
	itmp = erts_atomic_cmpxchg_mb(&last->next,
				      (erts_aint_t) this,
				      ERTS_AINT_NULL);
	if (itmp == ERTS_AINT_NULL)
	    break;
	ilast = itmp;
    }

    /* Move last pointer forward... */
    while (1) {
	if (erts_atomic_read_rb(&this->next) != ERTS_AINT_NULL) {
	    /* Someone else will move it forward */
	    ilast = erts_atomic_read_rb(&q->tail.data.last);
	    return (ErtsThrQElement_t *) ilast;
	}
	itmp = erts_atomic_cmpxchg_mb(&q->tail.data.last,
				      (erts_aint_t) this,
				      ilast);
	if (ilast == itmp)
	    return this;
	ilast = itmp;
    }
}

static ERTS_INLINE ErtsThrQElement_t *
enqueue_marker(ErtsThrQ_t *q, ErtsThrQElement_t **headp)
{
    int maybe_notify;
    erts_aint_t inext;
    ErtsThrQElement_t *last, *head;

    if (headp)
	head = *headp;
    else
	head = ErtsThrQDirtyReadEl(&q->head.head);

    ASSERT(!q->head.used_marker);
    q->head.used_marker = 1;
    last = enqueue_managed(q, &q->tail.data.marker);
    maybe_notify = &q->tail.data.marker == last;
    inext = erts_atomic_read_acqb(&head->next);
    if (inext == (erts_aint_t) &q->tail.data.marker) {
	ErtsThrQDirtySetEl(&q->head.head, &q->tail.data.marker);
	if (headp)
	    *headp = &q->tail.data.marker;
    }
    else if (maybe_notify) {
	/*
	 * We need to notify; otherwise, we might loose a notification
	 * for a concurrently inserted element.
	 */
	q->head.notify(q->head.arg);
    }
    return last;
}


static ErtsThrQCleanState_t
clean(ErtsThrQ_t *q, int max_ops, int do_notify)
{
    ErtsThrQElement_t *head;
    erts_aint_t ilast;
    int um_refc_ix;
    int ops;

    for (ops = 0; ops < max_ops; ops++) {
	ErtsThrQElement_t *tmp;
    restart:
	ASSERT(q->head.first);
	head = ErtsThrQDirtyReadEl(&q->head.head);
	if (q->head.first == head) {
	    q->head.clean_reached_head_count++;
	    if (q->head.clean_reached_head_count
		>= ERTS_THR_Q_MAX_CLEAN_REACHED_HEAD_COUNT) {
		q->head.clean_reached_head_count = 0;
		break;
	    }
	    goto inspect_head;
	}
	if (q->head.first == q->head.unref_end)	    
	    break;
	if (q->head.first == &q->tail.data.marker) {
	    q->head.used_marker = 0;
	    q->head.first = ErtsThrQDirtyReadEl(&q->head.first->next);
	    goto restart;
	}
	tmp = q->head.first;
	q->head.first = ErtsThrQDirtyReadEl(&q->head.first->next);
	if (q->head.deq_fini.automatic)
	    element_free(q, tmp);
	else {
	    tmp->data.ptr = (void *) (UWord) q->head.live;
	    if (!q->head.deq_fini.start)
		q->head.deq_fini.start = tmp;
	    else if (ErtsThrQDirtyReadEl(&q->head.deq_fini.end->next)
		     == &q->tail.data.marker)
		ErtsThrQDirtySetEl(&q->head.deq_fini.end->next, tmp);
	    q->head.deq_fini.end = tmp;
	}
    }

    ilast = erts_atomic_read_nob(&q->tail.data.last);
    if (q->head.first == ((ErtsThrQElement_t *) ilast)
	&& ((ErtsThrQElement_t *) ilast) == &q->tail.data.marker
	&& q->head.first == &q->tail.data.marker) {
	/* Empty and clean queue */
	if (q->q.finalizing)
	    destroy(q);
	return ERTS_THR_Q_CLEAN;
    }

#ifdef ERTS_SMP
    if (q->head.next.thr_progress_reached
	|| erts_thr_progress_has_reached(q->head.next.thr_progress)) {
	q->head.next.thr_progress_reached = 1;
#endif
	um_refc_ix = q->head.next.um_refc_ix;
	if (erts_atomic_read_acqb(&q->tail.data.um_refc[um_refc_ix]) == 0) {
	    /* Move unreferenced end pointer forward... */
	    q->head.clean_reached_head_count = 0;
	    q->head.unref_end = q->head.next.unref_end;

	    if (!q->head.used_marker
		&& q->head.unref_end == (ErtsThrQElement_t *) ilast)
		ilast = (erts_aint_t) enqueue_marker(q, NULL);

	    if (q->head.unref_end == (ErtsThrQElement_t *) ilast)
		ERTS_SMP_MEMORY_BARRIER;
	    else {
		q->head.next.unref_end = (ErtsThrQElement_t *) ilast;
#ifdef ERTS_SMP
		q->head.next.thr_progress = erts_thr_progress_later(NULL);
#endif
		erts_atomic32_set_relb(&q->tail.data.um_refc_ix,
				       um_refc_ix);
		q->head.next.um_refc_ix = um_refc_ix == 0 ? 1 : 0;
#ifdef ERTS_SMP
		q->head.next.thr_progress_reached = 0;
#endif
	    }
	}
#ifdef ERTS_SMP
    }
#endif

    head = ErtsThrQDirtyReadEl(&q->head.head);
    if (q->head.first == head) {
    inspect_head:
	if (!q->head.used_marker) {
	    erts_aint_t inext;
	    inext = erts_atomic_read_acqb(&head->next);
	    if (inext == ERTS_AINT_NULL) {
		enqueue_marker(q, &head);
		if (head == &q->tail.data.marker)
		    goto check_thr_progress;
	    }
	}

	if (q->q.finalizing) {
	    ilast = erts_atomic_read_nob(&q->tail.data.last);
	    if (q->head.first == ((ErtsThrQElement_t *) ilast)
		&& ((ErtsThrQElement_t *) ilast) == &q->tail.data.marker
		&& q->head.first == &q->tail.data.marker) {
		destroy(q);
	    }
	    else {
		goto dirty;
	    }
	}
	return ERTS_THR_Q_CLEAN;
    }

    if (q->head.first != q->head.unref_end)
	goto dirty;

check_thr_progress:

#ifdef ERTS_SMP
    if (q->head.next.thr_progress_reached)
#endif
    {
	int um_refc_ix = q->head.next.um_refc_ix;
	if (erts_atomic_read_acqb(&q->tail.data.um_refc[um_refc_ix]) == 0) {
	dirty:
	    if (do_notify)
		q->head.notify(q->head.arg);
	    return ERTS_THR_Q_DIRTY;
	}
    }

    return ERTS_THR_Q_NEED_THR_PRGR;
}

#endif

ErtsThrQCleanState_t
erts_thr_q_clean(ErtsThrQ_t *q)
{
#ifdef USE_THREADS
    return clean(q, ERTS_THR_Q_MAX_SCHED_CLEAN_OPS, 0);
#else
    return ERTS_THR_Q_CLEAN;
#endif
}

ErtsThrQCleanState_t
erts_thr_q_inspect(ErtsThrQ_t *q, int ensure_empty)
{
#ifndef USE_THREADS
    return ERTS_THR_Q_CLEAN;
#else
    ErtsThrQElement_t *head = ErtsThrQDirtyReadEl(&q->head.head);
    if (ensure_empty) {
	erts_aint_t inext;
	inext = erts_atomic_read_acqb(&head->next);
	if (inext != ERTS_AINT_NULL) {
	    if (&q->tail.data.marker != (ErtsThrQElement_t *) inext)
		return ERTS_THR_Q_DIRTY;
	    else {
		head = (ErtsThrQElement_t *) inext;
		ErtsThrQDirtySetEl(&q->head.head, head);
		inext = erts_atomic_read_acqb(&head->next);
		if (inext != ERTS_AINT_NULL)
		    return ERTS_THR_Q_DIRTY;
	    }
	}
    }

    if (q->head.first == head) {
	if (!q->head.used_marker) {
	    erts_aint_t inext;
	    inext = erts_atomic_read_acqb(&head->next);
	    if (inext == ERTS_AINT_NULL)
		return ERTS_THR_Q_DIRTY;
	}
	return ERTS_THR_Q_CLEAN;
    }

    if (q->head.first != q->head.unref_end)
	return ERTS_THR_Q_DIRTY;

#ifdef ERTS_SMP
    if (q->head.next.thr_progress_reached)
#endif
    {
	int um_refc_ix = q->head.next.um_refc_ix;
	if (erts_atomic_read_acqb(&q->tail.data.um_refc[um_refc_ix]) == 0)
	    return ERTS_THR_Q_DIRTY;
    }
    return ERTS_THR_Q_NEED_THR_PRGR;
#endif
}

static void
enqueue(ErtsThrQ_t *q, void *data, ErtsThrQElement_t *this)
{
#ifndef USE_THREADS
    ASSERT(data);

    this->next = NULL;
    this->data.ptr = data;

    if (q->last)
	q->last->next = this;
    else {
	q->first = q->last = this;
	q->init.notify(q->init.arg);
    }
#else
    int notify;
    int um_refc_ix = 0;
#ifdef ERTS_SMP
    int unmanaged_thread;
#endif

#if ERTS_THR_Q_DBG_CHK_DATA
    if (!data)
	erts_exit(ERTS_ABORT_EXIT, "Missing data in enqueue\n");
#endif

    ASSERT(!q->q.finalizing);

    this->data.ptr = data;

#ifdef ERTS_SMP
    unmanaged_thread = !erts_thr_progress_is_managed_thread();
    if (unmanaged_thread)
#endif
    {
	um_refc_ix = erts_atomic32_read_acqb(&q->tail.data.um_refc_ix);
	while (1) {
	    int tmp_um_refc_ix;
	    erts_atomic_inc_acqb(&q->tail.data.um_refc[um_refc_ix]);
	    tmp_um_refc_ix = erts_atomic32_read_acqb(&q->tail.data.um_refc_ix);
	    if (tmp_um_refc_ix == um_refc_ix)
		break;
	    erts_atomic_dec_relb(&q->tail.data.um_refc[um_refc_ix]);
	    um_refc_ix = tmp_um_refc_ix;
	}
    }

    notify = this == enqueue_managed(q, this);
	

#ifdef ERTS_SMP
    if (unmanaged_thread)
#endif
    {
	if (notify)
	    erts_atomic_dec_relb(&q->tail.data.um_refc[um_refc_ix]);
	else if (erts_atomic_dec_read_relb(&q->tail.data.um_refc[um_refc_ix]) == 0)
	    notify = 1;
    }
    if (notify)
	q->tail.data.notify(q->tail.data.arg);
#endif
}

void
erts_thr_q_enqueue(ErtsThrQ_t *q, void *data)
{
    enqueue(q, data, element_alloc(q));
}

ErtsThrQPrepEnQ_t *
erts_thr_q_prepare_enqueue(ErtsThrQ_t *q)
{
    return (ErtsThrQPrepEnQ_t *) element_alloc(q);
}

int
erts_thr_q_get_finalize_dequeue_data(ErtsThrQ_t *q, ErtsThrQFinDeQ_t *fdp)
{
#ifndef USE_THREADS
    return 0;
#else
#ifdef DEBUG
    if (!q->head.deq_fini.start) {
	ASSERT(!q->head.deq_fini.end);
    }
    else {
	ErtsThrQElement_t *e = q->head.deq_fini.start;
	ErtsThrQElement_t *end = q->head.deq_fini.end;
	while (e != end) {
	    ASSERT(ErtsThrQDirtyReadEl(&q->head.head) != e);
	    ASSERT(q->head.first != e);
	    ASSERT(q->head.unref_end != e);
	    e = ErtsThrQDirtyReadEl(&e->next);
	}
    }	
#endif
    fdp->start = q->head.deq_fini.start;
    fdp->end = q->head.deq_fini.end;
    if (fdp->end)
	ErtsThrQDirtySetEl(&fdp->end->next, NULL);
    q->head.deq_fini.start = NULL;
    q->head.deq_fini.end = NULL;
    return fdp->start != NULL;
#endif
}

void
erts_thr_q_append_finalize_dequeue_data(ErtsThrQFinDeQ_t *fdp0,
					ErtsThrQFinDeQ_t *fdp1)
{
#ifdef USE_THREADS
    if (fdp1->start) {
	if (fdp0->end)
	    ErtsThrQDirtySetEl(&fdp0->end->next, fdp1->start);
	else
	    fdp0->start = fdp1->start;
	fdp0->end = fdp1->end;
    }
#endif
}


int erts_thr_q_finalize_dequeue(ErtsThrQFinDeQ_t *state)
{
#ifdef USE_THREADS
    ErtsThrQElement_t *start = state->start;
    if (start) {
	ErtsThrQLive_t live;
	int i;
	for (i = 0; i < ERTS_THR_Q_MAX_FINI_DEQ_OPS; i++) {
	    ErtsThrQElement_t *tmp;
	    if (!start)
		break;
	    tmp = start;
	    start = ErtsThrQDirtyReadEl(&start->next);
	    live = (ErtsThrQLive_t) (UWord) tmp->data.ptr;
	    element_live_free(live, tmp);
	}
	state->start = start;
	if (start)
	    return 1; /* More to do */
	state->end = NULL;
    }
#endif
    return 0;
}

void
erts_thr_q_finalize_dequeue_state_init(ErtsThrQFinDeQ_t *state)
{
#ifdef USE_THREADS
    state->start = NULL;
    state->end = NULL;
#endif
}


void
erts_thr_q_enqueue_prepared(ErtsThrQ_t *q, void *data, ErtsThrQPrepEnQ_t *prep)
{
    ASSERT(prep);
    enqueue(q, data, (ErtsThrQElement_t *) prep);
}

void *
erts_thr_q_dequeue(ErtsThrQ_t *q)
{
#ifndef USE_THREADS
    void *res;
    ErtsThrQElement_t *tmp;

    if (!q->first)
	return NULL;
    tmp = q->first;
    res = tmp->data.ptr;
    q->first = tmp->next;
    if (!q->first)
	q->last = NULL;

    element_free(q, tmp);

    return res;
#else
    ErtsThrQElement_t *head;
    erts_aint_t inext;
    void *res;

    head = ErtsThrQDirtyReadEl(&q->head.head);
    inext = erts_atomic_read_acqb(&head->next);
    if (inext == ERTS_AINT_NULL)
	return NULL;
    head = (ErtsThrQElement_t *) inext;
    ErtsThrQDirtySetEl(&q->head.head, head);
    if (head == &q->tail.data.marker) {
	inext = erts_atomic_read_acqb(&head->next);
	if (inext == ERTS_AINT_NULL)
	    return NULL;
	head = (ErtsThrQElement_t *) inext;
	ErtsThrQDirtySetEl(&q->head.head, head);
    }
    res = head->data.ptr;
#if ERTS_THR_Q_DBG_CHK_DATA
    head->data.ptr = NULL;
    if (!res)
	erts_exit(ERTS_ABORT_EXIT, "Missing data in dequeue\n");
#endif
    clean(q,
	  (q->head.deq_fini.automatic
	   ? ERTS_THR_Q_MAX_DEQUEUE_CLEAN_OPS
	   : ERTS_THR_Q_MAX_SCHED_CLEAN_OPS), 1);
    return res;
#endif
}

#ifdef USE_LTTNG_VM_TRACEPOINTS
int
erts_thr_q_length_dirty(ErtsThrQ_t *q)
{
    int n = 0;
#ifndef USE_THREADS
    void *res;
    ErtsThrQElement_t *tmp;

    for (tmp = q->first; tmp != NULL; tmp = tmp->next) {
        n++;
    }
#else
    ErtsThrQElement_t *e;
    erts_aint_t inext;

    e = ErtsThrQDirtyReadEl(&q->head.head);
    inext = erts_atomic_read_acqb(&e->next);

    while (inext != ERTS_AINT_NULL) {
        e = (ErtsThrQElement_t *) inext;
        if (e != &q->tail.data.marker) {
            /* don't count marker */
            n++;
        }
        inext = erts_atomic_read_acqb(&e->next);
    }
#endif
    return n;
}
#endif
