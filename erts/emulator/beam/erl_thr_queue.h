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
 *              Usage instructions can be found in erts_thr_queue.c
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_THR_QUEUE_H__
#define ERL_THR_QUEUE_H__

#include "sys.h"
#include "erl_threads.h"
#include "erl_alloc.h"
#include "erl_thr_progress.h"

typedef enum {
    ERTS_THR_Q_LIVE_UNDEF,
    ERTS_THR_Q_LIVE_SHORT,
    ERTS_THR_Q_LIVE_LONG
} ErtsThrQLive_t;

#define ERTS_THR_Q_INIT_DEFAULT						\
{									\
    {									\
	ERTS_THR_Q_LIVE_UNDEF,						\
	ERTS_THR_Q_LIVE_SHORT						\
    },									\
    NULL,								\
    NULL,								\
    1									\
}

typedef struct ErtsThrQ_t_ ErtsThrQ_t;

typedef struct {
    struct {
	ErtsThrQLive_t queue;
	ErtsThrQLive_t objects;
    } live;
    void *arg;
    void (*notify)(void *);
    int auto_finalize_dequeue;
} ErtsThrQInit_t;

typedef struct ErtsThrQElement_t_ ErtsThrQElement_t;
typedef struct ErtsThrQElement_t ErtsThrQPrepEnQ_t;

struct ErtsThrQElement_t_ {
#ifdef USE_THREADS
    erts_atomic_t next;
#else
    ErtsThrQElement_t *next;
#endif
    union {
	erts_atomic_t atmc;
	void *ptr;
    } data;
};

typedef struct {
    ErtsThrQElement_t *start;
    ErtsThrQElement_t *end;
} ErtsThrQFinDeQ_t;

typedef enum {
    ERTS_THR_Q_CLEAN,
    ERTS_THR_Q_NEED_THR_PRGR,
    ERTS_THR_Q_DIRTY,
} ErtsThrQCleanState_t;

#ifdef USE_THREADS

typedef struct {
    ErtsThrQElement_t marker;
    erts_atomic_t last;
    erts_atomic_t um_refc[2];
    erts_atomic32_t um_refc_ix;
    ErtsThrQLive_t live;
#ifdef ERTS_SMP
    erts_atomic32_t thr_prgr_clean_scheduled;
#endif
    void *arg;
    void (*notify)(void *);
} ErtsThrQTail_t;

struct ErtsThrQ_t_ {
    /*
     * This structure needs to be cache line aligned for best
     * performance.
     */
    union {
	/* Modified by threads enqueuing */
	ErtsThrQTail_t data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsThrQTail_t))];
    } tail;
    /*
     * Everything below this point is *only* accessed by the
     * thread dequeuing.
     */
    struct {
	erts_atomic_t head;
	ErtsThrQLive_t live;
	ErtsThrQElement_t *first;
	ErtsThrQElement_t *unref_end;
	int clean_reached_head_count;
	struct {
	    int automatic;
	    ErtsThrQElement_t *start;
	    ErtsThrQElement_t *end;
	} deq_fini;
	struct {
#ifdef ERTS_SMP
	    ErtsThrPrgrVal thr_progress;
	    int thr_progress_reached;
#endif
	    int um_refc_ix;
	    ErtsThrQElement_t *unref_end;
	} next;
	int used_marker;
	void *arg;
	void (*notify)(void *);
    } head;
    struct {
	int finalizing;
	ErtsThrQLive_t live;
	void *blk;
    } q;
};

#else /* !USE_THREADS */

struct ErtsThrQ_t_ {
    ErtsThrQInit_t init;
    ErtsThrQElement_t *first;
    ErtsThrQElement_t *last;
    struct {
	void *blk;
    } q;
};

#endif

void erts_thr_q_init(void);
void erts_thr_q_initialize(ErtsThrQ_t *, ErtsThrQInit_t *);
ErtsThrQCleanState_t erts_thr_q_finalize(ErtsThrQ_t *);
ErtsThrQ_t *erts_thr_q_create(ErtsThrQInit_t *);
ErtsThrQCleanState_t erts_thr_q_destroy(ErtsThrQ_t *);
ErtsThrQCleanState_t erts_thr_q_clean(ErtsThrQ_t *);
ErtsThrQCleanState_t erts_thr_q_inspect(ErtsThrQ_t *, int);
ErtsThrQPrepEnQ_t *erts_thr_q_prepare_enqueue(ErtsThrQ_t *);
void erts_thr_q_enqueue_prepared(ErtsThrQ_t *, void *, ErtsThrQPrepEnQ_t *);
void erts_thr_q_enqueue(ErtsThrQ_t *, void *);
void * erts_thr_q_dequeue(ErtsThrQ_t *);
int erts_thr_q_get_finalize_dequeue_data(ErtsThrQ_t *,
					 ErtsThrQFinDeQ_t *);
void erts_thr_q_append_finalize_dequeue_data(ErtsThrQFinDeQ_t *,
					     ErtsThrQFinDeQ_t *);
int erts_thr_q_finalize_dequeue(ErtsThrQFinDeQ_t *);
void erts_thr_q_finalize_dequeue_state_init(ErtsThrQFinDeQ_t *);

#ifdef USE_LTTNG_VM_TRACEPOINTS
int erts_thr_q_length_dirty(ErtsThrQ_t *);
#endif

#ifdef ERTS_SMP
ERTS_GLB_INLINE ErtsThrPrgrVal erts_thr_q_need_thr_progress(ErtsThrQ_t *q);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ERTS_SMP
ERTS_GLB_INLINE ErtsThrPrgrVal
erts_thr_q_need_thr_progress(ErtsThrQ_t *q)
{
    return q->head.next.thr_progress;
}
#endif

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERL_THR_QUEUE_H__ */
