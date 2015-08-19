/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2013. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_threads.h"
#include "erl_thr_queue.h"
#include "erl_async.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"

#define ERTS_MAX_ASYNC_READY_CALLS_IN_SEQ 20

#define ERTS_ASYNC_PRINT_JOB 0

#if !defined(ERTS_SMP) && defined(USE_THREADS) && !ERTS_USE_ASYNC_READY_Q
#  error "Need async ready queue in non-smp case"
#endif

typedef struct _erl_async {
    DE_Handle*         hndl;   /* The DE_Handle is needed when port is gone */
    Eterm              port;
    long               async_id;
    void*              async_data;
    ErlDrvPDL          pdl;
    void (*async_invoke)(void*);
    void (*async_free)(void*);
#if ERTS_USE_ASYNC_READY_Q
    Uint               sched_id;
    union {
	ErtsThrQPrepEnQ_t *prep_enq;
	ErtsThrQFinDeQ_t   fin_deq;
    } q;
#endif
} ErtsAsync;

#if ERTS_USE_ASYNC_READY_Q

/*
 * We can do without the enqueue mutex since it isn't needed for
 * thread safety. Its only purpose is to put async threads to sleep
 * during a blast of ready async jobs. This in order to reduce
 * contention on the enqueue end of the async ready queues. During
 * such a blast without the enqueue mutex much cpu time is consumed
 * by the async threads without them doing much progress which in turn
 * slow down progress of scheduler threads.
 */
#define ERTS_USE_ASYNC_READY_ENQ_MTX 1

#if ERTS_USE_ASYNC_READY_ENQ_MTX

typedef struct {
    erts_mtx_t enq_mtx;
} ErtsAsyncReadyQXData;

#endif

typedef struct {
#if ERTS_USE_ASYNC_READY_ENQ_MTX
    union {
	ErtsAsyncReadyQXData data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(
		sizeof(ErtsAsyncReadyQXData))];
    } x;
#endif
    ErtsThrQ_t thr_q;
    ErtsThrQFinDeQ_t fin_deq;
} ErtsAsyncReadyQ;


typedef union {
    ErtsAsyncReadyQ arq;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAsyncReadyQ))];
} ErtsAlgndAsyncReadyQ;

#endif /* ERTS_USE_ASYNC_READY_Q */

typedef struct {
    ErtsThrQ_t thr_q;
    erts_tid_t thr_id;
} ErtsAsyncQ;

typedef union {
    ErtsAsyncQ aq;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAsyncQ))];
} ErtsAlgndAsyncQ;

typedef struct {
    int no_initialized;
    erts_mtx_t mtx;
    erts_cnd_t cnd;
    erts_atomic_t id;
} ErtsAsyncInit;

typedef struct {
    union {
	ErtsAsyncInit data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAsyncInit))];
    } init;
    ErtsAlgndAsyncQ *queue;
#if ERTS_USE_ASYNC_READY_Q
    ErtsAlgndAsyncReadyQ *ready_queue;
#endif
} ErtsAsyncData;

#if defined(USE_THREADS) && defined(USE_VM_PROBES)

/*
 * Some compilers, e.g. GCC 4.2.1 and -O3, will optimize away DTrace
 * calls if they're the last thing in the function.  :-(
 * Many thanks to Trond Norbye, via:
 * https://github.com/memcached/memcached/commit/6298b3978687530bc9d219b6ac707a1b681b2a46
 */
static unsigned gcc_optimizer_hack = 0;
#endif

int erts_async_max_threads; /* Initialized by erl_init.c */
int erts_async_thread_suggested_stack_size; /* Initialized by erl_init.c */

static ErtsAsyncData *async;

#ifndef USE_THREADS

void
erts_init_async(void)
{

}

#else

static void *async_main(void *);

static ERTS_INLINE ErtsAsyncQ *
async_q(int i)
{
    return &async->queue[i].aq;
}

#if ERTS_USE_ASYNC_READY_Q

static ERTS_INLINE ErtsAsyncReadyQ *
async_ready_q(Uint sched_id)
{
    return &async->ready_queue[((int)sched_id)-1].arq;
}

#endif

void
erts_init_async(void)
{
    async = NULL;
    if (erts_async_max_threads > 0) {
#if ERTS_USE_ASYNC_READY_Q
	ErtsThrQInit_t qinit = ERTS_THR_Q_INIT_DEFAULT;
#endif
	erts_thr_opts_t thr_opts = ERTS_THR_OPTS_DEFAULT_INITER;
	char *ptr, thr_name[16];
	size_t tot_size = 0;
	int i;

	tot_size += ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAsyncData));
	tot_size += sizeof(ErtsAlgndAsyncQ)*erts_async_max_threads;
#if ERTS_USE_ASYNC_READY_Q
	tot_size += sizeof(ErtsAlgndAsyncReadyQ)*erts_no_schedulers;
#endif

	ptr = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_ASYNC_DATA,
						 tot_size);

	async = (ErtsAsyncData *) ptr;
	ptr += ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsAsyncData));

	async->init.data.no_initialized = 0;
	erts_mtx_init(&async->init.data.mtx, "async_init_mtx");
	erts_cnd_init(&async->init.data.cnd);
	erts_atomic_init_nob(&async->init.data.id, 0);

	async->queue = (ErtsAlgndAsyncQ *) ptr;
	ptr += sizeof(ErtsAlgndAsyncQ)*erts_async_max_threads;

#if ERTS_USE_ASYNC_READY_Q

	qinit.live.queue = ERTS_THR_Q_LIVE_LONG;
	qinit.live.objects = ERTS_THR_Q_LIVE_SHORT;
	qinit.notify = erts_notify_check_async_ready_queue;

	async->ready_queue = (ErtsAlgndAsyncReadyQ *) ptr;
	ptr += sizeof(ErtsAlgndAsyncReadyQ)*erts_no_schedulers;

	for (i = 1; i <= erts_no_schedulers; i++) {
	    ErtsAsyncReadyQ *arq = async_ready_q(i);
#if ERTS_USE_ASYNC_READY_ENQ_MTX
	    erts_mtx_init(&arq->x.data.enq_mtx, "async_enq_mtx");
#endif
	    erts_thr_q_finalize_dequeue_state_init(&arq->fin_deq);
	    qinit.arg = (void *) (SWord) i;
	    erts_thr_q_initialize(&arq->thr_q, &qinit);
	}

#endif

	/* Create async threads... */

	thr_opts.detached = 0;
	thr_opts.suggested_stack_size
	    = erts_async_thread_suggested_stack_size;

	thr_opts.name = thr_name;

	for (i = 0; i < erts_async_max_threads; i++) {
	    ErtsAsyncQ *aq = async_q(i);

            erts_snprintf(thr_opts.name, 16, "async_%d", i+1);

	    erts_thr_create(&aq->thr_id, async_main, (void*) aq, &thr_opts);
	}

	/* Wait for async threads to initialize... */

	erts_mtx_lock(&async->init.data.mtx);
	while (async->init.data.no_initialized != erts_async_max_threads)
	    erts_cnd_wait(&async->init.data.cnd, &async->init.data.mtx);
	erts_mtx_unlock(&async->init.data.mtx);

	erts_mtx_destroy(&async->init.data.mtx);
	erts_cnd_destroy(&async->init.data.cnd);

    }
}

#if ERTS_USE_ASYNC_READY_Q

void *
erts_get_async_ready_queue(Uint sched_id)
{
    return (void *) async ? async_ready_q(sched_id) : NULL;
}

#endif

static ERTS_INLINE void async_add(ErtsAsync *a, ErtsAsyncQ* q)
{
#ifdef USE_VM_PROBES
    int len;
#endif

    if (is_internal_port(a->port)) {
#if ERTS_USE_ASYNC_READY_Q
	ErtsAsyncReadyQ *arq = async_ready_q(a->sched_id);
	a->q.prep_enq = erts_thr_q_prepare_enqueue(&arq->thr_q);
#endif
	/* make sure the driver will stay around */
	if (a->hndl)
	    erts_ddll_reference_referenced_driver(a->hndl);
    }

#if ERTS_ASYNC_PRINT_JOB
    erts_fprintf(stderr, "-> %ld\n", a->async_id);
#endif

    erts_thr_q_enqueue(&q->thr_q, a);
#ifdef USE_LTTNG_VM_TRACEPOINTS
    if (LTTNG_ENABLED(aio_pool_add)) {
        lttng_decl_portbuf(port_str);
        lttng_portid_to_str(a->port, port_str);
        LTTNG2(aio_pool_add, port_str, -1);
    }
#endif
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(aio_pool_add)) {
        DTRACE_CHARBUF(port_str, 16);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", a->port);
        /* DTRACE TODO: Get the queue length from erts_thr_q_enqueue() ? */
        len = -1;
        DTRACE2(aio_pool_add, port_str, len);
    }
    gcc_optimizer_hack++;
#endif
}

static ERTS_INLINE ErtsAsync *async_get(ErtsThrQ_t *q,
					erts_tse_t *tse,
					ErtsThrQPrepEnQ_t **prep_enq)
{
#if ERTS_USE_ASYNC_READY_Q
    int saved_fin_deq = 0;
    ErtsThrQFinDeQ_t fin_deq;
#endif
#ifdef USE_VM_PROBES
    int len;
#endif

    while (1) {
	ErtsAsync *a = (ErtsAsync *) erts_thr_q_dequeue(q);
	if (a) {

#if ERTS_USE_ASYNC_READY_Q
	    *prep_enq = a->q.prep_enq;
	    erts_thr_q_get_finalize_dequeue_data(q, &a->q.fin_deq);
	    if (saved_fin_deq)
		erts_thr_q_append_finalize_dequeue_data(&a->q.fin_deq, &fin_deq);
#endif
#ifdef USE_LTTNG_VM_TRACEPOINTS
            if (LTTNG_ENABLED(aio_pool_get)) {
                lttng_decl_portbuf(port_str);
                int length = erts_thr_q_length_dirty(q);
                lttng_portid_to_str(a->port, port_str);
                LTTNG2(aio_pool_get, port_str, length);
            }
#endif
#ifdef USE_VM_PROBES
            if (DTRACE_ENABLED(aio_pool_get)) {
                DTRACE_CHARBUF(port_str, 16);

                erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                              "%T", a->port);
                /* DTRACE TODO: Get the length from erts_thr_q_dequeue() ? */
                len = -1;
                DTRACE2(aio_pool_get, port_str, len);
            }
#endif
	    return a;
	}

	if (ERTS_THR_Q_DIRTY != erts_thr_q_clean(q)) {
	    ErtsThrQFinDeQ_t tmp_fin_deq;

	    erts_tse_reset(tse);

#if ERTS_USE_ASYNC_READY_Q
	chk_fin_deq:
	    if (erts_thr_q_get_finalize_dequeue_data(q, &tmp_fin_deq)) {
		if (!saved_fin_deq) {
		    erts_thr_q_finalize_dequeue_state_init(&fin_deq);
		    saved_fin_deq = 1;
		}
		erts_thr_q_append_finalize_dequeue_data(&fin_deq,
							&tmp_fin_deq);
	    }
#endif

	    switch (erts_thr_q_inspect(q, 1)) {
	    case ERTS_THR_Q_DIRTY:
		break;
	    case ERTS_THR_Q_NEED_THR_PRGR:
#ifdef ERTS_SMP
	    {
		ErtsThrPrgrVal prgr = erts_thr_q_need_thr_progress(q);
		erts_thr_progress_wakeup(NULL, prgr);
		/*
		 * We do no dequeue finalizing in hope that a new async
		 * job will arrive before we are woken due to thread
		 * progress...
		 */
		erts_tse_wait(tse);
		break;
	    }
#endif
	    case ERTS_THR_Q_CLEAN:

#if ERTS_USE_ASYNC_READY_Q
		if (saved_fin_deq) {
		    if (erts_thr_q_finalize_dequeue(&fin_deq))
			goto chk_fin_deq;
		    else
			saved_fin_deq = 0;
		}
#endif

		erts_tse_wait(tse);
		break;

	    default:
		ASSERT(0);
		break;
	    }

	}
    }
}

static ERTS_INLINE void call_async_ready(ErtsAsync *a)
{
#if ERTS_USE_ASYNC_READY_Q
    Port *p = erts_id2port_sflgs(a->port,
				 NULL,
				 0,
				 ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
#else
    Port *p = erts_thr_id2port_sflgs(a->port,
				     ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
#endif
    if (!p) {
	if (a->async_free) {
            ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_PORT);
	    a->async_free(a->async_data);
            ERTS_MSACC_POP_STATE();
        }
    }
    else {
	if (async_ready(p, a->async_data)) {
	    if (a->async_free) {
                ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_PORT);
		a->async_free(a->async_data);
                ERTS_MSACC_POP_STATE();
            }
	}
#if ERTS_USE_ASYNC_READY_Q
	erts_port_release(p);
#else
	erts_thr_port_release(p);
#endif
    }
    if (a->pdl)
	driver_pdl_dec_refc(a->pdl);
    if (a->hndl)
	erts_ddll_dereference_driver(a->hndl);
}

static ERTS_INLINE void async_reply(ErtsAsync *a, ErtsThrQPrepEnQ_t *prep_enq)
{
#if ERTS_USE_ASYNC_READY_Q
    ErtsAsyncReadyQ *arq;

#if ERTS_ASYNC_PRINT_JOB
    erts_fprintf(stderr, "=>> %ld\n", a->async_id);
#endif

    arq = async_ready_q(a->sched_id);

#if ERTS_USE_ASYNC_READY_ENQ_MTX
	erts_mtx_lock(&arq->x.data.enq_mtx);
#endif

	erts_thr_q_enqueue_prepared(&arq->thr_q, (void *) a, prep_enq);

#if ERTS_USE_ASYNC_READY_ENQ_MTX
	erts_mtx_unlock(&arq->x.data.enq_mtx);
#endif

#else /* ERTS_USE_ASYNC_READY_Q */

	call_async_ready(a);
	erts_free(ERTS_ALC_T_ASYNC, (void *) a);

#endif /* ERTS_USE_ASYNC_READY_Q */
}


static void
async_wakeup(void *vtse)
{
    erts_tse_set((erts_tse_t *) vtse);
}

static erts_tse_t *async_thread_init(ErtsAsyncQ *aq)
{
    ErtsThrQInit_t qinit = ERTS_THR_Q_INIT_DEFAULT;
    erts_tse_t *tse = erts_tse_fetch();
    ERTS_DECLARE_DUMMY(Uint no);

#ifdef ERTS_SMP
    ErtsThrPrgrCallbacks callbacks;

    callbacks.arg = (void *) tse;
    callbacks.wakeup = async_wakeup;
    callbacks.prepare_wait = NULL;
    callbacks.wait = NULL;

    erts_thr_progress_register_unmanaged_thread(&callbacks);
#endif

    qinit.live.queue = ERTS_THR_Q_LIVE_LONG;
    qinit.live.objects = ERTS_THR_Q_LIVE_SHORT;
    qinit.arg = (void *) tse;
    qinit.notify = async_wakeup;
#if ERTS_USE_ASYNC_READY_Q
    qinit.auto_finalize_dequeue = 0;
#endif

    erts_thr_q_initialize(&aq->thr_q, &qinit);

    /* Inform main thread that we are done initializing... */
    erts_mtx_lock(&async->init.data.mtx);
    no = async->init.data.no_initialized++;
    erts_cnd_signal(&async->init.data.cnd);
    erts_mtx_unlock(&async->init.data.mtx);

    erts_msacc_init_thread("async", no, 0);

    return tse;
}

static void *async_main(void* arg)
{
    ErtsAsyncQ *aq = (ErtsAsyncQ *) arg;
    erts_tse_t *tse = async_thread_init(aq);
    ERTS_MSACC_DECLARE_CACHE();

    while (1) {
	ErtsThrQPrepEnQ_t *prep_enq;
	ErtsAsync *a = async_get(&aq->thr_q, tse, &prep_enq);
	if (is_nil(a->port))
	    break; /* Time to die */

        ERTS_MSACC_UPDATE_CACHE();

#if ERTS_ASYNC_PRINT_JOB
	erts_fprintf(stderr, "<- %ld\n", a->async_id);
#endif
        ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_PORT);
	a->async_invoke(a->async_data);
        ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);

	async_reply(a, prep_enq);
    }

    return NULL;
}

#endif /* USE_THREADS */

void
erts_exit_flush_async(void)
{
#ifdef USE_THREADS
    int i;
    ErtsAsync a;
    a.port = NIL;
    /*
     * Terminate threads in order to flush queues. We do not
     * bother to clean everything up since we are about to
     * terminate the runtime system and a cleanup would only
     * delay the termination.
     */
    for (i = 0; i < erts_async_max_threads; i++)
	async_add(&a, async_q(i));
    for (i = 0; i < erts_async_max_threads; i++)
	erts_thr_join(async->queue[i].aq.thr_id, NULL);
#endif
}

#if defined(USE_THREADS) && ERTS_USE_ASYNC_READY_Q

int erts_check_async_ready(void *varq)
{
    ErtsAsyncReadyQ *arq = (ErtsAsyncReadyQ *) varq;
    int res = 1;
    int i;

    for (i = 0; i < ERTS_MAX_ASYNC_READY_CALLS_IN_SEQ; i++) {
	ErtsAsync *a = (ErtsAsync *) erts_thr_q_dequeue(&arq->thr_q);
	if (!a) {
	    res = 0;
	    break;
	}

#if ERTS_ASYNC_PRINT_JOB
	erts_fprintf(stderr, "<<= %ld\n", a->async_id);
#endif
	erts_thr_q_append_finalize_dequeue_data(&arq->fin_deq, &a->q.fin_deq);
	call_async_ready(a);
	erts_free(ERTS_ALC_T_ASYNC, (void *) a);
    }

    erts_thr_q_finalize_dequeue(&arq->fin_deq);
    
    return res;
}

int erts_async_ready_clean(void *varq, void *val)
{
    ErtsAsyncReadyQ *arq = (ErtsAsyncReadyQ *) varq;
    ErtsThrQCleanState_t cstate;

    cstate = erts_thr_q_clean(&arq->thr_q);

    if (erts_thr_q_finalize_dequeue(&arq->fin_deq))
	return ERTS_ASYNC_READY_DIRTY;

    switch (cstate) {
    case ERTS_THR_Q_DIRTY:
	return ERTS_ASYNC_READY_DIRTY;
    case ERTS_THR_Q_NEED_THR_PRGR:
#ifdef ERTS_SMP
	*((ErtsThrPrgrVal *) val)
	    = erts_thr_q_need_thr_progress(&arq->thr_q);
	return ERTS_ASYNC_READY_NEED_THR_PRGR;
#endif
    case ERTS_THR_Q_CLEAN:
	break;
    }
    return ERTS_ASYNC_READY_CLEAN;
}

#endif

/*
** Generate a fair async key prom an ErlDrvPort
** The port data gives a fair distribution grom port pointer
** to unsigned integer - to be used in key for driver_async below.
*/
unsigned int driver_async_port_key(ErlDrvPort port)
{
    ErlDrvTermData td = driver_mk_port(port);
    if (td == (ErlDrvTermData) NIL) {
	return 0;
    }
    return (unsigned int) (UWord) internal_port_data(td);
}

/*
** Schedule async_invoke on a worker thread
** NOTE will be syncrounous when threads are unsupported
** return values:
**  0  completed 
**  -1 error
**  N  handle value
**  arguments:
**      ix             driver index 
**      key            pointer to secedule queue (NULL means round robin)
**      async_invoke   function to run in thread
**      async_data     data to pass to invoke function
**      async_free     function for relase async_data in case of failure
*/
long driver_async(ErlDrvPort ix, unsigned int* key,
		  void (*async_invoke)(void*), void* async_data,
		  void (*async_free)(void*))
{
    ErtsAsync* a;
    Port* prt;
    long id;
    unsigned int qix;
#if ERTS_USE_ASYNC_READY_Q
    Uint sched_id;
    ERTS_MSACC_PUSH_STATE();

    sched_id = erts_get_scheduler_id();
    if (!sched_id)
	sched_id = 1;
#else
    ERTS_MSACC_PUSH_STATE();
#endif

    prt = erts_drvport2port(ix);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    a = (ErtsAsync*) erts_alloc(ERTS_ALC_T_ASYNC, sizeof(ErtsAsync));

#if ERTS_USE_ASYNC_READY_Q
    a->sched_id = sched_id;
#endif
    a->hndl = (DE_Handle*)prt->drv_ptr->handle;
    a->port = prt->common.id;
    a->pdl = NULL;
    a->async_data = async_data;
    a->async_invoke = async_invoke;
    a->async_free = async_free;

    if (!async)
	id = 0;
    else {
	do {
	    id = erts_atomic_inc_read_nob(&async->init.data.id);
	} while (id == 0);
	if (id < 0)
	    id *= -1;
	ASSERT(id > 0);
    }

    a->async_id = id;

    if (key == NULL) {
	qix = (erts_async_max_threads > 0)
	    ? (id % erts_async_max_threads) : 0;
    }
    else {
	qix = (erts_async_max_threads > 0) ? 
	    (*key % erts_async_max_threads) : 0;
	*key = qix;
    }
#ifdef USE_THREADS
    if (erts_async_max_threads > 0) {
	if (prt->port_data_lock) {
	    driver_pdl_inc_refc(prt->port_data_lock);
	    a->pdl = prt->port_data_lock;
	}
	async_add(a, async_q(qix));
	return id;
    }
#endif
    
    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_PORT);
    (*a->async_invoke)(a->async_data);
    ERTS_MSACC_POP_STATE();

    if (async_ready(prt, a->async_data)) {
	if (a->async_free != NULL) {
            ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_PORT);
	    (*a->async_free)(a->async_data);
            ERTS_MSACC_POP_STATE();
        }
    }
    erts_free(ERTS_ALC_T_ASYNC, (void *) a);

    return id;
}
