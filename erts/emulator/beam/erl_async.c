/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_threads.h"

typedef struct _erl_async {
    struct _erl_async* next;
    struct _erl_async* prev;
    DE_Handle*         hndl;   /* The DE_Handle is needed when port is gone */
    Eterm              port;
    long               async_id;
    void*              async_data;
    ErlDrvPDL          pdl;
    void (*async_invoke)(void*);
    void (*async_free)(void*);
} ErlAsync;

typedef struct {
    erts_mtx_t mtx;
    erts_cnd_t cv;
    erts_tid_t thr;
    int   len;
#ifndef ERTS_SMP
    int   hndl;
#endif
    ErlAsync* head;
    ErlAsync* tail;
#ifdef ERTS_ENABLE_LOCK_CHECK
    int no;
#endif
} AsyncQueue;

static erts_smp_spinlock_t async_id_lock;
static long async_id = 0;


#ifndef ERTS_SMP

erts_mtx_t async_ready_mtx;
static ErlAsync* async_ready_list = NULL;

#endif

/*
** Initialize worker threads (if supported)
*/

/* Detach from driver */
static void async_detach(DE_Handle* dh)
{
    /* XXX:PaN what should happen here? we want to unload the driver or??? */
    return;
}


#ifdef USE_THREADS

static AsyncQueue* async_q;

static void* async_main(void*);
static void async_add(ErlAsync*, AsyncQueue*);

#ifndef ERTS_SMP
typedef struct ErtsAsyncReadyCallback_ ErtsAsyncReadyCallback;
struct ErtsAsyncReadyCallback_ {
    struct ErtsAsyncReadyCallback_ *next;
    void (*callback)(void);
};

static ErtsAsyncReadyCallback *callbacks;
static int async_handle;

int erts_register_async_ready_callback(void (*funcp)(void))
{
    ErtsAsyncReadyCallback *cb = erts_alloc(ERTS_ALC_T_ARCALLBACK,
					    sizeof(ErtsAsyncReadyCallback));
    cb->next = callbacks;
    cb->callback = funcp;
    erts_mtx_lock(&async_ready_mtx);
    callbacks = cb;
    erts_mtx_unlock(&async_ready_mtx);
    return async_handle;
}
#endif

int init_async(int hndl)
{
    erts_thr_opts_t thr_opts = ERTS_THR_OPTS_DEFAULT_INITER;
    AsyncQueue* q;
    int i;

    thr_opts.detached = 0;
    thr_opts.suggested_stack_size = erts_async_thread_suggested_stack_size;

#ifndef ERTS_SMP
    callbacks = NULL;
    async_handle = hndl;
    erts_mtx_init(&async_ready_mtx, "async_ready");
    async_ready_list = NULL;
#endif

    async_id = 0;
    erts_smp_spinlock_init(&async_id_lock, "async_id");

    async_q = q = (AsyncQueue*)
	(erts_async_max_threads
	 ? erts_alloc(ERTS_ALC_T_ASYNC_Q,
		      erts_async_max_threads * sizeof(AsyncQueue))
	 : NULL);
    for (i = 0; i < erts_async_max_threads; i++) {
	q->head = NULL;
	q->tail = NULL;
	q->len = 0;
#ifndef ERTS_SMP
	q->hndl = hndl;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
	q->no = i;
#endif
	erts_mtx_init(&q->mtx, "asyncq");
	erts_cnd_init(&q->cv);
	erts_thr_create(&q->thr, async_main, (void*)q, &thr_opts);
	q++;
    }
    return 0;
}


int exit_async()
{
    int i;

    /* terminate threads */
    for (i = 0; i < erts_async_max_threads; i++) {
	ErlAsync* a = (ErlAsync*) erts_alloc(ERTS_ALC_T_ASYNC,
					     sizeof(ErlAsync));
	a->port = NIL;
	async_add(a, &async_q[i]);
    }

    for (i = 0; i < erts_async_max_threads; i++) {
	erts_thr_join(async_q[i].thr, NULL);
	erts_mtx_destroy(&async_q[i].mtx);
	erts_cnd_destroy(&async_q[i].cv);
    }
#ifndef ERTS_SMP
    erts_mtx_destroy(&async_ready_mtx);
#endif
    if (async_q)
	erts_free(ERTS_ALC_T_ASYNC_Q, (void *) async_q);
    return 0;
}


static void async_add(ErlAsync* a, AsyncQueue* q)
{
    /* XXX:PaN Is this still necessary when ports lock drivers? */
    if (is_internal_port(a->port)) {
	ERTS_LC_ASSERT(erts_drvportid2port(a->port));
	/* make sure the driver will stay around */
	driver_lock_driver(internal_port_index(a->port));
    }

    erts_mtx_lock(&q->mtx);

    if (q->len == 0) {
	q->head = a;
	q->tail = a;
	q->len = 1;
	erts_cnd_signal(&q->cv);
    }
    else { /* no need to signal (since the worker is working) */
	a->next = q->head;
	q->head->prev = a;
	q->head = a;
	q->len++;
    }
    erts_mtx_unlock(&q->mtx);
}

static ErlAsync* async_get(AsyncQueue* q)
{
    ErlAsync* a;

    erts_mtx_lock(&q->mtx);
    while((a = q->tail) == NULL) {
	erts_cnd_wait(&q->cv, &q->mtx);
    }
#ifdef ERTS_SMP
    ASSERT(a && q->tail == a);
#endif
    if (q->head == q->tail) {
	q->head = q->tail = NULL;
	q->len = 0;
    }
    else {
	q->tail->prev->next = NULL;
	q->tail = q->tail->prev;
	q->len--;
    }
    erts_mtx_unlock(&q->mtx);
    return a;
}


static int async_del(long id)
{
    int i;
    /* scan all queue for an entry with async_id == 'id' */

    for (i = 0; i < erts_async_max_threads; i++) {
	ErlAsync* a;
	erts_mtx_lock(&async_q[i].mtx);
	
	a = async_q[i].head;
	while(a != NULL) {
	    if (a->async_id == id) {
		if (a->prev != NULL)
		    a->prev->next = a->next;
		else
		    async_q[i].head = a->next;
		if (a->next != NULL)
		    a->next->prev = a->prev;
		else
		    async_q[i].tail = a->prev;
		async_q[i].len--;
		erts_mtx_unlock(&async_q[i].mtx);
		if (a->async_free != NULL)
		    a->async_free(a->async_data);
		async_detach(a->hndl);
		erts_free(ERTS_ALC_T_ASYNC, a);
		return 1;
	    }
	}
	erts_mtx_unlock(&async_q[i].mtx);
    }
    return 0;
}

static void* async_main(void* arg)
{
    AsyncQueue* q = (AsyncQueue*) arg;

#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[27];
	erts_snprintf(&buf[0], 27, "async %d", q->no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif

    while(1) {
	ErlAsync* a = async_get(q);

	if (a->port == NIL) { /* TIME TO DIE SIGNAL */
	    erts_free(ERTS_ALC_T_ASYNC, (void *) a);
	    break;
	}
	else {
	    (*a->async_invoke)(a->async_data);
	    /* Major problem if the code for async_invoke
	       or async_free is removed during a blocking operation */
#ifdef ERTS_SMP
	    {
		Port *p;
		p = erts_id2port_sflgs(a->port,
				       NULL,
				       0,
				       ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
		if (!p) {
		    if (a->async_free)
			(*a->async_free)(a->async_data);
		}
		else {
		    if (async_ready(p, a->async_data)) {
			if (a->async_free)
			    (*a->async_free)(a->async_data);
		    }
		    async_detach(a->hndl);
		    erts_port_release(p);
		}
		if (a->pdl) {
		    driver_pdl_dec_refc(a->pdl);
		}
		erts_free(ERTS_ALC_T_ASYNC, (void *) a);
	    }
#else
	    if (a->pdl) {
		driver_pdl_dec_refc(a->pdl);
	    }
	    erts_mtx_lock(&async_ready_mtx);
	    a->next = async_ready_list;
	    async_ready_list = a;
	    erts_mtx_unlock(&async_ready_mtx);
	    sys_async_ready(q->hndl);
#endif
	}
    }

    return NULL;
}


#endif

#ifndef ERTS_SMP

int check_async_ready(void)
{
#ifdef USE_THREADS
    ErtsAsyncReadyCallback *cbs;
#endif
    ErlAsync* a;
    int count = 0;

    erts_mtx_lock(&async_ready_mtx);
    a = async_ready_list;
    async_ready_list = NULL;
#ifdef USE_THREADS
    cbs = callbacks;
#endif
    erts_mtx_unlock(&async_ready_mtx);

    while(a != NULL) {
	ErlAsync* a_next = a->next;
	/* Every port not dead */
	Port *p = erts_id2port_sflgs(a->port,
				     NULL,
				     0,
				     ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
	if (!p) {
	    if (a->async_free)
		(*a->async_free)(a->async_data);
	}
	else {
	    count++;
	    if (async_ready(p, a->async_data)) {
		if (a->async_free != NULL)
		    (*a->async_free)(a->async_data);
	    }
	    async_detach(a->hndl);
	    erts_port_release(p);
	}
	erts_free(ERTS_ALC_T_ASYNC, (void *) a);
	a = a_next;
    }
#ifdef USE_THREADS
    for (; cbs; cbs = cbs->next)
	(*cbs->callback)();
#endif
    return count;
}

#endif


/*
** Schedule async_invoke on a worker thread
** NOTE will be syncrounous when threads are unsupported
** return values:
**  0  completed 
**  -1 error
**  N  handle value (used with async_cancel)
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
    ErlAsync* a = (ErlAsync*) erts_alloc(ERTS_ALC_T_ASYNC, sizeof(ErlAsync));
    Port* prt = erts_drvport2port(ix);
    long id;
    unsigned int qix;


    if (!prt)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    a->next = NULL;
    a->prev = NULL;
    a->hndl = (DE_Handle*)prt->drv_ptr->handle;
    a->port = prt->id;
    a->pdl = NULL;
    a->async_data = async_data;
    a->async_invoke = async_invoke;
    a->async_free = async_free;

    erts_smp_spin_lock(&async_id_lock);
    async_id = (async_id + 1) & 0x7fffffff;
    if (async_id == 0)
	async_id++;
    id = async_id;
    erts_smp_spin_unlock(&async_id_lock);

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
	async_add(a, &async_q[qix]);
	return id;
    }
#endif

    (*a->async_invoke)(a->async_data);

    if (async_ready(prt, a->async_data)) {
	if (a->async_free != NULL)
	    (*a->async_free)(a->async_data);
    }
    erts_free(ERTS_ALC_T_ASYNC, (void *) a);

    return id;
}

int driver_async_cancel(unsigned int id)
{
#ifdef USE_THREADS
    if (erts_async_max_threads > 0)
	return async_del(id);
#endif
    return 0;
}





