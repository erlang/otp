/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

#ifndef ERL_PORT_TYPE__
#define ERL_PORT_TYPE__
typedef struct _erl_drv_port Port;
typedef struct ErtsProc2PortSigData_ ErtsProc2PortSigData;
#endif

#if !defined(ERL_PORT_H__) && !defined(ERL_PORT_GET_PORT_TYPE_ONLY__)
#define ERL_PORT_H__

#include "erl_port_task.h"
#include "erl_ptab.h"
#include "erl_thr_progress.h"
#include "erl_trace.h"

#ifndef __WIN32__
#define ERTS_DEFAULT_MAX_PORTS (1 << 16)
#else
/* 
 *  Do not default to as many max ports on Windows
 *  as there are no os limits to stop system
 *  from running amok. If allowed to go too high
 *  windows rarely recovers from the errors and
 *  other OS processes can be effected. 
 */
#define ERTS_DEFAULT_MAX_PORTS (1 << 13)
#endif /* __WIN32__ */
#define ERTS_MIN_PORTS 1024

extern int erts_port_synchronous_ops;
extern int erts_port_schedule_all_ops;
extern int erts_port_parallelism;

typedef struct erts_driver_t_ erts_driver_t;

/*
 * It would have been preferred to use NULL as value of
 * ERTS_INVALID_ERL_DRV_PORT. That would, however, not be
 * backward compatible. In pre-R16 systems, 0 was a valid
 * port handle and -1 was used as invalid handle, so we
 * are stuck with it.
 */
#define ERTS_INVALID_ERL_DRV_PORT ((struct _erl_drv_port *) ((SWord) -1))
#ifdef DEBUG
/* Make sure we use this api, and do not cast directly */
#define ERTS_ErlDrvPort2Port(PH)	\
    ((PH) == ERTS_INVALID_ERL_DRV_PORT	\
     ? ERTS_INVALID_ERL_DRV_PORT	\
     : ((Port *) ((PH) - 4711)))
#define ERTS_Port2ErlDrvPort(PH)	\
    ((PH) == ERTS_INVALID_ERL_DRV_PORT	\
     ? ERTS_INVALID_ERL_DRV_PORT	\
     : ((ErlDrvPort) ((PH) + 4711)))
#else
#define ERTS_ErlDrvPort2Port(PH) ((Port *) (PH))
#define ERTS_Port2ErlDrvPort(PH) ((ErlDrvPort) (PH))
#endif

#define SMALL_IO_QUEUE 5   /* Number of fixed elements */

typedef struct {
    ErlDrvSizeT size;       /* total size in bytes */

    SysIOVec* v_start;
    SysIOVec* v_end;
    SysIOVec* v_head;
    SysIOVec* v_tail;
    SysIOVec  v_small[SMALL_IO_QUEUE];

    ErlDrvBinary** b_start;
    ErlDrvBinary** b_end;
    ErlDrvBinary** b_head;
    ErlDrvBinary** b_tail;
    ErlDrvBinary*  b_small[SMALL_IO_QUEUE];
} ErlIOQueue;

typedef struct line_buf {  /* Buffer used in line oriented I/O */
    ErlDrvSizeT bufsiz;      /* Size of character buffer */
    ErlDrvSizeT ovlen;       /* Length of overflow data */
    ErlDrvSizeT ovsiz;       /* Actual size of overflow buffer */
    char data[1];            /* Starting point of buffer data,
			      data[0] is a flag indicating an unprocess CR,
			      The rest is the overflow buffer. */
} LineBuf;

/*
 * Items part of erlang:port_info/1 result. Note am_registered_name
 * *need* to be first.
 */

#define ERTS_PORT_INFO_1_ITEMS				\
    {	am_registered_name,	/* Needs to be first */	\
	am_name,					\
	am_links,					\
	am_id,						\
	am_connected,					\
	am_input,					\
	am_output,					\
	am_os_pid }

/*
 * Port Specific Data.
 *
 * Only use PrtSD for very rarely used data.
 */

#define ERTS_PRTSD_SCHED_ID 0

#define ERTS_PRTSD_SIZE 1

typedef struct {
    void *data[ERTS_PRTSD_SIZE];
} ErtsPrtSD;

#ifdef ERTS_SMP
typedef struct ErtsXPortsList_ ErtsXPortsList;
#endif

/*
 * Port locking:
 *
 * Locking is done either driver specific or port specific. When
 * driver specific locking is used, all instances of the driver,
 * i.e. ports running the driver, share the same lock. When port
 * specific locking is used each instance have its own lock.
 *
 * Most fields in the Port structure are protected by the lock
 * referred to by the 'lock' field. This lock is shared between
 * all ports running the same driver when driver specific locking
 * is used.
 *
 * The 'sched' field is protected by the run queue lock that the
 * port currently is assigned to.
 *
 */

struct _erl_drv_port {
    ErtsPTabElementCommon common; /* *Need* to be first in struct */

    ErtsPortTaskSched sched;
    ErtsPortTaskHandle timeout_task;
#ifdef ERTS_SMP
    erts_mtx_t *lock;
    ErtsXPortsList *xports;
    erts_smp_atomic_t run_queue;
#else
    erts_atomic32_t refc;
    int cleanup;
#endif
    erts_atomic_t connected;	/* A connected process */
    Eterm caller;		/* Current caller. */
    erts_smp_atomic_t data;	/* Data associated with port. */
    Uint bytes_in;		/* Number of bytes read */
    Uint bytes_out;		/* Number of bytes written */

    ErlIOQueue ioq;              /* driver accessible i/o queue */
    DistEntry *dist_entry;       /* Dist entry used in DISTRIBUTION */
    char *name;		         /* String used in the open */
    erts_driver_t* drv_ptr;
    UWord drv_data;
    SWord os_pid;                /* Child process ID */
    ErtsProcList *suspended;	 /* List of suspended processes. */
    LineBuf *linebuf;            /* Buffer to hold data not ready for
				    process to get (line oriented I/O)*/
    erts_atomic32_t state;	 /* Status and type flags */
    int control_flags;		 /* Flags for port_control()  */
    ErlDrvPDL port_data_lock;

    erts_smp_atomic_t psd;	 /* Port specific data */
    int reds; /* Only used while executing driver callbacks */

    struct {
        Eterm to;
        Uint32 ref[ERTS_MAX_REF_NUMBERS];
    } *async_open_port;         /* Reference used with async open port */
};


void erts_init_port_data(Port *);
void erts_cleanup_port_data(Port *);
Uint erts_port_data_size(Port *);
ErlOffHeap *erts_port_data_offheap(Port *);

#define ERTS_PORT_GET_CONNECTED(PRT) \
    ((Eterm) erts_atomic_read_nob(&(PRT)->connected))
#define ERTS_PORT_SET_CONNECTED(PRT, PID) \
    erts_atomic_set_relb(&(PRT)->connected, (erts_aint_t) (PID))
#define ERTS_PORT_INIT_CONNECTED(PRT, PID) \
    erts_atomic_init_nob(&(PRT)->connected, (erts_aint_t) (PID))


struct erl_drv_port_data_lock {
    erts_mtx_t mtx;
    erts_atomic_t refc;
    Port *prt;
};

ERTS_GLB_INLINE ErtsRunQueue *erts_port_runq(Port *prt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsRunQueue *
erts_port_runq(Port *prt)
{
#ifdef ERTS_SMP
    ErtsRunQueue *rq1, *rq2;
    rq1 = (ErtsRunQueue *) erts_smp_atomic_read_nob(&prt->run_queue);
    if (!rq1)
	return NULL;
    while (1) {
	erts_smp_runq_lock(rq1);
	rq2 = (ErtsRunQueue *) erts_smp_atomic_read_nob(&prt->run_queue);
	if (rq1 == rq2)
	    return rq1;
	erts_smp_runq_unlock(rq1);
	rq1 = rq2;
	if (!rq1)
	    return NULL;
    }
#else
    return ERTS_RUNQ_IX(0);
#endif
}

#endif


ERTS_GLB_INLINE void *erts_prtsd_get(Port *p, int ix);
ERTS_GLB_INLINE void *erts_prtsd_set(Port *p, int ix, void *new);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_prtsd_get(Port *prt, int ix)
{
    ErtsPrtSD *psd = (ErtsPrtSD *) erts_smp_atomic_read_nob(&prt->psd);
    if (!psd)
	return NULL;
    ERTS_SMP_DATA_DEPENDENCY_READ_MEMORY_BARRIER;
    return psd->data[ix];
}

ERTS_GLB_INLINE void *
erts_prtsd_set(Port *prt, int ix, void *data)
{
    ErtsPrtSD *psd, *new_psd;
    void *old;
    int i;

    psd = (ErtsPrtSD *) erts_smp_atomic_read_nob(&prt->psd);

    if (psd) {
#ifdef ERTS_SMP
#ifdef ETHR_ORDERED_READ_DEPEND
	ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
#else
	ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreStore);
#endif
#endif
	old = psd->data[ix];
	psd->data[ix] = data;
	return old;
    }

    if (!data)
	return NULL;

    new_psd = erts_alloc(ERTS_ALC_T_PRTSD, sizeof(ErtsPrtSD));
    for (i = 0; i < ERTS_PRTSD_SIZE; i++)
	new_psd->data[i] = NULL;
    psd = (ErtsPrtSD *) erts_smp_atomic_cmpxchg_mb(&prt->psd,
						   (erts_aint_t) new_psd,
						   (erts_aint_t) NULL);
    if (psd)
	erts_free(ERTS_ALC_T_PRTSD, new_psd);
    else
	psd = new_psd;
    old = psd->data[ix];
    psd->data[ix] = data;
    return old;
}

#endif

Eterm erts_request_io_bytes(Process *c_p);


/* port status flags */

#define ERTS_PORT_SFLG_CONNECTED	((Uint32) (1 <<  0))
/* Port have begun exiting */
#define ERTS_PORT_SFLG_EXITING		((Uint32) (1 <<  1))
/* Distribution port */
#define ERTS_PORT_SFLG_DISTRIBUTION	((Uint32) (1 <<  2))
#define ERTS_PORT_SFLG_BINARY_IO	((Uint32) (1 <<  3))
#define ERTS_PORT_SFLG_SOFT_EOF		((Uint32) (1 <<  4))
/* Flow control */
/* Port is closing (no i/o accepted) */
#define ERTS_PORT_SFLG_CLOSING		((Uint32) (1 <<  5))
/* Send a closed message when terminating */
#define ERTS_PORT_SFLG_SEND_CLOSED	((Uint32) (1 <<  6))
/* Line orinted io on port */  
#define ERTS_PORT_SFLG_LINEBUF_IO	((Uint32) (1 <<  7))
/* Immortal port (only certain system ports) */
#define ERTS_PORT_SFLG_FREE		((Uint32) (1 <<  8))
#define ERTS_PORT_SFLG_INITIALIZING	((Uint32) (1 <<  9))
/* Port uses port specific locking (opposed to driver specific locking) */
#define ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK ((Uint32) (1 << 10))
#define ERTS_PORT_SFLG_INVALID		((Uint32) (1 << 11))
/* Last port to terminate halts the emulator */
#define ERTS_PORT_SFLG_HALT		((Uint32) (1 << 12))
#ifdef DEBUG
/* Only debug: make sure all flags aren't cleared unintentionally */
#define ERTS_PORT_SFLG_PORT_DEBUG	((Uint32) (1 << 31))
#endif

/* Combinations of port status flags */ 
#define ERTS_PORT_SFLGS_DEAD						\
  (ERTS_PORT_SFLG_FREE | ERTS_PORT_SFLG_INITIALIZING)
#define ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP				\
  (ERTS_PORT_SFLGS_DEAD | ERTS_PORT_SFLG_INVALID)
#define ERTS_PORT_SFLGS_INVALID_LOOKUP					\
  (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP				\
   | ERTS_PORT_SFLG_EXITING						\
   | ERTS_PORT_SFLG_CLOSING)
#define ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP				\
  (ERTS_PORT_SFLGS_INVALID_LOOKUP					\
   | ERTS_PORT_SFLG_DISTRIBUTION)

/*
 * Costs in reductions for some port operations.
 */
#define ERTS_PORT_REDS_EXECUTE		(CONTEXT_REDS/4)
#define ERTS_PORT_REDS_FREE		(CONTEXT_REDS/400)
#define ERTS_PORT_REDS_TIMEOUT		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_INPUT		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_OUTPUT		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_EVENT		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_CMD_OUTPUTV	(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_CMD_OUTPUT	(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_EXIT		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_CONNECT		(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_UNLINK		(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_LINK		(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_MONITOR		(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_DEMONITOR	(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_BADSIG		(CONTEXT_REDS/200)
#define ERTS_PORT_REDS_CONTROL		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_CALL		(CONTEXT_REDS/50)
#define ERTS_PORT_REDS_INFO		(CONTEXT_REDS/100)
#define ERTS_PORT_REDS_TERMINATE	(CONTEXT_REDS/50)

void print_port_info(Port *, int, void *);
void erts_port_free(Port *);
#ifndef ERTS_SMP
void erts_port_cleanup(Port *);
#endif
void erts_fire_port_monitor(Port *prt, Eterm ref);
#ifdef ERTS_SMP
int erts_port_handle_xports(Port *);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_is_port_locked(Port *);
#endif

ERTS_GLB_INLINE void erts_port_inc_refc(Port *prt);
ERTS_GLB_INLINE void erts_port_dec_refc(Port *prt);
ERTS_GLB_INLINE void erts_port_add_refc(Port *prt, Sint32 add_refc);
ERTS_GLB_INLINE Sint erts_port_read_refc(Port *prt);

ERTS_GLB_INLINE int erts_smp_port_trylock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_lock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_unlock(Port *prt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_port_inc_refc(Port *prt)
{
    erts_ptab_atmc_inc_refc(&prt->common);
}

ERTS_GLB_INLINE void erts_port_dec_refc(Port *prt)
{
    int referred = erts_ptab_atmc_dec_test_refc(&prt->common);
    if (!referred)
	erts_port_free(prt);
}

ERTS_GLB_INLINE void erts_port_add_refc(Port *prt, Sint32 add_refc)
{
    int referred = erts_ptab_atmc_add_test_refc(&prt->common, add_refc);
    if (!referred)
	erts_port_free(prt);
}

ERTS_GLB_INLINE Sint erts_port_read_refc(Port *prt)
{
    return erts_ptab_atmc_read_refc(&prt->common);
}

ERTS_GLB_INLINE int
erts_smp_port_trylock(Port *prt)
{
#ifdef ERTS_SMP
    /* *Need* to be a managed thread */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());
    return erts_mtx_trylock(prt->lock);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_lock(Port *prt)
{
#ifdef ERTS_SMP
    /* *Need* to be a managed thread */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());
    erts_mtx_lock(prt->lock);
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_unlock(Port *prt)
{
#ifdef ERTS_SMP
    /* *Need* to be a managed thread */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());
    erts_mtx_unlock(prt->lock);
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#define ERTS_INVALID_PORT_OPT(PP, ID, FLGS)			\
    (!(PP)							\
     || (erts_atomic32_read_nob(&(PP)->state) & (FLGS))	\
     || (PP)->common.id != (ID))

/* port lookup */

#define INVALID_PORT(PP, ID) \
  ERTS_INVALID_PORT_OPT((PP), (ID), ERTS_PORT_SFLGS_INVALID_LOOKUP)

/* Invalidate trace port if anything suspicious, for instance
 * that the port is a distribution port or it is busy.
 */
#define INVALID_TRACER_PORT(PP, ID)					\
  ERTS_INVALID_PORT_OPT((PP), (ID), ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP)

#define ERTS_PORT_SCHED_ID(P, ID) \
  ((Uint) (UWord) erts_prtsd_set((P), ERTS_PSD_SCHED_ID, (void *) (UWord) (ID)))

extern const Port erts_invalid_port;
#define ERTS_PORT_LOCK_BUSY ((Port *) &erts_invalid_port)

int erts_is_port_ioq_empty(Port *);
void erts_terminate_port(Port *);

#ifdef ERTS_SMP
Port *erts_de2port(DistEntry *, Process *, ErtsProcLocks);
#endif

ERTS_GLB_INLINE Port *erts_pix2port(int);
ERTS_GLB_INLINE Port *erts_port_lookup_raw(Eterm);
ERTS_GLB_INLINE Port *erts_port_lookup(Eterm, Uint32);
ERTS_GLB_INLINE Port*erts_id2port(Eterm id);
ERTS_GLB_INLINE Port *erts_id2port_sflgs(Eterm, Process *, ErtsProcLocks, Uint32);
ERTS_GLB_INLINE void erts_port_release(Port *);
#ifdef ERTS_SMP
ERTS_GLB_INLINE Port *erts_thr_port_lookup(Eterm id, Uint32 invalid_sflgs);
ERTS_GLB_INLINE Port *erts_thr_id2port_sflgs(Eterm id, Uint32 invalid_sflgs);
ERTS_GLB_INLINE void erts_thr_port_release(Port *prt);
#endif
ERTS_GLB_INLINE Port *erts_thr_drvport2port(ErlDrvPort, int);
ERTS_GLB_INLINE Port *erts_drvport2port_state(ErlDrvPort, erts_aint32_t *);
ERTS_GLB_INLINE Eterm erts_drvport2id(ErlDrvPort);
ERTS_GLB_INLINE Uint32 erts_portid2status(Eterm);
ERTS_GLB_INLINE int erts_is_port_alive(Eterm);
ERTS_GLB_INLINE int erts_is_valid_tracer_port(Eterm);
ERTS_GLB_INLINE int erts_port_driver_callback_epilogue(Port *, erts_aint32_t *);

#define erts_drvport2port(Prt) erts_drvport2port_state((Prt), NULL)

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Port *erts_pix2port(int ix)
{
    Port *prt;
    ASSERT(0 <= ix && ix < erts_ptab_max(&erts_port));
    prt = (Port *) erts_ptab_pix2intptr_nob(&erts_port, ix);
    return prt == ERTS_PORT_LOCK_BUSY ? NULL : prt;
}

ERTS_GLB_INLINE Port *
erts_port_lookup_raw(Eterm id)
{
    Port *prt;

    ERTS_SMP_LC_ASSERT(erts_thr_progress_lc_is_delaying());

    if (is_not_internal_port(id))
	return NULL;

    prt = (Port *) erts_ptab_pix2intptr_ddrb(&erts_port,
					     internal_port_index(id));
    return prt && prt->common.id == id ? prt : NULL;
}

ERTS_GLB_INLINE Port *
erts_port_lookup(Eterm id, Uint32 invalid_sflgs)
{
    Port *prt = erts_port_lookup_raw(id);
    return (!prt
	    ? NULL
	    : ((invalid_sflgs & erts_atomic32_read_nob(&prt->state))
	       ? NULL
	       : prt));
}


ERTS_GLB_INLINE Port*
erts_id2port(Eterm id)
{
    erts_aint32_t state;
    Port *prt;

    /* Only allowed to be called from managed threads */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());

    if (is_not_internal_port(id))
	return NULL;

    prt = (Port *) erts_ptab_pix2intptr_ddrb(&erts_port,
					     internal_port_index(id));

    if (!prt || prt->common.id != id)
	return NULL;

    erts_smp_port_lock(prt);
    state = erts_atomic32_read_nob(&prt->state);
    if (state & ERTS_PORT_SFLGS_INVALID_LOOKUP) {
	erts_smp_port_unlock(prt);
	return NULL;
    }

    return prt;
}


ERTS_GLB_INLINE Port*
erts_id2port_sflgs(Eterm id,
		   Process *c_p, ErtsProcLocks c_p_locks,
		   Uint32 invalid_sflgs)
{
#ifdef ERTS_SMP
    int no_proc_locks = !c_p || !c_p_locks;
#endif
    erts_aint32_t state;
    Port *prt;

    /* Only allowed to be called from managed threads */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());

    if (is_not_internal_port(id))
	return NULL;

    prt = (Port *) erts_ptab_pix2intptr_ddrb(&erts_port,
					     internal_port_index(id));

    if (!prt || prt->common.id != id)
	return NULL;

#ifdef ERTS_SMP
    if (no_proc_locks)
	erts_smp_port_lock(prt);
    else if (erts_smp_port_trylock(prt) == EBUSY) {
	/* Unlock process locks, and acquire locks in lock order... */
	erts_smp_proc_unlock(c_p, c_p_locks);
	erts_smp_port_lock(prt);
	erts_smp_proc_lock(c_p, c_p_locks);
    }
#endif
    state = erts_atomic32_read_nob(&prt->state);
    if (state & invalid_sflgs) {
#ifdef ERTS_SMP
	erts_smp_port_unlock(prt);
#endif
	return NULL;
    }

    return prt;
}

ERTS_GLB_INLINE void
erts_port_release(Port *prt)
{
    /* Only allowed to be called from managed threads */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());
#ifdef ERTS_SMP
    erts_smp_port_unlock(prt);
#else
    if (prt->cleanup) {
	prt->cleanup = 0;
	erts_port_cleanup(prt);
    }
#endif
}

#ifdef ERTS_SMP
/*
 * erts_thr_id2port_sflgs() and erts_port_dec_refc(prt) can
 * be used by unmanaged threads in the SMP case.
 */
ERTS_GLB_INLINE Port *
erts_thr_port_lookup(Eterm id, Uint32 invalid_sflgs)
{
    Port *prt;
    ErtsThrPrgrDelayHandle dhndl;

    if (is_not_internal_port(id))
	return NULL;

    dhndl = erts_thr_progress_unmanaged_delay();

    prt = (Port *) erts_ptab_pix2intptr_ddrb(&erts_port,
					     internal_port_index(id));

    if (!prt || prt->common.id != id) {
	erts_thr_progress_unmanaged_continue(dhndl);
	return NULL;
    }
    else {
	erts_aint32_t state;
	erts_port_inc_refc(prt);

	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
	    erts_thr_progress_unmanaged_continue(dhndl);

	state = erts_atomic32_read_acqb(&prt->state);
	if (state & invalid_sflgs) {
	    erts_port_dec_refc(prt);
	    return NULL;
	}

	return prt;
    }
}

/*
 * erts_thr_id2port_sflgs() and erts_thr_port_release() can
 * be used by unmanaged threads in the SMP case.
 */
ERTS_GLB_INLINE Port *
erts_thr_id2port_sflgs(Eterm id, Uint32 invalid_sflgs)
{
    Port *prt;
    ErtsThrPrgrDelayHandle dhndl;

    if (is_not_internal_port(id))
	return NULL;

    dhndl = erts_thr_progress_unmanaged_delay();

    prt = (Port *) erts_ptab_pix2intptr_ddrb(&erts_port,
					     internal_port_index(id));

    if (!prt || prt->common.id != id) {
	erts_thr_progress_unmanaged_continue(dhndl);
	prt = NULL;
    }
    else {
	erts_aint32_t state;
	if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED) {
	    erts_port_inc_refc(prt);
	    erts_thr_progress_unmanaged_continue(dhndl);
	}

	erts_mtx_lock(prt->lock);
	state = erts_atomic32_read_nob(&prt->state);
	if (state & invalid_sflgs) {
	    erts_mtx_unlock(prt->lock);
	    if (dhndl != ERTS_THR_PRGR_DHANDLE_MANAGED)
		erts_port_dec_refc(prt);
	    prt = NULL;
	}
    }

    return prt;
}

ERTS_GLB_INLINE void
erts_thr_port_release(Port *prt)
{
    erts_mtx_unlock(prt->lock);
#ifdef ERTS_SMP
    if (!erts_thr_progress_is_managed_thread())
	erts_port_dec_refc(prt);
#endif
}

#endif

ERTS_GLB_INLINE Port *
erts_thr_drvport2port(ErlDrvPort drvport, int lock_pdl)
{
    Port *prt = ERTS_ErlDrvPort2Port(drvport);
    ASSERT(prt != NULL);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return ERTS_INVALID_ERL_DRV_PORT;

    if (lock_pdl && prt->port_data_lock)
	driver_pdl_lock(prt->port_data_lock);

#if ERTS_ENABLE_LOCK_CHECK
    if (!ERTS_IS_CRASH_DUMPING) {
	if (erts_lc_is_emu_thr()) {
	    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
	    ERTS_LC_ASSERT(!prt->port_data_lock
			   || erts_lc_mtx_is_locked(&prt->port_data_lock->mtx));
	}
	else {
	    ERTS_LC_ASSERT(prt->port_data_lock);
	    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&prt->port_data_lock->mtx));
	}
    }
#endif

    if (erts_atomic32_read_nob(&prt->state)
	& ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP) {
	if (lock_pdl && prt->port_data_lock)
	    driver_pdl_unlock(prt->port_data_lock);
	return ERTS_INVALID_ERL_DRV_PORT;
    }
    return prt;
}

ERTS_GLB_INLINE Port *
erts_drvport2port_state(ErlDrvPort drvport, erts_aint32_t *statep)
{
    Port *prt = ERTS_ErlDrvPort2Port(drvport);
    erts_aint32_t state;
    ASSERT(prt);
//    ERTS_LC_ASSERT(erts_lc_is_emu_thr());
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return ERTS_INVALID_ERL_DRV_PORT;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt)
		       || ERTS_IS_CRASH_DUMPING);
    /* 
     * This state check is only needed since a driver callback
     * might terminate the port, and then call back into the
     * emulator. Drivers should preferably have been forbidden
     * to call into the emulator after terminating the port,
     * but it has been like this for ages. Perhaps forbid this
     * in some future major release?
     */
    state = erts_atomic32_read_nob(&prt->state);
    if (state & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	return ERTS_INVALID_ERL_DRV_PORT;
    if (statep)
	*statep = state;
    return prt;
}

ERTS_GLB_INLINE Eterm
erts_drvport2id(ErlDrvPort drvport)
{
    Port *prt = erts_drvport2port(drvport);
    if (prt == ERTS_INVALID_ERL_DRV_PORT)
	return am_undefined;
    else
	return prt->common.id;
}

ERTS_GLB_INLINE Uint32
erts_portid2status(Eterm id)
{
    Port *prt = erts_port_lookup_raw(id);
    if (prt)
	return (Uint32) erts_atomic32_read_acqb(&prt->state);
    else
	return ERTS_PORT_SFLG_INVALID;
}

ERTS_GLB_INLINE int
erts_is_port_alive(Eterm id)
{
    return !(erts_portid2status(id) & (ERTS_PORT_SFLG_INVALID
				       | ERTS_PORT_SFLGS_DEAD));
}

ERTS_GLB_INLINE int
erts_is_valid_tracer_port(Eterm id)
{
    return !(erts_portid2status(id) & ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP);
}

ERTS_GLB_INLINE int
erts_port_driver_callback_epilogue(Port *prt, erts_aint32_t *statep)
{
    int reds = 0;
    erts_aint32_t state;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    state = erts_atomic32_read_nob(&prt->state);
    if ((state & ERTS_PORT_SFLG_CLOSING) && erts_is_port_ioq_empty(prt)) {
	reds += ERTS_PORT_REDS_TERMINATE;
	erts_terminate_port(prt);
	state = erts_atomic32_read_nob(&prt->state);
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    }

#ifdef ERTS_SMP
    if (prt->xports) {
	reds += erts_port_handle_xports(prt);
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
	ASSERT(!prt->xports);
    }
#endif

    if (statep)
	*statep = state;

    return reds;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

void erts_port_resume_procs(Port *);

struct binary;

enum {
    ERTS_P2P_SIG_TYPE_BAD       = 0,
    ERTS_P2P_SIG_TYPE_OUTPUT    = 1,
    ERTS_P2P_SIG_TYPE_OUTPUTV   = 2,
    ERTS_P2P_SIG_TYPE_CONNECT   = 3,
    ERTS_P2P_SIG_TYPE_EXIT      = 4,
    ERTS_P2P_SIG_TYPE_CONTROL   = 5,
    ERTS_P2P_SIG_TYPE_CALL      = 6,
    ERTS_P2P_SIG_TYPE_INFO      = 7,
    ERTS_P2P_SIG_TYPE_LINK      = 8,
    ERTS_P2P_SIG_TYPE_UNLINK    = 9,
    ERTS_P2P_SIG_TYPE_MONITOR   = 10,
    ERTS_P2P_SIG_TYPE_DEMONITOR = 11
};

#define ERTS_P2P_SIG_TYPE_BITS			4
#define ERTS_P2P_SIG_TYPE_MASK \
    ((1 << ERTS_P2P_SIG_TYPE_BITS) - 1)

#define ERTS_P2P_SIG_DATA_FLG(N) \
    (1 << (ERTS_P2P_SIG_TYPE_BITS + (N)))
#define ERTS_P2P_SIG_DATA_FLG_BANG_OP		ERTS_P2P_SIG_DATA_FLG(0)
#define ERTS_P2P_SIG_DATA_FLG_REPLY		ERTS_P2P_SIG_DATA_FLG(1)
#define ERTS_P2P_SIG_DATA_FLG_NOSUSPEND		ERTS_P2P_SIG_DATA_FLG(2)
#define ERTS_P2P_SIG_DATA_FLG_FORCE		ERTS_P2P_SIG_DATA_FLG(3)
#define ERTS_P2P_SIG_DATA_FLG_BAD_OUTPUT	ERTS_P2P_SIG_DATA_FLG(4)
#define ERTS_P2P_SIG_DATA_FLG_BROKEN_LINK	ERTS_P2P_SIG_DATA_FLG(5)
#define ERTS_P2P_SIG_DATA_FLG_SCHED		ERTS_P2P_SIG_DATA_FLG(6)
#define ERTS_P2P_SIG_DATA_FLG_ASYNC		ERTS_P2P_SIG_DATA_FLG(7)

struct ErtsProc2PortSigData_ {
    int flags;
    Eterm caller;
    Uint32 ref[ERTS_MAX_REF_NUMBERS];
    union {
	struct {
	    Eterm from;
	    ErlIOVec *evp;
	    ErlDrvBinary *cbinp;
	} outputv;
	struct {
	    Eterm from;
	    char *bufp;
	    ErlDrvSizeT size;
	} output;
	struct {
	    Eterm from;
	    Eterm connected;
	} connect;
	struct {
	    Eterm from;
	    Eterm reason;
	    ErlHeapFragment *bp;
	} exit;
	struct {
	    struct binary *binp;
	    unsigned int command;
	    char *bufp;
	    ErlDrvSizeT size;
	} control;
	struct {
	    unsigned int command;
	    char *bufp;
	    ErlDrvSizeT size;
	} call;
	struct {
	    Eterm item;
	} info;
	struct {
	    Eterm port;
	    Eterm to;
	} link;
	struct {
	    Eterm from;
	} unlink;
        struct {
            Eterm origin;   /* who receives monitor event, pid */
            Eterm name;     /* either name for named monitor, or port id */
        } monitor;
        struct {
            Eterm origin;   /* who is at the other end of the monitor, pid */
            Eterm name;     /* port id */
            Uint32 ref[ERTS_MAX_REF_NUMBERS]; /* box contents of a ref */
        } demonitor;
    } u;
} ;

ERTS_GLB_INLINE int
erts_proc2port_sig_is_command_op(ErtsProc2PortSigData *sigdp);
ERTS_GLB_INLINE ErlDrvSizeT
erts_proc2port_sig_command_data_size(ErtsProc2PortSigData *sigdp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_proc2port_sig_is_command_op(ErtsProc2PortSigData *sigdp)
{
    switch (sigdp->flags & ERTS_P2P_SIG_TYPE_MASK) {
    case ERTS_P2P_SIG_TYPE_OUTPUT:	return !0;
    case ERTS_P2P_SIG_TYPE_OUTPUTV:	return !0;
    default:				return 0;
    }
}

ERTS_GLB_INLINE ErlDrvSizeT
erts_proc2port_sig_command_data_size(ErtsProc2PortSigData *sigdp)
{
    switch (sigdp->flags & ERTS_P2P_SIG_TYPE_MASK) {
    case ERTS_P2P_SIG_TYPE_OUTPUT:	return sigdp->u.output.size;
    case ERTS_P2P_SIG_TYPE_OUTPUTV:	return sigdp->u.outputv.evp->size;
    default:				return (ErlDrvSizeT) 0;
    }
}

#endif

#define ERTS_PROC2PORT_SIG_EXEC			0
#define ERTS_PROC2PORT_SIG_ABORT		1
#define ERTS_PROC2PORT_SIG_ABORT_NOSUSPEND	2
#define ERTS_PROC2PORT_SIG_ABORT_CLOSED		3

typedef int (*ErtsProc2PortSigCallback)(Port *,
					erts_aint32_t,
					int,
					ErtsProc2PortSigData *);

typedef enum {
    ERTS_PORT_OP_BADARG,
    ERTS_PORT_OP_CALLER_EXIT,
    ERTS_PORT_OP_BUSY,
    ERTS_PORT_OP_BUSY_SCHEDULED,
    ERTS_PORT_OP_SCHEDULED,
    ERTS_PORT_OP_DROPPED,
    ERTS_PORT_OP_DONE
} ErtsPortOpResult;

ErtsPortOpResult
erts_schedule_proc2port_signal(Process *,
			       Port *,
			       Eterm,
			       Eterm *,
			       ErtsProc2PortSigData *,
			       int,
			       ErtsPortTaskHandle *,
			       ErtsProc2PortSigCallback);

int erts_deliver_port_exit(Port *, Eterm, Eterm, int, int);

/*
 * Port signal flags
 */
#define ERTS_PORT_SIG_FLG_BANG_OP		ERTS_P2P_SIG_DATA_FLG_BANG_OP
#define ERTS_PORT_SIG_FLG_NOSUSPEND		ERTS_P2P_SIG_DATA_FLG_NOSUSPEND
#define ERTS_PORT_SIG_FLG_FORCE			ERTS_P2P_SIG_DATA_FLG_FORCE
#define ERTS_PORT_SIG_FLG_BROKEN_LINK		ERTS_P2P_SIG_DATA_FLG_BROKEN_LINK
#define ERTS_PORT_SIG_FLG_BAD_OUTPUT		ERTS_P2P_SIG_DATA_FLG_BAD_OUTPUT
#define ERTS_PORT_SIG_FLG_FORCE_SCHED		ERTS_P2P_SIG_DATA_FLG_SCHED
#define ERTS_PORT_SIG_FLG_ASYNC			ERTS_P2P_SIG_DATA_FLG_ASYNC
/* ERTS_PORT_SIG_FLG_FORCE_IMM_CALL only when crash dumping... */
#define ERTS_PORT_SIG_FLG_FORCE_IMM_CALL	ERTS_P2P_SIG_DATA_FLG_BAD_OUTPUT

/*
 * Port ! {Owner, {command, Data}}
 * Port ! {Owner, {connect, NewOwner}}
 * Port ! {Owner, close}
 */
ErtsPortOpResult erts_port_command(Process *, int, Port *, Eterm, Eterm *);

/*
 * Signals from processes to ports.
 */
ErtsPortOpResult erts_port_output(Process *, int, Port *, Eterm, Eterm, Eterm *);
ErtsPortOpResult erts_port_exit(Process *, int, Port *, Eterm, Eterm, Eterm *);
ErtsPortOpResult erts_port_connect(Process *, int, Port *, Eterm, Eterm, Eterm *);
ErtsPortOpResult erts_port_link(Process *, Port *, Eterm, Eterm *);
ErtsPortOpResult erts_port_unlink(Process *, Port *, Eterm, Eterm *);
ErtsPortOpResult erts_port_control(Process *, Port *, unsigned int, Eterm, Eterm *);
ErtsPortOpResult erts_port_call(Process *, Port *, unsigned int, Eterm, Eterm *);
ErtsPortOpResult erts_port_info(Process *, Port *, Eterm, Eterm *);

/* Creates monitor between Origin and Target. Ref must be initialized to
 * a reference (ref may be rewritten to be used to serve additionally as a
 * signal id). Name is atom if user monitors port by name or NIL */
ErtsPortOpResult erts_port_monitor(Process *origin, Port *target, Eterm name,
                                   Eterm *ref);

typedef enum {
    /* Normal demonitor rules apply with locking and reductions bump */
    ERTS_PORT_DEMONITOR_NORMAL = 1,
    /* Relaxed demonitor rules when process is about to die, which means that
     * pid lookup won't work, locks won't work, no reductions bump. */
    ERTS_PORT_DEMONITOR_ORIGIN_ON_DEATHBED = 2,
} ErtsDemonitorMode;

/* Removes monitor between origin and target, identified by ref.
 * origin_is_dying can be 0 (false, normal locking rules and reductions bump
 * apply) or 1 (true, in case when we avoid origin locking) */
ErtsPortOpResult erts_port_demonitor(Process *origin, ErtsDemonitorMode mode,
                                    Port *target, Eterm ref,
                                    Eterm *trap_ref);
/* defined in erl_bif_port.c */
Port *erts_sig_lookup_port(Process *c_p, Eterm id_or_name);

int erts_port_output_async(Port *, Eterm, Eterm);

/*
 * Signals from ports to ports. Used by sys drivers.
 */
int erl_drv_port_control(Eterm, char, char*, ErlDrvSizeT);

#endif
