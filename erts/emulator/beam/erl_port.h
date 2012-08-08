/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
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

#ifndef ERL_PORT_H__
#define ERL_PORT_H__

typedef struct port Port;
#include "erl_port_task.h"
#include "erl_ptab.h"

typedef struct erts_driver_t_ erts_driver_t;

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

struct port {
    ErtsPTabElementCommon common; /* *Need* to be first in struct */

    ErtsPortTaskSched sched;
    ErtsPortTaskHandle timeout_task;
#ifdef ERTS_SMP
    erts_smp_mtx_t *lock;
    ErtsXPortsList *xports;
    erts_smp_atomic_t run_queue;
    erts_smp_spinlock_t state_lck;  /* protects: id, status, snapshot */
#endif
    Eterm connected;            /* A connected process */
    Eterm caller;		/* Current caller. */
    Eterm data;			/* Data associated with port. */
    ErlHeapFragment* bp;	/* Heap fragment holding data (NULL if imm data). */
    Uint bytes_in;		/* Number of bytes read */
    Uint bytes_out;		/* Number of bytes written */

    ErlIOQueue ioq;              /* driver accessible i/o queue */
    DistEntry *dist_entry;       /* Dist entry used in DISTRIBUTION */
    char *name;		         /* String used in the open */
    erts_driver_t* drv_ptr;
    UWord drv_data;
    ErtsProcList *suspended;	 /* List of suspended processes. */
    LineBuf *linebuf;            /* Buffer to hold data not ready for
				    process to get (line oriented I/O)*/
    erts_smp_atomic32_t state;	 /* Status and type flags */
    int control_flags;		 /* Flags for port_control()  */
    erts_aint32_t snapshot;      /* Next snapshot that port should be part of */
    ErlDrvPDL port_data_lock;

    ErtsPrtSD *psd;		 /* Port specific data */
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
    return prt->psd ? prt->psd->data[ix] : NULL;
}

ERTS_GLB_INLINE void *
erts_prtsd_set(Port *prt, int ix, void *data)
{
    if (prt->psd) {
	void *old = prt->psd->data[ix];
	prt->psd->data[ix] = data;
	return old;
    }
    else {
	prt->psd = erts_alloc(ERTS_ALC_T_PRTSD, sizeof(ErtsPrtSD));
	prt->psd->data[ix] = data;
	return NULL;
    }
}

#endif

/* arrays that get malloced at startup */
extern Port* erts_port;

extern Uint erts_max_ports;
extern Uint erts_port_tab_index_mask;
extern erts_smp_atomic32_t erts_ports_snapshot;
extern erts_smp_atomic_t erts_dead_ports_ptr;

ERTS_GLB_INLINE void erts_may_save_closed_port(Port *prt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_may_save_closed_port(Port *prt)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_spinlock_is_locked(&prt->state_lck));
    if (prt->snapshot != erts_smp_atomic32_read_acqb(&erts_ports_snapshot)) {
	/* Dead ports are added from the end of the snapshot buffer */
	Eterm* tombstone;
	tombstone = (Eterm*) erts_smp_atomic_add_read_nob(&erts_dead_ports_ptr,
							  -(erts_aint_t)sizeof(Eterm));
	ASSERT(tombstone+1 != NULL);
	ASSERT(prt->snapshot == erts_smp_atomic32_read_nob(&erts_ports_snapshot) - 1);
	*tombstone = prt->common.id;
    }
    /*else no ongoing snapshot or port was already included or created after snapshot */
}

#endif

extern erts_smp_atomic_t erts_bytes_out;	/* no bytes written out */
extern erts_smp_atomic_t erts_bytes_in;		/* no bytes sent into the system */


/* port status flags */

#define ERTS_PORT_SFLG_CONNECTED	((Uint32) (1 <<  0))
/* Port have begun exiting */
#define ERTS_PORT_SFLG_EXITING		((Uint32) (1 <<  1))
/* Distribution port */
#define ERTS_PORT_SFLG_DISTRIBUTION	((Uint32) (1 <<  2))
#define ERTS_PORT_SFLG_BINARY_IO	((Uint32) (1 <<  3))
#define ERTS_PORT_SFLG_SOFT_EOF		((Uint32) (1 <<  4))
/* Flow control */
#define ERTS_PORT_SFLG_PORT_BUSY	((Uint32) (1 <<  5))
/* Port is closing (no i/o accepted) */
#define ERTS_PORT_SFLG_CLOSING		((Uint32) (1 <<  6))
/* Send a closed message when terminating */
#define ERTS_PORT_SFLG_SEND_CLOSED	((Uint32) (1 <<  7))
/* Line orinted io on port */  
#define ERTS_PORT_SFLG_LINEBUF_IO	((Uint32) (1 <<  8))
/* Immortal port (only certain system ports) */
#define ERTS_PORT_SFLG_IMMORTAL		((Uint32) (1 <<  9))
#define ERTS_PORT_SFLG_FREE		((Uint32) (1 << 10))
#define ERTS_PORT_SFLG_FREE_SCHEDULED	((Uint32) (1 << 11))
#define ERTS_PORT_SFLG_INITIALIZING	((Uint32) (1 << 12))
/* Port uses port specific locking (opposed to driver specific locking) */
#define ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK ((Uint32) (1 << 13))
#define ERTS_PORT_SFLG_INVALID		((Uint32) (1 << 14))
/* Last port to terminate halts the emulator */
#define ERTS_PORT_SFLG_HALT		((Uint32) (1 << 15))
#ifdef DEBUG
/* Only debug: make sure all flags aren't cleared unintentionally */
#define ERTS_PORT_SFLG_PORT_DEBUG	((Uint32) (1 << 31))
#endif

/* Combinations of port status flags */ 
#define ERTS_PORT_SFLGS_DEAD						\
  (ERTS_PORT_SFLG_FREE							\
   | ERTS_PORT_SFLG_FREE_SCHEDULED					\
   | ERTS_PORT_SFLG_INITIALIZING)
#define ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP				\
  (ERTS_PORT_SFLGS_DEAD | ERTS_PORT_SFLG_INVALID)
#define ERTS_PORT_SFLGS_INVALID_LOOKUP					\
  (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP				\
   | ERTS_PORT_SFLG_CLOSING)
#define ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP				\
  (ERTS_PORT_SFLGS_INVALID_LOOKUP					\
   | ERTS_PORT_SFLG_PORT_BUSY						\
   | ERTS_PORT_SFLG_DISTRIBUTION)


void erts_port_cleanup(Port *);
void erts_fire_port_monitor(Port *prt, Eterm ref);
#ifdef ERTS_SMP
void erts_smp_xports_unlock(Port *);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_is_port_locked(Port *);
#endif

ERTS_GLB_INLINE void erts_smp_port_minor_lock(Port*);
ERTS_GLB_INLINE void erts_smp_port_minor_unlock(Port*);

ERTS_GLB_INLINE int erts_smp_port_trylock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_lock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_unlock(Port *prt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_smp_port_minor_lock(Port* prt)
{
#ifdef ERTS_SMP
    erts_smp_spin_lock(&prt->state_lck);
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_minor_unlock(Port *prt)
{
#ifdef ERTS_SMP
    erts_smp_spin_unlock(&prt->state_lck);
#endif
}


ERTS_GLB_INLINE int
erts_smp_port_trylock(Port *prt)
{
    int res;

    ASSERT(erts_smp_atomic32_read_nob(&prt->common.refc) > 0);
    erts_smp_atomic32_inc_nob(&prt->common.refc);

#ifdef ERTS_SMP
    res = erts_smp_mtx_trylock(prt->lock);
    if (res == EBUSY) {
	erts_smp_atomic32_dec_nob(&prt->common.refc);
    }
#else
    res = 0;
#endif

    return res;
}

ERTS_GLB_INLINE void
erts_smp_port_lock(Port *prt)
{
    ASSERT(erts_smp_atomic32_read_nob(&prt->common.refc) > 0);
    erts_smp_atomic32_inc_nob(&prt->common.refc);
#ifdef ERTS_SMP
    erts_smp_mtx_lock(prt->lock);
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_unlock(Port *prt)
{
    erts_aint32_t refc;
#ifdef ERTS_SMP
    erts_smp_mtx_unlock(prt->lock);
#endif
    refc = erts_smp_atomic32_dec_read_nob(&prt->common.refc);
    ASSERT(refc >= 0);
    if (refc == 0)
	erts_port_cleanup(prt);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#define ERTS_INVALID_PORT_OPT(PP, ID, FLGS)			\
    (!(PP)							\
     || (erts_smp_atomic32_read_nob(&(PP)->state) & (FLGS))	\
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

#ifdef ERTS_SMP
Port *erts_de2port(DistEntry *, Process *, ErtsProcLocks);
#endif

#define erts_id2port(ID, P, PL) \
  erts_id2port_sflgs((ID), (P), (PL), ERTS_PORT_SFLGS_INVALID_LOOKUP)

ERTS_GLB_INLINE Port*erts_id2port_sflgs(Eterm, Process *, ErtsProcLocks, Uint32);
ERTS_GLB_INLINE void erts_port_release(Port *);
ERTS_GLB_INLINE Port*erts_drvport2port(ErlDrvPort, erts_aint32_t *);
ERTS_GLB_INLINE Port*erts_drvportid2port(Eterm);
ERTS_GLB_INLINE Uint32 erts_portid2status(Eterm id);
ERTS_GLB_INLINE int erts_is_port_alive(Eterm id);
ERTS_GLB_INLINE int erts_is_valid_tracer_port(Eterm id);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Port*
erts_id2port_sflgs(Eterm id, Process *c_p, ErtsProcLocks c_p_locks, Uint32 sflgs)
{
#ifdef ERTS_SMP
    int no_proc_locks = !c_p || !c_p_locks;
#endif
    Port *prt;

    if (is_not_internal_port(id))
	return NULL;

    prt = &erts_port[internal_port_index(id)];

    erts_smp_port_minor_lock(prt);
    if (ERTS_INVALID_PORT_OPT(prt, id, sflgs)) {
	erts_smp_port_minor_unlock(prt);
	prt = NULL;
    }
    else {
	erts_smp_atomic32_inc_nob(&prt->common.refc);
	erts_smp_port_minor_unlock(prt);

#ifdef ERTS_SMP
 	if (no_proc_locks)
	    erts_smp_mtx_lock(prt->lock);
	else if (erts_smp_mtx_trylock(prt->lock) == EBUSY) {
	    /* Unlock process locks, and acquire locks in lock order... */
	    erts_smp_proc_unlock(c_p, c_p_locks);
	    erts_smp_mtx_lock(prt->lock);
	    erts_smp_proc_lock(c_p, c_p_locks);
	}

	/* The id may not have changed... */
	ERTS_SMP_LC_ASSERT(prt->common.id == id);
	/* ... but state may have... */
	if (erts_smp_atomic32_read_nob(&prt->state) & sflgs) {
	    erts_smp_port_unlock(prt); /* Also decrements common.refc... */
	    prt = NULL;
	}
#endif

    }

    return prt;
}

ERTS_GLB_INLINE void
erts_port_release(Port *prt)
{
    erts_smp_port_unlock(prt);
}

ERTS_GLB_INLINE Port*
erts_drvport2port(ErlDrvPort drvport, erts_aint32_t *statep)
{
    int ix = (int) drvport;
    erts_aint32_t state;
    if (ix < 0 || erts_max_ports <= ix)
	return NULL;
    state = erts_smp_atomic32_read_nob(&erts_port[ix].state);
    if (state & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	return NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[ix]));
    if (statep)
	*statep = state;
    return &erts_port[ix];
}

ERTS_GLB_INLINE Port*
erts_drvportid2port(Eterm id)
{
    int ix;
    erts_aint32_t state;
    if (is_not_internal_port(id))
	return NULL;
    ix = (int) internal_port_index(id);
    if (erts_max_ports <= ix)
	return NULL;
    state = erts_smp_atomic32_read_nob(&erts_port[ix].state);
    if (state & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	return NULL;
    if (erts_port[ix].common.id != id)
	return NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[ix]));
    return &erts_port[ix];
}

ERTS_GLB_INLINE Uint32
erts_portid2status(Eterm id)
{
    if (is_not_internal_port(id))
	return ERTS_PORT_SFLG_INVALID;
    else {
	erts_aint32_t state;
	int ix = internal_port_index(id);
	if (erts_max_ports <= ix)
	    return ERTS_PORT_SFLG_INVALID;
	state = erts_smp_atomic32_read_ddrb(&erts_port[ix].state);
	if (erts_port[ix].common.id != id)
	    return ERTS_PORT_SFLG_INVALID;
	return state;
    }
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
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif
