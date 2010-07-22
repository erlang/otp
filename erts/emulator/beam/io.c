/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
 * I/O routines for manipulating ports.
 */

#define ERL_IO_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"

/* must be included BEFORE global.h (since it includes erl_driver.h) */
#include "erl_sys_driver.h"
#include "erl_nif.h"

#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "dist.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_version.h"
#include "error.h"

extern ErlDrvEntry fd_driver_entry;
extern ErlDrvEntry vanilla_driver_entry;
extern ErlDrvEntry spawn_driver_entry;
extern ErlDrvEntry *driver_tab[]; /* table of static drivers, only used during initialization */

erts_driver_t *driver_list; /* List of all drivers, static and dynamic. */
erts_smp_mtx_t erts_driver_list_lock; /* Mutex for driver list */
static erts_smp_tsd_key_t driver_list_lock_status_key; /*stop recursive locks when calling 
							 driver init */
static erts_smp_tsd_key_t driver_list_last_error_key;  /* Save last DDLL error on a 
							  per thread basis (for BC interfaces) */

Port*      erts_port; /* The port table */
erts_smp_atomic_t erts_ports_alive;
erts_smp_atomic_t erts_bytes_out;	/* No bytes sent out of the system */
erts_smp_atomic_t erts_bytes_in;	/* No bytes gotten into the system */

Uint erts_max_ports;
Uint erts_port_tab_index_mask;

const ErlDrvTermData driver_term_nil = (ErlDrvTermData)NIL;

erts_driver_t vanilla_driver;
erts_driver_t spawn_driver;
erts_driver_t fd_driver;

static int init_driver(erts_driver_t *, ErlDrvEntry *, DE_Handle *);
static void terminate_port(Port *p);
static void pdl_init(void);
#ifdef ERTS_SMP
static void driver_monitor_lock_pdl(Port *p);
static void driver_monitor_unlock_pdl(Port *p);
#define DRV_MONITOR_LOCK_PDL(Port) driver_monitor_lock_pdl(Port)
#define DRV_MONITOR_UNLOCK_PDL(Port) driver_monitor_unlock_pdl(Port)
#else
#define DRV_MONITOR_LOCK_PDL(Port) /* nothing */
#define DRV_MONITOR_UNLOCK_PDL(Port) /* nothing */
#endif

static ERTS_INLINE ErlIOQueue*
drvport2ioq(ErlDrvPort drvport)
{
    int ix = (int) drvport;
    Uint32 status;

    if (ix < 0 || erts_max_ports <= ix)
	return NULL;

    if (erts_get_scheduler_data()) {
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[ix]));
	ERTS_LC_ASSERT(!erts_port[ix].port_data_lock
		       || erts_lc_mtx_is_locked(
			   &erts_port[ix].port_data_lock->mtx));

	status = erts_port[ix].status;
    }
    else {
	erts_smp_port_state_lock(&erts_port[ix]);
	status = erts_port[ix].status;
	erts_smp_port_state_unlock(&erts_port[ix]);

	ERTS_LC_ASSERT((status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
		       || erts_port[ix].port_data_lock);
	ERTS_LC_ASSERT(!erts_port[ix].port_data_lock
		       || erts_lc_mtx_is_locked(
			   &erts_port[ix].port_data_lock->mtx));

    }

    return ((status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	    ? NULL
	    : &erts_port[ix].ioq);
}

static ERTS_INLINE int
is_port_ioq_empty(Port *pp)
{
    int res;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    if (!pp->port_data_lock)
	res = (pp->ioq.size == 0);
    else {
	ErlDrvPDL pdl = pp->port_data_lock;
	erts_mtx_lock(&pdl->mtx);
	res = (pp->ioq.size == 0);
	erts_mtx_unlock(&pdl->mtx);
    }
    return res;
}

int
erts_is_port_ioq_empty(Port *pp)
{
    return is_port_ioq_empty(pp);
}

Uint
erts_port_ioq_size(Port *pp)
{
    int res;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    if (!pp->port_data_lock)
	res = pp->ioq.size;
    else {
	ErlDrvPDL pdl = pp->port_data_lock;
	erts_mtx_lock(&pdl->mtx);
	res = pp->ioq.size;
	erts_mtx_unlock(&pdl->mtx);
    }
    return (Uint) res;
}

/*
 * Line buffered I/O.
 */
typedef struct line_buf_context {
    LineBuf **b;
    char *buf;
    int left;
    int retlen;
} LineBufContext;

#define LINEBUF_EMPTY 0
#define LINEBUF_EOL 1
#define LINEBUF_NOEOL 2
#define LINEBUF_ERROR -1

#define LINEBUF_STATE(LBC) ((*(LBC).b)->data[0])

#define LINEBUF_DATA(LBC) (((*(LBC).b)->data) + 1)
#define LINEBUF_DATALEN(LBC) ((LBC).retlen)

#define LINEBUF_INITIAL 100


/* The 'number' field in a port now has two parts: the lowest bits
   contain the index in the port table, and the higher bits are a counter
   which is incremented each time we look for a free port and start from
   the beginning of the table. erts_max_ports is the number of file descriptors,
   rounded up to a power of 2.
   To get the index from a port, use the macro 'internal_port_index';
   'port_number' returns the whole number field.
*/

static erts_smp_spinlock_t get_free_port_lck;
static Uint last_port_num;
static Uint port_num_mask;
erts_smp_atomic_t erts_ports_snapshot; /* Identifies the _next_ snapshot (not the ongoing) */


static ERTS_INLINE void
kill_port(Port *pp)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    erts_port_task_free_port(pp);
    ASSERT(pp->status & ERTS_PORT_SFLGS_DEAD);
}

#ifdef ERTS_SMP

#ifdef ERTS_ENABLE_LOCK_CHECK
int
erts_lc_is_port_locked(Port *prt)
{
    if (!prt)
	return 0;
    return erts_smp_lc_mtx_is_locked(prt->lock);
}
#endif

#endif /* #ifdef ERTS_SMP */

static int
get_free_port(void)
{
    Uint num;
    Uint tries = erts_max_ports;
    Port* port;    

    erts_smp_spin_lock(&get_free_port_lck);
    num = last_port_num + 1;
    for (;; ++num) {	
	port = &erts_port[num & erts_port_tab_index_mask];

	erts_smp_port_state_lock(port);
	if (port->status & ERTS_PORT_SFLG_FREE) {
	    last_port_num = num;
	    erts_smp_spin_unlock(&get_free_port_lck);
	    break;
	}
	erts_smp_port_state_unlock(port);

	if (--tries == 0) {
	    erts_smp_spin_unlock(&get_free_port_lck);
	    return -1;
	}
    }
    port->status = ERTS_PORT_SFLG_INITIALIZING;
#ifdef ERTS_SMP
    ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&port->refc) == 0);
    erts_smp_atomic_set(&port->refc, 2); /* Port alive + lock */
#endif	
    erts_smp_port_state_unlock(port);
    return num & port_num_mask;
}

/*
 * erts_test_next_port() is only used for testing.
 */
Sint
erts_test_next_port(int set, Uint next)
{
    Uint i, num;
    Sint res = -1;

    erts_smp_spin_lock(&get_free_port_lck);
    if (set) {
	last_port_num = (next - 1) & port_num_mask;
    }
    num = last_port_num + 1;

    for (i=0; i < erts_max_ports && res<0; ++i, ++num) {	
	
	Port* port = &erts_port[num & erts_port_tab_index_mask];

	erts_smp_port_state_lock(port);

	if (port->status & ERTS_PORT_SFLG_FREE) {	   
	    last_port_num = num - 1;
	    res = num & port_num_mask;
	}
	erts_smp_port_state_unlock(port);
    }
    erts_smp_spin_unlock(&get_free_port_lck);
    return res;
}


static void port_cleanup(Port *prt);

#ifdef ERTS_SMP

static void
sched_port_cleanup(void *vprt)
{
    Port *prt = (Port *) vprt;
    erts_smp_mtx_lock(prt->lock);
    port_cleanup(prt);
}

#endif

void
erts_port_cleanup(Port *prt)
{
#ifdef ERTS_SMP
    if (erts_smp_mtx_trylock(prt->lock) == EBUSY)
	erts_schedule_misc_op(sched_port_cleanup, (void *) prt);
    else
#endif
	port_cleanup(prt);
}

void
port_cleanup(Port *prt)
{
#ifdef ERTS_SMP
    Uint32 port_specific;
    erts_smp_mtx_t *mtx;
#endif
    erts_driver_t *driver;

    erts_smp_port_state_lock(prt);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    driver = prt->drv_ptr;
    prt->drv_ptr = NULL;
    ASSERT(driver);

#ifdef ERTS_SMP

    ASSERT(prt->status & ERTS_PORT_SFLG_FREE_SCHEDULED);
    ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&prt->refc) == 0);

    port_specific = (prt->status & ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK);

    mtx = prt->lock;
    ASSERT(mtx);

    prt->lock = NULL;

    ASSERT(prt->status & ERTS_PORT_SFLG_PORT_DEBUG);
    ASSERT(!(prt->status & ERTS_PORT_SFLG_FREE));
    prt->status = ERTS_PORT_SFLG_FREE;

    erts_smp_port_state_unlock(prt);    
    erts_smp_mtx_unlock(mtx);

    if (port_specific) {
	erts_smp_mtx_destroy(mtx);
	erts_free(ERTS_ALC_T_PORT_LOCK, mtx);
    }
#endif

    if (driver->handle)
	erts_ddll_dereference_driver(driver->handle);
}


/*
** Initialize v_start to point to the small fixed vector.
** Once (reallocated) we never reset the pointer to the small vector
** This is a possible optimisation.
*/
static void initq(Port* prt)
{
    ErlIOQueue* q = &prt->ioq;

    ERTS_LC_ASSERT(!prt->port_data_lock);

    q->size = 0;
    q->v_head = q->v_tail = q->v_start = q->v_small;
    q->v_end = q->v_small + SMALL_IO_QUEUE;
    q->b_head = q->b_tail = q->b_start = q->b_small;
    q->b_end = q->b_small + SMALL_IO_QUEUE;
}

static void stopq(Port* prt)
{
    ErlIOQueue* q;
    ErlDrvBinary** binp;

    if (prt->port_data_lock)
	driver_pdl_lock(prt->port_data_lock);

    q = &prt->ioq;
    binp = q->b_head;

    if (q->v_start != q->v_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);

    while(binp < q->b_tail) {
	if (*binp != NULL)
	    driver_free_binary(*binp);
	binp++;
    }
    if (q->b_start != q->b_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
    q->v_start = q->v_end = q->v_head = q->v_tail = NULL;
    q->b_start = q->b_end = q->b_head = q->b_tail = NULL;
    q->size = 0;

    if (prt->port_data_lock) {
	driver_pdl_unlock(prt->port_data_lock);
	driver_pdl_dec_refc(prt->port_data_lock);
	prt->port_data_lock = NULL;
    }
}



static void
setup_port(Port* prt, Eterm pid, erts_driver_t *driver, 
	   ErlDrvData drv_data, char *name, Uint32 xstatus)
{
    ErtsRunQueue *runq = erts_get_runq_current(NULL);
    char *new_name, *old_name;
#ifdef DEBUG
    /* Make sure the debug flags survives until port is freed */
    xstatus |= ERTS_PORT_SFLG_PORT_DEBUG;
#endif
    ASSERT(runq);
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    
    new_name = (char*) erts_alloc(ERTS_ALC_T_PORT_NAME, sys_strlen(name)+1);
    sys_strcpy(new_name, name);
    erts_smp_runq_lock(runq);
    erts_smp_atomic_inc(&erts_ports_alive);
    erts_smp_port_state_lock(prt);    
    prt->status = ERTS_PORT_SFLG_CONNECTED | xstatus;
    prt->snapshot = (Uint32) erts_smp_atomic_read(&erts_ports_snapshot);    
    old_name = prt->name;
    prt->name = new_name;
#ifdef ERTS_SMP
    erts_smp_atomic_set(&prt->run_queue, (long) runq);
#endif
    ASSERT(!prt->drv_ptr);
    prt->drv_ptr = driver;
    erts_smp_port_state_unlock(prt);
    erts_smp_runq_unlock(runq);
#ifdef ERTS_SMP
    ASSERT(!prt->xports);
#endif
    if (old_name) {
	erts_free(ERTS_ALC_T_PORT_NAME, (void *) old_name);
    }

    prt->control_flags = 0;
    prt->connected = pid;
    prt->drv_data = (long) drv_data;
    prt->bytes_in = 0;
    prt->bytes_out = 0;
    prt->dist_entry = NULL;
    prt->reg = NULL;
#ifdef ERTS_SMP
    prt->ptimer = NULL;
#else
    sys_memset(&prt->tm, 0, sizeof(ErlTimer));
#endif
    erts_port_task_handle_init(&prt->timeout_task);
    prt->suspended  = NULL;
    sys_strcpy(prt->name, name);
    prt->nlinks = NULL;
    prt->monitors = NULL;
    prt->linebuf = NULL;
    prt->bp = NULL;
    prt->data = am_undefined;
    /* Set default tracing */
    erts_get_default_tracing(&(prt->trace_flags), &(prt->tracer_proc));

    prt->psd = NULL;

    initq(prt);
}

void
erts_wake_process_later(Port *prt, Process *process)
{
    ErtsProcList** p;
    ErtsProcList* new_p;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt->status & ERTS_PORT_SFLGS_DEAD)
	return;

    for (p = &(prt->suspended); *p != NULL; p = &((*p)->next))
	/* Empty loop body */;

    new_p = erts_proclist_create(process);
    new_p->next = NULL;
    *p = new_p;
}

/*
   Opens a driver.
   Returns the non-negative port number, if successful.
   If there is an error, -1 or -2 or -3 is returned. -2 means that
   there is valid error information in *error_number_ptr.
   Returning -3 means that an error in the given options was detected
   (*error_number_ptr must contain either BADARG or SYSTEM_LIMIT).
   The driver start function must obey the same conventions.
*/
int
erts_open_driver(erts_driver_t* driver,	/* Pointer to driver. */
		 Eterm pid,		/* Current process. */
		 char* name,		/* Driver name. */
		 SysDriverOpts* opts,	/* Options. */
		 int *error_number_ptr)	/* errno in case -2 is returned */
{
    int port_num;
    int port_ix;
    ErlDrvData drv_data = 0;
    Uint32 xstatus = 0;
    Port *port;
    int fpe_was_unmasked;

    if (error_number_ptr)
	*error_number_ptr = 0;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if ((port_num = get_free_port()) < 0) {
	if (error_number_ptr) {
	    *error_number_ptr = SYSTEM_LIMIT;
	}
	return -3;
    }

    port_ix = port_num & erts_port_tab_index_mask;
    port = &erts_port[port_ix];
    port->id = make_internal_port(port_num);

    erts_smp_mtx_lock(&erts_driver_list_lock);
    if (!driver) {
	for (driver = driver_list; driver; driver = driver->next) {
	    if (sys_strcmp(driver->name, name) == 0)
		break;
	}
	if (!driver) { 
	    erts_smp_mtx_unlock(&erts_driver_list_lock);
	    if (error_number_ptr)
		*error_number_ptr = BADARG;
	    return -3;
	}
    }
    if (driver == &spawn_driver) {
	char *p;
	erts_driver_t *d;

	/*
	 * Dig out the name of the driver or port program.
	 */

	if (!(opts->spawn_type & ERTS_SPAWN_EXECUTABLE)) {
	    /* No spawn driver default */
	    driver = NULL;
	}


	if (opts->spawn_type != ERTS_SPAWN_EXECUTABLE) {
	    p = name;
	    while(*p != '\0' && *p != ' ')
		p++;
	    if (*p == '\0')
		p = NULL;
	    else
		*p = '\0';

	    /*
	     * Search for a driver having this name.  Defaults to spawn_driver
	     * if not found.
	     */
	    
	    for (d = driver_list; d; d = d->next) {
		if (strcmp(d->name, name) == 0 && 
		    erts_ddll_driver_ok(d->handle)) {
		    driver = d;
		    break;
		}
	    }
	    if (p != NULL)
		*p = ' ';
	}
    }

    if (driver == NULL || (driver != &spawn_driver && opts->exit_status)) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
	if (error_number_ptr) {
	    *error_number_ptr = BADARG;
	}
	/* Need to mark the port as free again */
	erts_smp_port_state_lock(port);
	port->status = ERTS_PORT_SFLG_FREE;
#ifdef ERTS_SMP
	ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&port->refc) == 2);
	erts_smp_atomic_set(&port->refc, 0); 
#endif	
	erts_smp_port_state_unlock(port);
	return -3;
    }

    /*
     * We'll set up the port before calling the start function,
     * to allow message sending and setting timers in the start function.
     */

#ifdef ERTS_SMP
    ASSERT(!port->lock);
    port->lock = driver->lock;
    if (!port->lock) {
	port->lock = erts_alloc(ERTS_ALC_T_PORT_LOCK,
					      sizeof(erts_smp_mtx_t));
	erts_smp_mtx_init_x(port->lock,
			    "port_lock",
			    port->id);
	xstatus |= ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK;
    }
#endif

    if (driver->handle != NULL) {
	erts_ddll_increment_port_count(driver->handle);
	erts_ddll_reference_driver(driver->handle);
    }
    erts_smp_mtx_unlock(&erts_driver_list_lock);

#ifdef ERTS_SMP
    erts_smp_mtx_lock(port->lock);
#endif

    setup_port(port, pid, driver, drv_data, name, xstatus);
    
    if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
	trace_port_open(port,
		pid,		
		am_atom_put(port->name, strlen(port->name)));
    }
    
    if (driver->start) {
	if (IS_TRACED_FL(port, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(port, am_in, am_start);
	}
	port->caller = pid;
	fpe_was_unmasked = erts_block_fpe();
	drv_data = (*driver->start)((ErlDrvPort)(port_ix),
				    name, opts);
	erts_unblock_fpe(fpe_was_unmasked);
	port->caller = NIL;
	erts_unblock_fpe(fpe_was_unmasked);
	if (IS_TRACED_FL(port, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(port, am_out, am_start);
	}
	if (error_number_ptr && ((long) drv_data) == (long) -2)
	    *error_number_ptr = errno;
#ifdef ERTS_SMP
	if (port->xports)
	    erts_smp_xports_unlock(port);
	ASSERT(!port->xports);
#endif
    }

    if (((long)drv_data) == -1 || 
	((long)drv_data) == -2 || 
	((long)drv_data) == -3) {
	int res = (int) ((long) drv_data);

	if (res == -3 && error_number_ptr) {
	    *error_number_ptr = BADARG;
	}

	/*
	 * Must clean up the port.
	 */
#ifdef ERTS_SMP
	erts_cancel_smp_ptimer(port->ptimer);
#else
	erl_cancel_timer(&(port->tm));
#endif
	stopq(port);
	kill_port(port);
	if (port->linebuf != NULL) {
	    erts_free(ERTS_ALC_T_LINEBUF,
		      (void *) port->linebuf);
	    port->linebuf = NULL;
	}
	if (driver->handle != NULL) {
	    erts_smp_mtx_lock(&erts_driver_list_lock);
	    erts_ddll_decrement_port_count(driver->handle);
	    erts_smp_mtx_unlock(&erts_driver_list_lock);
	}
	erts_port_release(port);
	return res;
    }
    port->drv_data = (long) drv_data;
    return port_ix;
}

#ifdef ERTS_SMP

struct ErtsXPortsList_ {
    ErtsXPortsList *next;
    Port *port;
};

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(xports_list, ErtsXPortsList, 50, ERTS_ALC_T_XPORTS_LIST)

#endif

/*
 * Driver function to create new instances of a driver
 * Historical reason: to be used with inet_drv for creating
 * accept sockets inorder to avoid a global table.
 */
ErlDrvPort
driver_create_port(ErlDrvPort creator_port_ix, /* Creating port */
		   ErlDrvTermData pid,    /* Owner/Caller */
		   char* name,            /* Driver name */
		   ErlDrvData drv_data)   /* Driver data */
{
    Port *creator_port;
    Port* port;
    erts_driver_t *driver;
    Process *rp;
    int port_num;
    Eterm port_id;
    Uint32 xstatus = 0;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    creator_port = erts_drvport2port(creator_port_ix);
    if (!creator_port)
	return (ErlDrvTermData) -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(creator_port));

    driver = creator_port->drv_ptr;
    erts_smp_mtx_lock(&erts_driver_list_lock);
    if (!erts_ddll_driver_ok(driver->handle)) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
	return (ErlDrvTermData) -1;
    }

    rp = erts_pid2proc(NULL, 0, pid, ERTS_PROC_LOCK_LINK);
    if (!rp) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
	return (ErlDrvTermData) -1;   /* pid does not exist */
    }
    if ((port_num = get_free_port()) < 0) {
	errno = ENFILE;
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	erts_smp_mtx_unlock(&erts_driver_list_lock);
	return (ErlDrvTermData) -1;
    }

    port_id = make_internal_port(port_num);
    port = &erts_port[port_num & erts_port_tab_index_mask];

#ifdef ERTS_SMP
    ASSERT(!port->lock);
    port->lock = driver->lock;
    if (!port->lock) {
	ErtsXPortsList *xplp = xports_list_alloc();
	xplp->port = port;
	xplp->next = creator_port->xports;
	creator_port->xports = xplp;
	port->lock = erts_alloc(ERTS_ALC_T_PORT_LOCK,
					      sizeof(erts_smp_mtx_t));
	erts_smp_mtx_init_locked_x(port->lock, "port_lock", port_id);
	xstatus |= ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK;
    }

#endif

    if (driver->handle != NULL) {
	erts_ddll_increment_port_count(driver->handle);
	erts_ddll_reference_referenced_driver(driver->handle);
    }
    erts_smp_mtx_unlock(&erts_driver_list_lock);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(port));

    setup_port(port, pid, driver, drv_data, name, xstatus);
    port->id = port_id;

    erts_add_link(&(port->nlinks), LINK_PID, pid);
    erts_add_link(&(rp->nlinks), LINK_PID, port_id);
    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    return port_num & erts_port_tab_index_mask;
}

#ifdef ERTS_SMP
void
erts_smp_xports_unlock(Port *prt)
{
    ErtsXPortsList *xplp;

    ASSERT(prt);
    xplp = prt->xports;
    ASSERT(xplp);
    while (xplp) {
	ErtsXPortsList *free_xplp;
	if (xplp->port->xports)
	    erts_smp_xports_unlock(xplp->port);
	erts_port_release(xplp->port);
	free_xplp = xplp;
	xplp = xplp->next;
	xports_list_free(free_xplp);
    }
    prt->xports = NULL;
}
#endif

/* Fills a possibly deep list of chars and binaries into vec
** Small characters are first stored in the buffer buf of length ln
** binaries found are copied and linked into msoh
** Return  vector length on succsess,
**        -1 on overflow
**        -2 on type error
*/

#define SET_VEC(iov, bv, bin, ptr, len, vlen) do {	\
   (iov)->iov_base = (ptr);				\
   (iov)->iov_len = (len);				\
   *(bv)++ = (bin);					\
   (iov)++;						\
   (vlen)++;						\
} while(0)

static int
io_list_to_vec(Eterm obj,	/* io-list */
	       SysIOVec* iov,	/* io vector */
	       ErlDrvBinary** binv, /* binary reference vector */
	       ErlDrvBinary* cbin, /* binary to store characters */
	       int bin_limit)	/* small binaries limit */
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    char *buf  = cbin->orig_bytes;
    int len    = cbin->orig_size;
    int csize  = 0;
    int vlen   = 0;
    char* cptr = buf;

    goto L_jump_start;  /* avoid push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		csize++;
		len--;
	    } else if (is_binary(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto handle_binary;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;    /* on head */
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }	    
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		goto handle_binary;
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    Eterm real_bin;
	    Uint offset;
	    Eterm* bptr;
	    int size;
	    int bitoffs;
	    int bitsize;

	handle_binary:
	    size = binary_size(obj);
	    ERTS_GET_REAL_BIN(obj, real_bin, offset, bitoffs, bitsize);
	    ASSERT(bitsize == 0);
	    bptr = binary_val(real_bin);
	    if (*bptr == HEADER_PROC_BIN) {
		ProcBin* pb = (ProcBin *) bptr;
		if (bitoffs != 0) {
		    if (len < size) {
			goto L_overflow;
		    }
		    erts_copy_bits(pb->bytes+offset, bitoffs, 1,
				   (byte *) buf, 0, 1, size*8);
		    csize += size;
		    buf += size;
		    len -= size;
		} else if (bin_limit && size < bin_limit) {
		    if (len < size) {
			goto L_overflow;
		    }
		    sys_memcpy(buf, pb->bytes+offset, size);
		    csize += size;
		    buf += size;
		    len -= size;
		} else {
		    if (csize != 0) {
			SET_VEC(iov, binv, cbin, cptr, csize, vlen);
			cptr = buf;
			csize = 0;
		    }
		    if (pb->flags) {
			erts_emasculate_writable_binary(pb);
		    }
		    SET_VEC(iov, binv, Binary2ErlDrvBinary(pb->val),
			    pb->bytes+offset, size, vlen);
		}
	    } else {
		ErlHeapBin* hb = (ErlHeapBin *) bptr;
		if (len < size) {
		    goto L_overflow;
		}
		copy_binary_to_buffer(buf, 0,
				      ((byte *) hb->data)+offset, bitoffs,
				      8*size);
		csize += size;
		buf += size;
		len -= size;
	    }
	} else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    if (csize != 0) {
	SET_VEC(iov, binv, cbin, cptr, csize, vlen);
    }

    DESTROY_ESTACK(s);
    return vlen;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

#define IO_LIST_VEC_COUNT(obj)						\
do {									\
    int _size = binary_size(obj);					\
    Eterm _real;							\
    Uint _offset;							\
    int _bitoffs;							\
    int _bitsize;							\
    ERTS_GET_REAL_BIN(obj, _real, _offset, _bitoffs, _bitsize);		\
    ASSERT(_bitsize == 0);						\
    if (thing_subtag(*binary_val(_real)) == REFC_BINARY_SUBTAG &&	\
	_bitoffs == 0) {						\
	b_size += _size;						\
	in_clist = 0;							\
	v_size++;							\
        if (_size >= bin_limit) {					\
            p_in_clist = 0;						\
            p_v_size++;							\
        } else {							\
            p_c_size += _size;						\
            if (!p_in_clist) {						\
                p_in_clist = 1;						\
                p_v_size++;						\
            }								\
        }								\
    } else {								\
	c_size += _size;						\
	if (!in_clist) {						\
	    in_clist = 1;						\
	    v_size++;							\
	}								\
	p_c_size += _size;						\
	if (!p_in_clist) {						\
	    p_in_clist = 1;						\
	    p_v_size++;							\
	}								\
    }									\
} while (0)


/* 
** Size of a io list in bytes
** return -1 if error
** returns:            - Total size of io list
**           vsize     - SysIOVec size needed for a writev
**           csize     - Number of bytes not in binary (in the common binary)
**           pvsize    - SysIOVec size needed if packing small binaries
**           pcsize    - Number of bytes in the common binary if packing
*/

static int 
io_list_vec_len(Eterm obj, int* vsize, int* csize, 
		int bin_limit, /* small binaries limit */
		int * pvsize, int * pcsize)
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    int v_size = 0;
    int c_size = 0;
    int b_size = 0;
    int in_clist = 0;
    int p_v_size = 0;
    int p_c_size = 0;
    int p_in_clist = 0;

    goto L_jump_start;  /* avoid a push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);

	    if (is_byte(obj)) {
		c_size++;
		if (!in_clist) {
		    in_clist = 1;
		    v_size++;
		}
		p_c_size++;
		if (!p_in_clist) {
		    p_in_clist = 1;
		    p_v_size++;
		}
	    }
	    else if (is_binary(obj)) {
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;   /* on head */
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list;   /* on tail */
	    else if (is_binary(obj)) {  /* binary tail is OK */
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	}
	else if (is_binary(obj)) {
	    IO_LIST_VEC_COUNT(obj);
	}
	else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    if (vsize != NULL)
	*vsize = v_size;
    if (csize != NULL)
	*csize = c_size;
    if (pvsize != NULL)
	*pvsize = p_v_size;
    if (pcsize != NULL)
	*pcsize = p_c_size;
    return c_size + b_size;

 L_type_error:
    DESTROY_ESTACK(s);
    return -1;
}

#define ERL_SMALL_IO_BIN_LIMIT (4*ERL_ONHEAP_BIN_LIMIT)
#define SMALL_WRITE_VEC  16


/* write data to a port */
int erts_write_to_port(Eterm caller_id, Port *p, Eterm list)
{
    char *buf;
    erts_driver_t *drv = p->drv_ptr;
    int size;
    int fpe_was_unmasked;
    
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    p->caller = caller_id;
    if (drv->outputv != NULL) {
	int vsize;
	int csize;
	int pvsize;
	int pcsize;
	int blimit;
	SysIOVec iv[SMALL_WRITE_VEC];
	ErlDrvBinary* bv[SMALL_WRITE_VEC];
	SysIOVec* ivp;
	ErlDrvBinary**  bvp;
	ErlDrvBinary* cbin;
	ErlIOVec ev;

	if ((size = io_list_vec_len(list, &vsize, &csize, 
				    ERL_SMALL_IO_BIN_LIMIT,
				    &pvsize, &pcsize)) < 0) {
	    goto bad_value;
	}
	/* To pack or not to pack (small binaries) ...? */
	vsize++;
	if (vsize <= SMALL_WRITE_VEC) {
	    /* Do NOT pack */
	    blimit = 0;
	} else {
	    /* Do pack */
	    vsize = pvsize + 1;
	    csize = pcsize;
	    blimit = ERL_SMALL_IO_BIN_LIMIT;
	}
	/* Use vsize and csize from now on */
	if (vsize <= SMALL_WRITE_VEC) {
	    ivp = iv;
	    bvp = bv;
	} else {
	    ivp = (SysIOVec *) erts_alloc(ERTS_ALC_T_TMP,
					  vsize * sizeof(SysIOVec));
	    bvp = (ErlDrvBinary**) erts_alloc(ERTS_ALC_T_TMP,
					      vsize * sizeof(ErlDrvBinary*));
	}
	cbin = driver_alloc_binary(csize);
	if (!cbin)
	    erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, ERTS_SIZEOF_Binary(csize));

	/* Element 0 is for driver usage to add header block */
	ivp[0].iov_base = NULL;
	ivp[0].iov_len = 0;
	bvp[0] = NULL;
	ev.vsize = io_list_to_vec(list, ivp+1, bvp+1, cbin, blimit);
	ev.vsize++;
#if 0
	/* This assertion may say something useful, but it can
	   be falsified during the emulator test suites. */
	ASSERT((ev.vsize >= 0) && (ev.vsize == vsize));
#endif
	ev.size = size;  /* total size */
	ev.iov = ivp;
	ev.binv = bvp;
	fpe_was_unmasked = erts_block_fpe();
	(*drv->outputv)((ErlDrvData)p->drv_data, &ev);
	erts_unblock_fpe(fpe_was_unmasked);
	if (ivp != iv) {
	    erts_free(ERTS_ALC_T_TMP, (void *) ivp);
	}
	if (bvp != bv) {
	    erts_free(ERTS_ALC_T_TMP, (void *) bvp);
	}
	driver_free_binary(cbin);
    } else {
	int r;
	
	/* Try with an 8KB buffer first (will often be enough I guess). */
	size = 8*1024;
	/* See below why the extra byte is added. */
	buf = erts_alloc(ERTS_ALC_T_TMP, size+1);
	r = io_list_to_buf(list, buf, size);

	if (r >= 0) {
	    size -= r;
	    fpe_was_unmasked = erts_block_fpe();
	    (*drv->output)((ErlDrvData)p->drv_data, buf, size);
	    erts_unblock_fpe(fpe_was_unmasked);
	    erts_free(ERTS_ALC_T_TMP, buf);
	}
	else if (r == -2) {
	    erts_free(ERTS_ALC_T_TMP, buf);
	    goto bad_value;
	}
	else {
	    ASSERT(r == -1); /* Overflow */
	    erts_free(ERTS_ALC_T_TMP, buf);
	    if ((size = io_list_len(list)) < 0) {
		goto bad_value;
	    }

	    /*
	     * I know drivers that pad space with '\0' this is clearly
	     * incorrect but I don't feel like fixing them now, insted
	     * add ONE extra byte.
	     */
	    buf = erts_alloc(ERTS_ALC_T_TMP, size+1); 
	    r = io_list_to_buf(list, buf, size);
	    fpe_was_unmasked = erts_block_fpe();
	    (*drv->output)((ErlDrvData)p->drv_data, buf, size);
	    erts_unblock_fpe(fpe_was_unmasked);
	    erts_free(ERTS_ALC_T_TMP, buf);
	}
    }
    p->bytes_out += size;
    erts_smp_atomic_add(&erts_bytes_out, size);

#ifdef ERTS_SMP
    if (p->xports)
	erts_smp_xports_unlock(p);
    ASSERT(!p->xports);
#endif
    p->caller = NIL;
    return 0;

 bad_value: 
    p->caller = NIL;
    {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Bad value on output port '%s'\n", p->name);
	erts_send_error_to_logger_nogl(dsbufp);
	return 1;
    }
}

/* initialize the port array */
void init_io(void)
{
    int i;
    ErlDrvEntry** dp;
    ErlDrvEntry* drv;
    char maxports[21]; /* enough for any 64-bit integer */
    size_t maxportssize = sizeof(maxports);
    Uint ports_bits = ERTS_PORTS_BITS;
    Sint port_extra_shift;

#ifdef ERTS_SMP
    init_xports_list_alloc();
#endif

    pdl_init();

    if (erts_sys_getenv("ERL_MAX_PORTS", maxports, &maxportssize) == 0) 
	erts_max_ports = atoi(maxports);
    else
	erts_max_ports = sys_max_files();

    if (erts_max_ports > ERTS_MAX_PORTS)
	erts_max_ports = ERTS_MAX_PORTS;
    if (erts_max_ports < 1024)
	erts_max_ports = 1024;

    if (erts_use_r9_pids_ports) {
	ports_bits = ERTS_R9_PORTS_BITS;
	if (erts_max_ports > ERTS_MAX_R9_PORTS)
	    erts_max_ports = ERTS_MAX_R9_PORTS;
    }

    port_extra_shift = erts_fit_in_bits(erts_max_ports - 1);
    port_num_mask = (1 << ports_bits) - 1;

    erts_port_tab_index_mask = ~(~((Uint) 0) << port_extra_shift);
    erts_max_ports = 1 << port_extra_shift;

    erts_smp_mtx_init(&erts_driver_list_lock,"driver_list");
    driver_list = NULL;
    erts_smp_tsd_key_create(&driver_list_lock_status_key);
    erts_smp_tsd_key_create(&driver_list_last_error_key);

    if (erts_max_ports * sizeof(Port) <= erts_max_ports) {
	/* More memory needed than the whole address space. */
	erts_alloc_enomem(ERTS_ALC_T_PORT_TABLE, ~((Uint) 0));
    }

    erts_port = (Port *) erts_alloc(ERTS_ALC_T_PORT_TABLE,
				    erts_max_ports * sizeof(Port));

    erts_smp_atomic_init(&erts_bytes_out, 0);
    erts_smp_atomic_init(&erts_bytes_in, 0);
    erts_smp_atomic_init(&erts_ports_alive, 0);

    for (i = 0; i < erts_max_ports; i++) {
	erts_port_task_init_sched(&erts_port[i].sched);
#ifdef ERTS_SMP
	erts_smp_atomic_init(&erts_port[i].refc, 0);
	erts_port[i].lock = NULL;
	erts_port[i].xports = NULL;
	erts_smp_spinlock_init_x(&erts_port[i].state_lck, "port_state", make_small(i));
#endif
	erts_port[i].tracer_proc = NIL;
	erts_port[i].trace_flags = 0;

	erts_port[i].drv_ptr = NULL;
	erts_port[i].status = ERTS_PORT_SFLG_FREE;
	erts_port[i].name = NULL;
	erts_port[i].nlinks = NULL;
	erts_port[i].monitors = NULL;
	erts_port[i].linebuf = NULL;
	erts_port[i].port_data_lock = NULL;
    }

    erts_smp_atomic_init(&erts_ports_snapshot, (long) 0);
    last_port_num = 0;
    erts_smp_spinlock_init(&get_free_port_lck, "get_free_port");

    sys_init_io();

    erts_smp_tsd_set(driver_list_lock_status_key, (void *) 1);
    erts_smp_mtx_lock(&erts_driver_list_lock);

    init_driver(&fd_driver, &fd_driver_entry, NULL);
    init_driver(&vanilla_driver, &vanilla_driver_entry, NULL);
    init_driver(&spawn_driver, &spawn_driver_entry, NULL);
    for (dp = driver_tab; *dp != NULL; dp++) {
	drv = *dp;
	erts_add_driver_entry(*dp, NULL, 1);
    }

    erts_smp_tsd_set(driver_list_lock_status_key, NULL);
    erts_smp_mtx_unlock(&erts_driver_list_lock);
}

/*
 * Buffering of data when using line oriented I/O on ports
 */

/* 
 * Buffer states 
 */
#define LINEBUF_MAIN 0
#define LINEBUF_FULL 1
#define LINEBUF_CR_INSIDE 2
#define LINEBUF_CR_AFTER 3

/*
 * Creates a LineBuf to be added to the port structure,
 * Returns: Pointer to a newly allocated and initialized LineBuf.
 * Parameters:
 * bufsiz - The (maximum) size of the line buffer.
 */
LineBuf *allocate_linebuf(bufsiz)
int bufsiz;
{
    int ovsiz = (bufsiz < LINEBUF_INITIAL) ? bufsiz : LINEBUF_INITIAL;
    LineBuf *lb = (LineBuf *) erts_alloc(ERTS_ALC_T_LINEBUF,
					 sizeof(LineBuf)+ovsiz);
    lb->ovsiz = ovsiz;
    lb->bufsiz = bufsiz;
    lb->ovlen = 0;
    lb->data[0] = LINEBUF_MAIN; /* state */
    return lb;
}

/*
 * Initializes a LineBufContext to be used in calls to read_linebuf
 * or flush_linebuf.
 * Returns: 0 if ok, <0 on error.
 * Parameters:
 * lc - Pointer to an allocated LineBufContext.
 * lb - Pointer to a LineBuf structure (probably from the Port structure).
 * buf - A buffer containing the data to be read and split to lines.
 * len - The number of bytes in buf.
 */
static int init_linebuf_context(LineBufContext *lc, LineBuf **lb, char *buf, int len)
{
    if(lc == NULL || lb == NULL)
	return -1;
    lc->b = lb;
    lc->buf = buf;
    lc->left = len;
    return 0;
}

static void resize_linebuf(LineBuf **b)
{
    int newsiz = (((*b)->ovsiz * 2) > (*b)->bufsiz) ? (*b)->bufsiz : 
	(*b)->ovsiz * 2;
    *b = (LineBuf *) erts_realloc(ERTS_ALC_T_LINEBUF,
				  (void *) *b,
				  sizeof(LineBuf)+newsiz);
    (*b)->ovsiz = newsiz;
}

/*
 * Delivers all data in the buffer regardless of newlines (always
 * an LINEBUF_NOEOL. Has to be called until it return LINEBUF_EMPTY.
 * Return values and barameters as read_linebuf (see below).
 */
static int flush_linebuf(LineBufContext *bp)
{
    bp->retlen = (*bp->b)->ovlen;
    switch(LINEBUF_STATE(*bp)){
    case LINEBUF_CR_INSIDE:
	if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
	    resize_linebuf(bp->b);
	LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	++bp->retlen; /* fall through instead of switching state... */
    case LINEBUF_MAIN:
    case LINEBUF_FULL:
	(*bp->b)->ovlen = 0;
	LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    case LINEBUF_CR_AFTER:
	LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	(*bp->b)->ovlen = 0;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    default:
	return LINEBUF_ERROR;
    }    
}

/*
 * Reads input from a buffer and "chops" it up in lines.
 * Has to be called repeatedly until it returns LINEBUF_EMPTY
 * to get all lines in buffer.
 * Handles both <LF> and <CR><LF> style newlines.
 * On Unix, this is slightly incorrect, as <CR><LF> is NOT to be regarded
 * as a newline together, but i treat newlines equally in all systems
 * to avoid putting this in sys.c or clutter it with #ifdef's.
 * Returns: LINEBUF_EMPTY if there is no more data that can be
 *   determined as a line (only part of a line left), LINEBUF_EOL if a whole
 *   line could be delivered and LINEBUF_NOEOL if the buffer size has been
 *   exceeded. The data and the data length can be accesed through the 
 *   LINEBUF_DATA and the LINEBUF_DATALEN macros applied to the LineBufContext.
 * Parameters: 
 * bp - A LineBufContext that is initialized with 
 *   the init_linebuf_context call. The context has to be retained during
 *   all calls that returns other than LINEBUF_EMPTY. When LINEBUF_EMPTY
 *   is returned the context can be discarded and a new can be created when new
 *   data arrives (the state is saved in the Port structure).
 */
static int read_linebuf(LineBufContext *bp)
{
    for(;;){
	if(bp->left == 0)
	    return LINEBUF_EMPTY;
	if(*bp->buf == '\n'){
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    ++(bp->buf);
	    --(bp->left);
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    return LINEBUF_EOL;
	}
	switch(LINEBUF_STATE(*bp)){
	case LINEBUF_MAIN:
	    if((*bp->b)->ovlen == (*bp->b)->bufsiz)
		LINEBUF_STATE(*bp) = LINEBUF_FULL;
	    else if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    } else {
		if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		    resize_linebuf(bp->b);
		LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = *((bp->buf)++);
		--(bp->left);
	    }
	    continue;
	case LINEBUF_FULL:
	    if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_AFTER;
	    } else {
		bp->retlen = (*bp->b)->ovlen;
		(*bp->b)->ovlen = 0;
		LINEBUF_STATE(*bp) = LINEBUF_MAIN;
		return LINEBUF_NOEOL;
	    }
	    continue;
	case LINEBUF_CR_INSIDE:
	    if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		resize_linebuf(bp->b);
	    LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    continue;
	case LINEBUF_CR_AFTER:
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    return LINEBUF_NOEOL;
	default:
	    return LINEBUF_ERROR;
	}
    }
}

static void
deliver_result(Eterm sender, Eterm pid, Eterm res)
{
    Process *rp;
    ErtsProcLocks rp_locks = 0;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ASSERT(is_internal_port(sender)
	   && is_internal_pid(pid)
	   && internal_pid_index(pid) < erts_max_processes);

    rp = erts_pid2proc_opt(NULL, 0, pid, 0, ERTS_P2P_FLG_SMP_INC_REFC);

    if (rp) {
	Eterm tuple;
	ErlHeapFragment *bp;
	ErlOffHeap *ohp;
	Eterm* hp;
	Uint sz_res;
	    sz_res = size_object(res);
	    hp = erts_alloc_message_heap(sz_res + 3, &bp, &ohp, rp, &rp_locks);
	    res = copy_struct(res, sz_res, &hp, ohp);
	    tuple = TUPLE2(hp, sender, res);
	erts_queue_message(rp, &rp_locks, bp, tuple, NIL);
	erts_smp_proc_unlock(rp, rp_locks);
	erts_smp_proc_dec_refc(rp);
    }
}


/* 
 * Deliver a "read" message.
 * hbuf -- byte that are always formated as a list
 * hlen -- number of byte in header
 * buf  -- data 
 * len  -- length of data
 */

static void deliver_read_message(Port* prt, Eterm to,
				 char *hbuf, int hlen,
				 char *buf, int len, int eol)
{
    int need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;
    ErlHeapFragment *bp;
    ErlOffHeap *ohp;
    ErtsProcLocks rp_locks = 0;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));    
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    need = 3 + 3 + 2*hlen;
    if (prt->status & ERTS_PORT_SFLG_LINEBUF_IO) {
	need += 3;
    }
    if (prt->status & ERTS_PORT_SFLG_BINARY_IO && buf != NULL) {
	need += PROC_BIN_SIZE;
    } else {
	need += 2*len;
    }

    rp = erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_SMP_INC_REFC);
    if (!rp)
	return;

    hp = erts_alloc_message_heap(need, &bp, &ohp, rp, &rp_locks);

    listp = NIL;
    if ((prt->status & ERTS_PORT_SFLG_BINARY_IO) == 0) {
	listp = buf_to_intlist(&hp, buf, len, listp);
    } else if (buf != NULL) {
	ProcBin* pb;
	Binary* bptr;

	bptr = erts_bin_nrml_alloc(len);
	bptr->flags = 0;
	bptr->orig_size = len;
	erts_refc_init(&bptr->refc, 1);
	sys_memcpy(bptr->orig_bytes, buf, len);

	pb = (ProcBin *) hp;
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = len;
	pb->next = ohp->first;
	ohp->first = (struct erl_off_heap_header*)pb;
	pb->val = bptr;
	pb->bytes = (byte*) bptr->orig_bytes;
	pb->flags = 0;
	hp += PROC_BIN_SIZE;

	OH_OVERHEAD(ohp, pb->size / sizeof(Eterm));
	listp = make_binary(pb);
    }

    /* Prepend the header */
    if (hlen > 0) {
	listp = buf_to_intlist(&hp, hbuf, hlen, listp);
    }

    if (prt->status & ERTS_PORT_SFLG_LINEBUF_IO){
	listp = TUPLE2(hp, (eol) ? am_eol : am_noeol, listp); 
	hp += 3;
    }
    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;

    tuple = TUPLE2(hp, prt->id, tuple);
    hp += 3;

    erts_queue_message(rp, &rp_locks, bp, tuple, am_undefined);
    erts_smp_proc_unlock(rp, rp_locks);
    erts_smp_proc_dec_refc(rp);
}

/* 
 * Deliver all lines in a line buffer, repeats calls to
 * deliver_read_message, and takes the same parameters.
 */
static void deliver_linebuf_message(Port* prt, Eterm to, 
				    char* hbuf, int hlen,
				    char *buf, int len)
{
    LineBufContext lc;
    int ret;
    if(init_linebuf_context(&lc,&(prt->linebuf), buf, len) < 0)
	return;
    while((ret = read_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt, to, hbuf, hlen, LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc), (ret == LINEBUF_EOL));
}

/*
 * Deliver any nonterminated lines in the line buffer before the
 * port gets closed.
 * Has to be called before terminate_port.
 * Parameters:
 * prt -  Pointer to a Port structure for this port.
 */
static void flush_linebuf_messages(Port *prt)
{
    LineBufContext lc;
    int ret;

    ERTS_SMP_LC_ASSERT(!prt || erts_lc_is_port_locked(prt));
    if(prt == NULL || !(prt->status & ERTS_PORT_SFLG_LINEBUF_IO))
	return;

    if(init_linebuf_context(&lc,&(prt->linebuf), NULL, 0) < 0)
	return;
    while((ret = flush_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt,
			     prt->connected,
			     NULL,
			     0,
			     LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc),
			     (ret == LINEBUF_EOL));
}    

static void
deliver_vec_message(Port* prt,			/* Port */
		    Eterm to,			/* Receiving pid */
		    char* hbuf,			/* "Header" buffer... */
		    int hlen,			/* ... and its length */
		    ErlDrvBinary** binv,	/* Vector of binaries */
		    SysIOVec* iov,		/* I/O vector */
		    int vsize,			/* Size of binv & iov */
		    int csize)			/* Size of characters in 
						   iov (not hlen) */
{
    int need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;
    ErlHeapFragment *bp;
    ErlOffHeap *ohp;
    ErtsProcLocks rp_locks = 0;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    /*
     * Check arguments for validity.
     */

    rp = erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_SMP_INC_REFC);
    if (!rp)
	return;

    /*
     * Calculate the exact number of heap words needed.
     */

    need = 3 + 3;		/* Heap space for two tuples */
    if (prt->status & ERTS_PORT_SFLG_BINARY_IO) {
	need += (2+PROC_BIN_SIZE)*vsize - 2 + hlen*2;
    } else {
	need += (hlen+csize)*2;
    }

    hp = erts_alloc_message_heap(need, &bp, &ohp, rp, &rp_locks);

    listp = NIL;
    iov += vsize;

    if ((prt->status & ERTS_PORT_SFLG_BINARY_IO) == 0) {
	Eterm* thp = hp;
	while (vsize--) {
	    iov--;
	    listp = buf_to_intlist(&thp, iov->iov_base, iov->iov_len, listp);
	}
	hp = thp;
    } else {
	binv += vsize;
	while (vsize--) {
	    ErlDrvBinary* b;
	    ProcBin* pb = (ProcBin*) hp;
	    byte* base;

	    iov--;
	    binv--;
	    if ((b = *binv) == NULL) {
		b = driver_alloc_binary(iov->iov_len);
		sys_memcpy(b->orig_bytes, iov->iov_base, iov->iov_len);
		base = (byte*) b->orig_bytes;
	    } else {
		/* Must increment reference count, caller calls free */
		driver_binary_inc_refc(b);
		base = iov->iov_base;
	    }
	    pb->thing_word = HEADER_PROC_BIN;
	    pb->size = iov->iov_len;
	    pb->next = ohp->first;
	    ohp->first = (struct erl_off_heap_header*)pb;
	    pb->val = ErlDrvBinary2Binary(b);
	    pb->bytes = base;
	    pb->flags = 0;
	    hp += PROC_BIN_SIZE;
	    
	    OH_OVERHEAD(ohp, iov->iov_len / sizeof(Eterm));

	    if (listp == NIL) {  /* compatible with deliver_bin_message */
		listp = make_binary(pb);
	    } else {
		listp = CONS(hp, make_binary(pb), listp);
		hp += 2;
	    }
	}
    }

    if (hlen > 0) {		/* Prepend the header */
	Eterm* thp = hp;
	listp = buf_to_intlist(&thp, hbuf, hlen, listp);
	hp = thp;
    }

    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;
    tuple = TUPLE2(hp, prt->id, tuple);
    hp += 3;

    erts_queue_message(rp, &rp_locks, bp, tuple, am_undefined);
    erts_smp_proc_unlock(rp, rp_locks);
    erts_smp_proc_dec_refc(rp);
}


static void deliver_bin_message(Port*  prt,         /* port */
				Eterm to,           /* receiving pid */
				char* hbuf,         /* "header" buffer */
				int hlen,           /* and it's length */
				ErlDrvBinary* bin,  /* binary data */
				int offs,           /* offset into binary */
				int len)            /* length of binary */
{
    SysIOVec vec;

    vec.iov_base = bin->orig_bytes+offs;
    vec.iov_len = len;
    deliver_vec_message(prt, to, hbuf, hlen, &bin, &vec, 1, len);
}

/* flush the port I/O queue and terminate if empty */
/* 
 * Note. 
 *
 * The test for (p->status & ERTS_PORT_SFLGS_DEAD) == 0 is important since the
 * driver's  flush function might call driver_async, which when using no
 * threads and being short circuited will notice that the io queue is empty
 * (after calling the driver's async_ready) and recursively call 
 * terminate_port. So when we get back here, the port is already terminated.
 */
static void flush_port(Port *p)
{
    int fpe_was_unmasked;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

    if (p->drv_ptr->flush != NULL) {
        if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(p, am_in, am_flush);
	}
	fpe_was_unmasked = erts_block_fpe();
	(*p->drv_ptr->flush)((ErlDrvData)p->drv_data);
	erts_unblock_fpe(fpe_was_unmasked);
        if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(p, am_out, am_flush);
	}
#ifdef ERTS_SMP
	if (p->xports)
	    erts_smp_xports_unlock(p);
	ASSERT(!p->xports);
#endif
    }
    if ((p->status & ERTS_PORT_SFLGS_DEAD) == 0 && is_port_ioq_empty(p)) {
	terminate_port(p);
    }
}

/* stop and delete a port that is ERTS_PORT_SFLG_CLOSING */
static void
terminate_port(Port *prt)
{
    Eterm send_closed_port_id;
    Eterm connected_id = NIL /* Initialize to silence compiler */;
    erts_driver_t *drv;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    ASSERT(!prt->nlinks);
    ASSERT(!prt->monitors);

    if (prt->status & ERTS_PORT_SFLG_SEND_CLOSED) {
	erts_port_status_band_set(prt, ~ERTS_PORT_SFLG_SEND_CLOSED);
	send_closed_port_id = prt->id;
	connected_id = prt->connected;
    }
    else {
	send_closed_port_id = NIL;
    }

#ifdef ERTS_SMP
    erts_cancel_smp_ptimer(prt->ptimer);
#else
    erl_cancel_timer(&prt->tm);
#endif

    drv = prt->drv_ptr;
    if ((drv != NULL) && (drv->stop != NULL)) {
	int fpe_was_unmasked = erts_block_fpe();
	(*drv->stop)((ErlDrvData)prt->drv_data);
	erts_unblock_fpe(fpe_was_unmasked);
#ifdef ERTS_SMP
	if (prt->xports)
	    erts_smp_xports_unlock(prt);
	ASSERT(!prt->xports);
#endif
    }
    if(drv->handle != NULL) {
	erts_smp_mtx_lock(&erts_driver_list_lock);
	erts_ddll_decrement_port_count(drv->handle); 
	erts_smp_mtx_unlock(&erts_driver_list_lock);
    }
    stopq(prt);        /* clear queue memory */
    if(prt->linebuf != NULL){
	erts_free(ERTS_ALC_T_LINEBUF, (void *) prt->linebuf);
	prt->linebuf = NULL;
    }
    if (prt->bp != NULL) {
	free_message_buffer(prt->bp);
	prt->bp = NULL;
	prt->data = am_undefined;
    }

    if (prt->psd)
	erts_free(ERTS_ALC_T_PRTSD, prt->psd);

    kill_port(prt);

    /*
     * We don't want to send the closed message until after the
     * port has been removed from the port table (in kill_port()).
     */
    if (is_internal_port(send_closed_port_id))
	deliver_result(send_closed_port_id, connected_id, am_closed);

    ASSERT(prt->dist_entry == NULL);
}

void
erts_terminate_port(Port *pp)
{
    terminate_port(pp);
}

static void sweep_one_monitor(ErtsMonitor *mon, void *vpsc)
{
    ErtsMonitor *rmon;
    Process *rp;

    ASSERT(mon->type == MON_ORIGIN);
    ASSERT(is_internal_pid(mon->pid));
    rp = erts_pid2proc(NULL, 0, mon->pid, ERTS_PROC_LOCK_LINK);
    if (!rp) {
	goto done;
    }
    rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    if (rmon == NULL) {
	goto done;
    }
    erts_destroy_monitor(rmon);
 done:
    erts_destroy_monitor(mon);
}



typedef struct {
    Eterm port;
    Eterm reason;
} SweepContext;

static void sweep_one_link(ErtsLink *lnk, void *vpsc)
{
    SweepContext *psc = vpsc;
    DistEntry *dep;
    Process *rp;
    

    ASSERT(lnk->type == LINK_PID);
    
    if (is_external_pid(lnk->pid)) {
	dep = external_pid_dist_entry(lnk->pid);
	if(dep != erts_this_dist_entry) {
	    ErtsDistLinkData dld;
	    ErtsDSigData dsd;
	    int code;
	    code = erts_dsig_prepare(&dsd, dep, NULL, ERTS_DSP_NO_LOCK, 0);
	    switch (code) {
	    case ERTS_DSIG_PREP_NOT_ALIVE:
	    case ERTS_DSIG_PREP_NOT_CONNECTED:
		break;
	    case ERTS_DSIG_PREP_CONNECTED:
		erts_remove_dist_link(&dld, psc->port, lnk->pid, dep);
		erts_destroy_dist_link(&dld);
		code = erts_dsig_send_exit(&dsd, psc->port, lnk->pid,
					   psc->reason);
		ASSERT(code == ERTS_DSIG_SEND_OK);
		break;
	    default:
		ASSERT(! "Invalid dsig prepare result");
		break;
	    }
	}
    } else {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
	ASSERT(is_internal_pid(lnk->pid));
	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (rp) {
	    ErtsLink *rlnk = erts_remove_link(&(rp->nlinks), psc->port);

	    if (rlnk) {
		int xres = erts_send_exit_signal(NULL,
						 psc->port,
						 rp,
						 &rp_locks, 
						 psc->reason,
						 NIL,
						 NULL,
						 0);
		if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
		    /* We didn't exit the process and it is traced */
		    if (IS_TRACED_FL(rp, F_TRACE_PROCS)) {
			trace_proc(NULL, rp, am_getting_unlinked,
				   psc->port);
		    }
		}
		erts_destroy_link(rlnk);
	    }

	    erts_smp_proc_unlock(rp, rp_locks);
	}
    }
    erts_destroy_link(lnk);
}

/* 'from' is sending 'this_port' an exit signal, (this_port must be internal).
 * If reason is normal we don't do anything, *unless* from is our connected
 * process in which case we close the port. Any other reason kills the port.
 * If 'from' is ourself we always die.
 * When a driver has data in ioq then driver will be set to closing
 * and become inaccessible to the processes. One exception exists and
 * that is to kill a port till reason kill. Then the port is stopped.
 * 
 */
void
erts_do_exit_port(Port *p, Eterm from, Eterm reason)
{
   ErtsLink *lnk;
   Eterm rreason;

   ERTS_SMP_CHK_NO_PROC_LOCKS;
   ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

   rreason = (reason == am_kill) ? am_killed : reason;

   if ((p->status & (ERTS_PORT_SFLGS_DEAD
		     | ERTS_PORT_SFLG_EXITING
		     | ERTS_PORT_SFLG_IMMORTAL))
       || ((reason == am_normal) &&
	   ((from != p->connected) && (from != p->id)))) {
      return;
   }

   if (IS_TRACED_FL(p, F_TRACE_PORTS)) {
   	trace_port(p, am_closed, reason);
   }

   erts_trace_check_exiting(p->id);

   /*
    * Setting the port to not busy here, frees the list of pending
    * processes and makes them runnable.
    */
   set_busy_port((ErlDrvPort)internal_port_index(p->id), 0);

   if (p->reg != NULL)
       (void) erts_unregister_name(NULL, 0, p, p->reg->name);

   erts_port_status_bor_set(p, ERTS_PORT_SFLG_EXITING);

   {
       SweepContext sc = {p->id, rreason};
       lnk = p->nlinks;
       p->nlinks = NULL;
       erts_sweep_links(lnk, &sweep_one_link, &sc);
   }
   DRV_MONITOR_LOCK_PDL(p);
   {
       ErtsMonitor *moni = p->monitors;
       p->monitors = NULL;
       erts_sweep_monitors(moni, &sweep_one_monitor, NULL);
   } 
   DRV_MONITOR_UNLOCK_PDL(p);

   if ((p->status & ERTS_PORT_SFLG_DISTRIBUTION) && p->dist_entry) {
       erts_do_net_exits(p->dist_entry, rreason);
       erts_deref_dist_entry(p->dist_entry); 
       p->dist_entry = NULL; 
       erts_port_status_band_set(p, ~ERTS_PORT_SFLG_DISTRIBUTION);
   }
       
   if ((reason != am_kill) && !is_port_ioq_empty(p)) {
       erts_port_status_bandor_set(p,
				   ~ERTS_PORT_SFLG_EXITING, /* must turn it off */
				   ERTS_PORT_SFLG_CLOSING);
      flush_port(p);
   }
   else {
       terminate_port(p);
   }
}

/* About the states ERTS_PORT_SFLG_EXITING and ERTS_PORT_SFLG_CLOSING used above.
**
** ERTS_PORT_SFLG_EXITING is a recursion protection for erts_do_exit_port().
** It is unclear  whether this state is necessary or not, it might be possible
** to merge it with ERTS_PORT_SFLG_CLOSING. ERTS_PORT_SFLG_EXITING only persists
** over a section of sequential (but highly recursive) code.
**
** ERTS_PORT_SFLG_CLOSING is a state where the port is in Limbo, waiting to
** pass on. All links are removed, and the port receives in/out-put events so
** as soon as the port queue gets empty terminate_port() is called.
*/



/* Command should be of the form
**   {PID, close}
**   {PID, {command, io-list}}
**   {PID, {connect, New_PID}}
**
**
*/
void erts_port_command(Process *proc,
		       Eterm caller_id,
		       Port *port,
		       Eterm command)
{
    Eterm *tp;
    Eterm pid;

    if (!port)
	return;

    erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ASSERT(!INVALID_PORT(port, port->id));

    if (is_tuple_arity(command, 2)) {
	tp = tuple_val(command);
	if ((pid = port->connected) == tp[1]) {
	    /* PID must be connected */
	    if (tp[2] == am_close) {
		erts_port_status_bor_set(port, ERTS_PORT_SFLG_SEND_CLOSED);
		erts_do_exit_port(port, pid, am_normal);
		goto done;
	    } else if (is_tuple_arity(tp[2], 2)) {
		tp = tuple_val(tp[2]);
		if (tp[1] == am_command) {
		    if (erts_write_to_port(caller_id, port, tp[2]) == 0)
			goto done;
		} else if ((tp[1] == am_connect) && is_internal_pid(tp[2])) {
		    port->connected = tp[2];
		    deliver_result(port->id, pid, am_connected);
		    goto done;
		}
	    }
	}
    }

    {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	Process* rp = erts_pid2proc_opt(NULL, 0,
					port->connected, rp_locks,
					ERTS_P2P_FLG_SMP_INC_REFC);
	if (rp) {
	    (void) erts_send_exit_signal(NULL,
					 port->id,
					 rp,
					 &rp_locks, 
					 am_badsig,
					 NIL,
					 NULL,
					 0);
	    erts_smp_proc_unlock(rp, rp_locks);
	    erts_smp_proc_dec_refc(rp);
	}

    }
 done:
    erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);
}

/*
 * Control a port synchronously. 
 * Returns either a list or a binary.
 */
Eterm
erts_port_control(Process* p, Port* prt, Uint command, Eterm iolist)
{
    byte* to_port = NULL;	/* Buffer to write to port. */
				/* Initialization is for shutting up
				   warning about use before set. */
    int to_len = 0;		/* Length of buffer. */
    int must_free = 0;		/* True if the buffer should be freed. */
    char port_result[ERL_ONHEAP_BIN_LIMIT];  /* Default buffer for result from port. */
    char* port_resp;		/* Pointer to result buffer. */
    int n;
    int (*control)(ErlDrvData, unsigned, char*, int, char**, int);
    int fpe_was_unmasked;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if ((control = prt->drv_ptr->control) == NULL) {
	return THE_NON_VALUE;
    }

    /*
     * Convert the iolist to a buffer, pointed to by to_port,
     * and with its length in to_len.
     */
    if (is_binary(iolist) && binary_bitoffset(iolist) == 0) {
	Uint bitoffs;
	Uint bitsize;
	ERTS_GET_BINARY_BYTES(iolist, to_port, bitoffs, bitsize);
	to_len = binary_size(iolist);
    } else {
	int r;

	/* Try with an 8KB buffer first (will often be enough I guess). */
	to_len = 8*1024;
	to_port = erts_alloc(ERTS_ALC_T_TMP, to_len);
	must_free = 1;

	/*
	 * In versions before R10B, we used to reserve random
	 * amounts of extra memory. From R10B, we allocate the
	 * exact amount.
	 */
	r = io_list_to_buf(iolist, (char*) to_port, to_len);
	if (r >= 0) {
	    to_len -= r;
	} else if (r == -2) {	/* Type error */
	    erts_free(ERTS_ALC_T_TMP, (void *) to_port);
	    return THE_NON_VALUE;
	} else {
	    ASSERT(r == -1);	/* Overflow */
	    erts_free(ERTS_ALC_T_TMP, (void *) to_port);
	    if ((to_len = io_list_len(iolist)) < 0) { /* Type error */
		return THE_NON_VALUE;
	    }
	    must_free = 1;
	    to_port = erts_alloc(ERTS_ALC_T_TMP, to_len);
	    r = io_list_to_buf(iolist, (char*) to_port, to_len);
	    ASSERT(r == 0);
	}
    }

    prt->caller = p->id;	/* Internal pid */

    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    /*
     * Call the port's control routine.
     */

    port_resp = port_result;
    fpe_was_unmasked = erts_block_fpe();
    n = control((ErlDrvData)prt->drv_data, command, (char*)to_port, to_len,
		&port_resp, sizeof(port_result));
    erts_unblock_fpe(fpe_was_unmasked);
    if (must_free) {
	erts_free(ERTS_ALC_T_TMP, (void *) to_port);
    }
    prt->caller = NIL;
#ifdef ERTS_SMP
    if (prt->xports)
	erts_smp_xports_unlock(prt);
    ASSERT(!prt->xports);
#endif

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    /*
     * Handle the result.
     */

    if (n < 0) {
	return THE_NON_VALUE;
    }

    if ((prt->control_flags & PORT_CONTROL_FLAG_BINARY) == 0) {	/* List result */
	Eterm ret;	
	Eterm* hp = HAlloc(p, 2*n);
	ret = buf_to_intlist(&hp, port_resp, n, NIL);
	if (port_resp != port_result) {
	    driver_free(port_resp);
	}
	return ret;
    } 
    else if (port_resp == NULL) {
	return NIL;
    } 
    else {                   /* Binary result */
	ErlDrvBinary *dbin;
	ErlHeapBin *hbin;
	if (port_resp != port_result) {
	    dbin = (ErlDrvBinary *) port_resp;
	    if (dbin->orig_size > ERL_ONHEAP_BIN_LIMIT) {
		ProcBin* pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
		pb->thing_word = HEADER_PROC_BIN;
		pb->size = dbin->orig_size;
		pb->next = MSO(p).first;
		MSO(p).first = (struct erl_off_heap_header*)pb;
		pb->val = ErlDrvBinary2Binary(dbin);
		pb->bytes = (byte*) dbin->orig_bytes;
		pb->flags = 0;
		OH_OVERHEAD(&(MSO(p)), dbin->orig_size / sizeof(Eterm));
		return make_binary(pb);
	    }
	    port_resp = dbin->orig_bytes;
	    n = dbin->orig_size;
	} else {
	    dbin = NULL;
	}
	hbin = (ErlHeapBin*) HAlloc(p, heap_bin_size(n));
	ASSERT(n <= ERL_ONHEAP_BIN_LIMIT);
	hbin->thing_word = header_heap_bin(n);
	hbin->size = n;
	sys_memcpy(hbin->data, port_resp, n);
	if (dbin != NULL) {
	    driver_free_binary(dbin);
	}
	return make_binary(hbin);
    }
}

typedef struct {
    int to;
    void *arg;
} prt_one_lnk_data;

static void prt_one_monitor(ErtsMonitor *mon, void *vprtd)
{
    prt_one_lnk_data *prtd = (prt_one_lnk_data *) vprtd;
    erts_print(prtd->to, prtd->arg, "(%T,%T)", mon->pid,mon->ref);
}

static void prt_one_lnk(ErtsLink *lnk, void *vprtd)
{
    prt_one_lnk_data *prtd = (prt_one_lnk_data *) vprtd;
    erts_print(prtd->to, prtd->arg, "%T", lnk->pid);
}

void
print_port_info(int to, void *arg, int i)
{
    Port* p = &erts_port[i];

    if (p->status & ERTS_PORT_SFLGS_DEAD)
	return;

    erts_print(to, arg, "=port:%T\n", p->id);
    erts_print(to, arg, "Slot: %d\n", i);
    if (p->status & ERTS_PORT_SFLG_CONNECTED) {
	erts_print(to, arg, "Connected: %T", p->connected);
	erts_print(to, arg, "\n");
    }

    if (p->nlinks != NULL) {
	prt_one_lnk_data prtd;
	prtd.to = to;
	prtd.arg = arg;
	erts_print(to, arg, "Links: ");
	erts_doforall_links(p->nlinks, &prt_one_lnk, &prtd);
	erts_print(to, arg, "\n");
    }
    if (p->monitors != NULL) {
	prt_one_lnk_data prtd;
	prtd.to = to;
	prtd.arg = arg;
	erts_print(to, arg, "Monitors: ");
	erts_doforall_monitors(p->monitors, &prt_one_monitor, &prtd);
	erts_print(to, arg, "\n");
    }

    if (p->reg != NULL)
	erts_print(to, arg, "Registered as: %T\n", p->reg->name);

    if (p->drv_ptr == &fd_driver) {
	erts_print(to, arg, "Port is UNIX fd not opened by emulator: %s\n", p->name);
    } else if (p->drv_ptr == &vanilla_driver) {
	erts_print(to, arg, "Port is a file: %s\n",p->name);
    } else if (p->drv_ptr == &spawn_driver) {
	erts_print(to, arg, "Port controls external process: %s\n",p->name);
    } else {
	erts_print(to, arg, "Port controls linked-in driver: %s\n",p->name);
    }
}

void
set_busy_port(ErlDrvPort port_num, int on)
{
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[port_num]));

    if (on) {
        erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_PORT_BUSY);
    } else {
        ErtsProcList* plp = erts_port[port_num].suspended;
        erts_port_status_band_set(&erts_port[port_num],
				  ~ERTS_PORT_SFLG_PORT_BUSY);
        erts_port[port_num].suspended = NULL;

	if (erts_port[port_num].dist_entry) {
	    /*
	     * Processes suspended on distribution ports are
	     * normally queued on the dist entry.
	     */
	    erts_dist_port_not_busy(&erts_port[port_num]);
	}

	/*
	 * Resume, in a round-robin fashion, all processes waiting on the port.
	 * 
	 * This version submitted by Tony Rogvall. The earlier version used
	 * to resume the processes in order, which caused starvation of all but
	 * the first process.
	 */

        if (plp) {
            /* First proc should be resumed last */
	    if (plp->next) {
		erts_resume_processes(plp->next);
		plp->next = NULL;
	    }
	    erts_resume_processes(plp);
        }
    }
}

void set_port_control_flags(ErlDrvPort port_num, int flags)
{

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[port_num]));

    erts_port[port_num].control_flags = flags;
}

int get_port_flags(ErlDrvPort ix) { 
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt == NULL)
	return 0;

    return (prt->status & ERTS_PORT_SFLG_BINARY_IO ? PORT_FLAG_BINARY : 0) 
	| (prt->status & ERTS_PORT_SFLG_LINEBUF_IO ? PORT_FLAG_LINE : 0);
}


void erts_raw_port_command(Port* p, byte* buf, Uint len)
{
    int fpe_was_unmasked;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

    if (len > (Uint) INT_MAX)
	erl_exit(ERTS_ABORT_EXIT,
		 "Absurdly large data buffer (%bpu bytes) passed to"
		 "output callback of %s driver.\n",
		 len,
		 p->drv_ptr->name ? p->drv_ptr->name : "unknown");

    p->caller = NIL;
    fpe_was_unmasked = erts_block_fpe();
    (*p->drv_ptr->output)((ErlDrvData)p->drv_data, (char*) buf, (int) len);
    erts_unblock_fpe(fpe_was_unmasked);
}

int async_ready(Port *p, void* data)
{
    int need_free = 1;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (p) {
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));
	ASSERT(!(p->status & ERTS_PORT_SFLGS_DEAD));
	if (p->drv_ptr->ready_async != NULL) {
	    (*p->drv_ptr->ready_async)((ErlDrvData)p->drv_data, data);
	    need_free = 0;
#ifdef ERTS_SMP
	    if (p->xports)
		erts_smp_xports_unlock(p);
	    ASSERT(!p->xports);
#endif
	}
	if ((p->status & ERTS_PORT_SFLG_CLOSING) && is_port_ioq_empty(p)) {
	    terminate_port(p);
	}
    }
    return need_free;
}

static void
report_missing_drv_callback(Port *p, char *drv_type, char *callback)
{
    ErtsPortNames *pnp = erts_get_port_names(p->id);
    char *unknown = "<unknown>";
    char *drv_name = pnp->driver_name ? pnp->driver_name : unknown;
    char *prt_name = pnp->name ? pnp->name : unknown;
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();    
    erts_dsprintf(dsbufp, "%T: %s driver '%s' ", p->id, drv_type, drv_name);
    if (sys_strcmp(drv_name, prt_name) != 0)
	erts_dsprintf(dsbufp, "(%s) ", prt_name);
    erts_dsprintf(dsbufp, "does not implement the %s callback!\n", callback);
    erts_free_port_names(pnp);
    erts_send_error_to_logger_nogl(dsbufp);
}

void
erts_stale_drv_select(Eterm port,
		      ErlDrvEvent hndl,
		      int mode,
		      int deselect)
{
    char *type;
    ErlDrvPort drv_port = internal_port_index(port);
    ErtsPortNames *pnp = erts_get_port_names(port);
    erts_dsprintf_buf_t *dsbufp;

    switch (mode) {
    case ERL_DRV_READ | ERL_DRV_WRITE:
	type = "Input/Output";
	goto deselect;
    case ERL_DRV_WRITE:
	type = "Output";
	goto deselect;
    case ERL_DRV_READ:
	type = "Input";
    deselect:
	if (deselect) {
	    driver_select(drv_port, hndl,
			  mode | ERL_DRV_USE_NO_CALLBACK,
			  0);
	}
	break;
    default:
	type = "Event";
	if (deselect)
	    driver_event(drv_port, hndl, NULL);
	break;
    }

    dsbufp = erts_create_logger_dsbuf();
    erts_dsprintf(dsbufp,
		  "%T: %s: %s driver gone away without deselecting!\n",
		  port,
		  pnp->name ? pnp->name : "<unknown>",
		  type);
    erts_free_port_names(pnp);
    erts_send_error_to_logger_nogl(dsbufp);
}

ErtsPortNames *
erts_get_port_names(Eterm id)
{
    ErtsPortNames *pnp;
    ASSERT(is_nil(id) || is_internal_port(id));
    
    if (is_not_internal_port(id)) {
	pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, sizeof(ErtsPortNames));
	pnp->name = NULL;
	pnp->driver_name = NULL;
    }
    else {
	Port* prt = &erts_port[internal_port_index(id)];
	int do_realloc = 1;
	int len = -1;
	size_t pnp_len = sizeof(ErtsPortNames);
#ifndef DEBUG
	pnp_len += 100; /* In most cases 100 characters will be enough... */
#endif
	pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, pnp_len);
	do {
	    int nlen;
	    char *name, *driver_name;
	    if (len > 0) {
		erts_free(ERTS_ALC_T_PORT_NAMES, pnp);
		pnp_len = sizeof(ErtsPortNames) + len;
		pnp = erts_alloc(ERTS_ALC_T_PORT_NAMES, pnp_len);
	    }
	    erts_smp_port_state_lock(prt);
	    if (id != prt->id) {
		len = nlen = 0;
		name = driver_name = NULL;
	    }
	    else {
		name = prt->name;
		len = nlen = name ? sys_strlen(name) + 1 : 0;
		driver_name = (prt->drv_ptr ? prt->drv_ptr->name : NULL);
		len += driver_name ? sys_strlen(driver_name) + 1 : 0;
	    }
	    if (len <= pnp_len - sizeof(ErtsPortNames)) {
		if (!name)
		    pnp->name = NULL;
		else {
		    pnp->name = ((char *) pnp) + sizeof(ErtsPortNames);
		    sys_strcpy(pnp->name, name);
		}
		if (!driver_name)
		    pnp->driver_name = NULL;
		else {
		    pnp->driver_name = (((char *) pnp)
					+ sizeof(ErtsPortNames)
					+ nlen);
		    sys_strcpy(pnp->driver_name, driver_name);
		}
		do_realloc = 0;
	    }
	    erts_smp_port_state_unlock(prt);
	} while (do_realloc);
    }
    return pnp;
}

void
erts_free_port_names(ErtsPortNames *pnp)
{
    erts_free(ERTS_ALC_T_PORT_NAMES, pnp);
}

static void schedule_port_timeout(Port *p)
{
    /*
     * Scheduling of port timeouts can be done without port locking, but
     * since the task handle is stored in the port structure and the ptimer
     * structure is protected by the port lock we require the port to be
     * locked for now...
     *
     * TODO: Implement scheduling of port timeouts without locking
     *       the port.
     * /Rickard
     */
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));
    (void) erts_port_task_schedule(p->id,
				   &p->timeout_task,
				   ERTS_PORT_TASK_TIMEOUT,
				   (ErlDrvEvent) -1,
				   NULL);
}

ErlDrvTermData driver_mk_term_nil(void)
{
    return driver_term_nil;
}

void driver_report_exit(int ix, int status)
{
   Port* prt = erts_drvport2port(ix);
   Eterm* hp;
   Eterm tuple;
   Process *rp;
   Eterm pid;
   ErlHeapFragment *bp = NULL;
   ErlOffHeap *ohp;
   ErtsProcLocks rp_locks = 0;

   ERTS_SMP_CHK_NO_PROC_LOCKS;
   ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

   pid = prt->connected;
   ASSERT(is_internal_pid(pid));
   rp = erts_pid2proc_opt(NULL, 0, pid, 0, ERTS_P2P_FLG_SMP_INC_REFC);
   if (!rp)
       return;

   hp = erts_alloc_message_heap(3+3, &bp, &ohp, rp, &rp_locks);

   tuple = TUPLE2(hp, am_exit_status, make_small(status));
   hp += 3;
   tuple = TUPLE2(hp, prt->id, tuple);

   erts_queue_message(rp, &rp_locks, bp, tuple, am_undefined);

   erts_smp_proc_unlock(rp, rp_locks);
   erts_smp_proc_dec_refc(rp);
}


static ERTS_INLINE int
deliver_term_check_port(ErlDrvPort drvport)
{
    int res;
    int ix = (int) drvport;
    if (ix < 0 || erts_max_ports <= ix)
	res = -1; /* invalid */
    else {
	Port* prt = &erts_port[ix];
	erts_smp_port_state_lock(prt);
	if (!(prt->status & ERTS_PORT_SFLGS_INVALID_LOOKUP))
	    res = 1; /* ok */
	else if (prt->status & ERTS_PORT_SFLG_CLOSING)
	    res = 0; /* closing */
	else
	    res = -1; /* invalid (dead) */
	erts_smp_port_state_unlock(prt);
    }
    return res;
}

#define ERTS_B2T_STATES_DEF_STATES_SZ 5
#define ERTS_B2T_STATES_DEF_STATES_INC 100

struct b2t_states__ {
    int len;
    int ix;
    int used;
    ErtsBinary2TermState *state;
    ErtsBinary2TermState def_states[ERTS_B2T_STATES_DEF_STATES_SZ];
#ifdef DEBUG
    byte **org_ext;
    byte *def_org_ext[ERTS_B2T_STATES_DEF_STATES_SZ];
#endif
};

static ERTS_INLINE void
init_b2t_states(struct b2t_states__ *b2tsp)
{
    b2tsp->len = ERTS_B2T_STATES_DEF_STATES_SZ;
    b2tsp->ix = 0;
    b2tsp->used = 0;
    b2tsp->state = &b2tsp->def_states[0];
#ifdef DEBUG
    b2tsp->org_ext = &b2tsp->def_org_ext[0];
#endif
}

static ERTS_INLINE void
grow_b2t_states(struct b2t_states__ *b2tsp)
{
    if (b2tsp->state != &b2tsp->def_states[0]) {
	b2tsp->len += ERTS_B2T_STATES_DEF_STATES_INC;
	b2tsp->state = erts_realloc(ERTS_ALC_T_TMP,
				    b2tsp->state,
				    sizeof(ErtsBinary2TermState)*b2tsp->len);
#ifdef DEBUG
	b2tsp->org_ext = erts_realloc(ERTS_ALC_T_TMP,
				      b2tsp->org_ext,
				      sizeof(char *)*b2tsp->len);
#endif
    }
    else {
	ErtsBinary2TermState *new_states;
	new_states = erts_alloc(ERTS_ALC_T_TMP,
				(sizeof(ErtsBinary2TermState)
				 *ERTS_B2T_STATES_DEF_STATES_INC));
	sys_memcpy((void *) new_states,
		   (void *) b2tsp->state,
		   sizeof(ErtsBinary2TermState)*ERTS_B2T_STATES_DEF_STATES_SZ);
	b2tsp->state = new_states;
	b2tsp->len = ERTS_B2T_STATES_DEF_STATES_INC;
#ifdef DEBUG
	{
	    byte **new_org_ext = erts_alloc(ERTS_ALC_T_TMP,
					    (sizeof(char *)
					     *ERTS_B2T_STATES_DEF_STATES_INC));
	    sys_memcpy((void *) new_org_ext,
		       (void *) b2tsp->org_ext,
		       sizeof(char *)*ERTS_B2T_STATES_DEF_STATES_SZ);
	    b2tsp->org_ext = new_org_ext;
	}
#endif
    }
}

static ERTS_INLINE void
cleanup_b2t_states(struct b2t_states__ *b2tsp)
{
    if (b2tsp->state != &b2tsp->def_states[0]) {
	erts_free(ERTS_ALC_T_TMP, b2tsp->state);
#ifdef DEBUG
	erts_free(ERTS_ALC_T_TMP, b2tsp->org_ext);
#endif
    }
}


/*
 * Generate an Erlang term from data in an array (representing a simple stack
 * machine to build terms).
 * Returns:
 * 	-1 on error in input data
 *       0 if the message was not delivered (bad to pid or closed port)
 *       1 if the message was delivered successfully
 */

static int
driver_deliver_term(ErlDrvPort port,
		    Eterm to,
		    ErlDrvTermData* data,
		    int len)
{
#define ERTS_DDT_FAIL do { res = -1; goto done; } while (0)
    Uint need = 0;
    int depth = 0;
    int res;
    Eterm *hp = NULL, *hp_start = NULL, *hp_end = NULL;
    ErlDrvTermData* ptr;
    ErlDrvTermData* ptr_end;
    DECLARE_ESTACK(stack); 
    Eterm mess = NIL;		/* keeps compiler happy */
    Process* rp = NULL;
    ErlHeapFragment *bp = NULL;
    ErlOffHeap *ohp;
    ErtsProcLocks rp_locks = 0;
    struct b2t_states__ b2t;

    init_b2t_states(&b2t);

    /*
     * We used to check port and process here. In the SMP enabled emulator,
     * however, we don't want to that until we have verified the term.
     */

    /*
     * Check ErlDrvTermData for consistency and calculate needed heap size
     * and stack depth.
     */
    ptr = data;
    ptr_end = ptr + len;

    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;

#define ERTS_DDT_CHK_ENOUGH_ARGS(NEED) \
	if (ptr+((NEED)-1) >= ptr_end) ERTS_DDT_FAIL;

	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    depth++;
	    break;
	case ERL_DRV_ATOM: /* atom argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_atom(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_INT:  /* signed int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    /* check for bignum */
	    if (!IS_SSMALL((Sint)ptr[0]))
		need += BIG_UINT_HEAP_SIZE;  /* use small_to_big */
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_UINT:  /* unsigned int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    /* check for bignum */
	    if (!IS_USMALL(0, (Uint)ptr[0]))
		need += BIG_UINT_HEAP_SIZE;  /* use small_to_big */
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_INT64:  /* pointer to signed 64-bit int argument  */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    erts_bld_sint64(NULL, &need, *((Sint64 *) ptr[0]));
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_UINT64:  /* pointer to unsigned 64-bit int argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    erts_bld_uint64(NULL, &need, *((Uint64 *) ptr[0]));
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_PORT:  /* port argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_internal_port(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_BINARY: { /* ErlDrvBinary*, size, offs */
	    ErlDrvBinary* b;
	    Uint size;
	    Uint offset;
	    ERTS_DDT_CHK_ENOUGH_ARGS(3);
	    b = (ErlDrvBinary*) ptr[0];
	    size = ptr[1];
	    offset = ptr[2];
	    if (!b || size + offset > b->orig_size)
		ERTS_DDT_FAIL; /* No binary or outside the binary */
	    need += (size <= ERL_ONHEAP_BIN_LIMIT
			 ? heap_bin_size(size)
			 : PROC_BIN_SIZE);
	    ptr += 3;
	    depth++;
	    break;
	}
	case ERL_DRV_BUF2BINARY: { /* char*, size */
	    byte *bufp;
	    Uint size;
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    bufp = (byte *) ptr[0];
	    size = (Uint) ptr[1];
	    if (!bufp && size > 0) ERTS_DDT_FAIL; 
	    need += (size <= ERL_ONHEAP_BIN_LIMIT
		     ? heap_bin_size(size)
		     : PROC_BIN_SIZE);
	    ptr += 2;
	    depth++;
	    break;
	}
	case ERL_DRV_STRING: /* char*, length */
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    if ((char *) ptr[0] == NULL || (int) ptr[1] < 0) ERTS_DDT_FAIL;
	    need += ptr[1] * 2;
	    ptr += 2;
	    depth++;
	    break;
	case ERL_DRV_STRING_CONS: /* char*, length */
	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    if ((char *) ptr[0] == NULL || (int) ptr[1] < 0) ERTS_DDT_FAIL;
	    need += ptr[1] * 2;
	    if (depth < 1) ERTS_DDT_FAIL;
	    ptr += 2;
	    break;
	case ERL_DRV_LIST: /* int */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if ((int) ptr[0] <= 0) ERTS_DDT_FAIL;
	    need += (ptr[0]-1)*2;  /* list cells */
	    depth -= ptr[0];
	    if (depth < 0) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_TUPLE: { /* int */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if ((int) ptr[0] < 0) ERTS_DDT_FAIL;
	    need += ptr[0]+1;   /* vector positions + arityval */
	    depth -= ptr[0];
	    if (depth < 0) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	}
	case ERL_DRV_PID: /* pid argument */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    if (is_not_internal_pid(ptr[0])) ERTS_DDT_FAIL;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_FLOAT: /* double * */
	    ERTS_DDT_CHK_ENOUGH_ARGS(1);
	    need += FLOAT_SIZE_OBJECT;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_EXT2TERM: { /* char *ext, int size */
	    byte* ext;
	    Sint size;
	    Sint hsz;

	    ERTS_DDT_CHK_ENOUGH_ARGS(2);
	    ext = (byte *) ptr[0];
	    size = (Sint) ptr[1];
	    if (!ext || size <= 0)
		ERTS_DDT_FAIL;
	    if (b2t.len <= b2t.ix)
		grow_b2t_states(&b2t);
#ifdef DEBUG
	    b2t.org_ext[b2t.ix] = ext;
#endif
	    hsz = erts_binary2term_prepare(&b2t.state[b2t.ix++], ext, size);
	    if (hsz < 0)
		ERTS_DDT_FAIL; /* Invalid data */
	    need += hsz;
	    ptr += 2;
	    depth++;
	    break;
	}
	default:
	    ERTS_DDT_FAIL;
	}
#undef ERTS_DDT_CHK_ENOUGH_ARGS
    }

    if ((depth != 1) || (ptr != ptr_end))
	ERTS_DDT_FAIL;

    b2t.used = b2t.ix;
    b2t.ix = 0;

    /*
     * The term is OK. Go ahead and validate the port and process.
     */
    res = deliver_term_check_port(port);
    if (res <= 0)
	goto done;

    rp = erts_pid2proc_opt(NULL, 0, to, rp_locks, ERTS_P2P_FLG_SMP_INC_REFC);
    if (!rp) {
	res = 0;
	goto done;
    }

    hp_start = hp = erts_alloc_message_heap(need, &bp, &ohp, rp, &rp_locks);
    hp_end = hp + need;

    /*
     * Interpret the instructions and build the term.
     */
    ptr = data;
    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;

	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    mess = NIL;
	    break;

	case ERL_DRV_ATOM: /* atom argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_INT:  /* signed int argument */
	    if (IS_SSMALL((Sint)ptr[0]))
		mess = make_small((Sint)ptr[0]);
	    else {
		mess = small_to_big((Sint)ptr[0], hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    ptr++;
	    break;

	case ERL_DRV_UINT:  /* unsigned int argument */
	    if (IS_USMALL(0, (Uint)ptr[0]))
		mess = make_small((Uint)ptr[0]);
	    else {
		mess = uint_to_big((Uint)ptr[0], hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    ptr++;
	    break;

	case ERL_DRV_INT64: /* pointer to unsigned 64-bit int argument */
	    mess = erts_bld_sint64(&hp, NULL, *((Sint64 *) ptr[0]));
	    ptr++;
	    break;

	case ERL_DRV_UINT64: /* pointer to unsigned 64-bit int argument */
	    mess = erts_bld_uint64(&hp, NULL, *((Uint64 *) ptr[0]));
	    ptr++;
	    break;

	case ERL_DRV_PORT:  /* port argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_BINARY: { /* ErlDrvBinary*, size, offs */
	    ErlDrvBinary* b = (ErlDrvBinary*) ptr[0];
	    Uint size = ptr[1];
	    Uint offset = ptr[2];

	    if (size <= ERL_ONHEAP_BIN_LIMIT) {
		ErlHeapBin* hbp = (ErlHeapBin *) hp;
		hp += heap_bin_size(size);
		hbp->thing_word = header_heap_bin(size);
		hbp->size = size;
		if (size > 0) {
		    sys_memcpy((void *) hbp->data, (void *) (((byte*) b->orig_bytes) + offset), size);
		}
		mess = make_binary(hbp);
	    }
	    else {
		ProcBin* pb = (ProcBin *) hp;
		driver_binary_inc_refc(b);  /* caller will free binary */
		pb->thing_word = HEADER_PROC_BIN;
		pb->size = size;
		pb->next = ohp->first;
		ohp->first = (struct erl_off_heap_header*)pb;
		pb->val = ErlDrvBinary2Binary(b);
		pb->bytes = ((byte*) b->orig_bytes) + offset;
		pb->flags = 0;
		mess =  make_binary(pb);
		hp += PROC_BIN_SIZE;
		OH_OVERHEAD(ohp, pb->size / sizeof(Eterm));
	    }
	    ptr += 3;
	    break;
	}

	case ERL_DRV_BUF2BINARY: { /* char*, size */
	    byte *bufp = (byte *) ptr[0];
	    Uint size = (Uint) ptr[1];
	    if (size <= ERL_ONHEAP_BIN_LIMIT) {
		ErlHeapBin* hbp = (ErlHeapBin *) hp;
		hp += heap_bin_size(size);
		hbp->thing_word = header_heap_bin(size);
		hbp->size = size;
		if (size > 0) {
		    ASSERT(bufp);
		    sys_memcpy((void *) hbp->data, (void *) bufp, size);
		}
		mess = make_binary(hbp);
	    }
	    else {
		ProcBin* pbp;
		Binary* bp = erts_bin_nrml_alloc(size);
		ASSERT(bufp);
		bp->flags = 0;
		bp->orig_size = (long) size;
		erts_refc_init(&bp->refc, 1);
		sys_memcpy((void *) bp->orig_bytes, (void *) bufp, size);
		pbp = (ProcBin *) hp;
		hp += PROC_BIN_SIZE;
		pbp->thing_word = HEADER_PROC_BIN;
		pbp->size = size;
		pbp->next = ohp->first;
		ohp->first = (struct erl_off_heap_header*)pbp;
		pbp->val = bp;
		pbp->bytes = (byte*) bp->orig_bytes;
		pbp->flags = 0;
		OH_OVERHEAD(ohp, pbp->size / sizeof(Eterm));
		mess = make_binary(pbp);
	    }
	    ptr += 2;
	    break;
	}

	case ERL_DRV_STRING: /* char*, length */
	    mess = buf_to_intlist(&hp, (char*)ptr[0], ptr[1], NIL);
	    ptr += 2;
	    break;

	case ERL_DRV_STRING_CONS:  /* char*, length */
	    mess = ESTACK_POP(stack);
	    mess = buf_to_intlist(&hp, (char*)ptr[0], ptr[1], mess);
	    ptr += 2;
	    break;

	case ERL_DRV_LIST: { /* unsigned */
	    Uint i = (int) ptr[0]; /* i > 0 */

	    mess = ESTACK_POP(stack);
	    i--;
	    while(i > 0) {
		Eterm hd = ESTACK_POP(stack);

		mess = CONS(hp, hd, mess);
		hp += 2;
		i--;
	    }
	    ptr++;
	    break;
	}

	case ERL_DRV_TUPLE: { /* int */
	    int size = (int)ptr[0];
	    Eterm* tp = hp;

	    *tp = make_arityval(size);
	    mess = make_tuple(tp);

	    tp += size;   /* point at last element */
	    hp = tp+1;    /* advance "heap" pointer */

	    while(size--) {
		*tp-- = ESTACK_POP(stack);
	    }
	    ptr++;
	    break;
	}

	case ERL_DRV_PID: /* pid argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_FLOAT: { /* double * */
	    FloatDef f;

	    mess = make_float(hp);
	    f.fd = *((double *) ptr[0]);
	    PUT_DOUBLE(f, hp);
	    hp += FLOAT_SIZE_OBJECT;
	    ptr++;
	    break;
	}

	case ERL_DRV_EXT2TERM: /* char *ext, int size */
	    ASSERT(b2t.org_ext[b2t.ix] == (byte *) ptr[0]);
	    mess = erts_binary2term_create(&b2t.state[b2t.ix++], &hp, ohp);
	    if (mess == THE_NON_VALUE)
		ERTS_DDT_FAIL;
	    ptr += 2;
	    break;

	}
	ESTACK_PUSH(stack, mess);
    }

    res = 1;

 done:

    if (res > 0) {
	mess = ESTACK_POP(stack);  /* get resulting value */
	if (bp)
	    bp = erts_resize_message_buffer(bp, hp - hp_start, &mess, 1);
	else {
	    ASSERT(hp);
	    HRelease(rp, hp_end, hp);	    
	}
	/* send message */
	erts_queue_message(rp, &rp_locks, bp, mess, am_undefined);
    }
    else {
	if (b2t.ix > b2t.used)
	    b2t.used = b2t.ix;
	for (b2t.ix = 0; b2t.ix < b2t.used; b2t.ix++)
	    erts_binary2term_abort(&b2t.state[b2t.ix]);
	if (bp)
	    free_message_buffer(bp);
	else if (hp) {
	    HRelease(rp, hp_end, hp);
	}
    }
#ifdef ERTS_SMP
    if (rp) {
	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);
	erts_smp_proc_dec_refc(rp);
    }
#endif
    cleanup_b2t_states(&b2t);
    DESTROY_ESTACK(stack);
    return res;
#undef ERTS_DDT_FAIL
}


int 
driver_output_term(ErlDrvPort ix, ErlDrvTermData* data, int len)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt == NULL)
	return -1;
    return driver_deliver_term(ix, prt->connected, data, len);
}


int
driver_send_term(ErlDrvPort ix, ErlDrvTermData to, ErlDrvTermData* data, int len)
{
    return driver_deliver_term(ix, to, data, len);
}


/*
 * Output a binary with hlen bytes from hbuf as list header
 * and data is len length of bin starting from offset offs.
 */

int driver_output_binary(ErlDrvPort ix, char* hbuf, int hlen,
			 ErlDrvBinary* bin, int offs, int len)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == NULL)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (prt->status & ERTS_PORT_SFLG_CLOSING)
	return 0;

    prt->bytes_in += (hlen + len);
    erts_smp_atomic_add(&erts_bytes_in, (long) (hlen + len));
    if (prt->status & ERTS_PORT_SFLG_DISTRIBUTION) {
	return erts_net_message(prt,
				prt->dist_entry,
				(byte*) hbuf, hlen,
				(byte*) (bin->orig_bytes+offs), len);
    }
    else
	deliver_bin_message(prt, prt->connected, 
			    hbuf, hlen, bin, offs, len);
    return 0;
}

/* driver_output2:
** Delivers hlen bytes from hbuf to the port owner as a list;
** after that, the port settings apply, buf is sent as binary or list.
**
** Example: if hlen = 3 then the port owner will receive the data
** [H1,H2,H3 | T]
*/
int driver_output2(ErlDrvPort ix, char* hbuf, int hlen, char* buf, int len)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == NULL)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt->status & ERTS_PORT_SFLG_CLOSING)
	return 0;
    
    prt->bytes_in += (hlen + len);
    erts_smp_atomic_add(&erts_bytes_in, (long) (hlen + len));
    if (prt->status & ERTS_PORT_SFLG_DISTRIBUTION) {
	if (len == 0)
	    return erts_net_message(prt,
				    prt->dist_entry,
				    NULL, 0,
				    (byte*) hbuf, hlen);
	else
	    return erts_net_message(prt,
				    prt->dist_entry,
				    (byte*) hbuf, hlen,
				    (byte*) buf, len);
    }
    else if(prt->status & ERTS_PORT_SFLG_LINEBUF_IO)
	deliver_linebuf_message(prt, prt->connected, hbuf, hlen, buf, len);
    else
	deliver_read_message(prt, prt->connected, hbuf, hlen, buf, len, 0);
    return 0;
}

/* Interface functions available to driver writers */

int driver_output(ErlDrvPort ix, char* buf, int len)
{
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return driver_output2(ix, NULL, 0, buf, len);
}

int driver_outputv(ErlDrvPort ix, char* hbuf, int hlen, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    Port* prt;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    size = vec->size - skip;   /* Size of remaining bytes in vector */
    ASSERT(size >= 0);
    if (size <= 0)
	return driver_output2(ix, hbuf, hlen, NULL, 0);
    ASSERT(hlen >= 0);       /* debug only */
    if (hlen < 0)
	hlen = 0;

    prt = erts_drvport2port(ix);
    if (prt == NULL)
	return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (prt->status & ERTS_PORT_SFLG_CLOSING)
	return 0;

    /* size > 0 ! */
    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;
    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	} else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while (skip > 0);

    /* XXX handle distribution !!! */
    prt->bytes_in += (hlen + size);
    erts_smp_atomic_add(&erts_bytes_in, (long) (hlen + size));
    deliver_vec_message(prt, prt->connected, hbuf, hlen, binv, iov, n, size);
    return 0;
}

/* Copy bytes from a vector into a buffer
** input is a vector a buffer and a max length
** return bytes copied
*/
int driver_vec_to_buf(vec, buf, len)
ErlIOVec* vec; 
char* buf;
int len;
{
    SysIOVec* iov = vec->iov;
    int n = vec->vsize;
    int orig_len = len;

    while(n--) {
	int ilen = iov->iov_len;
	if (ilen < len) {
	    sys_memcpy(buf, iov->iov_base, ilen);
	    len -= ilen;
	    buf += ilen;
	    iov++;
	}
	else {
	    sys_memcpy(buf, iov->iov_base, len);
	    return orig_len;
	}
    }
    return (orig_len - len);
}


/*
 * - driver_alloc_binary() is thread safe (efile driver depend on it).
 * - driver_realloc_binary(), and driver_free_binary() are *not* thread safe.
 */

/*
 * reference count on driver binaries...
 */

long
driver_binary_get_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return erts_refc_read(&bp->refc, 1);
}

long
driver_binary_inc_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return erts_refc_inctest(&bp->refc, 2);
}

long
driver_binary_dec_refc(ErlDrvBinary *dbp)
{
    Binary* bp = ErlDrvBinary2Binary(dbp);
    return erts_refc_dectest(&bp->refc, 1);
}


/*
** Allocation/Deallocation of binary objects 
*/

ErlDrvBinary*
driver_alloc_binary(int size)
{
    Binary* bin;

    if (size < 0)
	return NULL;

    bin = erts_bin_drv_alloc_fnf((Uint) size);
    if (!bin)
	return NULL; /* The driver write must take action */
    bin->flags = BIN_FLAG_DRV;
    erts_refc_init(&bin->refc, 1);
    bin->orig_size = (long) size;
    return Binary2ErlDrvBinary(bin);
}

/* Reallocate space hold by binary */

ErlDrvBinary* driver_realloc_binary(ErlDrvBinary* bin, int size)
{
    Binary* oldbin;
    Binary* newbin;

    if (!bin || size < 0) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Bad use of driver_realloc_binary(%p, %d): "
		      "called with ",
		      bin, size);
	if (!bin) {
	    erts_dsprintf(dsbufp, "NULL pointer as first argument");
	    if (size < 0)
		erts_dsprintf(dsbufp, ", and ");
	}
	if (size < 0) {
	    erts_dsprintf(dsbufp, "negative size as second argument");
	    size = 0;
	}
	erts_send_warning_to_logger_nogl(dsbufp);
	if (!bin)
	    return driver_alloc_binary(size);
    }

    oldbin = ErlDrvBinary2Binary(bin);
    newbin = (Binary *) erts_bin_realloc_fnf(oldbin, size);
    if (!newbin)
	return NULL;

    newbin->orig_size = size;
    return Binary2ErlDrvBinary(newbin);
}


void driver_free_binary(dbin)
ErlDrvBinary* dbin;
{
    Binary *bin;
    if (!dbin) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Bad use of driver_free_binary(%p): called with "
		      "NULL pointer as argument", dbin);
	erts_send_warning_to_logger_nogl(dsbufp);
	return;
    }

    bin = ErlDrvBinary2Binary(dbin);
    if (erts_refc_dectest(&bin->refc, 0) == 0)
	erts_bin_free(bin);
}


/* 
 * Allocation/deallocation of memory for drivers 
 */

void *driver_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_DRV, (Uint) size);
}

void *driver_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_DRV, ptr, (Uint) size);
}

void driver_free(void *ptr)
{
    erts_free(ERTS_ALC_T_DRV, ptr);
}

/*
 * Port Data Lock
 */

static void
pdl_init(void)
{
}

static ERTS_INLINE void
pdl_init_refc(ErlDrvPDL pdl)
{
    erts_atomic_init(&pdl->refc, 1);
}

static ERTS_INLINE long
pdl_read_refc(ErlDrvPDL pdl)
{
    long refc = erts_atomic_read(&pdl->refc);
    ERTS_LC_ASSERT(refc >= 0);
    return refc;
}

static ERTS_INLINE void
pdl_inc_refc(ErlDrvPDL pdl)
{
    erts_atomic_inc(&pdl->refc);
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) > 1);
}

static ERTS_INLINE long
pdl_inctest_refc(ErlDrvPDL pdl)
{
    long refc = erts_atomic_inctest(&pdl->refc);
    ERTS_LC_ASSERT(refc > 1);
    return refc;
}

#if 0 /* unused */
static ERTS_INLINE void
pdl_dec_refc(ErlDrvPDL pdl)
{
    erts_atomic_dec(&pdl->refc);
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) > 0);
}
#endif

static ERTS_INLINE long
pdl_dectest_refc(ErlDrvPDL pdl)
{
    long refc = erts_atomic_dectest(&pdl->refc);
    ERTS_LC_ASSERT(refc >= 0);
    return refc;
}

static ERTS_INLINE void pdl_destroy(ErlDrvPDL pdl)
{
    ERTS_LC_ASSERT(driver_pdl_get_refc(pdl) == 0);
    erts_mtx_destroy(&pdl->mtx);
    erts_free(ERTS_ALC_T_PORT_DATA_LOCK, pdl);
}

#ifdef ERTS_SMP

static void driver_monitor_lock_pdl(Port *p) {
    if (p->port_data_lock) {
	driver_pdl_lock(p->port_data_lock);
    }
    /* Now we either have the port lock or the port_data_lock */
    ERTS_LC_ASSERT(!p->port_data_lock
		   || erts_lc_mtx_is_locked(&(p->port_data_lock->mtx)));
    ERTS_SMP_LC_ASSERT(p->port_data_lock
		       || erts_lc_is_port_locked(p));
}

static void driver_monitor_unlock_pdl(Port *p) {
    /* We should either have the port lock or the port_data_lock */
    ERTS_LC_ASSERT(!p->port_data_lock
		   || erts_lc_mtx_is_locked(&(p->port_data_lock->mtx)));
    ERTS_SMP_LC_ASSERT(p->port_data_lock
		       || erts_lc_is_port_locked(p));
    if (p->port_data_lock) {
	driver_pdl_unlock(p->port_data_lock);
    }
}

#endif

/*
 * exported driver_pdl_* functions ...
 */

ErlDrvPDL
driver_pdl_create(ErlDrvPort dp)
{
    ErlDrvPDL pdl;
    Port *pp = erts_drvport2port(dp);
    if (!pp || pp->port_data_lock)
	return NULL;
    pdl = erts_alloc(ERTS_ALC_T_PORT_DATA_LOCK,
		     sizeof(struct erl_drv_port_data_lock));
    erts_mtx_init(&pdl->mtx, "port_data_lock");
    pdl_init_refc(pdl);
    pp->port_data_lock = pdl;
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_create(%T) -> 0x%08X\r\n",pp->id,(unsigned) pdl);
#endif
    return pdl;
}

void
driver_pdl_lock(ErlDrvPDL pdl)
{
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_lock(0x%08X)\r\n",(unsigned) pdl);
#endif
    pdl_inc_refc(pdl);
    erts_mtx_lock(&pdl->mtx);
}

void
driver_pdl_unlock(ErlDrvPDL pdl)
{
    long refc;
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_unlock(0x%08X)\r\n",(unsigned) pdl);
#endif
    erts_mtx_unlock(&pdl->mtx);
    refc = pdl_dectest_refc(pdl);
    if (!refc)
	pdl_destroy(pdl);
}

long
driver_pdl_get_refc(ErlDrvPDL pdl)
{
    return pdl_read_refc(pdl);
}

long
driver_pdl_inc_refc(ErlDrvPDL pdl)
{
    long refc = pdl_inctest_refc(pdl);
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_inc_refc(0x%08X) -> %ld\r\n",(unsigned) pdl, refc);
#endif
    return refc;
}

long
driver_pdl_dec_refc(ErlDrvPDL pdl)
{
    long refc = pdl_dectest_refc(pdl);
#ifdef HARDDEBUG
    erts_fprintf(stderr, "driver_pdl_dec_refc(0x%08X) -> %ld\r\n",(unsigned) pdl, refc);
#endif
    if (!refc)
	pdl_destroy(pdl);
    return refc;
}

/* expand queue to hold n elements in tail or head */
static int expandq(ErlIOQueue* q, int n, int tail)
/* tail: 0 if make room in head, make room in tail otherwise */
{
    int h_sz;  /* room before header */
    int t_sz;  /* room after tail */
    int q_sz;  /* occupied */
    int nvsz;
    SysIOVec* niov;
    ErlDrvBinary** nbinv;

    h_sz = q->v_head - q->v_start;
    t_sz = q->v_end -  q->v_tail;
    q_sz = q->v_tail - q->v_head;

    if (tail && (n <= t_sz)) /* do we need to expand tail? */
	return 0;
    else if (!tail && (n <= h_sz))  /* do we need to expand head? */
	return 0;
    else if (n > (h_sz + t_sz)) { /* need to allocate */
	/* we may get little extra but it ok */
	nvsz = (q->v_end - q->v_start) + n; 

	niov = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(SysIOVec));
	if (!niov)
	    return -1;
	nbinv = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(ErlDrvBinary**));
	if (!nbinv) {
	    erts_free(ERTS_ALC_T_IOQ, (void *) niov);
	    return -1;
	}
	if (tail) {
	    sys_memcpy(niov, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_head = q->v_start;
	    q->v_tail = q->v_head + q_sz;

	    sys_memcpy(nbinv, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_head = q->b_start;
	    q->b_tail = q->b_head + q_sz;	
	}
	else {
	    sys_memcpy(niov+nvsz-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_tail = q->v_end;
	    q->v_head = q->v_tail - q_sz;
	    
	    sys_memcpy(nbinv+nvsz-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_tail = q->b_end;
	    q->b_head = q->b_tail - q_sz;
	}
    }
    else if (tail) {  /* move to beginning to make room in tail */
	sys_memmove(q->v_start, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_head = q->v_start;
	q->v_tail = q->v_head + q_sz;
	sys_memmove(q->b_start, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_head = q->b_start;
	q->b_tail = q->b_head + q_sz;
    }
    else {   /* move to end to make room */
	sys_memmove(q->v_end-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_tail = q->v_end;
	q->v_head = q->v_tail-q_sz;
	sys_memmove(q->b_end-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_tail = q->b_end;
	q->b_head = q->b_tail-q_sz;
    }

    return 0;
}



/* Put elements from vec at q tail */
int driver_enqv(ErlDrvPort ix, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary*  b;
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return -1;

    size = vec->size - skip;
    ASSERT(size >= 0);       /* debug only */
    if (size <= 0)
	return 0;

    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_tail + n >= q->v_end)
	expandq(q, n, 1);

    /* Queue and reference all binaries (remove zero length items) */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*q->b_tail++ = b;
		q->v_tail->iov_len = len;
		q->v_tail->iov_base = b->orig_bytes;
		q->v_tail++;
	    }
	    else {
		driver_binary_inc_refc(b);
		*q->b_tail++ = b;
		*q->v_tail++ = *iov;
	    }
	}
	iov++;
	binv++;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}

/* Put elements from vec at q head */
int driver_pushqv(ErlDrvPort ix, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary* b;
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return -1;

    if ((size = vec->size - skip) <= 0)
	return 0;
    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_head - n < q->v_start)
	expandq(q, n, 0);

    /* Queue and reference all binaries (remove zero length items) */
    iov += (n-1);  /* move to end */
    binv += (n-1); /* move to end */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*--q->b_head = b;
		q->v_head--;
		q->v_head->iov_len = len;
		q->v_head->iov_base = b->orig_bytes;
	    }
	    else {
		driver_binary_inc_refc(b);
		*--q->b_head = b;
		*--q->v_head = *iov;
	    }
	}
	iov--;
	binv--;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}


/*
** Remove size bytes from queue head
** Return number of bytes that remain in queue
*/
int driver_deq(ErlDrvPort ix, int size)
{
    ErlIOQueue* q = drvport2ioq(ix);
    int len;
    int sz;

    if ((q == NULL) || (sz = (q->size - size)) < 0)
	return -1;
    q->size = sz;
    while (size > 0) {
	ASSERT(q->v_head != q->v_tail);

	len = q->v_head->iov_len;
	if (len <= size) {
	    size -= len;
	    driver_free_binary(*q->b_head);
	    *q->b_head++ = NULL;
	    q->v_head++;
	}
	else {
	    q->v_head->iov_base += size;
	    q->v_head->iov_len -= size;
	    size = 0;
	}
    }

    /* restart pointers (optimised for enq) */
    if (q->v_head == q->v_tail) {
	q->v_head = q->v_tail = q->v_start;
	q->b_head = q->b_tail = q->b_start;
    }
    return sz;
}


int driver_peekqv(ErlDrvPort ix, ErlIOVec *ev) {
    ErlIOQueue *q = drvport2ioq(ix);
    ASSERT(ev);

    if (! q) {
	return -1;
    } else {
	if ((ev->vsize = q->v_tail - q->v_head) == 0) {
	    ev->size = 0;
	    ev->iov = NULL;
	    ev->binv = NULL;
	} else {
	    ev->size = q->size;
	    ev->iov = q->v_head;
	    ev->binv = q->b_head;
	}
	return q->size;
    }
}

SysIOVec* driver_peekq(ErlDrvPort ix, int* vlenp)  /* length of io-vector */
{
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL) {
	*vlenp = -1;
	return NULL;
    }
    if ((*vlenp = (q->v_tail - q->v_head)) == 0)
	return NULL;
    return q->v_head;
}


int driver_sizeq(ErlDrvPort ix)
{
    ErlIOQueue* q = drvport2ioq(ix);

    if (q == NULL)
	return -1;
    return q->size;
}


/* Utils */

/* Enqueue a binary */
int driver_enq_bin(ErlDrvPort ix, ErlDrvBinary* bin, int offs, int len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_enqv(ix, &ev, 0);
}

int driver_enq(ErlDrvPort ix, char* buffer, int len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_enq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

int driver_pushq_bin(ErlDrvPort ix, ErlDrvBinary* bin, int offs, int len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_pushqv(ix, &ev, 0);
}

int driver_pushq(ErlDrvPort ix, char* buffer, int len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;

    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_pushq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

static ERTS_INLINE void
drv_cancel_timer(Port *prt)
{
#ifdef ERTS_SMP
    erts_cancel_smp_ptimer(prt->ptimer);
#else
    erl_cancel_timer(&prt->tm);
#endif
    if (erts_port_task_is_scheduled(&prt->timeout_task))
	erts_port_task_abort(prt->id, &prt->timeout_task);
}

int driver_set_timer(ErlDrvPort ix, UWord t)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == NULL)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (prt->drv_ptr->timeout == NULL)
	return -1;
    drv_cancel_timer(prt);
#ifdef ERTS_SMP
    erts_create_smp_ptimer(&prt->ptimer,
			   prt->id,
			   (ErlTimeoutProc) schedule_port_timeout,
			   t);
#else
    erl_set_timer(&prt->tm,
		  (ErlTimeoutProc) schedule_port_timeout,
		  NULL,
		  prt,
		  t);
#endif
    return 0;
}

int driver_cancel_timer(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    if (prt == NULL)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    drv_cancel_timer(prt);
    return 0;
}


int
driver_read_timer(ErlDrvPort ix, unsigned long* t)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == NULL)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
#ifdef ERTS_SMP
    *t = prt->ptimer ? time_left(&prt->ptimer->timer.tm) : 0;
#else
    *t = time_left(&prt->tm);
#endif
    return 0;
}

int 
driver_get_now(ErlDrvNowData *now_data)
{
    Uint mega,secs,micro;
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (now_data == NULL) {
	return -1;
    }
    get_now(&mega,&secs,&micro);
    now_data->megasecs = (unsigned long) mega;
    now_data->secs = (unsigned long) secs;
    now_data->microsecs = (unsigned long) micro;
    return 0;
}

static void ref_to_driver_monitor(Eterm ref, ErlDrvMonitor *mon)
{
    RefThing *refp;
    ASSERT(is_internal_ref(ref));
    ASSERT(sizeof(RefThing) <= sizeof(ErlDrvMonitor));
    refp = ref_thing_ptr(ref);
    memset(mon,0,sizeof(ErlDrvMonitor));
    memcpy(mon,refp,sizeof(RefThing));
}


static int do_driver_monitor_process(Port *prt,
				     Eterm *buf,
				     ErlDrvTermData process,
				     ErlDrvMonitor *monitor)
{
    Process *rp;
    Eterm ref;

    if (prt->drv_ptr->process_exit == NULL) {
	return -1;
    }
    rp = erts_pid2proc_opt(NULL, 0,
			   (Eterm) process, ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    if (!rp) {
	return 1;
    }

    ref = erts_make_ref_in_buffer(buf);
    erts_add_monitor(&(prt->monitors), MON_ORIGIN, ref, rp->id, NIL);
    erts_add_monitor(&(rp->monitors), MON_TARGET, ref, prt->id, NIL);

    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    ref_to_driver_monitor(ref,monitor);
    return 0;
}

/*
 * This can be called from a non scheduler thread iff a port_data_lock exists
 */
int driver_monitor_process(ErlDrvPort port,
			   ErlDrvTermData process,
			   ErlDrvMonitor *monitor)
{
    Port *prt;
    int ret;
    Uint32 status;
    ErtsSchedulerData *sched = erts_get_scheduler_data();
    int ix = (int) port;
    if (ix < 0 || erts_max_ports <= ix) {
	return -1;
    }
    prt = &erts_port[ix];

    DRV_MONITOR_LOCK_PDL(prt);

    if (sched) {
	status = erts_port[ix].status;
    } else {
	erts_smp_port_state_lock(prt);
	status = erts_port[ix].status;
	erts_smp_port_state_unlock(prt);
    }

    if (status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP) {
	DRV_MONITOR_UNLOCK_PDL(prt);
	return -1;
    }

    /* Now (in SMP) we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));

#if !HEAP_ON_C_STACK
    if (!sched) {
	/* Need a separate allocation for the ref :( */
	Eterm *buf = erts_alloc(ERTS_ALC_T_TEMP_TERM,
				sizeof(Eterm)*REF_THING_SIZE);
	ret = do_driver_monitor_process(prt,buf,process,monitor);
	erts_free(ERTS_ALC_T_TEMP_TERM,buf);
    } else
#endif
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_monitor_process(prt,buf,process,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}

static int do_driver_demonitor_process(Port *prt, Eterm *buf,
				       const ErlDrvMonitor *monitor)
{
    Process *rp;
    Eterm ref;
    ErtsMonitor *mon;
    Eterm to;

    memcpy(buf,monitor,sizeof(Eterm)*REF_THING_SIZE);
    ref = make_internal_ref(buf);
    mon = erts_lookup_monitor(prt->monitors, ref);
    if (mon == NULL) {
	return 1;
    }
    ASSERT(mon->type == MON_ORIGIN);
    to = mon->pid;
    ASSERT(is_internal_pid(to));
    rp = erts_pid2proc_opt(NULL,
			   0,
			   to,
			   ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    mon = erts_remove_monitor(&(prt->monitors), ref);
    if (mon) {
	erts_destroy_monitor(mon);
    }
    if (rp) {
	ErtsMonitor *rmon;
	rmon = erts_remove_monitor(&(rp->monitors), ref);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	if (rmon != NULL) {
	    erts_destroy_monitor(rmon);
	}
    } 
    return 0;
}

int driver_demonitor_process(ErlDrvPort port,
			     const ErlDrvMonitor *monitor)
{
    Port *prt;
    int ret;
    Uint32 status;
    ErtsSchedulerData *sched = erts_get_scheduler_data();
    int ix = (int) port;
    if (ix < 0 || erts_max_ports <= ix) {
	return -1;
    }
    prt = &erts_port[ix];

    DRV_MONITOR_LOCK_PDL(prt);

    if (sched) {
	status = erts_port[ix].status;
    } else {
	erts_smp_port_state_lock(prt);
	status = erts_port[ix].status;
	erts_smp_port_state_unlock(prt);
    }

    if (status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP) {
	DRV_MONITOR_UNLOCK_PDL(prt);
	return -1;
    }

    /* Now we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));
#if !HEAP_ON_C_STACK
    if (!sched) {
	/* Need a separate allocation for the ref :( */
	Eterm *buf = erts_alloc(ERTS_ALC_T_TEMP_TERM,
				sizeof(Eterm)*REF_THING_SIZE);
	ret = do_driver_demonitor_process(prt,buf,monitor);
	erts_free(ERTS_ALC_T_TEMP_TERM,buf);
    } else
#endif
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_demonitor_process(prt,buf,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}

static ErlDrvTermData do_driver_get_monitored_process(Port *prt, Eterm *buf,
					    const ErlDrvMonitor *monitor)
{
    Eterm ref;
    ErtsMonitor *mon;
    Eterm to;

    memcpy(buf,monitor,sizeof(Eterm)*REF_THING_SIZE);
    ref = make_internal_ref(buf);
    mon = erts_lookup_monitor(prt->monitors, ref);
    if (mon == NULL) {
	return driver_term_nil;
    }
    ASSERT(mon->type == MON_ORIGIN);
    to = mon->pid;
    ASSERT(is_internal_pid(to));
    return (ErlDrvTermData) to;
}


ErlDrvTermData driver_get_monitored_process(ErlDrvPort port,
					    const ErlDrvMonitor *monitor)
{
    Port *prt;
    ErlDrvTermData ret;
    Uint32 status;
    ErtsSchedulerData *sched = erts_get_scheduler_data();
    int ix = (int) port;
    if (ix < 0 || erts_max_ports <= ix) {
	return driver_term_nil;
    }
    prt = &erts_port[ix];

    DRV_MONITOR_LOCK_PDL(prt);

    if (sched) {
	status = erts_port[ix].status;
    } else {
	erts_smp_port_state_lock(prt);
	status = erts_port[ix].status;
	erts_smp_port_state_unlock(prt);
    }

    if (status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP) {
	DRV_MONITOR_UNLOCK_PDL(prt);
	return driver_term_nil;
    }

    /* Now we should have either the port lock (if we have a scheduler) or the port data lock
       (if we're a driver thread) */
    ERTS_SMP_LC_ASSERT((sched != NULL || prt->port_data_lock));

#if !HEAP_ON_C_STACK
    if (!sched) {
	/* Need a separate allocation for the ref :( */
	Eterm *buf = erts_alloc(ERTS_ALC_T_TEMP_TERM,
				sizeof(Eterm)*REF_THING_SIZE);
	ret = do_driver_get_monitored_process(prt,buf,monitor);
	erts_free(ERTS_ALC_T_TEMP_TERM,buf);
    } else
#endif
    {
	DeclareTmpHeapNoproc(buf,REF_THING_SIZE);
	UseTmpHeapNoproc(REF_THING_SIZE);
	ret = do_driver_get_monitored_process(prt,buf,monitor);
	UnUseTmpHeapNoproc(REF_THING_SIZE);
    }
    DRV_MONITOR_UNLOCK_PDL(prt);
    return ret;
}


int driver_compare_monitors(const ErlDrvMonitor *monitor1,
			    const ErlDrvMonitor *monitor2)
{
    return memcmp(monitor1,monitor2,sizeof(ErlDrvMonitor));
}

void erts_fire_port_monitor(Port *prt, Eterm ref)
{
    ErtsMonitor *rmon;
    void (*callback)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
    ErlDrvMonitor drv_monitor;
    int fpe_was_unmasked;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    ASSERT(prt->drv_ptr != NULL);    
    DRV_MONITOR_LOCK_PDL(prt);
    if (erts_lookup_monitor(prt->monitors,ref) == NULL) {
	DRV_MONITOR_UNLOCK_PDL(prt);
	return;
    }
    callback = prt->drv_ptr->process_exit;
    ASSERT(callback != NULL);
    ref_to_driver_monitor(ref,&drv_monitor);
    DRV_MONITOR_UNLOCK_PDL(prt);
    fpe_was_unmasked = erts_block_fpe();
    (*callback)((ErlDrvData) (prt->drv_data), &drv_monitor);
    erts_unblock_fpe(fpe_was_unmasked);
    DRV_MONITOR_LOCK_PDL(prt);
    /* remove monitor *after* callback */
    rmon = erts_remove_monitor(&(prt->monitors),ref);
    DRV_MONITOR_UNLOCK_PDL(prt);
    if (rmon) {
	erts_destroy_monitor(rmon);
    }
}


static int
driver_failure_term(ErlDrvPort ix, Eterm term, int eof)
{
    Port* prt = erts_drvport2port(ix);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    if (prt == NULL)
	return -1;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if (eof)
	flush_linebuf_messages(prt);
    if (prt->status & ERTS_PORT_SFLG_CLOSING) {
	terminate_port(prt);
    } else if (eof && (prt->status & ERTS_PORT_SFLG_SOFT_EOF)) {
	deliver_result(prt->id, prt->connected, am_eof);
    } else {
	/* XXX UGLY WORK AROUND, Let do_exit_port terminate the port */
	if (prt->port_data_lock)
	    driver_pdl_lock(prt->port_data_lock);
	prt->ioq.size = 0;
	if (prt->port_data_lock)
	    driver_pdl_unlock(prt->port_data_lock);
	erts_do_exit_port(prt, prt->id, eof ? am_normal : term);
    }
    return 0;
}



/*
** Do a (soft) exit. unlink the connected process before doing
** driver posix error or (normal)
*/
int driver_exit(ErlDrvPort ix, int err)
{
    Port* prt = erts_drvport2port(ix);
    Process* rp;
    ErtsLink *lnk, *rlnk = NULL;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
  
    if (prt == NULL)
        return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    rp = erts_pid2proc(NULL, 0, prt->connected, ERTS_PROC_LOCK_LINK);
    if (rp) {
	rlnk = erts_remove_link(&(rp->nlinks),prt->id);
    }

    lnk = erts_remove_link(&(prt->nlinks),prt->connected);

#ifdef ERTS_SMP
    if (rp)
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
#endif

    if (rlnk != NULL) {
	erts_destroy_link(rlnk);
    }

    if (lnk != NULL) {
	erts_destroy_link(lnk);
    }

    if (err == 0)
        return driver_failure_term(ix, am_normal, 0);
    else {
        char* err_str = erl_errno_id(err);
        Eterm am_err = am_atom_put(err_str, sys_strlen(err_str));
        return driver_failure_term(ix, am_err, 0);
    }
}


int driver_failure(ErlDrvPort ix, int code)
{
    return driver_failure_term(ix, make_small(code), code == 0);
}

int driver_failure_atom(ErlDrvPort ix, char* string)
{
    Eterm am = am_atom_put(string, strlen(string));
    return driver_failure_term(ix, am, 0);
}

int driver_failure_posix(ErlDrvPort ix, int err)
{
    return driver_failure_atom(ix, erl_errno_id(err));
}

int driver_failure_eof(ErlDrvPort ix)
{
    return driver_failure_term(ix, NIL, 1);
}



ErlDrvTermData driver_mk_atom(char* string)
{
    Eterm am = am_atom_put(string, sys_strlen(string));
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return (ErlDrvTermData) am;
}

ErlDrvTermData driver_mk_port(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return (ErlDrvTermData) prt->id;
}

ErlDrvTermData driver_connected(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (prt == NULL)
	return NIL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return prt->connected;
}

ErlDrvTermData driver_caller(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (prt == NULL)
	return NIL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    return prt->caller;
}

int driver_lock_driver(ErlDrvPort ix)
{
    Port* prt = erts_drvport2port(ix);
    DE_Handle* dh;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    erts_smp_mtx_lock(&erts_driver_list_lock);

    if (prt == NULL) return -1;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    if ((dh = (DE_Handle*)prt->drv_ptr->handle ) == NULL) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
	return -1;
    }
    erts_ddll_lock_driver(dh, prt->drv_ptr->name);
    erts_smp_mtx_unlock(&erts_driver_list_lock);
    return 0;
}


static int maybe_lock_driver_list(void) 
{
    void *rec_lock;
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    if (rec_lock == 0) {
	erts_smp_mtx_lock(&erts_driver_list_lock);
	return 1;
    }
    return 0;
}
static void maybe_unlock_driver_list(int doit)
{
    if (doit) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
    }
}
/* 
   These old interfaces are certainly not MT friendly. Hopefully they are only used internally,
   but you never know, so they are kept for BC. As The sys ddll code has no notion
   of locking, I use the driver list lock to mutex this from the code in erl_bif_ddll.c.
   To allow dynamic code loading in the init functions of a driver, recursive locking is 
   handled as in add_driver_entry etc.
   A TSD variable holds the last error for a thread, so that code like
   ...
   x = driver_dl_open(...);
   if (x == NULL)
      y = driver_dl_error();
   ...
   works as long as execution happens in one driver callback even in an SMP emulator. 
   Writing code using these interfaces spanning several driver callbacks between loading/lookup 
   and error handling may give undesired results...
*/
void *driver_dl_open(char * path)
{
    void *ptr;
    int res;
    int *last_error_p = erts_smp_tsd_get(driver_list_last_error_key);
    int locked = maybe_lock_driver_list();
    if ((res = erts_sys_ddll_open(path, &ptr)) == 0) {
	maybe_unlock_driver_list(locked);
	return ptr;
    } else {
	if (!last_error_p) {
	    last_error_p = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, sizeof(int));
	    erts_smp_tsd_set(driver_list_last_error_key,last_error_p);
	}
	*last_error_p = res;
	maybe_unlock_driver_list(locked);
	return NULL;
    }
}

void *driver_dl_sym(void * handle, char *func_name)
{
    void *ptr;
    int res;
    int *last_error_p = erts_smp_tsd_get(driver_list_lock_status_key);
    int locked = maybe_lock_driver_list();
    if ((res = erts_sys_ddll_sym(handle, func_name, &ptr)) == 0) {
	maybe_unlock_driver_list(locked);
	return ptr;
    } else {
	if (!last_error_p) {
	    last_error_p = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, sizeof(int));
	    erts_smp_tsd_set(driver_list_lock_status_key,last_error_p);
	}
	*last_error_p = res;
	maybe_unlock_driver_list(locked);
	return NULL;
    }
}
    
int driver_dl_close(void *handle)
{
    int res;
    int locked = maybe_lock_driver_list();
    res = erts_sys_ddll_close(handle);
    maybe_unlock_driver_list(locked);
    return res;
}

char *driver_dl_error(void) 
{
    char *res;
    int *last_error_p = erts_smp_tsd_get(driver_list_lock_status_key);
    int locked = maybe_lock_driver_list();
    res = erts_ddll_error((last_error_p != NULL) ? (*last_error_p) : ERL_DE_ERROR_UNSPECIFIED); 
    maybe_unlock_driver_list(locked);
    return res;
}


#define ERL_DRV_SYS_INFO_SIZE(LAST_FIELD) \
  (((size_t) &((ErlDrvSysInfo *) 0)->LAST_FIELD) \
   + sizeof(((ErlDrvSysInfo *) 0)->LAST_FIELD))

void
driver_system_info(ErlDrvSysInfo *sip, size_t si_size)
{
    /*
     * When adding fields in the ErlDrvSysInfo struct
     * remember to increment ERL_DRV_EXTENDED_MINOR_VERSION
     */

    /*
     * 'smp_support' is the last field in the first version
     * of ErlDrvSysInfo (introduced in driver version 1.0).
     */
    if (!sip || si_size < ERL_DRV_SYS_INFO_SIZE(smp_support)) 
	erl_exit(1,
		 "driver_system_info(%p, %ld) called with invalid arguments\n",
		 sip, si_size);

    /*
     * 'smp_support' is the last field in the first version
     * of ErlDrvSysInfo (introduced in driver version 1.0).
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(smp_support)) {
	sip->driver_major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
	sip->driver_minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
	sip->erts_version = ERLANG_VERSION;
	sip->otp_release = ERLANG_OTP_RELEASE;
	sip->thread_support = 
#ifdef USE_THREADS
	    1
#else
	    0
#endif
	    ;
	sip->smp_support = 
#ifdef ERTS_SMP
	    1
#else
	    0
#endif
	    ;

    }

    /*
     * 'scheduler_threads' is the last field in the second version
     * of ErlDrvSysInfo (introduced in driver version 1.1).
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(scheduler_threads)) {
	sip->async_threads = erts_async_max_threads;
	sip->scheduler_threads = erts_no_schedulers;
    }
    /*
     * 'nif_minor_version' is the last field in the third version
     * (driver version 1.5, NIF version 1.0) 
     */
    if (si_size >= ERL_DRV_SYS_INFO_SIZE(nif_minor_version)) {
	sip->nif_major_version = ERL_NIF_MAJOR_VERSION;
	sip->nif_minor_version = ERL_NIF_MINOR_VERSION;
    }
}


static ERTS_INLINE Port *
get_current_port(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ASSERT(esdp);
    ASSERT(esdp->current_port);
    return esdp->current_port;
}

/*
 * Default callbacks used if not supplied by driver.
 */

static void
no_output_callback(ErlDrvData drv_data, char *buf, int len)
{

}

static void
no_event_callback(ErlDrvData drv_data, ErlDrvEvent event, ErlDrvEventData event_data)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Event", "event()");
    driver_event((ErlDrvPort) internal_port_index(prt->id), event, NULL);
}

static void
no_ready_input_callback(ErlDrvData drv_data, ErlDrvEvent event)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Input", "ready_input()");
    driver_select((ErlDrvPort) internal_port_index(prt->id), event, 
		  (ERL_DRV_READ | ERL_DRV_USE_NO_CALLBACK), 0);
}

static void
no_ready_output_callback(ErlDrvData drv_data, ErlDrvEvent event)
{
    Port *prt = get_current_port();
    report_missing_drv_callback(prt, "Output", "ready_output()");
    driver_select((ErlDrvPort) internal_port_index(prt->id), event,
		  (ERL_DRV_WRITE | ERL_DRV_USE_NO_CALLBACK), 0);
}

static void
no_timeout_callback(ErlDrvData drv_data)
{

}

static void
no_stop_select_callback(ErlDrvEvent event, void* private)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();    
    erts_dsprintf(dsbufp, "Driver does not implement stop_select callback "
			  "(event=%ld, private=%p)!\n", (long)event, private);
    erts_send_error_to_logger_nogl(dsbufp);
}


static int
init_driver(erts_driver_t *drv, ErlDrvEntry *de, DE_Handle *handle)
{
    drv->name = de->driver_name;
    if (de->extended_marker == ERL_DRV_EXTENDED_MARKER) {
	drv->version.major = de->major_version;
	drv->version.minor = de->minor_version;
	drv->flags = de->driver_flags;
    }
    else {
	drv->version.major = 0;
	drv->version.minor = 0;
	drv->flags = 0;
    }
    drv->handle = handle;
#ifdef ERTS_SMP
    if (drv->flags & ERL_DRV_FLAG_USE_PORT_LOCKING)
	drv->lock = NULL;
    else {
	drv->lock = erts_alloc(ERTS_ALC_T_DRIVER_LOCK,
			       sizeof(erts_smp_mtx_t));
	erts_smp_mtx_init_x(drv->lock,
			    "driver_lock",
#if defined(ERTS_ENABLE_LOCK_CHECK) || defined(ERTS_ENABLE_LOCK_COUNT)
			    am_atom_put(drv->name, sys_strlen(drv->name))
#else
			    NIL
#endif
	    );
    }
#endif
    drv->entry = de;

    drv->start = de->start;
    drv->stop = de->stop;
    drv->finish = de->finish;
    drv->flush = de->flush;
    drv->output = de->output ? de->output : no_output_callback;
    drv->outputv = de->outputv;
    drv->control = de->control;
    drv->call = de->call;
    drv->event = de->event ? de->event : no_event_callback;
    drv->ready_input = de->ready_input ? de->ready_input : no_ready_input_callback;
    drv->ready_output = de->ready_output ? de->ready_output : no_ready_output_callback;
    drv->timeout = de->timeout ? de->timeout : no_timeout_callback;
    drv->ready_async = de->ready_async;
    if (de->extended_marker == ERL_DRV_EXTENDED_MARKER)
	drv->process_exit = de->process_exit;
    else
	drv->process_exit = NULL;
    if (de->minor_version >= 3/*R13A*/ && de->stop_select)
	drv->stop_select = de->stop_select;
    else
	drv->stop_select = no_stop_select_callback;

    if (!de->init)
	return 0;
    else {
	int res;
	int fpe_was_unmasked = erts_block_fpe();
	res = (*de->init)();
	erts_unblock_fpe(fpe_was_unmasked);
	return res;
    }
}

void
erts_destroy_driver(erts_driver_t *drv)
{
#ifdef ERTS_SMP
    if (drv->lock) {
	erts_smp_mtx_destroy(drv->lock);
	erts_free(ERTS_ALC_T_DRIVER_LOCK, drv->lock);
    }
#endif    
    erts_free(ERTS_ALC_T_DRIVER, drv);
}

/* 
 * Functions for maintaining a list of driver_entry struct
 * Exposed in the driver interface, and therefore possibly locking directly.
 */

void add_driver_entry(ErlDrvEntry *drv){
    void *rec_lock;
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    /* 
     * Ignore result of erts_add_driver_entry, the init is not
     * allowed to fail when drivers are added by drivers.
     */
    erts_add_driver_entry(drv, NULL, rec_lock != NULL); 
}

int erts_add_driver_entry(ErlDrvEntry *de, DE_Handle *handle, int driver_list_locked)
{
    erts_driver_t *dp = erts_alloc(ERTS_ALC_T_DRIVER, sizeof(erts_driver_t));
    int res;

    if (!driver_list_locked) {
	erts_smp_mtx_lock(&erts_driver_list_lock);
    }

    dp->next = driver_list;
    dp->prev = NULL;
    if (driver_list != NULL) {
	driver_list->prev = dp;
    }
    driver_list = dp;

    if (!driver_list_locked) {
	erts_smp_tsd_set(driver_list_lock_status_key, (void *) 1);
    }

    res = init_driver(dp, de, handle);

    if (res != 0) {
	/* 
	 * Remove it all again...
	 */
	driver_list = dp->next;
	if (driver_list != NULL) {
	    driver_list->prev = NULL;
	}
	erts_destroy_driver(dp);
    }
	
    if (!driver_list_locked) {
	erts_smp_tsd_set(driver_list_lock_status_key, NULL);
	erts_smp_mtx_unlock(&erts_driver_list_lock);
    }
    return res;
}

/* Not allowed for dynamic drivers */
int remove_driver_entry(ErlDrvEntry *drv)
{
    erts_driver_t *dp;
    void *rec_lock;
    
    rec_lock = erts_smp_tsd_get(driver_list_lock_status_key);
    if (rec_lock == NULL) {
	erts_smp_mtx_lock(&erts_driver_list_lock);
    }
    dp = driver_list;
    while (dp && dp->entry != drv)
	dp = dp->next;
    if (dp) {
	if (dp->handle) {
	    if (rec_lock == NULL) {
		erts_smp_mtx_unlock(&erts_driver_list_lock);
	    }
	    return -1;
	}
	if (dp->prev == NULL) {
	    driver_list = dp->next;
	} else {
	    dp->prev->next = dp->next;
	}
	if (dp->next != NULL) {
	    dp->next->prev = dp->prev;
	}
	erts_destroy_driver(dp);
	if (rec_lock == NULL) {
	    erts_smp_mtx_unlock(&erts_driver_list_lock);
	}
	return 1;
    }
    if (rec_lock == NULL) {
	erts_smp_mtx_unlock(&erts_driver_list_lock);
    }
    return 0;
}

/* very useful function that can be used in entries that are not used
 * so that not every driver writer must supply a personal version
 */
int null_func(void)
{
    return 0;
}

int
erl_drv_putenv(char *key, char *value)
{
    return erts_write_env(key, value);
}

int
erl_drv_getenv(char *key, char *value, size_t *value_size)
{
    return erts_sys_getenv(key, value, value_size);
}
