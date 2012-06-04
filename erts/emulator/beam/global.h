/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2012. All Rights Reserved.
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

#ifndef __GLOBAL_H__
#define __GLOBAL_H__

#include "sys.h"
#include <stddef.h> /* offsetof() */
#include "erl_alloc.h"
#include "erl_vm.h"
#include "erl_node_container_utils.h"
#include "hash.h"
#include "index.h"
#include "atom.h"
#include "code_ix.h"
#include "export.h"
#include "module.h"
#include "register.h"
#include "erl_fun.h"
#include "erl_node_tables.h"
#include "benchmark.h"
#include "erl_process.h"
#include "erl_sys_driver.h"
#include "erl_debug.h"
#include "error.h"

typedef struct port Port;
#include "erl_port_task.h"

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

struct enif_environment_t /* ErlNifEnv */
{
    struct erl_module_nif* mod_nif;
    Process* proc;
    Eterm* hp;
    Eterm* hp_end;
    ErlHeapFragment* heap_frag;
    int fpe_was_unmasked;
    struct enif_tmp_obj_t* tmp_obj_list;
};
extern void erts_pre_nif(struct enif_environment_t*, Process*,
			 struct erl_module_nif*);
extern void erts_post_nif(struct enif_environment_t* env);
extern Eterm erts_nif_taints(Process* p);
extern void erts_print_nif_taints(int to, void* to_arg);
void erts_unload_nif(struct erl_module_nif* nif);
extern void erl_nif_init(void);

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
 * referred to by the lock field. I'v called it the port lock.
 * This lock is shared between all ports running the same driver
 * when driver specific locking is used.
 *
 * The 'sched' field is protected by the port tasks lock
 * (see erl_port_tasks.c)
 *
 * The 'status' field is protected by a combination of the port lock,
 * the port tasks lock, and the state_lck. It may be read if
 * the state_lck, or the port lock is held. It may only be
 * modified if both the port lock and the state_lck is held
 * (with one exception; see below). When changeing status from alive
 * to dead or vice versa, also the port task lock has to be held.
 * This in order to guarantee that tasks are scheduled only for
 * ports that are alive.
 *
 * The status field may be modified with only the state_lck
 * held when status is changed from dead to alive. This since no
 * threads can have any references to the port other than via the
 * port table.
 *
 * /rickard
 */

struct port {
    ErtsPortTaskSched sched;
    ErtsPortTaskHandle timeout_task;
    erts_smp_atomic_t refc;
#ifdef ERTS_SMP
    erts_smp_mtx_t *lock;
    ErtsXPortsList *xports;
    erts_smp_atomic_t run_queue;
    erts_smp_spinlock_t state_lck;  /* protects: id, status, snapshot */
#endif
    Eterm id;                   /* The Port id of this port */
    Eterm connected;            /* A connected process */
    Eterm caller;		/* Current caller. */
    Eterm data;			/* Data associated with port. */
    ErlHeapFragment* bp;	/* Heap fragment holding data (NULL if imm data). */
    ErtsLink *nlinks;
    ErtsMonitor *monitors;      /* Only MON_ORIGIN monitors of pid's */
    Uint bytes_in;		/* Number of bytes read */
    Uint bytes_out;		/* Number of bytes written */
#ifdef ERTS_SMP
    ErtsSmpPTimer *ptimer;
#else
    ErlTimer tm;                 /* Timer entry */
#endif
    
    Eterm tracer_proc;		/* If the port is traced, this is the tracer */
    Uint trace_flags;		/* Trace flags */

    ErlIOQueue ioq;              /* driver accessible i/o queue */
    DistEntry *dist_entry;       /* Dist entry used in DISTRIBUTION */
    char *name;		         /* String used in the open */
    erts_driver_t* drv_ptr;
    UWord drv_data;
    SWord os_pid;                 /* Child process ID */
    ErtsProcList *suspended;	 /* List of suspended processes. */
    LineBuf *linebuf;            /* Buffer to hold data not ready for
				    process to get (line oriented I/O)*/
    Uint32 status;		 /* Status and type flags */
    int control_flags;		 /* Flags for port_control()  */
    erts_aint32_t snapshot;      /* Next snapshot that port should be part of */
    struct reg_proc *reg;
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

/* Driver handle (wrapper for old plain handle) */
#define ERL_DE_OK      0
#define ERL_DE_UNLOAD  1
#define ERL_DE_FORCE_UNLOAD 2 
#define ERL_DE_RELOAD  3
#define ERL_DE_FORCE_RELOAD  4
#define ERL_DE_PERMANENT 5

#define ERL_DE_PROC_LOADED 0
#define ERL_DE_PROC_AWAIT_UNLOAD 1
#define ERL_DE_PROC_AWAIT_UNLOAD_ONLY 2
#define ERL_DE_PROC_AWAIT_LOAD 3

/* Flags for process entries */
#define ERL_DE_FL_DEREFERENCED 1

/* Flags for drivers, put locking policy here /PaN */
#define ERL_DE_FL_KILL_PORTS 1

#define ERL_FL_CONSISTENT_MASK ( ERL_DE_FL_KILL_PORTS )

/* System specific load errors are returned as positive values */
#define ERL_DE_NO_ERROR 0
#define ERL_DE_LOAD_ERROR_NO_INIT -1
#define ERL_DE_LOAD_ERROR_FAILED_INIT -2
#define ERL_DE_LOAD_ERROR_BAD_NAME -3
#define ERL_DE_LOAD_ERROR_NAME_TO_LONG -4
#define ERL_DE_LOAD_ERROR_INCORRECT_VERSION -5
#define ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY -6
#define ERL_DE_ERROR_UNSPECIFIED -7
#define ERL_DE_LOOKUP_ERROR_NOT_FOUND -8
#define ERL_DE_DYNAMIC_ERROR_OFFSET -10

typedef struct de_proc_entry {
    Process *proc;                   /* The process... */
    Uint    awaiting_status;         /* PROC_LOADED == Have loaded the driver
			                PROC_AWAIT_UNLOAD == Wants to be notified 
			                when we have unloaded the driver (was locked)
			                PROC_AWAIT_LOAD == Wants to be notified when we
			                reloaded the driver (old was locked) */
    Uint    flags;                   /* ERL_FL_DE_DEREFERENCED when reload in progress */
    Eterm   heap[REF_THING_SIZE];    /* "ref heap" */
    struct  de_proc_entry *next;
} DE_ProcEntry;

typedef struct {
    void         *handle;             /* Handle for DLL or SO (for dyn. drivers). */
    DE_ProcEntry *procs;              /* List of pids that have loaded this driver,
				         or that wait for it to change state */
    erts_refc_t  refc;                /* Number of ports/processes having
					 references to the driver */
    Uint         port_count;          /* Number of ports using the driver */
    Uint         flags;               /* ERL_DE_FL_KILL_PORTS */
    int          status;              /* ERL_DE_xxx */
    char         *full_path;          /* Full path of the driver */
    char         *reload_full_path;   /* If status == ERL_DE_RELOAD, this contains
				         full name of driver (path) */
    char         *reload_driver_name; /* ... and this contains the driver name */
    Uint         reload_flags;        /* flags for reloaded driver */
} DE_Handle;

/*
 * This structure represents a link to the next driver.
 */

struct erts_driver_t_ {
    erts_driver_t *next;
    erts_driver_t *prev;
    char *name;
    struct {
	int major;
	int minor;
    } version;
    int flags;
    DE_Handle *handle;
#ifdef ERTS_SMP
    erts_smp_mtx_t *lock;
#endif
    ErlDrvEntry *entry;
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
    void (*stop)(ErlDrvData drv_data);
    void (*finish)(void);
    void (*flush)(ErlDrvData drv_data);
    void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev); /* Might be NULL */
    ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command,
			    char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen); /* Might be NULL */
    ErlDrvSSizeT (*call)(ErlDrvData drv_data, unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen, /* Might be NULL */
			 unsigned int *flags);
    void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
    void (*timeout)(ErlDrvData drv_data);
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data); /* Might be NULL */ 
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
    void (*stop_select)(ErlDrvEvent event, void*); /* Might be NULL */
};

extern erts_driver_t *driver_list;
extern erts_smp_mtx_t erts_driver_list_lock;

extern void erts_ddll_init(void);
extern void erts_ddll_lock_driver(DE_Handle *dh, char *name);

/* These are for bookkeeping */
extern void erts_ddll_increment_port_count(DE_Handle *dh);
extern void erts_ddll_decrement_port_count(DE_Handle *dh);

/* These makes things happen, drivers may be scheduled for unload etc */
extern void erts_ddll_reference_driver(DE_Handle *dh);
extern void erts_ddll_reference_referenced_driver(DE_Handle *dh);
extern void erts_ddll_dereference_driver(DE_Handle *dh);

extern char *erts_ddll_error(int code);
extern void erts_ddll_proc_dead(Process *p, ErtsProcLocks plocks);
extern int erts_ddll_driver_ok(DE_Handle *dh);
extern void erts_ddll_remove_monitor(Process *p,
				     Eterm ref,
				     ErtsProcLocks plocks);
extern Eterm erts_ddll_monitor_driver(Process *p,
				      Eterm description,
				      ErtsProcLocks plocks);
/*
 * Max no. of drivers (linked in and dynamically loaded). Each table
 * entry uses 4 bytes.
 */
#define DRIVER_TAB_SIZE 32

/*
** Just like the driver binary but with initial flags
** Note that the two structures Binary and ErlDrvBinary HAVE to
** be equal except for extra fields in the beginning of the struct.
** ErlDrvBinary is defined in erl_driver.h.
** When driver_alloc_binary is called, a Binary is allocated, but 
** the pointer returned is to the address of the first element that
** also occurs in the ErlDrvBinary struct (driver.*binary takes care if this).
** The driver need never know about additions to the internal Binary of the
** emulator. One should however NEVER be sloppy when mixing ErlDrvBinary
** and Binary, the macros below can convert one type to the other, as they both
** in reality are equal.
*/

#ifdef ARCH_32
 /* *DO NOT USE* only for alignment. */
#define ERTS_BINARY_STRUCT_ALIGNMENT Uint32 align__;
#else
#define ERTS_BINARY_STRUCT_ALIGNMENT
#endif

/* Add fields in ERTS_INTERNAL_BINARY_FIELDS, otherwise the drivers crash */
#define ERTS_INTERNAL_BINARY_FIELDS				\
    UWord flags;							\
    erts_refc_t refc;						\
    ERTS_BINARY_STRUCT_ALIGNMENT

typedef struct binary {
    ERTS_INTERNAL_BINARY_FIELDS
    SWord orig_size;
    char orig_bytes[1]; /* to be continued */
} Binary;

#define ERTS_SIZEOF_Binary(Sz) \
    (offsetof(Binary,orig_bytes) + (Sz))

typedef struct {
    ERTS_INTERNAL_BINARY_FIELDS
    SWord orig_size;
    void (*destructor)(Binary *);
    char magic_bin_data[1];
} ErtsMagicBinary;

typedef union {
    Binary binary;
    ErtsMagicBinary magic_binary;
    struct {
	ERTS_INTERNAL_BINARY_FIELDS
	ErlDrvBinary binary;
    } driver;
} ErtsBinary;

/*
 * 'Binary' alignment:
 *   Address of orig_bytes[0] of a Binary should always be 8-byte aligned.
 * It is assumed that the flags, refc, and orig_size fields are 4 bytes on
 * 32-bits architectures and 8 bytes on 64-bits architectures.
 */

#define ERTS_MAGIC_BIN_DESTRUCTOR(BP) \
  ((ErtsBinary *) (BP))->magic_binary.destructor
#define ERTS_MAGIC_BIN_DATA(BP) \
  ((void *) ((ErtsBinary *) (BP))->magic_binary.magic_bin_data)
#define ERTS_MAGIC_BIN_DATA_SIZE(BP) \
  ((BP)->orig_size - sizeof(void (*)(Binary *)))
#define ERTS_MAGIC_BIN_ORIG_SIZE(Sz) \
  (sizeof(void (*)(Binary *)) + (Sz))
#define ERTS_MAGIC_BIN_SIZE(Sz) \
  (offsetof(ErtsMagicBinary,magic_bin_data) + (Sz))
#define ERTS_MAGIC_BIN_FROM_DATA(DATA) \
  ((ErtsBinary*)((char*)(DATA) - offsetof(ErtsMagicBinary,magic_bin_data)))

#define Binary2ErlDrvBinary(B) (&((ErtsBinary *) (B))->driver.binary)
#define ErlDrvBinary2Binary(D) ((Binary *) \
				(((char *) (D)) \
				 - offsetof(ErtsBinary, driver.binary)))

/* A "magic" binary flag */
#define BIN_FLAG_MAGIC      1
#define BIN_FLAG_USR1       2 /* Reserved for use by different modules too mark */
#define BIN_FLAG_USR2       4 /*  certain binaries as special (used by ets) */
#define BIN_FLAG_DRV        8

/*
 * This structure represents one type of a binary in a process.
 */

typedef struct proc_bin {
    Eterm thing_word;		/* Subtag REFC_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
#if HALFWORD_HEAP
    void* dummy_ptr_padding__;
#endif
    struct erl_off_heap_header *next;
    Binary *val;		/* Pointer to Binary structure. */
    byte *bytes;		/* Pointer to the actual data bytes. */
    Uint flags;			/* Flag word. */
} ProcBin;

#define PB_IS_WRITABLE 1	/* Writable (only one reference to ProcBin) */
#define PB_ACTIVE_WRITER 2	/* There is an active writer */

/*
 * ProcBin size in Eterm words.
 */
#define PROC_BIN_SIZE (sizeof(ProcBin)/sizeof(Eterm))

ERTS_GLB_INLINE Eterm erts_mk_magic_binary_term(Eterm **hpp,
						ErlOffHeap *ohp,
						Binary *mbp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm
erts_mk_magic_binary_term(Eterm **hpp, ErlOffHeap *ohp, Binary *mbp)
{
    ProcBin *pb = (ProcBin *) *hpp;
    *hpp += PROC_BIN_SIZE;

    ASSERT(mbp->flags & BIN_FLAG_MAGIC);

    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = ohp->first;
    ohp->first = (struct erl_off_heap_header*) pb;
    pb->val = mbp;
    pb->bytes = (byte *) mbp->orig_bytes;
    pb->flags = 0;

    erts_refc_inc(&mbp->refc, 1);

    return make_binary(pb);    
}

#endif

#define ERTS_TERM_IS_MAGIC_BINARY(T) \
  (is_binary((T)) \
   && (thing_subtag(*binary_val((T))) == REFC_BINARY_SUBTAG) \
   && (((ProcBin *) binary_val((T)))->val->flags & BIN_FLAG_MAGIC))


union erl_off_heap_ptr {
    struct erl_off_heap_header* hdr;
    ProcBin *pb;
    struct erl_fun_thing* fun;
    struct external_thing_* ext;
    Eterm* ep;
    void* voidp;
};

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
	*tombstone = prt->id;
    }
    /*else no ongoing snapshot or port was already included or created after snapshot */
}

#endif

/* controls warning mapping in error_logger */

extern Eterm node_cookie;
extern erts_smp_atomic_t erts_bytes_out;	/* no bytes written out */
extern erts_smp_atomic_t erts_bytes_in;		/* no bytes sent into the system */
extern Uint display_items;	/* no of items to display in traces etc */

extern int erts_backtrace_depth;
extern erts_smp_atomic32_t erts_max_gen_gcs;

extern int erts_disable_tolerant_timeofday;

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

/*
 * Here is an implementation of a lightweiht stack.
 *
 * Use it like this:
 *
 * DECLARE_ESTACK(Stack)	(At the start of a block)
 * ...
 * ESTACK_PUSH(Stack, Term)
 * ...
 * if (ESTACK_ISEMPTY(Stack)) {
 *    Stack is empty
 * } else {
 *    Term = ESTACK_POP(Stack);
 *    Process popped Term here
 * }
 * ...
 * DESTROY_ESTACK(Stack)
 */


void erl_grow_stack(Eterm** start, Eterm** sp, Eterm** end);
#define ESTK_CONCAT(a,b) a##b
#define ESTK_SUBSCRIPT(s,i) *((Eterm *)((byte *)ESTK_CONCAT(s,_start) + (i)))
#define DEF_ESTACK_SIZE (16)

#define DECLARE_ESTACK(s)						\
    Eterm ESTK_CONCAT(s,_default_stack)[DEF_ESTACK_SIZE];		\
    Eterm* ESTK_CONCAT(s,_start) = ESTK_CONCAT(s,_default_stack);	\
    Eterm* ESTK_CONCAT(s,_sp) = ESTK_CONCAT(s,_start);			\
    Eterm* ESTK_CONCAT(s,_end) = ESTK_CONCAT(s,_start) + DEF_ESTACK_SIZE

#define DESTROY_ESTACK(s)						\
do {									\
    if (ESTK_CONCAT(s,_start) != ESTK_CONCAT(s,_default_stack)) {	\
	erts_free(ERTS_ALC_T_ESTACK, ESTK_CONCAT(s,_start));		\
    }									\
} while(0)

#define ESTACK_PUSH(s, x)						\
do {									\
    if (ESTK_CONCAT(s,_sp) == ESTK_CONCAT(s,_end)) {			\
	erl_grow_stack(&ESTK_CONCAT(s,_start), &ESTK_CONCAT(s,_sp),	\
	               &ESTK_CONCAT(s,_end));				\
    }									\
    *ESTK_CONCAT(s,_sp)++ = (x);					\
} while(0)

#define ESTACK_PUSH2(s, x, y)						\
do {									\
    if (ESTK_CONCAT(s,_sp) > ESTK_CONCAT(s,_end) - 2) {			\
	erl_grow_stack(&ESTK_CONCAT(s,_start), &ESTK_CONCAT(s,_sp),	\
		&ESTK_CONCAT(s,_end));	\
    }									\
    *ESTK_CONCAT(s,_sp)++ = (x);					\
    *ESTK_CONCAT(s,_sp)++ = (y);					\
} while(0)

#define ESTACK_PUSH3(s, x, y, z)					\
do {									\
    if (ESTK_CONCAT(s,_sp) > ESTK_CONCAT(s,_end) - 3) {			\
	erl_grow_stack(&ESTK_CONCAT(s,_start), &ESTK_CONCAT(s,_sp),	\
		&ESTK_CONCAT(s,_end));					\
    }									\
    *ESTK_CONCAT(s,_sp)++ = (x);					\
    *ESTK_CONCAT(s,_sp)++ = (y);					\
    *ESTK_CONCAT(s,_sp)++ = (z);					\
} while(0)

#define ESTACK_COUNT(s) (ESTK_CONCAT(s,_sp) - ESTK_CONCAT(s,_start))

#define ESTACK_ISEMPTY(s) (ESTK_CONCAT(s,_sp) == ESTK_CONCAT(s,_start))
#define ESTACK_POP(s) (*(--ESTK_CONCAT(s,_sp)))


void erl_grow_wstack(UWord** start, UWord** sp, UWord** end);
#define WSTK_CONCAT(a,b) a##b
#define WSTK_SUBSCRIPT(s,i) *((UWord *)((byte *)WSTK_CONCAT(s,_start) + (i)))
#define DEF_WSTACK_SIZE (16)

#define DECLARE_WSTACK(s)						\
    UWord WSTK_CONCAT(s,_default_stack)[DEF_WSTACK_SIZE];		\
    UWord* WSTK_CONCAT(s,_start) = WSTK_CONCAT(s,_default_stack);	\
    UWord* WSTK_CONCAT(s,_sp) = WSTK_CONCAT(s,_start);			\
    UWord* WSTK_CONCAT(s,_end) = WSTK_CONCAT(s,_start) + DEF_WSTACK_SIZE

#define DESTROY_WSTACK(s)						\
do {									\
    if (WSTK_CONCAT(s,_start) != WSTK_CONCAT(s,_default_stack)) {	\
	erts_free(ERTS_ALC_T_ESTACK, WSTK_CONCAT(s,_start));		\
    }									\
} while(0)

#define WSTACK_PUSH(s, x)						\
do {									\
    if (WSTK_CONCAT(s,_sp) == WSTK_CONCAT(s,_end)) {			\
	erl_grow_wstack(&WSTK_CONCAT(s,_start), &WSTK_CONCAT(s,_sp),	\
	               &WSTK_CONCAT(s,_end));				\
    }									\
    *WSTK_CONCAT(s,_sp)++ = (x);					\
} while(0)

#define WSTACK_PUSH2(s, x, y)						\
do {									\
    if (WSTK_CONCAT(s,_sp) > WSTK_CONCAT(s,_end) - 2) {			\
	erl_grow_wstack(&WSTK_CONCAT(s,_start), &WSTK_CONCAT(s,_sp),	\
		&WSTK_CONCAT(s,_end));	\
    }									\
    *WSTK_CONCAT(s,_sp)++ = (x);					\
    *WSTK_CONCAT(s,_sp)++ = (y);					\
} while(0)

#define WSTACK_PUSH3(s, x, y, z)					\
do {									\
    if (WSTK_CONCAT(s,_sp) > WSTK_CONCAT(s,_end) - 3) {			\
	erl_grow_wstack(&WSTK_CONCAT(s,_start), &WSTK_CONCAT(s,_sp),	\
		&WSTK_CONCAT(s,_end));					\
    }									\
    *WSTK_CONCAT(s,_sp)++ = (x);					\
    *WSTK_CONCAT(s,_sp)++ = (y);					\
    *WSTK_CONCAT(s,_sp)++ = (z);					\
} while(0)

#define WSTACK_COUNT(s) (WSTK_CONCAT(s,_sp) - WSTK_CONCAT(s,_start))

#define WSTACK_ISEMPTY(s) (WSTK_CONCAT(s,_sp) == WSTK_CONCAT(s,_start))
#define WSTACK_POP(s) (*(--WSTK_CONCAT(s,_sp)))


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

/* binary.c */

void erts_emasculate_writable_binary(ProcBin* pb);
Eterm erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap);
Eterm erts_new_mso_binary(Process*, byte*, int);
Eterm new_binary(Process*, byte*, Uint);
Eterm erts_realloc_binary(Eterm bin, size_t size);

/* erl_bif_info.c */

void erts_bif_info_init(void);

/* bif.c */
Eterm erts_make_ref(Process *);
Eterm erts_make_ref_in_buffer(Eterm buffer[REF_THING_SIZE]);
void erts_queue_monitor_message(Process *,
				ErtsProcLocks*,
				Eterm,
				Eterm,
				Eterm,
				Eterm);
void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(Process*,Eterm*));
void erts_init_bif(void);
Eterm erl_send(Process *p, Eterm to, Eterm msg);

/* erl_bif_op.c */

Eterm erl_is_function(Process* p, Eterm arg1, Eterm arg2);

/* erl_bif_port.c */

/* erl_bif_trace.c */
Eterm erl_seq_trace_info(Process *p, Eterm arg1);
void erts_system_monitor_clear(Process *c_p);
void erts_system_profile_clear(Process *c_p);

/* beam_load.c */
typedef struct {
    BeamInstr* current;		/* Pointer to: Mod, Name, Arity */
    Uint needed;		/* Heap space needed for entire tuple */
    Uint32 loc;			/* Location in source code */
    Eterm* fname_ptr;		/* Pointer to fname table */
} FunctionInfo;

Binary* erts_alloc_loader_state(void);
Eterm erts_module_for_prepared_code(Binary* magic);
Eterm erts_prepare_loading(Binary* loader_state,  Process *c_p,
			   Eterm group_leader, Eterm* modp,
			   byte* code, Uint size);
Eterm erts_finish_loading(Binary* loader_state, Process* c_p,
			  ErtsProcLocks c_p_locks, Eterm* modp);
Eterm erts_preload_module(Process *c_p, ErtsProcLocks c_p_locks,
			  Eterm group_leader, Eterm* mod, byte* code, Uint size);
void init_load(void);
BeamInstr* find_function_from_pc(BeamInstr* pc);
Eterm* erts_build_mfa_item(FunctionInfo* fi, Eterm* hp,
			   Eterm args, Eterm* mfa_p);
void erts_set_current_function(FunctionInfo* fi, BeamInstr* current);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);
Eterm erts_make_stub_module(Process* p, Eterm Mod, Eterm Beam, Eterm Info);

/* beam_ranges.c */
void erts_init_ranges(void);
void erts_start_staging_ranges(void);
void erts_end_staging_ranges(int commit);
void erts_update_ranges(BeamInstr* code, Uint size);
void erts_remove_from_ranges(BeamInstr* code);
UWord erts_ranges_sz(void);
void erts_lookup_function_info(FunctionInfo* fi, BeamInstr* pc, int full_info);

/* break.c */
void init_break_handler(void);
void erts_set_ignore_break(void);
void erts_replace_intr(void);
void process_info(int, void *);
void print_process_info(int, void *, Process*);
void info(int, void *);
void loaded(int, void *);

/* config.c */

__decl_noreturn void __noreturn erl_exit(int n, char*, ...);
__decl_noreturn void __noreturn erl_exit_flush_async(int n, char*, ...);
void erl_error(char*, va_list);

/* copy.c */
Eterm copy_object(Eterm, Process*);

#if HALFWORD_HEAP
Uint size_object_rel(Eterm, Eterm*);
#  define size_object(A) size_object_rel(A,NULL)

Eterm copy_struct_rel(Eterm, Uint, Eterm**, ErlOffHeap*, Eterm* src_base, Eterm* dst_base);
#  define copy_struct(OBJ,SZ,HPP,OH) copy_struct_rel(OBJ,SZ,HPP,OH, NULL,NULL)

Eterm copy_shallow_rel(Eterm*, Uint, Eterm**, ErlOffHeap*, Eterm* src_base);
#  define copy_shallow(A,B,C,D) copy_shallow_rel(A,B,C,D,NULL)

#else /* !HALFWORD_HEAP */

Uint size_object(Eterm);
#  define size_object_rel(A,B) size_object(A)

Eterm copy_struct(Eterm, Uint, Eterm**, ErlOffHeap*);
#  define copy_struct_rel(OBJ,SZ,HPP,OH, SB,DB) copy_struct(OBJ,SZ,HPP,OH)

Eterm copy_shallow(Eterm*, Uint, Eterm**, ErlOffHeap*);
#  define copy_shallow_rel(A,B,C,D, BASE) copy_shallow(A,B,C,D)

#endif


void move_multi_frags(Eterm** hpp, ErlOffHeap*, ErlHeapFragment* first,
		      Eterm* refs, unsigned nrefs);

/* Utilities */
extern void erts_delete_nodes_monitors(Process *, ErtsProcLocks);
extern Eterm erts_monitor_nodes(Process *, Eterm, Eterm);
extern Eterm erts_processes_monitoring_nodes(Process *);
extern int erts_do_net_exits(DistEntry*, Eterm);
extern int distribution_info(int, void *);
extern int is_node_name_atom(Eterm a);

extern int erts_net_message(Port *, DistEntry *,
			    byte *, ErlDrvSizeT, byte *, ErlDrvSizeT);

extern void init_dist(void);
extern int stop_dist(void);

void erl_progressf(char* format, ...);

#ifdef MESS_DEBUG
void print_pass_through(int, byte*, int);
#endif

/* beam_emu.c */
int catchlevel(Process*);
void init_emulator(void);
void process_main(void);
Eterm build_stacktrace(Process* c_p, Eterm exc);
Eterm expand_error_value(Process* c_p, Uint freason, Eterm Value);
void erts_save_stacktrace(Process* p, struct StackTrace* s, int depth);

/* erl_init.c */

typedef struct {
    Eterm delay_time;
    int context_reds;
    int input_reds;
} ErtsModifiedTimings;

extern Export *erts_delay_trap;
extern int erts_modified_timing_level;
extern ErtsModifiedTimings erts_modified_timings[];
#define ERTS_USE_MODIFIED_TIMING() \
  (erts_modified_timing_level >= 0)
#define ERTS_MODIFIED_TIMING_DELAY \
  (erts_modified_timings[erts_modified_timing_level].delay_time)
#define ERTS_MODIFIED_TIMING_CONTEXT_REDS \
  (erts_modified_timings[erts_modified_timing_level].context_reds)
#define ERTS_MODIFIED_TIMING_INPUT_REDS \
  (erts_modified_timings[erts_modified_timing_level].input_reds)

extern int erts_no_line_info;
extern Eterm erts_error_logger_warnings;
extern int erts_initialized;
extern int erts_compat_rel;
extern int erts_use_sender_punish;
void erts_short_init(void);
void erl_start(int, char**);
void erts_usage(void);
Eterm erts_preloaded(Process* p);
/* erl_md5.c */

typedef struct {
    Uint32 state[4];		/* state (ABCD) */
    Uint32 count[2];		/* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];	/* input buffer */
} MD5_CTX;

void MD5Init(MD5_CTX *);
void MD5Update(MD5_CTX *, unsigned char *, unsigned int);
void MD5Final(unsigned char [16], MD5_CTX *);

/* ggc.c */


typedef struct {
    Uint garbage_collections;
    Uint reclaimed;
} ErtsGCInfo;

void erts_gc_info(ErtsGCInfo *gcip);
void erts_init_gc(void);
int erts_garbage_collect(Process*, int, Eterm*, int);
void erts_garbage_collect_hibernate(Process* p);
Eterm erts_gc_after_bif_call(Process* p, Eterm result, Eterm* regs, Uint arity);
void erts_garbage_collect_literals(Process* p, Eterm* literals,
				   Uint lit_size,
				   struct erl_off_heap_header* oh);
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(Process* p);

void erts_offset_off_heap(ErlOffHeap *, Sint, Eterm*, Eterm*);
void erts_offset_heap_ptr(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_offset_heap(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_free_heap_frags(Process* p);

/* io.c */

struct erl_drv_port_data_lock {
    erts_mtx_t mtx;
    erts_atomic_t refc;
};

typedef struct {
    char *name;
    char *driver_name;
} ErtsPortNames;

#define ERTS_SPAWN_DRIVER 1
#define ERTS_SPAWN_EXECUTABLE 2
#define ERTS_SPAWN_ANY (ERTS_SPAWN_DRIVER | ERTS_SPAWN_EXECUTABLE)

int erts_add_driver_entry(ErlDrvEntry *drv, DE_Handle *handle, int driver_list_locked);
void erts_destroy_driver(erts_driver_t *drv);
void erts_wake_process_later(Port*, Process*);
int erts_open_driver(erts_driver_t*, Eterm, char*, SysDriverOpts*, int *);
int erts_is_port_ioq_empty(Port *);
void erts_terminate_port(Port *);
void close_port(Eterm);
void init_io(void);
void cleanup_io(void);
void erts_do_exit_port(Port *, Eterm, Eterm);
void erts_port_command(Process *, Eterm, Port *, Eterm);
Eterm erts_port_control(Process*, Port*, Uint, Eterm);
int erts_write_to_port(Eterm caller_id, Port *p, Eterm list);
void print_port_info(int, void *, int);
void erts_raw_port_command(Port*, byte*, Uint);
void driver_report_exit(int, int);
LineBuf* allocate_linebuf(int);
int async_ready(Port *, void*);
Sint erts_test_next_port(int, Uint);
ErtsPortNames *erts_get_port_names(Eterm);
void erts_free_port_names(ErtsPortNames *);
Uint erts_port_ioq_size(Port *pp);
void erts_stale_drv_select(Eterm, ErlDrvEvent, int, int);
void erts_port_cleanup(Port *);
void erts_fire_port_monitor(Port *prt, Eterm ref);
#ifdef ERTS_SMP
void erts_smp_xports_unlock(Port *);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
void erts_lcnt_enable_io_lock_count(int enable);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_is_port_locked(Port *);
#endif

ERTS_GLB_INLINE void erts_smp_port_state_lock(Port*);
ERTS_GLB_INLINE void erts_smp_port_state_unlock(Port*);

ERTS_GLB_INLINE int erts_smp_port_trylock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_lock(Port *prt);
ERTS_GLB_INLINE void erts_smp_port_unlock(Port *prt);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_smp_port_state_lock(Port* prt)
{
#ifdef ERTS_SMP
    erts_smp_spin_lock(&prt->state_lck);
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_state_unlock(Port *prt)
{
#ifdef ERTS_SMP
    erts_smp_spin_unlock(&prt->state_lck);
#endif
}


ERTS_GLB_INLINE int
erts_smp_port_trylock(Port *prt)
{
    int res;

    ASSERT(erts_smp_atomic_read_nob(&prt->refc) > 0);
    erts_smp_atomic_inc_nob(&prt->refc);

#ifdef ERTS_SMP
    res = erts_smp_mtx_trylock(prt->lock);
    if (res == EBUSY) {
	erts_smp_atomic_dec_nob(&prt->refc);
    }
#else
    res = 0;
#endif

    return res;
}

ERTS_GLB_INLINE void
erts_smp_port_lock(Port *prt)
{
    ASSERT(erts_smp_atomic_read_nob(&prt->refc) > 0);
    erts_smp_atomic_inc_nob(&prt->refc);
#ifdef ERTS_SMP
    erts_smp_mtx_lock(prt->lock);
#endif
}

ERTS_GLB_INLINE void
erts_smp_port_unlock(Port *prt)
{
    erts_aint_t refc;
#ifdef ERTS_SMP
    erts_smp_mtx_unlock(prt->lock);
#endif
    refc = erts_smp_atomic_dec_read_nob(&prt->refc);
    ASSERT(refc >= 0);
    if (refc == 0)
	erts_port_cleanup(prt);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#define ERTS_INVALID_PORT_OPT(PP, ID, FLGS) \
  (!(PP) || ((PP)->status & (FLGS)) || (PP)->id != (ID))

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
ERTS_GLB_INLINE Port*erts_drvport2port(ErlDrvPort);
ERTS_GLB_INLINE Port*erts_drvportid2port(Eterm);
ERTS_GLB_INLINE Uint32 erts_portid2status(Eterm id);
ERTS_GLB_INLINE int erts_is_port_alive(Eterm id);
ERTS_GLB_INLINE int erts_is_valid_tracer_port(Eterm id);
ERTS_GLB_INLINE void erts_port_status_bandor_set(Port *, Uint32, Uint32);
ERTS_GLB_INLINE void erts_port_status_band_set(Port *, Uint32);
ERTS_GLB_INLINE void erts_port_status_bor_set(Port *, Uint32);
ERTS_GLB_INLINE void erts_port_status_set(Port *, Uint32);
ERTS_GLB_INLINE Uint32 erts_port_status_get(Port *);

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

    erts_smp_port_state_lock(prt);
    if (ERTS_INVALID_PORT_OPT(prt, id, sflgs)) {
	erts_smp_port_state_unlock(prt);
	prt = NULL;
    }
    else {
	erts_smp_atomic_inc_nob(&prt->refc);
	erts_smp_port_state_unlock(prt);

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
	ERTS_SMP_LC_ASSERT(prt->id == id);
	/* ... but status may have... */
	if (prt->status & sflgs) {
	    erts_smp_port_unlock(prt); /* Also decrements refc... */
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
erts_drvport2port(ErlDrvPort drvport)
{
    int ix = (int) drvport;
    if (ix < 0 || erts_max_ports <= ix)
	return NULL;
    if (erts_port[ix].status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	return NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(&erts_port[ix]));
    return &erts_port[ix];
}

ERTS_GLB_INLINE Port*
erts_drvportid2port(Eterm id)
{
    int ix;
    if (is_not_internal_port(id))
	return NULL;
    ix = (int) internal_port_index(id);
    if (erts_max_ports <= ix)
	return NULL;
    if (erts_port[ix].status & ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP)
	return NULL;
    if (erts_port[ix].id != id)
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
	Uint32 status;
	int ix = internal_port_index(id);
	if (erts_max_ports <= ix)
	    return ERTS_PORT_SFLG_INVALID;
	erts_smp_port_state_lock(&erts_port[ix]);
	if (erts_port[ix].id == id)
	    status = erts_port[ix].status;
	else
	    status = ERTS_PORT_SFLG_INVALID;
	erts_smp_port_state_unlock(&erts_port[ix]);
	return status;
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

ERTS_GLB_INLINE void erts_port_status_bandor_set(Port *prt,
						 Uint32 band_status,
						 Uint32 bor_status)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    erts_smp_port_state_lock(prt);
    prt->status &= band_status;
    prt->status |= bor_status;
    erts_smp_port_state_unlock(prt);
}

ERTS_GLB_INLINE void erts_port_status_band_set(Port *prt, Uint32 status)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    erts_smp_port_state_lock(prt);
    prt->status &= status;
    erts_smp_port_state_unlock(prt);
}

ERTS_GLB_INLINE void erts_port_status_bor_set(Port *prt, Uint32 status)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    erts_smp_port_state_lock(prt);
    prt->status |= status;
    erts_smp_port_state_unlock(prt);
}

ERTS_GLB_INLINE void erts_port_status_set(Port *prt, Uint32 status)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
    erts_smp_port_state_lock(prt);
    prt->status = status;
    erts_smp_port_state_unlock(prt);
}

ERTS_GLB_INLINE Uint32 erts_port_status_get(Port *prt)
{
    Uint32 res;
    erts_smp_port_state_lock(prt);
    res = prt->status;
    erts_smp_port_state_unlock(prt);
    return res;
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* erl_drv_thread.c */
void erl_drv_thr_init(void);

/* time.c */

/* utils.c */

typedef struct {
#ifdef DEBUG
    int smp_api;
#endif
    union {
	Uint64 not_atomic;
#ifdef ARCH_64
	erts_atomic_t atomic;
#else
	erts_dw_atomic_t atomic;
#endif
    } counter;
} erts_interval_t;

void erts_interval_init(erts_interval_t *);
void erts_smp_interval_init(erts_interval_t *);
Uint64 erts_step_interval_nob(erts_interval_t *);
Uint64 erts_step_interval_relb(erts_interval_t *);
Uint64 erts_smp_step_interval_nob(erts_interval_t *);
Uint64 erts_smp_step_interval_relb(erts_interval_t *);
Uint64 erts_ensure_later_interval_nob(erts_interval_t *, Uint64);
Uint64 erts_ensure_later_interval_acqb(erts_interval_t *, Uint64);
Uint64 erts_smp_ensure_later_interval_nob(erts_interval_t *, Uint64);
Uint64 erts_smp_ensure_later_interval_acqb(erts_interval_t *, Uint64);
#ifdef ARCH_32
ERTS_GLB_INLINE Uint64 erts_interval_dw_aint_to_val__(erts_dw_aint_t *);
#endif
ERTS_GLB_INLINE Uint64 erts_current_interval_nob__(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_acqb__(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_nob(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_acqb(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_smp_current_interval_nob(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_smp_current_interval_acqb(erts_interval_t *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ARCH_32

ERTS_GLB_INLINE Uint64
erts_interval_dw_aint_to_val__(erts_dw_aint_t *dw)
{
#ifdef ETHR_SU_DW_NAINT_T__
    return (Uint64) dw->dw_sint;
#else
    Uint64 res;
    res = (Uint64) ((Uint32) dw->sint[ERTS_DW_AINT_HIGH_WORD]);
    res <<= 32;
    res |= (Uint64) ((Uint32) dw->sint[ERTS_DW_AINT_LOW_WORD]);
    return res;
#endif
}

#endif

ERTS_GLB_INLINE Uint64
erts_current_interval_nob__(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_read_nob(&icp->counter.atomic);
#else
    erts_dw_aint_t dw;
    erts_dw_atomic_read_nob(&icp->counter.atomic, &dw);
    return erts_interval_dw_aint_to_val__(&dw);
#endif
}

ERTS_GLB_INLINE Uint64
erts_current_interval_acqb__(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_read_acqb(&icp->counter.atomic);
#else
    erts_dw_aint_t dw;
    erts_dw_atomic_read_acqb(&icp->counter.atomic, &dw);
    return erts_interval_dw_aint_to_val__(&dw);
#endif
}

ERTS_GLB_INLINE Uint64
erts_current_interval_nob(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return erts_current_interval_nob__(icp);
}

ERTS_GLB_INLINE Uint64
erts_current_interval_acqb(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return erts_current_interval_acqb__(icp);
}

ERTS_GLB_INLINE Uint64
erts_smp_current_interval_nob(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return erts_current_interval_nob__(icp);
#else
    return icp->counter.not_atomic;
#endif
}

ERTS_GLB_INLINE Uint64
erts_smp_current_interval_acqb(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return erts_current_interval_acqb__(icp);
#else
    return icp->counter.not_atomic;
#endif
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused);

void erts_cleanup_offheap(ErlOffHeap *offheap);

int erts_fit_in_bits_int64(Sint64);
int erts_fit_in_bits_int32(Sint32);
int list_length(Eterm);
Export* erts_find_function(Eterm, Eterm, unsigned int, ErtsCodeIndex);
int erts_is_builtin(Eterm, Eterm, int);
Uint32 make_broken_hash(Eterm);
Uint32 block_hash(byte *, unsigned, Uint32);
Uint32 make_hash2(Eterm);
Uint32 make_hash(Eterm);


Eterm erts_bld_atom(Uint **hpp, Uint *szp, char *str);
Eterm erts_bld_uint(Uint **hpp, Uint *szp, Uint ui);
Eterm erts_bld_uword(Uint **hpp, Uint *szp, UWord uw);
Eterm erts_bld_uint64(Uint **hpp, Uint *szp, Uint64 ui64);
Eterm erts_bld_sint64(Uint **hpp, Uint *szp, Sint64 si64);
Eterm erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr);
Eterm erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...);
Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[]);
Eterm erts_bld_string_n(Uint **hpp, Uint *szp, const char *str, Sint len);
#define erts_bld_string(hpp,szp,str) erts_bld_string_n(hpp,szp,str,strlen(str))
Eterm erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[]);
Eterm erts_bld_2tup_list(Uint **hpp, Uint *szp,
			 Sint length, Eterm terms1[], Uint terms2[]);
Eterm
erts_bld_atom_uint_2tup_list(Uint **hpp, Uint *szp,
			     Sint length, Eterm atoms[], Uint uints[]);
Eterm
erts_bld_atom_2uint_3tup_list(Uint **hpp, Uint *szp, Sint length,
			      Eterm atoms[], Uint uints1[], Uint uints2[]);

Eterm store_external_or_ref_in_proc_(Process *, Eterm);
Eterm store_external_or_ref_(Uint **, ErlOffHeap*, Eterm);

#define NC_HEAP_SIZE(NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? 0 : (thing_arityval(*boxed_val((NC))) + 1))
#define STORE_NC(Hpp, ETpp, NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_((Hpp), (ETpp), (NC)))
#define STORE_NC_IN_PROC(Pp, NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_in_proc_((Pp), (NC)))

void erts_init_utils(void);
void erts_init_utils_mem(void);

erts_dsprintf_buf_t *erts_create_tmp_dsbuf(Uint);
void erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *);

#if HALFWORD_HEAP
int eq_rel(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base);
#  define eq(A,B) eq_rel(A,NULL,B,NULL)
#else
int eq(Eterm, Eterm);
#  define eq_rel(A,A_BASE,B,B_BASE) eq(A,B)
#endif

#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

#if HALFWORD_HEAP
Sint cmp_rel(Eterm, Eterm*, Eterm, Eterm*);
#define CMP(A,B) cmp_rel(A,NULL,B,NULL)
#else
Sint cmp(Eterm, Eterm);
#define cmp_rel(A,A_BASE,B,B_BASE) cmp(A,B)
#define CMP(A,B) cmp(A,B)
#endif
#define cmp_lt(a,b)	(CMP((a),(b)) < 0)
#define cmp_le(a,b)	(CMP((a),(b)) <= 0)
#define cmp_eq(a,b)	(CMP((a),(b)) == 0)
#define cmp_ne(a,b)	(CMP((a),(b)) != 0)
#define cmp_ge(a,b)	(CMP((a),(b)) >= 0)
#define cmp_gt(a,b)	(CMP((a),(b)) > 0)

#define CMP_LT(a,b)	((a) != (b) && cmp_lt((a),(b)))
#define CMP_GE(a,b)	((a) == (b) || cmp_ge((a),(b)))
#define CMP_EQ(a,b)	((a) == (b) || cmp_eq((a),(b)))
#define CMP_NE(a,b)	((a) != (b) && cmp_ne((a),(b)))

/* duplicates from big.h */
int term_to_Uint(Eterm term, Uint *up);
int term_to_UWord(Eterm, UWord*);

#ifdef HAVE_ERTS_NOW_CPU
extern int erts_cpu_timestamp;
#endif
/* erl_bif_chksum.c */
void erts_init_bif_chksum(void);
/* erl_bif_re.c */
void erts_init_bif_re(void);
Sint erts_re_set_loop_limit(Sint limit);
/* erl_bif_binary.c */
void erts_init_bif_binary(void);
Sint erts_binary_set_loop_limit(Sint limit);

/* erl_unicode.c */
void erts_init_unicode(void);
Sint erts_unicode_set_loop_limit(Sint limit);

void erts_native_filename_put(Eterm ioterm, int encoding, byte *p) ;
Sint erts_native_filename_need(Eterm ioterm, int encoding);
void erts_copy_utf8_to_utf16_little(byte *target, byte *bytes, int num_chars);
int erts_analyze_utf8(byte *source, Uint size, 
			byte **err_pos, Uint *num_chars, int *left);
char *erts_convert_filename_to_native(Eterm name, ErtsAlcType_t alloc_type, int allow_empty);

#define ERTS_UTF8_OK 0
#define ERTS_UTF8_INCOMPLETE 1
#define ERTS_UTF8_ERROR 2
#define ERTS_UTF8_ANALYZE_MORE 3

/* erl_trace.c */
void erts_init_trace(void);
void erts_trace_check_exiting(Eterm exiting);
Eterm erts_set_system_seq_tracer(Process *c_p,
				 ErtsProcLocks c_p_locks,
				 Eterm new);
Eterm erts_get_system_seq_tracer(void);
void erts_change_default_tracing(int setflags, Uint *flagsp, Eterm *tracerp);
void erts_get_default_tracing(Uint *flagsp, Eterm *tracerp);
void erts_set_system_monitor(Eterm monitor);
Eterm erts_get_system_monitor(void);

#ifdef ERTS_SMP
void erts_check_my_tracer_proc(Process *);
void erts_block_sys_msg_dispatcher(void);
void erts_release_sys_msg_dispatcher(void);
void erts_foreach_sys_msg_in_q(void (*func)(Eterm,
					    Eterm,
					    Eterm,
					    ErlHeapFragment *));
void erts_queue_error_logger_message(Eterm, Eterm, ErlHeapFragment *);
#endif

void erts_send_sys_msg_proc(Eterm, Eterm, Eterm, ErlHeapFragment *);
void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm);
Uint32 erts_call_trace(Process *p, BeamInstr mfa[], Binary *match_spec, Eterm* args,
		       int local, Eterm *tracer_pid);
void erts_trace_return(Process* p, BeamInstr* fi, Eterm retval, Eterm *tracer_pid);
void erts_trace_exception(Process* p, BeamInstr mfa[], Eterm class, Eterm value,
			  Eterm *tracer);
void erts_trace_return_to(Process *p, BeamInstr *pc);
void trace_sched(Process*, Eterm);
void trace_proc(Process*, Process*, Eterm, Eterm);
void trace_proc_spawn(Process*, Eterm pid, Eterm mod, Eterm func, Eterm args);
void save_calls(Process *p, Export *);
void trace_gc(Process *p, Eterm what);
/* port tracing */
void trace_virtual_sched(Process*, Eterm);
void trace_sched_ports(Port *pp, Eterm);
void trace_sched_ports_where(Port *pp, Eterm, Eterm);
void trace_port(Port *, Eterm what, Eterm data);
void trace_port_open(Port *, Eterm calling_pid, Eterm drv_name);

/* system_profile */
void erts_set_system_profile(Eterm profile);
Eterm erts_get_system_profile(void);
void profile_scheduler(Eterm scheduler_id, Eterm);
void profile_scheduler_q(Eterm scheduler_id, Eterm state, Eterm no_schedulers, Uint Ms, Uint s, Uint us);
void profile_runnable_proc(Process* p, Eterm status);
void profile_runnable_port(Port* p, Eterm status);
void erts_system_profile_setup_active_schedulers(void);

/* system_monitor */
void monitor_long_gc(Process *p, Uint time);
void monitor_large_heap(Process *p);
void monitor_generic(Process *p, Eterm type, Eterm spec);
Uint erts_trace_flag2bit(Eterm flag);
int erts_trace_flags(Eterm List, 
		 Uint *pMask, Eterm *pTracer, int *pCpuTimestamp);
Eterm erts_bif_trace(int bif_index, Process* p, Eterm* args, BeamInstr *I);

#ifdef ERTS_SMP
void erts_send_pending_trace_msgs(ErtsSchedulerData *esdp);
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)				\
do {									\
    if ((ESDP)->pending_trace_msgs)					\
	erts_send_pending_trace_msgs((ESDP));				\
} while (0)
#else
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)
#endif

void bin_write(int, void*, byte*, size_t);
int intlist_to_buf(Eterm, char*, int); /* most callers pass plain char*'s */

struct Sint_buf {
#if defined(ARCH_64) && !HALFWORD_HEAP
    char s[22];
#else
    char s[12];
#endif
};	
char* Sint_to_buf(Sint, struct Sint_buf*);

#define ERTS_IOLIST_OK 0
#define ERTS_IOLIST_OVERFLOW 1
#define ERTS_IOLIST_TYPE 2

Eterm buf_to_intlist(Eterm**, char*, size_t, Eterm); /* most callers pass plain char*'s */
int io_list_to_buf(Eterm, char*, int);
int io_list_to_buf2(Eterm, char*, int);
int erts_iolist_size(Eterm, Uint *);
int is_string(Eterm);
void erl_at_exit(void (*) (void*), void*);
Eterm collect_memory(Process *);
void dump_memory_to_fd(int);
int dump_memory_data(const char *);

Eterm erts_mixed_plus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_minus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_times(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_rem(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_band(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bor(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bxor(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bnot(Process* p, Eterm arg);

Eterm erts_gc_mixed_plus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_minus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_times(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_rem(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_band(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bxor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bnot(Process* p, Eterm* reg, Uint live);

Eterm erts_gc_length_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bit_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_byte_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_abs_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_float_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_round_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_trunc_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_binary_part_3(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_binary_part_2(Process* p, Eterm* reg, Uint live);

Uint erts_current_reductions(Process* current, Process *p);

int erts_print_system_version(int to, void *arg, Process *c_p);

int erts_hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg);
#define seq_trace_output(token, msg, type, receiver, process) \
seq_trace_output_generic((token), (msg), (type), (receiver), (process), NIL)
#define seq_trace_output_exit(token, msg, type, receiver, exitfrom) \
seq_trace_output_generic((token), (msg), (type), (receiver), NULL, (exitfrom))
void seq_trace_output_generic(Eterm token, Eterm msg, Uint type, 
			      Eterm receiver, Process *process, Eterm exitfrom);

int seq_trace_update_send(Process *process);

Eterm erts_seq_trace(Process *process, 
		     Eterm atom_type, Eterm atom_true_or_false, 
		     int build_result);

struct trace_pattern_flags {
    unsigned int breakpoint : 1; /* Set if any other is set */
    unsigned int local      : 1; /* Local call trace breakpoint */
    unsigned int meta       : 1; /* Metadata trace breakpoint */
    unsigned int call_count : 1; /* Fast call count breakpoint */
    unsigned int call_time  : 1; /* Fast call time breakpoint */
};
extern const struct trace_pattern_flags erts_trace_pattern_flags_off;
extern int erts_call_time_breakpoint_tracing;
int erts_set_trace_pattern(Eterm* mfa, int specified, 
			   Binary* match_prog_set, Binary *meta_match_prog_set,
			   int on, struct trace_pattern_flags,
			   Eterm meta_tracer_pid);
void
erts_get_default_trace_pattern(int *trace_pattern_is_on,
			       Binary **match_spec,
			       Binary **meta_match_spec,
			       struct trace_pattern_flags *trace_pattern_flags,
			       Eterm *meta_tracer_pid);
int erts_is_default_trace_enabled(void);
void erts_bif_trace_init(void);

/*
** Call_trace uses this API for the parameter matching functions
*/

#define MatchSetRef(MPSP) 			\
do {						\
    if ((MPSP) != NULL) {			\
	erts_refc_inc(&(MPSP)->refc, 1);	\
    }						\
} while (0)

#define MatchSetUnref(MPSP)					\
do {								\
    if (((MPSP) != NULL) && erts_refc_dectest(&(MPSP)->refc, 0) <= 0) { \
	erts_bin_free(MPSP);					\
    }								\
} while(0)

#define MatchSetGetSource(MPSP) erts_match_set_get_source(MPSP)

extern Binary *erts_match_set_compile(Process *p, Eterm matchexpr);
Eterm erts_match_set_lint(Process *p, Eterm matchexpr); 
extern void erts_match_set_release_result(Process* p);

enum erts_pam_run_flags {
    ERTS_PAM_TMP_RESULT=0,
    ERTS_PAM_COPY_RESULT=1,
    ERTS_PAM_CONTIGUOUS_TUPLE=2
};
extern Eterm erts_match_set_run(Process *p, Binary *mpsp, 
				Eterm *args, int num_args,
				enum erts_pam_run_flags in_flags,
				Uint32 *return_flags);
extern Eterm erts_match_set_get_source(Binary *mpsp);
extern void erts_match_prog_foreach_offheap(Binary *b,
					    void (*)(ErlOffHeap *, void *),
					    void *);

#define MATCH_SET_RETURN_TRACE    (0x1) /* return trace requested */
#define MATCH_SET_RETURN_TO_TRACE (0x2) /* Misleading name, it is not actually
					   set by the match program, but by the
					   breakpoint functions */
#define MATCH_SET_EXCEPTION_TRACE (0x4) /* exception trace requested */
#define MATCH_SET_RX_TRACE (MATCH_SET_RETURN_TRACE|MATCH_SET_EXCEPTION_TRACE)
/*
 * Flag values when tracing bif
 * Future note: flag field is 8 bits
 */
#define BIF_TRACE_AS_LOCAL      (0x1)
#define BIF_TRACE_AS_GLOBAL     (0x2)
#define BIF_TRACE_AS_META       (0x4)
#define BIF_TRACE_AS_CALL_TIME  (0x8)

extern erts_driver_t vanilla_driver;
extern erts_driver_t spawn_driver;
extern erts_driver_t fd_driver;

/* Should maybe be placed in erl_message.h, but then we get an include mess. */
ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap_state(Uint size,
			      ErlHeapFragment **bpp,
			      ErlOffHeap **ohpp,
			      Process *receiver,
			      ErtsProcLocks *receiver_locks,
			      erts_aint32_t *statep);

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			ErtsProcLocks *receiver_locks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

/*
 * NOTE: erts_alloc_message_heap() releases msg q and status
 *       lock on receiver without ensuring that other locks are
 *       held. User is responsible to ensure that the receiver
 *       pointer cannot become invalid until after message has
 *       been passed. This is normal done either by increasing
 *       reference count on process (preferred) or by holding
 *       main or link lock over the whole message passing
 *       operation.
 */

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap_state(Uint size,
			      ErlHeapFragment **bpp,
			      ErlOffHeap **ohpp,
			      Process *receiver,
			      ErtsProcLocks *receiver_locks,
			      erts_aint32_t *statep)
{
    Eterm *hp;
    erts_aint32_t state;
#ifdef ERTS_SMP
    int locked_main = 0;
    state = erts_smp_atomic32_read_acqb(&receiver->state);
    if (statep)
	*statep = state;
    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))
	goto allocate_in_mbuf;
#endif

    if (size > (Uint) INT_MAX)
	erl_exit(ERTS_ABORT_EXIT, "HUGE size (%beu)\n", size);

    if (
#if defined(ERTS_SMP)
	*receiver_locks & ERTS_PROC_LOCK_MAIN
#else
	1
#endif
	) {
#ifdef ERTS_SMP
    try_allocate_on_heap:
#endif
	state = erts_smp_atomic32_read_nob(&receiver->state);
	if (statep)
	    *statep = state;
	if ((state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))
	    || HEAP_LIMIT(receiver) - HEAP_TOP(receiver) <= size) {
#ifdef ERTS_SMP
	    if (locked_main) {
		*receiver_locks &= ~ERTS_PROC_LOCK_MAIN;
		erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_MAIN);
	    }
#endif
	    goto allocate_in_mbuf;
	}
	hp = HEAP_TOP(receiver);
	HEAP_TOP(receiver) = hp + size;
	*bpp = NULL;
	*ohpp = &MSO(receiver);
    }
#ifdef ERTS_SMP
    else if (erts_smp_proc_trylock(receiver, ERTS_PROC_LOCK_MAIN) == 0) {
	locked_main = 1;
	*receiver_locks |= ERTS_PROC_LOCK_MAIN;
	goto try_allocate_on_heap;
    }
#endif
    else {
	ErlHeapFragment *bp;
    allocate_in_mbuf:
	bp = new_message_buffer(size);
	hp = bp->mem;
	*bpp = bp;
	*ohpp = &bp->off_heap;
    }

    return hp;
}

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			ErtsProcLocks *receiver_locks)
{
    return erts_alloc_message_heap_state(size, bpp, ohpp, receiver,
					 receiver_locks, NULL);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#if !HEAP_ON_C_STACK
#  if defined(DEBUG)
#    define DeclareTmpHeap(VariableName,Size,Process) \
       Eterm *VariableName = erts_debug_allocate_tmp_heap(Size,Process)
#    define DeclareTypedTmpHeap(Type,VariableName,Process)		\
      Type *VariableName = (Type *) erts_debug_allocate_tmp_heap(sizeof(Type)/sizeof(Eterm),Process)
#    define DeclareTmpHeapNoproc(VariableName,Size) \
       Eterm *VariableName = erts_debug_allocate_tmp_heap(Size,NULL)
#    define UseTmpHeap(Size,Proc) \
       do { \
         erts_debug_use_tmp_heap((Size),(Proc)); \
       } while (0)
#    define UnUseTmpHeap(Size,Proc) \
       do { \
         erts_debug_unuse_tmp_heap((Size),(Proc)); \
       } while (0)
#    define UseTmpHeapNoproc(Size) \
       do { \
         erts_debug_use_tmp_heap(Size,NULL); \
       } while (0)
#    define UnUseTmpHeapNoproc(Size) \
       do { \
         erts_debug_unuse_tmp_heap(Size,NULL); \
       } while (0)
#  else
#    define DeclareTmpHeap(VariableName,Size,Process) \
       Eterm *VariableName = (ERTS_PROC_GET_SCHDATA(Process)->tmp_heap)+(ERTS_PROC_GET_SCHDATA(Process)->num_tmp_heap_used)
#    define DeclareTypedTmpHeap(Type,VariableName,Process)		\
      Type *VariableName = (Type *) (ERTS_PROC_GET_SCHDATA(Process)->tmp_heap)+(ERTS_PROC_GET_SCHDATA(Process)->num_tmp_heap_used)
#    define DeclareTmpHeapNoproc(VariableName,Size) \
       Eterm *VariableName = (erts_get_scheduler_data()->tmp_heap)+(erts_get_scheduler_data()->num_tmp_heap_used)
#    define UseTmpHeap(Size,Proc) \
       do { \
         ERTS_PROC_GET_SCHDATA(Proc)->num_tmp_heap_used += (Size); \
       } while (0)
#    define UnUseTmpHeap(Size,Proc) \
       do { \
         ERTS_PROC_GET_SCHDATA(Proc)->num_tmp_heap_used -= (Size); \
       } while (0)
#    define UseTmpHeapNoproc(Size) \
       do { \
         erts_get_scheduler_data()->num_tmp_heap_used += (Size); \
       } while (0)
#    define UnUseTmpHeapNoproc(Size) \
       do { \
         erts_get_scheduler_data()->num_tmp_heap_used -= (Size); \
       } while (0)


#  endif

#else
#  define DeclareTmpHeap(VariableName,Size,Process) \
     Eterm VariableName[Size]
#  define DeclareTypedTmpHeap(Type,VariableName,Process)	\
     Type VariableName[1]
#  define DeclareTmpHeapNoproc(VariableName,Size) \
     Eterm VariableName[Size]
#  define UseTmpHeap(Size,Proc) /* Nothing */
#  define UnUseTmpHeap(Size,Proc) /* Nothing */
#  define UseTmpHeapNoproc(Size) /* Nothing */
#  define UnUseTmpHeapNoproc(Size) /* Nothing */
#endif /* HEAP_ON_C_STACK */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include "dtrace-wrapper.h"

ERTS_GLB_INLINE void
dtrace_pid_str(Eterm pid, char *process_buf)
{
    erts_snprintf(process_buf, DTRACE_TERM_BUF_SIZE, "<%lu.%lu.%lu>",
                  pid_channel_no(pid),
                  pid_number(pid),
                  pid_serial(pid));
}

ERTS_GLB_INLINE void
dtrace_proc_str(Process *process, char *process_buf)
{
    dtrace_pid_str(process->id, process_buf);
}

ERTS_GLB_INLINE void
dtrace_port_str(Port *port, char *port_buf)
{
    erts_snprintf(port_buf, DTRACE_TERM_BUF_SIZE, "#Port<%lu.%lu>",
                  port_channel_no(port->id),
                  port_number(port->id));
}

ERTS_GLB_INLINE void
dtrace_fun_decode(Process *process,
                  Eterm module, Eterm function, int arity,
                  char *process_buf, char *mfa_buf)
{
    if (process_buf) {
        dtrace_proc_str(process, process_buf);
    }

    erts_snprintf(mfa_buf, DTRACE_TERM_BUF_SIZE, "%T:%T/%d",
                  module, function, arity);
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__GLOBAL_H__ */
