/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

#ifndef __PROCESS_H__
#define __PROCESS_H__

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#if (defined(ERL_PROCESS_C__) \
     || defined(ERL_PORT_TASK_C__) \
     || (ERTS_GLB_INLINE_INCL_FUNC_DEF \
	 && defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)))
#define ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif

/* #define ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC */

#if !defined(ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC) && defined(DEBUG)
#  define ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
#endif

typedef struct process Process;

#include "sys.h"

#define ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#include "erl_process_lock.h" /* Only pull out important types... */
#undef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__

#include "erl_vm.h"
#include "erl_smp.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#include "erl_time.h"
#include "erl_atom_table.h"
#include "external.h"

#ifdef HIPE
#include "hipe_process.h"
#endif

struct ErtsNodesMonitor_;
struct port;

#define ERTS_MAX_NO_OF_SCHEDULERS 1024

#define ERTS_DEFAULT_MAX_PROCESSES (1 << 15)

#define ERTS_HEAP_ALLOC(Type, Size)					\
     erts_alloc((Type), (Size))

#define ERTS_HEAP_REALLOC(Type, Ptr, OldSize, NewSize)			\
     erts_realloc((Type), (Ptr), (NewSize))

#define ERTS_HEAP_FREE(Type, Ptr, Size)					\
     erts_free((Type), (Ptr))

#define INITIAL_MOD 0
#define INITIAL_FUN 1
#define INITIAL_ARI 2

#include "export.h"

struct saved_calls {
   int len;
   int n;
   int cur;
   Export *ct[1];
};

extern Export exp_send, exp_receive, exp_timeout;
extern Uint erts_no_schedulers;
extern Uint erts_no_run_queues;
extern int erts_sched_thread_suggested_stack_size;
#define ERTS_SCHED_THREAD_MIN_STACK_SIZE 4	/* Kilo words */
#define ERTS_SCHED_THREAD_MAX_STACK_SIZE 8192	/* Kilo words */

#ifdef ERTS_SMP
#include "erl_bits.h"
#endif

/* process priorities */
#define PRIORITY_MAX          0
#define PRIORITY_HIGH         1
#define PRIORITY_NORMAL       2
#define PRIORITY_LOW          3
#define ERTS_NO_PROC_PRIO_LEVELS      4

#define ERTS_PORT_PRIO_LEVEL ERTS_NO_PROC_PRIO_LEVELS

#define ERTS_RUNQ_FLGS_PROCS_QMASK \
  ((((Uint32) 1) << ERTS_NO_PROC_PRIO_LEVELS) - 1)

#define ERTS_NO_PRIO_LEVELS (ERTS_NO_PROC_PRIO_LEVELS + 1)
#define ERTS_RUNQ_FLGS_MIGRATE_QMASK \
  ((((Uint32) 1) << ERTS_NO_PRIO_LEVELS) - 1)

#define ERTS_RUNQ_FLGS_EMIGRATE_SHFT \
  ERTS_NO_PROC_PRIO_LEVELS
#define ERTS_RUNQ_FLGS_IMMIGRATE_SHFT \
  (ERTS_RUNQ_FLGS_EMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EVACUATE_SHFT \
  (ERTS_RUNQ_FLGS_IMMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_MIGRATE_QMASK << ERTS_RUNQ_FLGS_EMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_IMMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_MIGRATE_QMASK << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_EVACUATE_QMASK \
  (ERTS_RUNQ_FLGS_MIGRATE_QMASK << ERTS_RUNQ_FLGS_EVACUATE_SHFT)

#define ERTS_RUNQ_FLG_BASE2 \
  (ERTS_RUNQ_FLGS_EVACUATE_SHFT + ERTS_NO_PRIO_LEVELS)

#define ERTS_RUNQ_FLG_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 0))
#define ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 1))
#define ERTS_RUNQ_FLG_SUSPENDED \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 2))
#define ERTS_RUNQ_FLG_SHARED_RUNQ \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 3))
#define ERTS_RUNQ_FLG_CHK_CPU_BIND \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 4))
#define ERTS_RUNQ_FLG_INACTIVE \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 5))

#define ERTS_RUNQ_FLGS_MIGRATION_QMASKS	\
  (ERTS_RUNQ_FLGS_EMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_IMMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_EVACUATE_QMASK)
#define ERTS_RUNQ_FLGS_MIGRATION_INFO \
  (ERTS_RUNQ_FLGS_MIGRATION_QMASKS \
   | ERTS_RUNQ_FLG_INACTIVE \
   | ERTS_RUNQ_FLG_OUT_OF_WORK \
   | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK)

#define ERTS_RUNQ_FLG_EMIGRATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_EMIGRATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_EMIGRATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_EMIGRATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_EMIGRATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_EMIGRATE((PRIO)))

#define ERTS_RUNQ_FLG_IMMIGRATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_IMMIGRATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_IMMIGRATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_IMMIGRATE((PRIO)))

#define ERTS_RUNQ_FLG_EVACUATE(PRIO) \
  (((Uint32) 1) << (ERTS_RUNQ_FLGS_EVACUATE_SHFT + (PRIO)))
#define ERTS_CHK_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) & ERTS_RUNQ_FLG_EVACUATE((PRIO)))
#define ERTS_SET_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) |= ERTS_RUNQ_FLG_EVACUATE((PRIO)))
#define ERTS_UNSET_RUNQ_FLG_EVACUATE(FLGS, PRIO) \
  ((FLGS) &= ~ERTS_RUNQ_FLG_EVACUATE((PRIO)))

#define ERTS_RUNQ_IFLG_SUSPENDED		(((erts_aint32_t) 1) << 0)
#define ERTS_RUNQ_IFLG_NONEMPTY			(((erts_aint32_t) 1) << 1)


#ifdef DEBUG
#  if defined(ARCH_64) && !HALFWORD_HEAP
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
       (*((char **) &(RQP)) = (char *) (0xdeadbeefdead0003 | ((N) << 4)))
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP) \
do { \
    ASSERT((RQP) != NULL); \
    ASSERT(((((Uint) (RQP)) & ((Uint) 0x3))) == ((Uint) 0)); \
    ASSERT((((Uint) (RQP)) & ~((Uint) 0xffff)) != ((Uint) 0xdeadbeefdead0000));\
} while (0)
#  else
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
       (*((char **) &(RQP)) = (char *) (0xdead0003 | ((N) << 4)))
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP) \
do { \
    ASSERT((RQP) != NULL); \
    ASSERT(((((UWord) (RQP)) & ((UWord) 1))) == ((UWord) 0)); \
    ASSERT((((UWord) (RQP)) & ~((UWord) 0xffff)) != ((UWord) 0xdead0000)); \
} while (0)
#  endif
#else
#  define ERTS_DBG_SET_INVALID_RUNQP(RQP, N)
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP)
#endif

typedef enum {
    ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_DONE,
    ERTS_SCHDLR_SSPND_YIELD_RESTART,
    ERTS_SCHDLR_SSPND_YIELD_DONE,
    ERTS_SCHDLR_SSPND_EINVAL
} ErtsSchedSuspendResult;

typedef enum {
    ERTS_MIGRATE_SUCCESS,
    ERTS_MIGRATE_FAILED_NOT_IN_RUNQ,
    ERTS_MIGRATE_FAILED_RUNQ_CHANGED,
    ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED
} ErtsMigrateResult;

#define ERTS_SSI_FLG_SLEEPING		(((erts_aint32_t) 1) << 0)
#define ERTS_SSI_FLG_POLL_SLEEPING 	(((erts_aint32_t) 1) << 1)
#define ERTS_SSI_FLG_TSE_SLEEPING 	(((erts_aint32_t) 1) << 2)
#define ERTS_SSI_FLG_WAITING		(((erts_aint32_t) 1) << 3)
#define ERTS_SSI_FLG_SUSPENDED	 	(((erts_aint32_t) 1) << 4)

#define ERTS_SSI_FLGS_SLEEP_TYPE			\
 (ERTS_SSI_FLG_TSE_SLEEPING|ERTS_SSI_FLG_POLL_SLEEPING)

#define ERTS_SSI_FLGS_SLEEP				\
 (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLGS_SLEEP_TYPE)

#define ERTS_SSI_FLGS_ALL				\
 (ERTS_SSI_FLGS_SLEEP					\
  | ERTS_SSI_FLG_WAITING				\
  | ERTS_SSI_FLG_SUSPENDED)

#define ERTS_SCHED_NEED_BLOCKABLE_AUX_WORK

#define ERTS_SSI_AUX_WORK_CHECK_CHILDREN	(((erts_aint32_t) 1) << 0)
#define ERTS_SSI_AUX_WORK_MISC			(((erts_aint32_t) 1) << 1)

#define ERTS_SSI_BLOCKABLE_AUX_WORK_MASK \
  (ERTS_SSI_AUX_WORK_CHECK_CHILDREN \
   | ERTS_SSI_AUX_WORK_MISC)
#define ERTS_SSI_NONBLOCKABLE_AUX_WORK_MASK \
  (0)

typedef struct ErtsSchedulerSleepInfo_ ErtsSchedulerSleepInfo;

typedef struct {
    erts_smp_spinlock_t lock;
    ErtsSchedulerSleepInfo *list;
} ErtsSchedulerSleepList;

struct ErtsSchedulerSleepInfo_ {
    ErtsSchedulerSleepInfo *next;
    ErtsSchedulerSleepInfo *prev;
    erts_smp_atomic32_t flags;
    erts_tse_t *event;
    erts_smp_atomic32_t aux_work;
};

/* times to reschedule low prio process before running */
#define RESCHEDULE_LOW        8

#define ERTS_MAX_MISC_OPS 5

#define ERTS_FULL_REDS_HISTORY_AVG_SHFT 3
#define ERTS_FULL_REDS_HISTORY_SIZE \
   ((1 << ERTS_FULL_REDS_HISTORY_AVG_SHFT) - 1)

typedef struct ErtsProcList_ ErtsProcList;
struct ErtsProcList_ {
    Eterm pid;
    SysTimeval started;
    ErtsProcList* next;
};

typedef struct ErtsMiscOpList_ ErtsMiscOpList;
struct ErtsMiscOpList_ {
    ErtsMiscOpList *next;
    void (*func)(void *arg);
    void *arg;
};

typedef struct {
    Process* first;
    Process* last;
} ErtsRunPrioQueue;

typedef struct ErtsSchedulerData_ ErtsSchedulerData;

typedef struct ErtsRunQueue_ ErtsRunQueue;

typedef struct {
    int len;
    int max_len;
    int reds;
    struct {
	struct {
	    int this;
	    int other;
	} limit;
	ErtsRunQueue *runq;
    } migrate;
} ErtsRunQueueInfo;

struct ErtsRunQueue_ {
    int ix;
    erts_smp_atomic32_t info_flags;

    erts_smp_mtx_t mtx;
    erts_smp_cnd_t cnd;

#ifdef ERTS_SMP
    ErtsSchedulerSleepList sleepers;
#endif

    ErtsSchedulerData *scheduler;
    int waiting; /* < 0 in sys schedule; > 0 on cnd variable */
    int woken;
    Uint32 flags;
    int check_balance_reds;
    int full_reds_history_sum;
    int full_reds_history[ERTS_FULL_REDS_HISTORY_SIZE];
    int out_of_work_count;
    int max_len;
    int len;
    int wakeup_other;
    int wakeup_other_reds;

    struct {
	int len;
	ErtsProcList *pending_exiters;
	Uint context_switches;
	Uint reductions;

	ErtsRunQueueInfo prio_info[ERTS_NO_PROC_PRIO_LEVELS];

	/* We use the same prio queue for low and
	   normal prio processes */
	ErtsRunPrioQueue prio[ERTS_NO_PROC_PRIO_LEVELS-1];
    } procs;

    struct {
	ErtsMiscOpList *start;
	ErtsMiscOpList *end;
	ErtsRunQueue *evac_runq;
    } misc;

    struct {
	ErtsRunQueueInfo info;
	struct port *start;
	struct port *end;
    } ports;
};

typedef union {
    ErtsRunQueue runq;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsRunQueue))];
} ErtsAlignedRunQueue;

extern ErtsAlignedRunQueue *erts_aligned_run_queues;
extern ErtsRunQueue *erts_common_run_queue;

#define ERTS_PROC_REDUCTIONS_EXECUTED(RQ, PRIO, REDS, AREDS)	\
do {								\
    (RQ)->procs.reductions += (AREDS);				\
    (RQ)->procs.prio_info[p->prio].reds += (REDS);		\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (AREDS);				\
} while (0)

#define ERTS_PORT_REDUCTIONS_EXECUTED(RQ, REDS)			\
do {								\
    (RQ)->ports.info.reds += (REDS);				\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (REDS);				\
} while (0)

struct ErtsSchedulerData_ {
    /*
     * Keep X registers first (so we get as many low
     * numbered registers as possible in the same cache
     * line).
     */
    Eterm* x_reg_array;		/* X registers */
    FloatDef* f_reg_array;	/* Floating point registers. */

#ifdef ERTS_SMP
    ethr_tid tid;		/* Thread id */
    struct erl_bits_state erl_bits_state; /* erl_bits.c state */
    void *match_pseudo_process; /* erl_db_util.c:db_prog_match() */
    ErtsSchedulerSleepInfo *ssi;
    Process *free_process;
#endif
#if !HEAP_ON_C_STACK
    Eterm tmp_heap[TMP_HEAP_SIZE];
    int num_tmp_heap_used;
    Eterm beam_emu_tmp_heap[BEAM_EMU_TMP_HEAP_SIZE];
    Eterm cmp_tmp_heap[CMP_TMP_HEAP_SIZE];
    Eterm erl_arith_tmp_heap[ERL_ARITH_TMP_HEAP_SIZE];
#endif

    Process *current_process;
    Uint no;			/* Scheduler number */
    struct port *current_port;
    ErtsRunQueue *run_queue;
    int virtual_reds;
    int cpu_id;			/* >= 0 when bound */

    ErtsAtomCacheMap atom_cache_map;

#ifdef ERTS_SMP
    /* NOTE: These fields are modified under held mutexes by other threads */
    erts_smp_atomic32_t chk_cpu_bind; /* Only used when common run queue */
#endif

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_alloc_verify_func_t verify_unused_temp_alloc;
    Allctr_t *verify_unused_temp_alloc_data;
#endif
};

typedef union {
    ErtsSchedulerData esd;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerData))];
} ErtsAlignedSchedulerData;

extern ErtsAlignedSchedulerData *erts_aligned_scheduler_data;

#ifndef ERTS_SMP
extern ErtsSchedulerData *erts_scheduler_data;
#endif

/*
 * Process Specific Data.
 *
 * NOTE: Only use PSD for very rarely used data.
 */

#define ERTS_PSD_ERROR_HANDLER			0
#define ERTS_PSD_SAVED_CALLS_BUF		1
#define ERTS_PSD_SCHED_ID			2
#define ERTS_PSD_DIST_ENTRY			3
#define ERTS_PSD_CALL_TIME_BP			4

#define ERTS_PSD_SIZE				5

typedef struct {
    void *data[ERTS_PSD_SIZE];
} ErtsPSD;

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_LC_PSD_ANY_LOCK (~ERTS_PROC_LOCKS_ALL)

#define ERTS_PSD_ERROR_HANDLER_BUF_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_ERROR_HANDLER_BUF_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_SAVED_CALLS_BUF_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_SAVED_CALLS_BUF_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_SCHED_ID_GET_LOCKS ERTS_PROC_LOCK_STATUS
#define ERTS_PSD_SCHED_ID_SET_LOCKS ERTS_PROC_LOCK_STATUS

#define ERTS_PSD_DIST_ENTRY_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_DIST_ENTRY_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_CALL_TIME_BP_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_CALL_TIME_BP_SET_LOCKS ERTS_PROC_LOCK_MAIN

typedef struct {
    ErtsProcLocks get_locks;
    ErtsProcLocks set_locks;
} ErtsLcPSDLocks;

extern ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];

#endif

#define ERTS_SCHED_STAT_MODIFY_DISABLE		1
#define ERTS_SCHED_STAT_MODIFY_ENABLE		2
#define ERTS_SCHED_STAT_MODIFY_CLEAR		3

typedef struct {
    erts_smp_spinlock_t lock;
    int enabled;
    struct {
	Eterm name;
	Uint total_executed;
	Uint executed;
	Uint total_migrated;
	Uint migrated;
    } prio[ERTS_NO_PRIO_LEVELS];
} erts_sched_stat_t;

extern erts_sched_stat_t erts_sched_stat;

typedef struct {
    Eterm reason;
    ErlHeapFragment *bp;
} ErtsPendExit;

#ifdef ERTS_SMP

typedef struct ErtsPendingSuspend_ ErtsPendingSuspend;
struct ErtsPendingSuspend_ {
    ErtsPendingSuspend *next;
    ErtsPendingSuspend *end;
    Eterm pid;
    void (*handle_func)(Process *suspendee,
			ErtsProcLocks suspendee_locks,
			int suspendee_alive,
			Eterm pid);
};

#endif

/* Defines to ease the change of memory architecture */
#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop
#  define HEAP_LIMIT(p)     (p)->stop
#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
#  define STACK_START(p)    (p)->hend
#  define STACK_TOP(p)      (p)->stop
#  define STACK_END(p)      (p)->htop
#  define HIGH_WATER(p)     (p)->high_water
#  define OLD_HEND(p)       (p)->old_hend
#  define OLD_HTOP(p)       (p)->old_htop
#  define OLD_HEAP(p)       (p)->old_heap
#  define GEN_GCS(p)        (p)->gen_gcs
#  define MAX_GEN_GCS(p)    (p)->max_gen_gcs
#  define FLAGS(p)          (p)->flags
#  define MBUF(p)           (p)->mbuf
#  define HALLOC_MBUF(p)    (p)->halloc_mbuf
#  define MBUF_SIZE(p)      (p)->mbuf_sz
#  define MSO(p)            (p)->off_heap
#  define MIN_HEAP_SIZE(p)  (p)->min_heap_size

#  define MIN_VHEAP_SIZE(p)   (p)->min_vheap_size
#  define BIN_VHEAP_SZ(p)     (p)->bin_vheap_sz
#  define BIN_VHEAP_MATURE(p) (p)->bin_vheap_mature
#  define BIN_OLD_VHEAP_SZ(p) (p)->bin_old_vheap_sz
#  define BIN_OLD_VHEAP(p)    (p)->bin_old_vheap

struct process {
    /* All fields in the PCB that differs between different heap
     * architectures, have been moved to the end of this struct to
     * make sure that as few offsets as possible differ. Different
     * offsets between memory architectures in this struct, means that
     * native code have to use functions instead of constants.
     */

    Eterm* htop;		/* Heap top */
    Eterm* stop;		/* Stack top */
    Eterm* heap;		/* Heap start */
    Eterm* hend;		/* Heap end */
    Uint heap_sz;		/* Size of heap in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */
    Uint min_vheap_size;        /* Minimum size of virtual heap (in words). */

#if !defined(NO_FPE_SIGNALS)
    volatile unsigned long fp_exception;
#endif

#ifdef HIPE
    /* HiPE-specific process fields. Put it early in struct process,
       to enable smaller & faster addressing modes on the x86. */
    struct hipe_process_state hipe;
#endif

    /*
     * Saved x registers.
     */
    Uint arity;			/* Number of live argument registers (only valid
				 * when process is *not* running).
				 */
    Eterm* arg_reg;		/* Pointer to argument registers. */
    unsigned max_arg_reg;	/* Maximum number of argument registers available. */
    Eterm def_arg_reg[6];	/* Default array for argument registers. */

    BeamInstr* cp;		/* (untagged) Continuation pointer (for threaded code). */
    BeamInstr* i;		/* Program counter for threaded code. */
    Sint catches;		/* Number of catches on stack */
    Sint fcalls;		/* 
				 * Number of reductions left to execute.
				 * Only valid for the current process.
				 */
    Uint32 status;		/* process STATE */
    Uint32 gcstatus;		/* process gc STATE */
    Uint32 rstatus;		/* process resume STATE */
    Uint32 rcount;		/* suspend count */
    Eterm id;			/* The pid of this process */
    int  prio;			/* Priority of process */
    int  skipped;		/* Times a low prio process has been rescheduled */
    Uint reds;			/* No of reductions for this process  */
    Eterm tracer_proc;		/* If proc is traced, this is the tracer
				   (can NOT be boxed) */
    Uint trace_flags;		/* Trace flags (used to be in flags) */
    Eterm group_leader;		/* Pid in charge
				   (can be boxed) */
    Uint flags;			/* Trap exit, etc (no trace flags anymore) */
    Eterm fvalue;		/* Exit & Throw value (failure reason) */
    Uint freason;		/* Reason for detected failure */
    Eterm ftrace;		/* Latest exception stack trace dump */

    Process *next;		/* Pointer to next process in run queue */
    Process *prev;		/* Pointer to prev process in run queue */

    struct reg_proc *reg;	/* NULL iff not registered */
    ErtsLink *nlinks;
    ErtsMonitor *monitors;      /* The process monitors, both ends */

    struct ErtsNodesMonitor_ *nodes_monitors;

    ErtsSuspendMonitor *suspend_monitors; /* Processes suspended by
					     this process via
					     erlang:suspend_process/1 */

    ErlMessageQueue msg;	/* Message queue */

    ErtsBifTimer *bif_timers;	/* Bif timers aiming at this process */

    ProcDict  *dictionary;       /* Process dictionary, may be NULL */

    Uint seq_trace_clock;
    Uint seq_trace_lastcnt;
    Eterm seq_trace_token;	/* Sequential trace token (tuple size 5 see below) */

    BeamInstr initial[3];	/* Initial module(0), function(1), arity(2), often used instead
				   of pointer to funcinfo instruction, hence the BeamInstr datatype */
    BeamInstr* current;		/* Current Erlang function, part of the funcinfo:
				 * module(0), function(1), arity(2)
				 * (module and functions are tagged atoms;
				 * arity an untagged integer). BeamInstr * because it references code
				 */
    
    /*
     * Information mainly for post-mortem use (erl crash dump).
     */
    Eterm parent;		/* Pid of process that created this process. */
    SysTimeval started;		/* Time when started. */


    /* This is the place, where all fields that differs between memory
     * architectures, have gone to.
     */

    Eterm *high_water;
    Eterm *old_hend;            /* Heap pointers for generational GC. */
    Eterm *old_htop;
    Eterm *old_heap;
    Uint16 gen_gcs;		/* Number of (minor) generational GCs. */
    Uint16 max_gen_gcs;		/* Max minor gen GCs before fullsweep. */
    ErlOffHeap off_heap;	/* Off-heap data updated by copy_struct(). */
    ErlHeapFragment* mbuf;	/* Pointer to message buffer list */
    Uint mbuf_sz;		/* Size of all message buffers */
    ErtsPSD *psd;		/* Rarely used process specific data */

    Uint64 bin_vheap_sz;	/* Virtual heap block size for binaries */
    Uint64 bin_vheap_mature;	/* Virtual heap block size for binaries */
    Uint64 bin_old_vheap_sz;	/* Virtual old heap block size for binaries */
    Uint64 bin_old_vheap;	/* Virtual old heap size for binaries */

    union {
#ifdef ERTS_SMP
	ErtsSmpPTimer *ptimer;
#else
	ErlTimer tm;		/* Timer entry */
#endif
	void *exit_data;	/* Misc data referred during termination */
    } u;

    ErtsRunQueue *bound_runq;

#ifdef ERTS_SMP
    erts_proc_lock_t lock;
    ErtsSchedulerData *scheduler_data;
    int is_exiting;
    Uint32 runq_flags;
    Uint32 status_flags;
    ErlMessageInQueue msg_inq;
    Eterm suspendee;
    ErtsPendingSuspend *pending_suspenders;
    ErtsPendExit pending_exit;
    ErtsRunQueue *run_queue;
#ifdef HIPE
    struct hipe_process_state_smp hipe_smp;
#endif
#endif

#ifdef HYBRID
    Eterm *rrma;                /* Remembered roots to Message Area */
    Eterm **rrsrc;              /* The source of the root */
    Uint nrr;                   /* Number of remembered roots */
    Uint rrsz;                  /* Size of root array */
#endif

#ifdef HYBRID
    Uint active;                /* Active since last major collection? */
    Uint active_index;          /* Index in the active process array */
#endif
 
#ifdef INCREMENTAL
    Process *active_next; /* Active processes to scan for roots */
    Process *active_prev; /* in collection of the message area  */
    Eterm *scan_top;
#endif

#ifdef CHECK_FOR_HOLES
    Eterm* last_htop;		/* No need to scan the heap below this point. */
    ErlHeapFragment* last_mbuf;	/* No need to scan beyond this mbuf. */
#endif

#ifdef DEBUG
    Eterm* last_old_htop;	/*
				 * No need to scan the old heap below this point
				 * when looking for invalid pointers into the new heap or
				 * heap fragments.
				 */
#endif

#ifdef FORCE_HEAP_FRAGS
    Uint space_verified;        /* Avoid HAlloc forcing heap fragments when */ 
    Eterm* space_verified_from; /* we rely on available heap space (TestHeap) */
#endif
};

#ifdef CHECK_FOR_HOLES
# define INIT_HOLE_CHECK(p)			\
do {						\
  (p)->last_htop = 0;				\
  (p)->last_mbuf = 0;				\
} while (0)

# define ERTS_HOLE_CHECK(p) erts_check_for_holes((p))
void erts_check_for_holes(Process* p);
#else
# define INIT_HOLE_CHECK(p)
# define ERTS_HOLE_CHECK(p)
#endif

/*
 * The MBUF_GC_FACTOR decides how easily a process is subject to GC 
 * due to message buffers allocated outside the heap.
 * The larger the factor, the easier the process gets GCed.
 * On a small memory system with lots of processes, this makes a significant 
 * difference, especially since the GCs help fragmentation quite a bit too.
 */
#if defined(SMALL_MEMORY)
#define MBUF_GC_FACTOR 4
#else
#define MBUF_GC_FACTOR 1
#endif

#define SEQ_TRACE_TOKEN(p)  ((p)->seq_trace_token)

/* The sequential tracing token is a tuple of size 5:
 *
 *    {Flags, Label, Serial, Sender}
 */

#define SEQ_TRACE_TOKEN_ARITY(p)    (arityval(*(tuple_val(SEQ_TRACE_TOKEN(p)))))
#define SEQ_TRACE_TOKEN_FLAGS(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 1))
#define SEQ_TRACE_TOKEN_LABEL(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 2))
#define SEQ_TRACE_TOKEN_SERIAL(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 3))
#define SEQ_TRACE_TOKEN_SENDER(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 4))
#define SEQ_TRACE_TOKEN_LASTCNT(p)  (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 5))

/* used when we have unit32 token */
#define SEQ_TRACE_T_ARITY(token)    (arityval(*(tuple_val(token))))
#define SEQ_TRACE_T_FLAGS(token)    (*(tuple_val(token) + 1))
#define SEQ_TRACE_T_LABEL(token)    (*(tuple_val(token) + 2))
#define SEQ_TRACE_T_SERIAL(token)   (*(tuple_val(token) + 3))
#define SEQ_TRACE_T_SENDER(token)   (*(tuple_val(token) + 4))
#define SEQ_TRACE_T_LASTCNT(token)  (*(tuple_val(token) + 5))

/*
 * Possible flags for the flags field in ErlSpawnOpts below.
 */

#define SPO_LINK 1
#define SPO_USE_ARGS 2
#define SPO_MONITOR 4

/*
 * The following struct contains options for a process to be spawned.
 */
typedef struct {
    Uint flags;
    int error_code;		/* Error code returned from create_process(). */
    Eterm mref;			/* Monitor ref returned (if SPO_MONITOR was given). */

    /*
     * The following items are only initialized if the SPO_USE_ARGS flag is set.
     */
    Uint min_heap_size;		/* Minimum heap size (must be a valued returned
				 * from next_heap_size()).  */
    Uint min_vheap_size;	/* Minimum virtual heap size  */
    int priority;		/* Priority for process. */
    Uint16 max_gen_gcs;		/* Maximum number of gen GCs before fullsweep. */
    int scheduler;
} ErlSpawnOpts;

/*
 * The KILL_CATCHES(p) macro kills pending catches for process p.
 */

#define KILL_CATCHES(p) (p)->catches = -1

/* Shrink heap fragment from _last_ HAlloc.
*/
ERTS_GLB_INLINE void erts_heap_frag_shrink(Process* p, Eterm* hp);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void erts_heap_frag_shrink(Process* p, Eterm* hp)
{
    ErlHeapFragment* hf = MBUF(p);

    ASSERT(hf!=NULL && (hp - hf->mem < (unsigned long)hf->alloc_size));

    hf->used_size = hp - hf->mem;
}	
#endif /* inline */

Eterm* erts_heap_alloc(Process* p, Uint need, Uint xtra);
#ifdef CHECK_FOR_HOLES
Eterm* erts_set_hole_marker(Eterm* ptr, Uint sz);
#endif

extern Process** process_tab;
#ifdef HYBRID
extern Uint erts_num_active_procs;
extern Process** erts_active_procs;
#endif
extern Uint erts_max_processes;
extern Uint erts_process_tab_index_mask;
extern Uint erts_default_process_flags;
extern erts_smp_rwmtx_t erts_cpu_bind_rwmtx;
/* If any of the erts_system_monitor_* variables are set (enabled),
** erts_system_monitor must be != NIL, to allow testing on just
** the erts_system_monitor_* variables.
*/
extern Eterm erts_system_monitor;
extern Uint erts_system_monitor_long_gc;
extern Uint erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t {
	 unsigned int busy_port : 1;
    unsigned int busy_dist_port : 1;
};
extern struct erts_system_monitor_flags_t erts_system_monitor_flags;

/* system_profile, same rules as for system_monitor.
	erts_profile must be != NIL when 
	erts_profile_* is set. */

extern Eterm erts_system_profile;
struct erts_system_profile_flags_t {
    unsigned int scheduler : 1;
    unsigned int runnable_procs : 1;
    unsigned int runnable_ports : 1;
    unsigned int exclusive : 1;
};
extern struct erts_system_profile_flags_t erts_system_profile_flags;

#define INVALID_PID(p, pid)	((p) == NULL				\
				 || (p)->id != (pid)			\
				 || (p)->status == P_EXITING)
 
#define IS_TRACED(p)             ( (p)->tracer_proc != NIL )
#define ARE_TRACE_FLAGS_ON(p,tf) ( ((p)->trace_flags & (tf|F_SENSITIVE)) == (tf) )
#define IS_TRACED_FL(p,tf)       ( IS_TRACED(p) && ARE_TRACE_FLAGS_ON(p,tf) )

/* process flags */
#define F_TRAPEXIT           (1 <<  0)
#define F_INSLPQUEUE         (1 <<  1) /* Set if in timer queue */
#define F_TIMO               (1 <<  2) /* Set if timeout */
#define F_HEAP_GROW          (1 <<  3)
#define F_NEED_FULLSWEEP     (1 <<  4)
#define F_USING_DB           (1 <<  5) /* If have created tables */
#define F_DISTRIBUTION       (1 <<  6) /* Process used in distribution */
#define F_USING_DDLL         (1 <<  7) /* Process has used the DDLL interface */
#define F_HAVE_BLCKD_MSCHED  (1 <<  8) /* Process has blocked multi-scheduling */
#define F_P2PNR_RESCHED      (1 <<  9) /* Process has been rescheduled via erts_pid2proc_not_running() */
#define F_FORCE_GC           (1 << 10) /* Force gc at process in-scheduling */
#define F_HIBERNATE_SCHED    (1 << 11) /* Schedule out after hibernate op */

/* process trace_flags */
#define F_SENSITIVE          (1 << 0)
#define F_TRACE_SEND         (1 << 1)   
#define F_TRACE_RECEIVE      (1 << 2)
#define F_TRACE_SOS          (1 << 3) /* Set on spawn       */
#define F_TRACE_SOS1         (1 << 4) /* Set on first spawn */
#define F_TRACE_SOL          (1 << 5) /* Set on link        */
#define F_TRACE_SOL1         (1 << 6) /* Set on first link  */
#define F_TRACE_CALLS        (1 << 7)
#define F_TIMESTAMP          (1 << 8)
#define F_TRACE_PROCS        (1 << 9)
#define F_TRACE_FIRST_CHILD  (1 << 10)
#define F_TRACE_SCHED        (1 << 11)
#define F_TRACE_GC           (1 << 12)
#define F_TRACE_ARITY_ONLY   (1 << 13)
#define F_TRACE_RETURN_TO    (1 << 14) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       (1 << 15) /* No call trace msg suppress */
#define F_TRACER             (1 << 16) /* May be (has been) tracer */
#define F_EXCEPTION_TRACE    (1 << 17) /* May have exception trace on stack */

/* port trace flags, currently the same as process trace flags */
#define F_TRACE_SCHED_PORTS  (1 << 18) /* Trace of port scheduling */
#define F_TRACE_SCHED_PROCS  (1 << 19) /* With virtual scheduling */
#define F_TRACE_PORTS	     (1 << 20) /* Ports equivalent to F_TRACE_PROCS */
#define F_TRACE_SCHED_NO     (1 << 21) /* Trace with scheduler id */
#define F_TRACE_SCHED_EXIT   (1 << 22)

#define F_NUM_FLAGS          23
#ifdef DEBUG
#  define F_INITIAL_TRACE_FLAGS (5 << F_NUM_FLAGS)
#else
#  define F_INITIAL_TRACE_FLAGS 0
#endif



#define TRACEE_FLAGS ( F_TRACE_PROCS | F_TRACE_CALLS \
		     | F_TRACE_SOS |  F_TRACE_SOS1| F_TRACE_RECEIVE  \
		     | F_TRACE_SOL | F_TRACE_SOL1 | F_TRACE_SEND \
		     | F_TRACE_SCHED | F_TIMESTAMP | F_TRACE_GC \
		     | F_TRACE_ARITY_ONLY | F_TRACE_RETURN_TO \
                     | F_TRACE_SILENT | F_TRACE_SCHED_PROCS | F_TRACE_PORTS \
		     | F_TRACE_SCHED_PORTS | F_TRACE_SCHED_NO \
		     | F_TRACE_SCHED_EXIT)

#define ERTS_TRACEE_MODIFIER_FLAGS \
  (F_TRACE_SILENT | F_TIMESTAMP | F_TRACE_SCHED_NO)
#define ERTS_PORT_TRACEE_FLAGS \
  (ERTS_TRACEE_MODIFIER_FLAGS | F_TRACE_PORTS | F_TRACE_SCHED_PORTS)
#define ERTS_PROC_TRACEE_FLAGS \
  ((TRACEE_FLAGS & ~ERTS_PORT_TRACEE_FLAGS) | ERTS_TRACEE_MODIFIER_FLAGS)

/* Sequential trace flags */
#define SEQ_TRACE_SEND     (1 << 0)
#define SEQ_TRACE_RECEIVE  (1 << 1)
#define SEQ_TRACE_PRINT    (1 << 2)
#define SEQ_TRACE_TIMESTAMP (1 << 3)

#ifdef ERTS_SMP
/* Status flags ... */
#define ERTS_PROC_SFLG_PENDADD2SCHEDQ	(((Uint32) 1) << 0)	/* Pending
								   add to
								   schedule q */
#define ERTS_PROC_SFLG_INRUNQ		(((Uint32) 1) << 1)	/* Process is
								   in run q */
#define ERTS_PROC_SFLG_TRAPEXIT		(((Uint32) 1) << 2)	/* Process is
								   trapping
								   exit */
#define ERTS_PROC_SFLG_RUNNING		(((Uint32) 1) << 3)	/* Process is
								   running */
/* Scheduler flags in process struct... */
#define ERTS_PROC_RUNQ_FLG_RUNNING	(((Uint32) 1) << 0)	/* Process is
								   running */

#endif


#ifdef ERTS_SMP
#define ERTS_PROC_IS_TRAPPING_EXITS(P)					\
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P))			\
		      & ERTS_PROC_LOCK_STATUS),				\
   (P)->status_flags & ERTS_PROC_SFLG_TRAPEXIT)

#define ERTS_PROC_SET_TRAP_EXIT(P)					\
  (ERTS_SMP_LC_ASSERT(((ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)	\
		       & erts_proc_lc_my_proc_locks((P)))		\
		      == (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)),	\
   (P)->status_flags |= ERTS_PROC_SFLG_TRAPEXIT,			\
   (P)->flags |= F_TRAPEXIT,						\
   1)

#define ERTS_PROC_UNSET_TRAP_EXIT(P)					\
  (ERTS_SMP_LC_ASSERT(((ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)	\
		       & erts_proc_lc_my_proc_locks((P)))		\
		      == (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)),	\
   (P)->status_flags &= ~ERTS_PROC_SFLG_TRAPEXIT,			\
   (P)->flags &= ~F_TRAPEXIT,						\
   0)
#else
#define ERTS_PROC_IS_TRAPPING_EXITS(P) ((P)->flags & F_TRAPEXIT)
#define ERTS_PROC_SET_TRAP_EXIT(P) ((P)->flags |= F_TRAPEXIT, 1)
#define ERTS_PROC_UNSET_TRAP_EXIT(P) ((P)->flags &= ~F_TRAPEXIT, 0)
#endif

/* Option flags to erts_send_exit_signal() */
#define ERTS_XSIG_FLG_IGN_KILL		(((Uint32) 1) << 0)
#define ERTS_XSIG_FLG_NO_IGN_NORMAL	(((Uint32) 1) << 1)


/* Process status values */
#define P_FREE      0
#define P_RUNABLE   1
#define P_WAITING   2
#define P_RUNNING   3
#define P_EXITING   4
#define P_GARBING   5
#define P_SUSPENDED 6

#define CANCEL_TIMER(p) \
    do { \
	if ((p)->flags & (F_INSLPQUEUE)) \
	    cancel_timer(p); \
	else \
	    (p)->flags &= ~F_TIMO; \
    } while (0)

#define ERTS_RUNQ_IX(IX)						\
  (ASSERT_EXPR(0 <= (IX) && (IX) < erts_no_run_queues),			\
   &erts_aligned_run_queues[(IX)].runq)
#define ERTS_SCHEDULER_IX(IX)						\
  (ASSERT_EXPR(0 <= (IX) && (IX) < erts_no_schedulers),			\
   &erts_aligned_scheduler_data[(IX)].esd)

void erts_pre_init_process(void);
void erts_late_init_process(void);
void erts_early_init_scheduling(void);
void erts_init_scheduling(int, int, int);

ErtsProcList *erts_proclist_create(Process *);
void erts_proclist_destroy(ErtsProcList *);
int erts_proclist_same(ErtsProcList *, Process *);

int erts_sched_set_wakeup_limit(char *str);

#ifdef DEBUG
void erts_dbg_multi_scheduling_return_trap(Process *, Eterm);
#endif
int erts_get_max_no_executing_schedulers(void);
#ifdef ERTS_SMP
ErtsSchedSuspendResult
erts_schedulers_state(Uint *, Uint *, Uint *, int);
ErtsSchedSuspendResult
erts_set_schedulers_online(Process *p,
			   ErtsProcLocks plocks,
			   Sint new_no,
			   Sint *old_no);
ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *, ErtsProcLocks, int, int);
int erts_is_multi_scheduling_blocked(void);
Eterm erts_multi_scheduling_blockers(Process *);
void erts_start_schedulers(void);
void erts_smp_notify_check_children_needed(void);
void
erts_smp_schedule_misc_aux_work(int ignore_self,
				int max_sched,
				void (*func)(void *),
				void *arg);
#endif
void erts_sched_notify_check_cpu_bind(void);
Uint erts_active_schedulers(void);
void erts_init_process(int);
Eterm erts_process_status(Process *, ErtsProcLocks, Process *, Eterm);
Uint erts_run_queues_len(Uint *);
void erts_add_to_runq(Process *);
Eterm erts_bound_schedulers_term(Process *c_p);
Eterm erts_get_cpu_topology_term(Process *c_p, Eterm which);
Eterm erts_get_schedulers_binds(Process *c_p);
Eterm erts_set_cpu_topology(Process *c_p, Eterm term);
Eterm erts_bind_schedulers(Process *c_p, Eterm how);
ErtsRunQueue *erts_schedid2runq(Uint);
#ifdef ERTS_SMP
ErtsMigrateResult erts_proc_migrate(Process *,
				    ErtsProcLocks *,
				    ErtsRunQueue *,
				    int *,
				    ErtsRunQueue *,
				    int *);
#endif
Process *schedule(Process*, int);
void erts_schedule_misc_op(void (*)(void *), void *);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_do_exit_process(Process*, Eterm);
void erts_continue_exit_process(Process *);
void set_timer(Process*, Uint);
void cancel_timer(Process*);
/* Begin System profile */
Uint erts_runnable_process_count(void);
Uint erts_process_count(void);
/* End System profile */
void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);
#ifdef DEBUG
void erts_debug_verify_clean_empty_process(Process* p);
#endif
void erts_stack_dump(int to, void *to_arg, Process *);
void erts_program_counter_info(int to, void *to_arg, Process *);

Eterm erts_get_process_priority(Process *p);
Eterm erts_set_process_priority(Process *p, Eterm prio);

Uint erts_get_total_context_switches(void);
void erts_get_total_reductions(Uint *, Uint *);
void erts_get_exact_total_reductions(Process *, Uint *, Uint *);

Eterm erts_fake_scheduler_bindings(Process *p, Eterm how);

void erts_sched_stat_modify(int what);
Eterm erts_sched_stat_term(Process *p, int total);

void erts_free_proc(Process *);

void erts_suspend(Process*, ErtsProcLocks, struct port*);
void erts_resume(Process*, ErtsProcLocks);
int erts_resume_processes(ErtsProcList *);

int erts_send_exit_signal(Process *,
			  Eterm,
			  Process *,
			  ErtsProcLocks *,
			  Eterm,
			  Eterm,
			  Process *,
			  Uint32);
#ifdef ERTS_SMP
void erts_handle_pending_exit(Process *, ErtsProcLocks);
#define ERTS_PROC_PENDING_EXIT(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) & ERTS_PROC_LOCK_STATUS),\
   (P)->pending_exit.reason != THE_NON_VALUE)
#else
#define ERTS_PROC_PENDING_EXIT(P) 0
#endif

void erts_deep_process_dump(int, void *);

Eterm erts_get_reader_groups_map(Process *c_p);
Eterm erts_debug_reader_groups_map(Process *c_p, int groups);

Sint erts_test_next_pid(int, Uint);
Eterm erts_debug_processes(Process *c_p);
Eterm erts_debug_processes_bif_info(Process *c_p);
Uint erts_debug_nbalance(void);

#ifdef ERTS_SMP
#  define ERTS_GET_SCHEDULER_DATA_FROM_PROC(PROC) ((PROC)->scheduler_data)
#  define ERTS_PROC_GET_SCHDATA(PROC) ((PROC)->scheduler_data)
#else
#  define ERTS_GET_SCHEDULER_DATA_FROM_PROC(PROC) (erts_scheduler_data)
#  define ERTS_PROC_GET_SCHDATA(PROC) (erts_scheduler_data)
#endif

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(P)					\
do {										\
    ErtsSchedulerData *esdp__ = ((P)						\
				 ? ERTS_PROC_GET_SCHDATA((Process *) (P))	\
				 : erts_get_scheduler_data());			\
    if (esdp__)									\
	esdp__->verify_unused_temp_alloc(					\
	    esdp__->verify_unused_temp_alloc_data);				\
} while (0)
#else
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(ESDP)
#endif

#if defined(ERTS_SMP) || defined(USE_THREADS)
ErtsSchedulerData *erts_get_scheduler_data(void);
#else
ERTS_GLB_INLINE ErtsSchedulerData *erts_get_scheduler_data(void);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsSchedulerData *erts_get_scheduler_data(void)
{
    return erts_scheduler_data;
}
#endif
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)

#define ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__
#include "erl_process_lock.h"
#undef ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__

int erts_smp_lc_runq_is_locked(ErtsRunQueue *);
#define ERTS_SMP_LC_CHK_RUNQ_LOCK(RQ, L)				\
do {									\
    if ((L))								\
	ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked((RQ)));		\
    else								\
	ERTS_SMP_LC_ASSERT(!erts_smp_lc_runq_is_locked((RQ)));		\
} while (0)
#else
#define ERTS_SMP_LC_CHK_RUNQ_LOCK(RQ, L)
#endif

void *erts_psd_set_init(Process *p, ErtsProcLocks plocks, int ix, void *data);

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix);
ERTS_GLB_INLINE void *
erts_psd_set(Process *p, ErtsProcLocks plocks, int ix, void *new);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].get_locks)
	ERTS_SMP_LC_ASSERT(locks
			   || erts_is_system_blocked(0)
			   || (ERTS_IS_CRASH_DUMPING
			       && erts_is_system_blocked(ERTS_BS_FLG_ALLOW_GC)));
    else {
	locks &= erts_psd_required_locks[ix].get_locks;
	ERTS_SMP_LC_ASSERT(erts_psd_required_locks[ix].get_locks == locks
			   || erts_is_system_blocked(0)
			   || (ERTS_IS_CRASH_DUMPING
			       && erts_is_system_blocked(ERTS_BS_FLG_ALLOW_GC)));
    }
#endif
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    return p->psd ? p->psd->data[ix] : NULL;
}


/*
 * NOTE: erts_psd_set() might release and reacquire locks on 'p'.
 */
ERTS_GLB_INLINE void *
erts_psd_set(Process *p, ErtsProcLocks plocks, int ix, void *data)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].set_locks)
	ERTS_SMP_LC_ASSERT(locks
			   || erts_is_system_blocked(0)
			   || (ERTS_IS_CRASH_DUMPING
			       && erts_is_system_blocked(ERTS_BS_FLG_ALLOW_GC)));
    else {
	locks &= erts_psd_required_locks[ix].set_locks;
	ERTS_SMP_LC_ASSERT(erts_psd_required_locks[ix].set_locks == locks
			   || erts_is_system_blocked(0)
			   || (ERTS_IS_CRASH_DUMPING
			       && erts_is_system_blocked(ERTS_BS_FLG_ALLOW_GC)));
    }
#endif
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    if (p->psd) {
	void *old = p->psd->data[ix];
	p->psd->data[ix] = data;
	return old;
    }
    else {
	if (!data)
	    return NULL;
	else
	    return erts_psd_set_init(p, plocks, ix, data);
    }
}

#endif

#define ERTS_PROC_SCHED_ID(P, L, ID) \
  ((UWord) erts_psd_set((P), (L), ERTS_PSD_SCHED_ID, (void *) (ID)))

#define ERTS_PROC_GET_DIST_ENTRY(P) \
  ((DistEntry *) erts_psd_get((P), ERTS_PSD_DIST_ENTRY))
#define ERTS_PROC_SET_DIST_ENTRY(P, L, D) \
  ((DistEntry *) erts_psd_set((P), (L), ERTS_PSD_DIST_ENTRY, (void *) (D)))

#define ERTS_PROC_GET_SAVED_CALLS_BUF(P) \
  ((struct saved_calls *) erts_psd_get((P), ERTS_PSD_SAVED_CALLS_BUF))
#define ERTS_PROC_SET_SAVED_CALLS_BUF(P, L, SCB) \
  ((struct saved_calls *) erts_psd_set((P), (L), ERTS_PSD_SAVED_CALLS_BUF, (void *) (SCB)))

#define ERTS_PROC_GET_CALL_TIME(P) \
  ((process_breakpoint_time_t *) erts_psd_get((P), ERTS_PSD_CALL_TIME_BP))
#define ERTS_PROC_SET_CALL_TIME(P, L, PBT) \
  ((process_breakpoint_time_t *) erts_psd_set((P), (L), ERTS_PSD_CALL_TIME_BP, (void *) (PBT)))


ERTS_GLB_INLINE Eterm erts_proc_get_error_handler(Process *p);
ERTS_GLB_INLINE Eterm erts_proc_set_error_handler(Process *p,
						  ErtsProcLocks plocks,
						  Eterm handler);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm
erts_proc_get_error_handler(Process *p)
{
    void *val = erts_psd_get(p, ERTS_PSD_ERROR_HANDLER);
    if (!val)
	return am_error_handler;
    else {
	ASSERT(is_atom(((Eterm) (UWord) val)));
	return (Eterm) (UWord) val;
    }
}

ERTS_GLB_INLINE Eterm
erts_proc_set_error_handler(Process *p, ErtsProcLocks plocks, Eterm handler)
{
    void *old_val;
    void *new_val;
    ASSERT(is_atom(handler));
    new_val = (handler == am_error_handler) ? NULL : (void *) (UWord) handler;
    old_val = erts_psd_set(p, plocks, ERTS_PSD_ERROR_HANDLER, new_val);
    if (!old_val)
	return am_error_handler;
    else {
	ASSERT(is_atom(((Eterm) (UWord) old_val)));
	return (Eterm) (UWord) old_val;
    }
}

#endif

#ifdef ERTS_SMP
ErtsRunQueue *erts_prepare_emigrate(ErtsRunQueue *c_rq,
				    ErtsRunQueueInfo *c_rqi,
				    int prio);

ERTS_GLB_INLINE ErtsRunQueue *erts_check_emigration_need(ErtsRunQueue *c_rq,
							 int prio);
#endif

ERTS_GLB_INLINE int erts_is_scheduler_bound(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE Process *erts_get_current_process(void);
ERTS_GLB_INLINE Eterm erts_get_current_pid(void);
ERTS_GLB_INLINE Uint erts_get_scheduler_id(void);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_proc(Process *p);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_current(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE void erts_smp_runq_lock(ErtsRunQueue *rq);
ERTS_GLB_INLINE int erts_smp_runq_trylock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_smp_runq_unlock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_smp_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_smp_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_smp_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);
ERTS_GLB_INLINE void erts_smp_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ERTS_SMP
ERTS_GLB_INLINE ErtsRunQueue *
erts_check_emigration_need(ErtsRunQueue *c_rq, int prio)
{
    ErtsRunQueueInfo *c_rqi;

    if (!ERTS_CHK_RUNQ_FLG_EMIGRATE(c_rq->flags, prio))
	return NULL;

    if (prio == ERTS_PORT_PRIO_LEVEL)
	c_rqi = &c_rq->ports.info;
    else
	c_rqi = &c_rq->procs.prio_info[prio];

    if (!ERTS_CHK_RUNQ_FLG_EVACUATE(c_rq->flags, prio)
	&& !(c_rq->flags & ERTS_RUNQ_FLG_INACTIVE)
	&& c_rqi->len <= c_rqi->migrate.limit.this)
	return NULL;

    return erts_prepare_emigrate(c_rq, c_rqi, prio);
}
#endif

ERTS_GLB_INLINE
int erts_is_scheduler_bound(ErtsSchedulerData *esdp)
{
    if (!esdp)
	esdp = erts_get_scheduler_data();
    ASSERT(esdp);
    return esdp->cpu_id >= 0;
}

ERTS_GLB_INLINE
Process *erts_get_current_process(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    return esdp ? esdp->current_process : NULL;
}

ERTS_GLB_INLINE
Eterm erts_get_current_pid(void)
{
    Process *proc = erts_get_current_process();
    return proc ? proc->id : THE_NON_VALUE;
}

ERTS_GLB_INLINE
Uint erts_get_scheduler_id(void)
{
#ifdef ERTS_SMP
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    return esdp ? esdp->no : (Uint) 0;
#else
    return erts_get_scheduler_data() ? (Uint) 1 : (Uint) 0;
#endif
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_proc(Process *p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
#ifdef ERTS_SMP
    ASSERT(p->run_queue);
    return p->run_queue;
#else
    ASSERT(erts_common_run_queue);
    return erts_common_run_queue;
#endif
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_current(ErtsSchedulerData *esdp)
{
    ASSERT(!esdp || esdp == erts_get_scheduler_data());
#ifdef ERTS_SMP
    if (!esdp)
	esdp = erts_get_scheduler_data();
    return esdp->run_queue;
#else
    ASSERT(erts_common_run_queue);
    return erts_common_run_queue;
#endif
}

ERTS_GLB_INLINE void
erts_smp_runq_lock(ErtsRunQueue *rq)
{
#ifdef ERTS_SMP
    erts_smp_mtx_lock(&rq->mtx);
#endif
}

ERTS_GLB_INLINE int
erts_smp_runq_trylock(ErtsRunQueue *rq)
{
#ifdef ERTS_SMP
    return erts_smp_mtx_trylock(&rq->mtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_runq_unlock(ErtsRunQueue *rq)
{
#ifdef ERTS_SMP
    erts_smp_mtx_unlock(&rq->mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
#ifdef ERTS_SMP
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&rq->mtx));
    if (xrq != rq) {
	if (erts_smp_mtx_trylock(&xrq->mtx) == EBUSY) {
	    if (rq < xrq)
		erts_smp_mtx_lock(&xrq->mtx);
	    else {
		erts_smp_mtx_unlock(&rq->mtx);
		erts_smp_mtx_lock(&xrq->mtx);
		erts_smp_mtx_lock(&rq->mtx);
	    }
	}
    }
#endif
}

ERTS_GLB_INLINE void
erts_smp_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
#ifdef ERTS_SMP
    if (xrq != rq)
	erts_smp_mtx_unlock(&xrq->mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
#ifdef ERTS_SMP
    ASSERT(rq1 && rq2);
    if (rq1 == rq2)
	erts_smp_mtx_lock(&rq1->mtx);
    else if (rq1 < rq2) {
	erts_smp_mtx_lock(&rq1->mtx);
	erts_smp_mtx_lock(&rq2->mtx);	
    }
    else {
	erts_smp_mtx_lock(&rq2->mtx);
	erts_smp_mtx_lock(&rq1->mtx);	
    }
#endif
}

ERTS_GLB_INLINE void
erts_smp_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
#ifdef ERTS_SMP
    ASSERT(rq1 && rq2);
    erts_smp_mtx_unlock(&rq1->mtx);
    if (rq1 != rq2)
	erts_smp_mtx_unlock(&rq2->mtx);
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

ERTS_GLB_INLINE ErtsAtomCacheMap *erts_get_atom_cache_map(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE ErtsAtomCacheMap *
erts_get_atom_cache_map(Process *c_p)
{
    ErtsSchedulerData *esdp = (c_p
			       ? ERTS_PROC_GET_SCHDATA(c_p)
			       : erts_get_scheduler_data());
    ASSERT(esdp);
    return &esdp->atom_cache_map;
}
#endif

Process *erts_pid2proc_suspend(Process *,
			       ErtsProcLocks,
			       Eterm,
			       ErtsProcLocks);
#ifdef ERTS_SMP

Process *erts_pid2proc_not_running(Process *,
				   ErtsProcLocks,
				   Eterm,
				   ErtsProcLocks);
Process *erts_pid2proc_nropt(Process *c_p,
			     ErtsProcLocks c_p_locks,
			     Eterm pid,
			     ErtsProcLocks pid_locks);
extern int erts_disable_proc_not_running_opt;

#ifdef DEBUG
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P) \
  do { ASSERT(!(P)->is_exiting); } while (0)
#else
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)
#endif

/* NOTE: At least one process lock has to be held on P! */
#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_PROC_IS_EXITING(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) != 0 \
		      || erts_lc_pix_lock_is_locked(ERTS_PID2PIXLOCK((P)->id))),\
   (P)->is_exiting)
#else
#define ERTS_PROC_IS_EXITING(P) ((P)->is_exiting)
#endif

#else /* !ERTS_SMP */

#define ERTS_PROC_IS_EXITING(P) ((P)->status == P_EXITING)

#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)

#define erts_pid2proc_not_running erts_pid2proc
#define erts_pid2proc_nropt erts_pid2proc

#endif

/* Minimum NUMBER of processes for a small system to start */
#ifdef ERTS_SMP
#define ERTS_MIN_PROCESSES		ERTS_NO_OF_PIX_LOCKS
#else
#define ERTS_MIN_PROCESSES		16
#endif

void erts_smp_notify_inc_runq(ErtsRunQueue *runq);

#ifdef ERTS_SMP
void erts_sched_finish_poke(ErtsSchedulerSleepInfo *, erts_aint32_t);
ERTS_GLB_INLINE void erts_sched_poke(ErtsSchedulerSleepInfo *ssi);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_sched_poke(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t flags;
    ERTS_THR_MEMORY_BARRIER;
    flags = erts_smp_atomic32_read_nob(&ssi->flags);
    ASSERT(!(flags & ERTS_SSI_FLG_SLEEPING)
	   || (flags & ERTS_SSI_FLG_WAITING));
    if (flags & ERTS_SSI_FLG_SLEEPING) {
	flags = erts_smp_atomic32_read_band_nob(&ssi->flags, ~ERTS_SSI_FLGS_SLEEP);
	erts_sched_finish_poke(ssi, flags);
    }
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifdef ERTS_SMP */

#include "erl_process_lock.h"

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS

#endif



