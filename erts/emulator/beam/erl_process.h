/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

#ifndef __PROCESS_H__
#define __PROCESS_H__

#include "sys.h"

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

#define ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#include "erl_process_lock.h" /* Only pull out important types... */
#undef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__

#define ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_port.h"
#undef ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_vm.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitor_link.h"
#include "erl_hl_timer.h"
#include "erl_time.h"
#include "erl_atom_table.h"
#include "external.h"
#include "erl_mseg.h"
#include "erl_async.h"
#include "erl_gc.h"
#define ERTS_ONLY_INCLUDE_TRACE_FLAGS
#include "erl_trace.h"
#undef ERTS_ONLY_INCLUDE_TRACE_FLAGS
#define ERTS_ONLY_SCHED_SPEC_ETS_DATA
#include "erl_db.h"
#undef ERTS_ONLY_SCHED_SPEC_ETS_DATA

#ifdef HIPE
#include "hipe_process.h"
#endif

#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT	0
#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT		0

#define ERTS_MAX_NO_OF_SCHEDULERS 1024
#define ERTS_MAX_NO_OF_DIRTY_CPU_SCHEDULERS ERTS_MAX_NO_OF_SCHEDULERS
#define ERTS_MAX_NO_OF_DIRTY_IO_SCHEDULERS ERTS_MAX_NO_OF_SCHEDULERS

#define ERTS_DEFAULT_MAX_PROCESSES (1 << 18)

#define ERTS_HEAP_ALLOC(Type, Size)					\
     erts_alloc((Type), (Size))

#define ERTS_HEAP_REALLOC(Type, Ptr, OldSize, NewSize)			\
     erts_realloc((Type), (Ptr), (NewSize))

#define ERTS_HEAP_FREE(Type, Ptr, Size)					\
     erts_free((Type), (Ptr))

#include "export.h"

struct saved_calls {
   int len;
   int n;
   int cur;
   Export *ct[1];
};

extern Export exp_send, exp_receive, exp_timeout;
extern int ERTS_WRITE_UNLIKELY(erts_sched_compact_load);
extern int ERTS_WRITE_UNLIKELY(erts_sched_balance_util);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_total_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_cpu_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_io_schedulers);
extern Uint ERTS_WRITE_UNLIKELY(erts_no_run_queues);
extern int erts_sched_thread_suggested_stack_size;
extern int erts_dcpu_sched_thread_suggested_stack_size;
extern int erts_dio_sched_thread_suggested_stack_size;
#define ERTS_SCHED_THREAD_MIN_STACK_SIZE 20	/* Kilo words */
#define ERTS_SCHED_THREAD_MAX_STACK_SIZE 8192	/* Kilo words */

#include "erl_bits.h"

/* process priorities */
#define PRIORITY_MAX          0
#define PRIORITY_HIGH         1
#define PRIORITY_NORMAL       2
#define PRIORITY_LOW          3
#define ERTS_NO_PROC_PRIO_LEVELS      4
#define ERTS_NO_PROC_PRIO_QUEUES      3

#define ERTS_PORT_PRIO_LEVEL ERTS_NO_PROC_PRIO_LEVELS
#define ERTS_NO_PRIO_LEVELS (ERTS_NO_PROC_PRIO_LEVELS + 1)

#define ERTS_RUNQ_FLGS_PROCS_QMASK \
  ((((Uint32) 1) << ERTS_NO_PROC_PRIO_LEVELS) - 1)

#define ERTS_RUNQ_FLGS_QMASK \
  ((((Uint32) 1) << ERTS_NO_PRIO_LEVELS) - 1)

#define ERTS_RUNQ_FLGS_EMIGRATE_SHFT \
  ERTS_NO_PRIO_LEVELS
#define ERTS_RUNQ_FLGS_IMMIGRATE_SHFT \
  (ERTS_RUNQ_FLGS_EMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EVACUATE_SHFT \
  (ERTS_RUNQ_FLGS_IMMIGRATE_SHFT + ERTS_NO_PRIO_LEVELS)
#define ERTS_RUNQ_FLGS_EMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_EMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_IMMIGRATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)
#define ERTS_RUNQ_FLGS_EVACUATE_QMASK \
  (ERTS_RUNQ_FLGS_QMASK << ERTS_RUNQ_FLGS_EVACUATE_SHFT)

#define ERTS_RUNQ_FLG_BASE2 \
  (ERTS_RUNQ_FLGS_EVACUATE_SHFT + ERTS_NO_PRIO_LEVELS)

#define ERTS_RUNQ_FLG_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 0))
#define ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 1))
#define ERTS_RUNQ_FLG_SUSPENDED \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 2))
#define ERTS_RUNQ_FLG_CHK_CPU_BIND \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 3))
#define ERTS_RUNQ_FLG_INACTIVE \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 4))
#define ERTS_RUNQ_FLG_NONEMPTY \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 5))
#define ERTS_RUNQ_FLG_PROTECTED \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 6))
#define ERTS_RUNQ_FLG_EXEC \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 7))
#define ERTS_RUNQ_FLG_MSB_EXEC \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 8))
#define ERTS_RUNQ_FLG_MISC_OP \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 9))
#define ERTS_RUNQ_FLG_HALTING \
  (((Uint32) 1) << (ERTS_RUNQ_FLG_BASE2 + 10))

#define ERTS_RUNQ_FLG_MAX (ERTS_RUNQ_FLG_BASE2 + 11)

#define ERTS_RUNQ_FLGS_MIGRATION_QMASKS	\
  (ERTS_RUNQ_FLGS_EMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_IMMIGRATE_QMASK	\
   | ERTS_RUNQ_FLGS_EVACUATE_QMASK)

#define ERTS_RUNQ_FLGS_MIGRATION_INFO \
  (ERTS_RUNQ_FLG_INACTIVE \
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

#define ERTS_RUNQ_FLGS_INIT(RQ, INIT)					\
    erts_atomic32_init_nob(&(RQ)->flags, (erts_aint32_t) (INIT))
#define ERTS_RUNQ_FLGS_SET(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_bor_relb(&(RQ)->flags,		\
					      (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_SET_NOB(RQ, FLGS)				\
    ((Uint32) erts_atomic32_read_bor_nob(&(RQ)->flags,		\
					     (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_BSET(RQ, MSK, FLGS)				\
    ((Uint32) erts_atomic32_read_bset_relb(&(RQ)->flags,		\
					       (erts_aint32_t) (MSK),	\
					       (erts_aint32_t) (FLGS)))
#define ERTS_RUNQ_FLGS_UNSET(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_band_relb(&(RQ)->flags,		\
					       (erts_aint32_t) ~(FLGS)))
#define ERTS_RUNQ_FLGS_UNSET_NOB(RQ, FLGS)					\
    ((Uint32) erts_atomic32_read_band_nob(&(RQ)->flags,		\
					      (erts_aint32_t) ~(FLGS)))
#define ERTS_RUNQ_FLGS_GET(RQ)						\
    ((Uint32) erts_atomic32_read_acqb(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_GET_NOB(RQ)					\
    ((Uint32) erts_atomic32_read_nob(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_GET_MB(RQ)					\
    ((Uint32) erts_atomic32_read_mb(&(RQ)->flags))
#define ERTS_RUNQ_FLGS_READ_BSET(RQ, MSK, FLGS)		  		\
    ((Uint32) erts_atomic32_read_bset_relb(&(RQ)->flags, 		\
					       (erts_aint32_t) (MSK),	\
					       (erts_aint32_t) (FLGS)))

#define ERTS_RUNQ_POINTER_MASK  (~((erts_aint_t) 3))
#define ERTS_RUNQ_BOUND_FLAG    ((erts_aint_t) 1)

typedef enum {
    ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED,
    ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED,
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
#define ERTS_SSI_FLG_MSB_EXEC	 	(((erts_aint32_t) 1) << 5)

#define ERTS_SSI_FLGS_MAX                                       6

#define ERTS_SSI_FLGS_SLEEP_TYPE			\
 (ERTS_SSI_FLG_TSE_SLEEPING|ERTS_SSI_FLG_POLL_SLEEPING)

#define ERTS_SSI_FLGS_SLEEP				\
 (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLGS_SLEEP_TYPE)

#define ERTS_SSI_FLGS_ALL				\
 (ERTS_SSI_FLGS_SLEEP					\
  | ERTS_SSI_FLG_WAITING				\
  | ERTS_SSI_FLG_SUSPENDED                              \
  | ERTS_SSI_FLG_MSB_EXEC)

/*
 * Keep ERTS_SSI_AUX_WORK flags ordered in expected frequency
 * order relative eachother. Most frequent at lowest at lowest
 * index.
 *
 * ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX *need* to be
 * highest index...
 *
 * Remember to update description in erts_pre_init_process()
 * when adding new flags...
 */

typedef enum {
    ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP_IX,
    ERTS_SSI_AUX_WORK_DD_IX,
    ERTS_SSI_AUX_WORK_DD_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC_IX,
    ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM_IX,
    ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP_IX,
    ERTS_SSI_AUX_WORK_CNCLD_TMRS_IX,
    ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_ASYNC_READY_IX,
    ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN_IX,
    ERTS_SSI_AUX_WORK_MISC_THR_PRGR_IX,
    ERTS_SSI_AUX_WORK_MISC_IX,
    ERTS_SSI_AUX_WORK_SET_TMO_IX,
    ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK_IX,
    ERTS_SSI_AUX_WORK_YIELD_IX,
    ERTS_SSI_AUX_WORK_REAP_PORTS_IX,
    ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX, /* SHOULD be last flag index */

    ERTS_SSI_AUX_WORK_NO_FLAGS /* Not a flag index... */
} ErtsSsiAuxWorkFlagIndex;

#define ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP_IX)
#define ERTS_SSI_AUX_WORK_DD \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DD_IX)
#define ERTS_SSI_AUX_WORK_DD_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DD_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC_IX)
#define ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM_IX)
#define ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP_IX)
#define ERTS_SSI_AUX_WORK_CNCLD_TMRS \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_CNCLD_TMRS_IX)
#define ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_ASYNC_READY \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_ASYNC_READY_IX)
#define ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN_IX)
#define ERTS_SSI_AUX_WORK_MISC_THR_PRGR \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MISC_THR_PRGR_IX)
#define ERTS_SSI_AUX_WORK_MISC \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MISC_IX)
#define ERTS_SSI_AUX_WORK_SET_TMO \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_SET_TMO_IX)
#define ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK_IX)
#define ERTS_SSI_AUX_WORK_YIELD \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_YIELD_IX)
#define ERTS_SSI_AUX_WORK_REAP_PORTS \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_REAP_PORTS_IX)
#define ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED \
    (((erts_aint32_t) 1) << ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX)

typedef struct ErtsSchedulerSleepInfo_ ErtsSchedulerSleepInfo;

typedef struct {
    erts_spinlock_t lock;
    ErtsSchedulerSleepInfo *list;
} ErtsSchedulerSleepList;

struct ErtsSchedulerSleepInfo_ {
    struct ErtsSchedulerData_ *esdp;
    ErtsSchedulerSleepInfo *next;
    ErtsSchedulerSleepInfo *prev;
    erts_atomic32_t flags;
    erts_tse_t *event;
    struct erts_poll_thread *psi;
    erts_atomic32_t aux_work;
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
    Uint64 started_interval;
    ErtsProcList* next;
    ErtsProcList* prev;
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

typedef enum {
    ERTS_SCHED_NORMAL = 0,
    ERTS_SCHED_DIRTY_CPU = 1,
    ERTS_SCHED_DIRTY_IO = 2,

    ERTS_SCHED_TYPE_FIRST = ERTS_SCHED_NORMAL,
    ERTS_SCHED_TYPE_LAST = ERTS_SCHED_DIRTY_IO
} ErtsSchedType;

typedef struct ErtsSchedulerData_ ErtsSchedulerData;

typedef struct ErtsRunQueue_ ErtsRunQueue;

typedef struct {
    erts_atomic32_t len;
    erts_aint32_t max_len;
    int reds;
} ErtsRunQueueInfo;


#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
#  undef ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT
#  define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT 1
#endif


#undef ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
#define ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT_OPT

typedef erts_atomic64_t ErtsAtomicSchedTime;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
typedef struct {
    ErtsAtomicSchedTime last;
    struct {
	Uint64 short_interval;
	Uint64 long_interval;
    } worktime;
    int is_working;
} ErtsRunQueueSchedUtil;
#endif

typedef struct {
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    int sched_util;
#endif
    Uint32 flags;
    ErtsRunQueue *misc_evac_runq;
    struct {
	struct {
	    int this;
	    int other;
	} limit;
	ErtsRunQueue *runq;
	Uint32 flags;
    } prio[ERTS_NO_PRIO_LEVELS];
} ErtsMigrationPath;

typedef struct ErtsMigrationPaths_ ErtsMigrationPaths;

struct ErtsMigrationPaths_ {
    void *block;
    ErtsMigrationPaths *next;
    ErtsThrPrgrVal thr_prgr;
    ErtsMigrationPath mpath[1];
};


struct ErtsRunQueue_ {
    int ix;

    erts_mtx_t mtx;
    erts_cnd_t cnd;

    ErtsSchedulerSleepList sleepers;

    ErtsSchedulerData *scheduler;
    int waiting; /* < 0 in sys schedule; > 0 on cnd variable */
    int woken;
    erts_atomic32_t flags;
    int check_balance_reds;
    int full_reds_history_sum;
    int full_reds_history[ERTS_FULL_REDS_HISTORY_SIZE];
    int out_of_work_count;
    erts_aint32_t max_len;
    erts_atomic32_t len;
    int wakeup_other;
    int wakeup_other_reds;

    struct {
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
	erts_atomic_t evac_runq;
    } misc;

    struct {
	ErtsRunQueueInfo info;
	Port *start;
	Port *end;
    } ports;
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    ErtsRunQueueSchedUtil sched_util;
#endif
};

extern long erts_runq_supervision_interval;

typedef union {
    ErtsRunQueue runq;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsRunQueue))];
} ErtsAlignedRunQueue;

extern ErtsAlignedRunQueue * ERTS_WRITE_UNLIKELY(erts_aligned_run_queues);

#define ERTS_PROC_REDUCTIONS_EXECUTED(SD, RQ, PRIO, REDS, AREDS)\
do {								\
    (RQ)->procs.reductions += (AREDS);				\
    (RQ)->procs.prio_info[(PRIO)].reds += (REDS);		\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (AREDS);				\
    (SD)->check_time_reds += (AREDS);				\
} while (0)

#define ERTS_PORT_REDUCTIONS_EXECUTED(SD, RQ, REDS)		\
do {								\
    (RQ)->ports.info.reds += (REDS);				\
    (RQ)->check_balance_reds -= (REDS);				\
    (RQ)->wakeup_other_reds += (REDS);				\
    (SD)->check_time_reds += (REDS);				\
} while (0)

typedef struct {
    union {
        erts_atomic32_t mod; /* on dirty schedulers */
        int need; /* "+sbu true" or scheduler_wall_time enabled */
    } u;
    int enabled;
    Uint64 start;
    struct {
	Uint64 total;
	Uint64 start;
    } working;
} ErtsSchedWallTime;

typedef struct {
    int sched;
    erts_aint32_t aux_work;
} ErtsDelayedAuxWorkWakeupJob;

typedef struct {
    int sched_id;
    ErtsSchedulerData *esdp;
    ErtsSchedulerSleepInfo *ssi;
    ErtsThrPrgrVal current_thr_prgr;
    ErtsThrPrgrVal latest_wakeup;
    struct {
	int ix;
	ErtsThrPrgrVal thr_prgr;
    } misc;
    struct {
	ErtsThrPrgrVal thr_prgr;
    } dd;
    struct {
	ErtsThrPrgrVal thr_prgr;
    } cncld_tmrs;
    struct {
	ErtsThrPrgrVal thr_prgr;
	UWord size;
	ErtsThrPrgrLaterOp *first;
	ErtsThrPrgrLaterOp *last;
    } later_op;
    struct {
	int need_thr_prgr;
	ErtsThrPrgrVal thr_prgr;
	void *queue;
    } async_ready;
    struct {
	Uint64 next;
	int *sched2jix;
	int jix;
	ErtsDelayedAuxWorkWakeupJob *job;
    } delayed_wakeup;
    struct {
        ErtsEtsAllYieldData ets_all;
        /* Other yielding operations... */
    } yield;
    struct {
	struct {
	    erts_aint32_t flags;
	    void (*callback)(void *);
	    void *arg;
	} wait_completed;
    } debug;
} ErtsAuxWorkData;

#define ERTS_SCHED_AUX_YIELD_DATA(ESDP, NAME) \
    (&(ESDP)->aux_work_data.yield.NAME)
void erts_notify_new_aux_yield_work(ErtsSchedulerData *esdp);

typedef enum {
    ERTS_DIRTY_CPU_SCHEDULER,
    ERTS_DIRTY_IO_SCHEDULER
} ErtsDirtySchedulerType;


struct ErtsSchedulerData_ {
    /*
     * Keep X registers first (so we get as many low
     * numbered registers as possible in the same cache
     * line).
     */
    Eterm* x_reg_array;		/* X registers */
    FloatDef* f_reg_array;	/* Floating point registers. */

    ErtsTimerWheel *timer_wheel;
    ErtsNextTimeoutRef next_tmo_ref;
    ErtsHLTimerService *timer_service;
    ethr_tid tid;		/* Thread id */
    struct erl_bits_state erl_bits_state; /* erl_bits.c state */
    void *match_pseudo_process; /* erl_db_util.c:db_prog_match() */
    Process *free_process;
    ErtsThrPrgrData thr_progress_data;
    ErtsSchedulerSleepInfo *ssi;
    Process *current_process;
    ErtsSchedType type;
    Uint no;			/* Scheduler number for normal schedulers */
    Uint dirty_no;  /* Scheduler number for dirty schedulers */
    Process *dirty_shadow_process;
    Port *current_port;
    ErtsRunQueue *run_queue;
    int virtual_reds;
    int cpu_id;			/* >= 0 when bound */
    ErtsAuxWorkData aux_work_data;
    ErtsAtomCacheMap atom_cache_map;

    ErtsMonotonicTime last_monotonic_time;
    int check_time_reds;

    Uint32 thr_id;
    Uint64 unique;
    Uint64 ref;

    ErtsSchedAllocData alloc_data;

    struct {
	Uint64 out;
	Uint64 in;
    } io;

    Uint64 reductions;
    ErtsSchedWallTime sched_wall_time;
    ErtsGCInfo gc_info;
    ErtsPortTaskHandle nosuspend_port_task_handle;
    ErtsEtsTables ets_tables;
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_alloc_verify_func_t verify_unused_temp_alloc;
    Allctr_t *verify_unused_temp_alloc_data;
#endif
};

typedef union {
    ErtsSchedulerData esd;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerData))];
} ErtsAlignedSchedulerData;

extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_scheduler_data);
extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_cpu_scheduler_data);
extern ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_io_scheduler_data);


#if defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_runq_is_locked(ErtsRunQueue *);
#endif

void
erts_debug_later_op_foreach(void (*callback)(void*),
                            void (*func)(void *, ErtsThrPrgrVal, void *),
                            void *arg);

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

void erts_empty_runq(ErtsRunQueue *rq);
void erts_non_empty_runq(ErtsRunQueue *rq);


/*
 * Run queue locked during modifications. We use atomic ops since
 * other threads peek at values without run queue lock.
 */

ERTS_GLB_INLINE void erts_inc_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio);
ERTS_GLB_INLINE void erts_dec_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio);
ERTS_GLB_INLINE void erts_reset_max_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_inc_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rq->len);

    if (len == 0)
	erts_non_empty_runq(rq);
    len++;
    if (rq->max_len < len)
	rq->max_len = len;
    ASSERT(len > 0);
    erts_atomic32_set_nob(&rq->len, len);

    len = erts_atomic32_read_dirty(&rqi->len);
    ASSERT(len >= 0);
    if (len == 0) {
	ASSERT((erts_atomic32_read_nob(&rq->flags)
		& ((erts_aint32_t) (1 << prio))) == 0);
	erts_atomic32_read_bor_nob(&rq->flags,
				       (erts_aint32_t) (1 << prio));
    }
    len++;
    if (rqi->max_len < len)
	rqi->max_len = len;

    erts_atomic32_set_relb(&rqi->len, len);
}

ERTS_GLB_INLINE void
erts_dec_runq_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi, int prio)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rq->len);
    len--;
    ASSERT(len >= 0);
    erts_atomic32_set_nob(&rq->len, len);

    len = erts_atomic32_read_dirty(&rqi->len);
    len--;
    ASSERT(len >= 0);
    if (len == 0) {
	ASSERT((erts_atomic32_read_nob(&rq->flags)
		& ((erts_aint32_t) (1 << prio))));
	erts_atomic32_read_band_nob(&rq->flags,
					~((erts_aint32_t) (1 << prio)));
    }
    erts_atomic32_set_relb(&rqi->len, len);

}

ERTS_GLB_INLINE void
erts_reset_max_len(ErtsRunQueue *rq, ErtsRunQueueInfo *rqi)
{
    erts_aint32_t len;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    len = erts_atomic32_read_dirty(&rqi->len);
    ASSERT(rqi->max_len >= len);
    rqi->max_len = len;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#define RUNQ_READ_LEN(X) erts_atomic32_read_nob((X))

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

/*
 * Process Specific Data.
 *
 * NOTE: Only use PSD for very rarely used data.
 */

#define ERTS_PSD_ERROR_HANDLER			0
#define ERTS_PSD_SAVED_CALLS_BUF		1
#define ERTS_PSD_SCHED_ID			2
#define ERTS_PSD_CALL_TIME_BP			3
#define ERTS_PSD_DELAYED_GC_TASK_QS		4
#define ERTS_PSD_NIF_TRAP_EXPORT		5
#define ERTS_PSD_ETS_OWNED_TABLES               6
#define ERTS_PSD_ETS_FIXED_TABLES               7
#define ERTS_PSD_DIST_ENTRY	                8
#define ERTS_PSD_SUSPENDED_SAVED_CALLS_BUF	9 /* keep last... */

#define ERTS_PSD_SIZE				10

#if !defined(HIPE)
#  undef ERTS_PSD_SUSPENDED_SAVED_CALLS_BUF
#  undef ERTS_PSD_SIZE
#  define ERTS_PSD_SIZE 9
#endif

typedef struct {
    void *data[ERTS_PSD_SIZE];
} ErtsPSD;

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_LC_PSD_ANY_LOCK (~ERTS_PROC_LOCKS_ALL)

#define ERTS_PSD_ERROR_HANDLER_BUF_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_ERROR_HANDLER_BUF_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_SAVED_CALLS_BUF_GET_LOCKS ((ErtsProcLocks) 0)
#define ERTS_PSD_SAVED_CALLS_BUF_SET_LOCKS ((ErtsProcLocks) 0)

#define ERTS_PSD_SCHED_ID_GET_LOCKS ERTS_PROC_LOCK_STATUS
#define ERTS_PSD_SCHED_ID_SET_LOCKS ERTS_PROC_LOCK_STATUS

#define ERTS_PSD_CALL_TIME_BP_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_CALL_TIME_BP_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_DELAYED_GC_TASK_QS_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_DELAYED_GC_TASK_QS_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_NIF_TRAP_EXPORT_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_NIF_TRAP_EXPORT_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_ETS_OWNED_TABLES_GET_LOCKS ERTS_PROC_LOCK_STATUS
#define ERTS_PSD_ETS_OWNED_TABLES_SET_LOCKS ERTS_PROC_LOCK_STATUS

#define ERTS_PSD_ETS_FIXED_TABLES_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_ETS_FIXED_TABLES_SET_LOCKS ERTS_PROC_LOCK_MAIN

#define ERTS_PSD_DIST_ENTRY_GET_LOCKS ERTS_PROC_LOCK_MAIN
#define ERTS_PSD_DIST_ENTRY_SET_LOCKS ERTS_PROC_LOCK_MAIN

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
    erts_spinlock_t lock;
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

typedef struct ErtsProcSysTask_ ErtsProcSysTask;
typedef struct ErtsProcSysTaskQs_ ErtsProcSysTaskQs;


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
#  define BIN_OLD_VHEAP_SZ(p) (p)->bin_old_vheap_sz
#  define BIN_OLD_VHEAP(p)    (p)->bin_old_vheap

#  define MAX_HEAP_SIZE_GET(p)     ((p)->max_heap_size >> 2)
#  define MAX_HEAP_SIZE_SET(p, sz) ((p)->max_heap_size = ((sz) << 2) |  \
                                    MAX_HEAP_SIZE_FLAGS_GET(p))
#  define MAX_HEAP_SIZE_FLAGS_GET(p)          ((p)->max_heap_size & 0x3)
#  define MAX_HEAP_SIZE_FLAGS_SET(p, flags)   ((p)->max_heap_size = flags | \
                                               ((p)->max_heap_size & ~0x3))
#  define MAX_HEAP_SIZE_KILL 1
#  define MAX_HEAP_SIZE_LOG  2

struct process {
    ErtsPTabElementCommon common; /* *Need* to be first in struct */

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
    Eterm* abandoned_heap;
    Uint heap_sz;		/* Size of heap in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */
    Uint min_vheap_size;        /* Minimum size of virtual heap (in words). */
    Uint max_heap_size;         /* Maximum size of heap (in words). */

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
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
    Uint32 rcount;		/* suspend count */
    int  schedule_count;	/* Times left to reschedule a low prio process */
    Uint reds;			/* No of reductions for this process  */
    Eterm group_leader;		/* Pid in charge (can be boxed) */
    Uint flags;			/* Trap exit, etc (no trace flags anymore) */
    Eterm fvalue;		/* Exit & Throw value (failure reason) */
    Uint freason;		/* Reason for detected failure */
    Eterm ftrace;		/* Latest exception stack trace dump */

    Process *next;		/* Pointer to next process in run queue */

    ErtsMonitor *suspend_monitors; /* Processes suspended by this process via
                                      erlang:suspend_process/1 */

    ErtsSignalPrivQueues sig_qs; /* Signal queues */
    ErtsBifTimers *bif_timers;	/* Bif timers aiming at this process */

    ProcDict  *dictionary;       /* Process dictionary, may be NULL */

    Uint seq_trace_clock;
    Uint seq_trace_lastcnt;
    Eterm seq_trace_token;	/* Sequential trace token (tuple size 5 see below) */

#ifdef USE_VM_PROBES
    Eterm dt_utag;              /* Place to store the dynamc trace user tag */
    Uint dt_utag_flags;         /* flag field for the dt_utag */
#endif
    union {
	void *terminate;
	ErtsCodeMFA initial;	/* Initial module(0), function(1), arity(2),
                                   often used instead of pointer to funcinfo
                                   instruction. */
    } u;
    ErtsCodeMFA* current;	/* Current Erlang function, part of the funcinfo:
				 * module(0), function(1), arity(2)
				 * (module and functions are tagged atoms;
				 * arity an untagged integer).
				 */

    /*
     * Information mainly for post-mortem use (erl crash dump).
     */
    Eterm parent;		/* Pid of process that created this process. */

    Uint32 static_flags;        /* Flags that do *not* change */

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
    ErlHeapFragment* mbuf;	/* Pointer to heap fragment list */
    ErlHeapFragment* live_hf_end;
    ErtsMessage *msg_frag;	/* Pointer to message fragment list */
    Uint mbuf_sz;		/* Total size of heap fragments and message fragments */
    erts_atomic_t psd;		/* Rarely used process specific data */

    Uint64 bin_vheap_sz;	/* Virtual heap block size for binaries */
    Uint64 bin_old_vheap_sz;	/* Virtual old heap block size for binaries */
    Uint64 bin_old_vheap;	/* Virtual old heap size for binaries */

    ErtsProcSysTaskQs *sys_task_qs;
    ErtsProcSysTask *dirty_sys_tasks;

    erts_atomic32_t state;  /* Process state flags (see ERTS_PSFLG_*) */
    erts_atomic32_t dirty_state; /* Process dirty state flags (see ERTS_PDSFLG_*) */

    ErtsSignalInQueue sig_inq;
    ErlTraceMessageQueue *trace_msg_q;
    erts_proc_lock_t lock;
    ErtsSchedulerData *scheduler_data;
    Eterm suspendee;
    ErtsPendingSuspend *pending_suspenders;
    erts_atomic_t run_queue;
#ifdef HIPE
    struct hipe_process_state_smp hipe_smp;
#endif

#ifdef CHECK_FOR_HOLES
    Eterm* last_htop;		/* No need to scan the heap below this point. */
    ErlHeapFragment* last_mbuf;	/* No need to scan beyond this mbuf. */
    ErlHeapFragment* heap_hfrag; /* Heap abandoned, htop now lives in this frag */
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

#ifdef DEBUG
    Uint debug_reds_in;
#endif
};

extern Eterm erts_init_process_id; /* pid of init process */
extern const Process erts_invalid_process;

#ifdef CHECK_FOR_HOLES
# define INIT_HOLE_CHECK(p)			\
do {						\
  (p)->last_htop = 0;				\
  (p)->last_mbuf = 0;				\
  (p)->heap_hfrag = NULL;			\
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

#if ERTS_NO_PROC_PRIO_LEVELS > 4
#  error "Need to increase ERTS_PSFLG_PRIO_SHIFT"
#endif

#define ERTS_PSFLGS_PRIO_BITS 2
#define ERTS_PSFLGS_PRIO_MASK \
    ((((erts_aint32_t) 1) << ERTS_PSFLGS_PRIO_BITS) - 1)

#define ERTS_PSFLGS_ACT_PRIO_OFFSET (0*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_USR_PRIO_OFFSET (1*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_PRQ_PRIO_OFFSET (2*ERTS_PSFLGS_PRIO_BITS)
#define ERTS_PSFLGS_ZERO_BIT_OFFSET (3*ERTS_PSFLGS_PRIO_BITS)

#define ERTS_PSFLGS_QMASK_BITS 4
#define ERTS_PSFLGS_QMASK \
    ((((erts_aint32_t) 1) << ERTS_PSFLGS_QMASK_BITS) - 1)
#define ERTS_PSFLGS_IN_PRQ_MASK_OFFSET \
    ERTS_PSFLGS_ZERO_BIT_OFFSET

#define ERTS_PSFLG_BIT(N) \
    (((erts_aint32_t) 1) << (ERTS_PSFLGS_ZERO_BIT_OFFSET + (N)))

/*
 * ACT_PRIO -> Active prio, i.e., currently active prio. This
 *             prio may be higher than user prio.
 * USR_PRIO -> User prio. i.e., prio the user has set.
 * PRQ_PRIO -> Prio queue prio, i.e., prio queue currently
 *             enqueued in. 
 *
 * Update etp-proc-state-int in $ERL_TOP/erts/etc/unix/etp-commands.in
 * when changing ERTS_PSFLG_*.
 */
#define ERTS_PSFLGS_ACT_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_ACT_PRIO_OFFSET)
#define ERTS_PSFLGS_USR_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_USR_PRIO_OFFSET)
#define ERTS_PSFLGS_PRQ_PRIO_MASK \
    (ERTS_PSFLGS_PRIO_MASK << ERTS_PSFLGS_PRQ_PRIO_OFFSET)
#define ERTS_PSFLG_IN_PRQ_MAX 		ERTS_PSFLG_BIT(0)
#define ERTS_PSFLG_IN_PRQ_HIGH		ERTS_PSFLG_BIT(1)
#define ERTS_PSFLG_IN_PRQ_NORMAL	ERTS_PSFLG_BIT(2)
#define ERTS_PSFLG_IN_PRQ_LOW 		ERTS_PSFLG_BIT(3)
#define ERTS_PSFLG_FREE			ERTS_PSFLG_BIT(4)
#define ERTS_PSFLG_EXITING		ERTS_PSFLG_BIT(5)
#define ERTS_PSFLG_UNUSED		ERTS_PSFLG_BIT(6)
#define ERTS_PSFLG_ACTIVE		ERTS_PSFLG_BIT(7)
#define ERTS_PSFLG_IN_RUNQ		ERTS_PSFLG_BIT(8)
#define ERTS_PSFLG_RUNNING		ERTS_PSFLG_BIT(9)
#define ERTS_PSFLG_SUSPENDED		ERTS_PSFLG_BIT(10)
#define ERTS_PSFLG_GC			ERTS_PSFLG_BIT(11)
#define ERTS_PSFLG_SYS_TASKS		ERTS_PSFLG_BIT(12)
#define ERTS_PSFLG_SIG_IN_Q		ERTS_PSFLG_BIT(13)
#define ERTS_PSFLG_ACTIVE_SYS		ERTS_PSFLG_BIT(14)
#define ERTS_PSFLG_RUNNING_SYS		ERTS_PSFLG_BIT(15)
#define ERTS_PSFLG_PROXY		ERTS_PSFLG_BIT(16)
#define ERTS_PSFLG_DELAYED_SYS		ERTS_PSFLG_BIT(17)
#define ERTS_PSFLG_OFF_HEAP_MSGQ	ERTS_PSFLG_BIT(18)
#define ERTS_PSFLG_SIG_Q		ERTS_PSFLG_BIT(19)
#define ERTS_PSFLG_DIRTY_CPU_PROC	ERTS_PSFLG_BIT(20)
#define ERTS_PSFLG_DIRTY_IO_PROC	ERTS_PSFLG_BIT(21)
#define ERTS_PSFLG_DIRTY_ACTIVE_SYS	ERTS_PSFLG_BIT(22)
#define ERTS_PSFLG_DIRTY_RUNNING	ERTS_PSFLG_BIT(23)
#define ERTS_PSFLG_DIRTY_RUNNING_SYS	ERTS_PSFLG_BIT(24)

#define ERTS_PSFLG_MAX  (ERTS_PSFLGS_ZERO_BIT_OFFSET + 24)

#define ERTS_PSFLGS_DIRTY_WORK		(ERTS_PSFLG_DIRTY_CPU_PROC	\
					 | ERTS_PSFLG_DIRTY_IO_PROC	\
					 | ERTS_PSFLG_DIRTY_ACTIVE_SYS)

#define ERTS_PSFLGS_IN_PRQ_MASK 	(ERTS_PSFLG_IN_PRQ_MAX		\
					 | ERTS_PSFLG_IN_PRQ_HIGH	\
					 | ERTS_PSFLG_IN_PRQ_NORMAL	\
					 | ERTS_PSFLG_IN_PRQ_LOW)

#define ERTS_PSFLGS_VOLATILE_HEAP	(ERTS_PSFLG_EXITING		\
					 | ERTS_PSFLG_DIRTY_RUNNING	\
					 | ERTS_PSFLG_DIRTY_RUNNING_SYS)

#define ERTS_PSFLGS_GET_ACT_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_ACT_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)
#define ERTS_PSFLGS_GET_USR_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_USR_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)
#define ERTS_PSFLGS_GET_PRQ_PRIO(PSFLGS) \
    (((PSFLGS) >> ERTS_PSFLGS_PRQ_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK)


/*
 * Flags in the dirty_state field.
 */

#define ERTS_PDSFLG_IN_CPU_PRQ_MAX 	(((erts_aint32_t) 1) << 0)
#define ERTS_PDSFLG_IN_CPU_PRQ_HIGH	(((erts_aint32_t) 1) << 1)
#define ERTS_PDSFLG_IN_CPU_PRQ_NORMAL	(((erts_aint32_t) 1) << 2)
#define ERTS_PDSFLG_IN_CPU_PRQ_LOW 	(((erts_aint32_t) 1) << 3)
#define ERTS_PDSFLG_IN_IO_PRQ_MAX 	(((erts_aint32_t) 1) << 4)
#define ERTS_PDSFLG_IN_IO_PRQ_HIGH	(((erts_aint32_t) 1) << 5)
#define ERTS_PDSFLG_IN_IO_PRQ_NORMAL	(((erts_aint32_t) 1) << 6)
#define ERTS_PDSFLG_IN_IO_PRQ_LOW 	(((erts_aint32_t) 1) << 7)

#define ERTS_PDSFLGS_QMASK 		ERTS_PSFLGS_QMASK
#define ERTS_PDSFLGS_IN_CPU_PRQ_MASK_OFFSET 0
#define ERTS_PDSFLGS_IN_IO_PRQ_MASK_OFFSET ERTS_PSFLGS_QMASK_BITS

#define ERTS_PDSFLG_IN_CPU_PRQ_MASK 	(ERTS_PDSFLG_IN_CPU_PRQ_MAX	\
					 | ERTS_PDSFLG_IN_CPU_PRQ_HIGH	\
					 | ERTS_PDSFLG_IN_CPU_PRQ_NORMAL\
					 | ERTS_PDSFLG_IN_CPU_PRQ_LOW)
#define ERTS_PDSFLG_IN_IO_PRQ_MASK 	(ERTS_PDSFLG_IN_CPU_PRQ_MAX	\
					 | ERTS_PDSFLG_IN_CPU_PRQ_HIGH	\
					 | ERTS_PDSFLG_IN_CPU_PRQ_NORMAL\
					 | ERTS_PDSFLG_IN_CPU_PRQ_LOW)


/*
 * Static flags that do not change after process creation.
 */
#define ERTS_STC_FLG_SYSTEM_PROC	(((Uint32) 1) << 0)
#define ERTS_STC_FLG_SHADOW_PROC	(((Uint32) 1) << 1)

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

#ifdef USE_VM_PROBES
/* The dtrace probe for seq_trace only supports 'int' labels, so we represent
 * all values that won't fit into a 32-bit signed integer as ERTS_SINT32_MIN
 * (bigints, tuples, etc). */

#define SEQ_TRACE_T_DTRACE_LABEL(token) \
    DTRACE_SEQ_TRACE_LABEL__(SEQ_TRACE_T_LABEL(token))

#define DTRACE_SEQ_TRACE_LABEL__(label_term) \
    (is_small((label_term)) ? \
        ((signed_val((label_term)) <= ERTS_SINT32_MAX && \
          signed_val((label_term)) >= ERTS_SINT32_MIN) ? \
             signed_val((label_term)) : ERTS_SINT32_MIN) \
        : ERTS_SINT32_MIN)
#endif

/*

 * Possible flags for the flags field in ErlSpawnOpts below.
 */

#define SPO_LINK 1
#define SPO_USE_ARGS 2
#define SPO_MONITOR 4
#define SPO_SYSTEM_PROC 8
#define SPO_OFF_HEAP_MSGQ 16
#define SPO_ON_HEAP_MSGQ 32

extern int ERTS_WRITE_UNLIKELY(erts_default_spo_flags);

/*
 * The following struct contains options for a process to be spawned.
 */
typedef struct {
    int flags;
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
    Uint max_heap_size;         /* Maximum heap size in words */
    Uint max_heap_flags;        /* Maximum heap flags (kill | log) */
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
    Uint sz;

    ASSERT(hf!=NULL && (hp - hf->mem < hf->alloc_size));

    sz = hp - hf->mem;
    p->mbuf_sz -= hf->used_size - sz;
    hf->used_size = sz;
}	
#endif /* inline */

Eterm* erts_heap_alloc(Process* p, Uint need, Uint xtra);
#ifdef CHECK_FOR_HOLES
Eterm* erts_set_hole_marker(Eterm* ptr, Uint sz);
#endif

extern erts_rwmtx_t erts_cpu_bind_rwmtx;
/* If any of the erts_system_monitor_* variables are set (enabled),
** erts_system_monitor must be != NIL, to allow testing on just
** the erts_system_monitor_* variables.
*/
extern Eterm ERTS_WRITE_UNLIKELY(erts_system_monitor);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_gc);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_schedule);
extern Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_large_heap);
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
extern int erts_system_profile_ts_type;

/* process flags */
#define F_HIBERNATE_SCHED    (1 <<  0) /* Schedule out after hibernate op */
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
#define F_DISABLE_GC         (1 << 11) /* Disable GC (see below) */
#define F_OFF_HEAP_MSGQ      (1 << 12) /* Off heap msg queue */
#define F_ON_HEAP_MSGQ       (1 << 13) /* On heap msg queue */
#define F_OFF_HEAP_MSGQ_CHNG (1 << 14) /* Off heap msg queue changing */
#define F_ABANDONED_HEAP_USE (1 << 15) /* Have usage of abandoned heap */
#define F_DELAY_GC           (1 << 16) /* Similar to disable GC (see below) */
#define F_SCHDLR_ONLN_WAITQ  (1 << 17) /* Process enqueued waiting to change schedulers online */
#define F_HAVE_BLCKD_NMSCHED (1 << 18) /* Process has blocked normal multi-scheduling */
#define F_HIPE_MODE          (1 << 19) /* Process is executing in HiPE mode */
#define F_DELAYED_DEL_PROC   (1 << 20) /* Delay delete process (dirty proc exit case) */
#define F_DIRTY_CLA          (1 << 21) /* Dirty copy literal area scheduled */
#define F_DIRTY_GC_HIBERNATE (1 << 22) /* Dirty GC hibernate scheduled */
#define F_DIRTY_MAJOR_GC     (1 << 23) /* Dirty major GC scheduled */
#define F_DIRTY_MINOR_GC     (1 << 24) /* Dirty minor GC scheduled */
#define F_HIBERNATED         (1 << 25) /* Hibernated */
#define F_LOCAL_SIGS_ONLY    (1 << 26)
#define F_TRAP_EXIT          (1 << 27) /* Trapping exit */
#define F_DEFERRED_SAVED_LAST (1 << 28)

/*
 * F_DISABLE_GC and F_DELAY_GC are similar. Both will prevent
 * GC of the process, but it is important to use the right
 * one:
 * - F_DISABLE_GC should *only* be used by BIFs. This when
 *   the BIF needs to yield while preventig a GC.
 * - F_DELAY_GC should only be used when GC is temporarily
 *   disabled while the process is scheduled. A process must
 *   not be scheduled out while F_DELAY_GC is set.
 */

#define ERTS_TRACE_FLAGS_TS_TYPE_SHIFT			0

#define F_TRACE_FLAG(N)      (1 << (ERTS_TRACE_TS_TYPE_BITS + (N)))

/* process trace_flags */
#define F_NOW_TS             (ERTS_TRACE_FLG_NOW_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_STRICT_MON_TS      (ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_MON_TS             (ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP \
			      << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)
#define F_SENSITIVE          F_TRACE_FLAG(0)
#define F_TRACE_SEND         F_TRACE_FLAG(1)
#define F_TRACE_RECEIVE      F_TRACE_FLAG(2)
#define F_TRACE_SOS          F_TRACE_FLAG(3) /* Set on spawn       */
#define F_TRACE_SOS1         F_TRACE_FLAG(4) /* Set on first spawn */
#define F_TRACE_SOL          F_TRACE_FLAG(5) /* Set on link        */
#define F_TRACE_SOL1         F_TRACE_FLAG(6) /* Set on first link  */
#define F_TRACE_CALLS        F_TRACE_FLAG(7)
#define F_TRACE_PROCS        F_TRACE_FLAG(8)
#define F_TRACE_FIRST_CHILD  F_TRACE_FLAG(9)
#define F_TRACE_SCHED        F_TRACE_FLAG(10)
#define F_TRACE_GC           F_TRACE_FLAG(11)
#define F_TRACE_ARITY_ONLY   F_TRACE_FLAG(12)
#define F_TRACE_RETURN_TO    F_TRACE_FLAG(13) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       F_TRACE_FLAG(14) /* No call trace msg suppress */
#define F_EXCEPTION_TRACE    F_TRACE_FLAG(15) /* May have exception trace on stack */

/* port trace flags, currently the same as process trace flags */
#define F_TRACE_SCHED_PORTS  F_TRACE_FLAG(17) /* Trace of port scheduling */
#define F_TRACE_SCHED_PROCS  F_TRACE_FLAG(18) /* With virtual scheduling */
#define F_TRACE_PORTS	     F_TRACE_FLAG(19) /* Ports equivalent to F_TRACE_PROCS */
#define F_TRACE_SCHED_NO     F_TRACE_FLAG(20) /* Trace with scheduler id */
#define F_TRACE_SCHED_EXIT   F_TRACE_FLAG(21)

#define F_NUM_FLAGS          (ERTS_TRACE_TS_TYPE_BITS + 22)
#ifdef DEBUG
#  define F_INITIAL_TRACE_FLAGS (5 << F_NUM_FLAGS)
#else
#  define F_INITIAL_TRACE_FLAGS 0
#endif

/* F_TIMESTAMP_MASK is a bit-field of all all timestamp types */
#define F_TIMESTAMP_MASK \
    (ERTS_TRACE_TS_TYPE_MASK << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)

#define TRACEE_FLAGS ( F_TRACE_PROCS | F_TRACE_CALLS \
		     | F_TRACE_SOS |  F_TRACE_SOS1| F_TRACE_RECEIVE  \
		     | F_TRACE_SOL | F_TRACE_SOL1 | F_TRACE_SEND \
		     | F_TRACE_SCHED | F_TIMESTAMP_MASK | F_TRACE_GC \
		     | F_TRACE_ARITY_ONLY | F_TRACE_RETURN_TO \
                     | F_TRACE_SILENT | F_TRACE_SCHED_PROCS | F_TRACE_PORTS \
		     | F_TRACE_SCHED_PORTS | F_TRACE_SCHED_NO \
		     | F_TRACE_SCHED_EXIT )


#define ERTS_TRACEE_MODIFIER_FLAGS \
    (F_TRACE_SILENT | F_TIMESTAMP_MASK | F_TRACE_SCHED_NO \
     | F_TRACE_RECEIVE | F_TRACE_SEND)
#define ERTS_PORT_TRACEE_FLAGS                                     \
    (ERTS_TRACEE_MODIFIER_FLAGS | F_TRACE_PORTS | F_TRACE_SCHED_PORTS)
#define ERTS_PROC_TRACEE_FLAGS \
    ((TRACEE_FLAGS & ~ERTS_PORT_TRACEE_FLAGS) | ERTS_TRACEE_MODIFIER_FLAGS)

#define SEQ_TRACE_FLAG(N)        (1 << (ERTS_TRACE_TS_TYPE_BITS + (N)))

#define ERTS_SIG_ENABLE_TRACE_FLAGS \
    ( F_TRACE_RECEIVE | F_TRACE_PROCS)

/*
 * F_TRACE_RECEIVE is always enabled/disable via signaling.
 * F_TRACE_PROCS enable/disable F_TRACE_PROCS_SIG via signaling.
 */

/* Sequential trace flags */

/* SEQ_TRACE_TIMESTAMP_MASK is a bit-field */
#define SEQ_TRACE_TIMESTAMP_MASK \
    (ERTS_TRACE_TS_TYPE_MASK << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)

#define SEQ_TRACE_SEND     (1 << 0)
#define SEQ_TRACE_RECEIVE  (1 << 1)
#define SEQ_TRACE_PRINT    (1 << 2)

#define ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT 3

#define SEQ_TRACE_NOW_TS   (ERTS_TRACE_FLG_NOW_TIMESTAMP \
			    << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)
#define SEQ_TRACE_STRICT_MON_TS (ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP \
				 << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)
#define SEQ_TRACE_MON_TS (ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP \
			  << ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)

#ifdef USE_VM_PROBES
#define DT_UTAG_PERMANENT (1 << 0)
#define DT_UTAG_SPREADING (1 << 1)
#define DT_UTAG(P) ((P)->dt_utag)
#define DT_UTAG_FLAGS(P)  ((P)->dt_utag_flags) 
#endif

#define CANCEL_TIMER(P)					\
    do {						\
	if ((P)->flags & (F_INSLPQUEUE|F_TIMO)) {	\
	    if ((P)->flags & F_INSLPQUEUE)		\
		erts_cancel_proc_timer((P));		\
	    else					\
		(P)->flags &= ~F_TIMO;			\
	}						\
    } while (0)

#define ERTS_NUM_DIRTY_CPU_RUNQS 1
#define ERTS_NUM_DIRTY_IO_RUNQS 1

#define ERTS_NUM_DIRTY_RUNQS (ERTS_NUM_DIRTY_CPU_RUNQS+ERTS_NUM_DIRTY_IO_RUNQS)

#define ERTS_RUNQ_IX(IX)						\
  (ASSERT(0 <= (IX) && (IX) < erts_no_run_queues+ERTS_NUM_DIRTY_RUNQS), \
   &erts_aligned_run_queues[(IX)].runq)
#define ERTS_RUNQ_IX_IS_DIRTY(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_run_queues+ERTS_NUM_DIRTY_RUNQS), \
   (erts_no_run_queues <= (IX)))
#define ERTS_DIRTY_RUNQ_IX(IX)						\
  (ASSERT(ERTS_RUNQ_IX_IS_DIRTY(IX)),					\
   &erts_aligned_run_queues[(IX)].runq)
#define ERTS_DIRTY_CPU_RUNQ (&erts_aligned_run_queues[erts_no_run_queues].runq)
#define ERTS_DIRTY_IO_RUNQ  (&erts_aligned_run_queues[erts_no_run_queues+1].runq)
#define ERTS_RUNQ_IS_DIRTY_CPU_RUNQ(RQ) ((RQ) == ERTS_DIRTY_CPU_RUNQ)
#define ERTS_RUNQ_IS_DIRTY_IO_RUNQ(RQ) ((RQ) == ERTS_DIRTY_IO_RUNQ)
#define ERTS_SCHEDULER_IX(IX)						\
  (ASSERT(0 <= (IX) && (IX) < erts_no_schedulers),			\
   &erts_aligned_scheduler_data[(IX)].esd)
#define ERTS_DIRTY_CPU_SCHEDULER_IX(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_dirty_cpu_schedulers),		\
   &erts_aligned_dirty_cpu_scheduler_data[(IX)].esd)
#define ERTS_DIRTY_IO_SCHEDULER_IX(IX)					\
  (ASSERT(0 <= (IX) && (IX) < erts_no_dirty_io_schedulers),		\
   &erts_aligned_dirty_io_scheduler_data[(IX)].esd)
#define ERTS_SCHEDULER_IS_DIRTY(ESDP)					\
  ((ESDP)->type != ERTS_SCHED_NORMAL)
#define ERTS_SCHEDULER_IS_DIRTY_CPU(ESDP)				\
    ((ESDP)->type == ERTS_SCHED_DIRTY_CPU)
#define ERTS_SCHEDULER_IS_DIRTY_IO(ESDP)				\
    ((ESDP)->type == ERTS_SCHED_DIRTY_IO)

void erts_pre_init_process(void);
void erts_late_init_process(void);
void erts_early_init_scheduling(int);
void erts_init_scheduling(int, int, int, int, int, int);
void erts_execute_dirty_system_task(Process *c_p);
int erts_set_gc_state(Process *c_p, int enable);
Eterm erts_sched_wall_time_request(Process *c_p, int set, int enable,
                                   int dirty_cpu, int want_dirty_io);
Eterm erts_system_check_request(Process *c_p);
Eterm erts_gc_info_request(Process *c_p);
Uint64 erts_get_proc_interval(void);
Uint64 erts_ensure_later_proc_interval(Uint64);
Uint64 erts_step_proc_interval(void);

ErtsProcList *erts_proclist_create(Process *);
ErtsProcList *erts_proclist_copy(ErtsProcList *);
void erts_proclist_destroy(ErtsProcList *);
void erts_proclist_dump(fmtfn_t to, void *to_arg, ErtsProcList*);

ERTS_GLB_INLINE int erts_proclist_same(ErtsProcList *, Process *);
ERTS_GLB_INLINE void erts_proclist_store_first(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE void erts_proclist_store_last(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_first(ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_last(ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_next(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_prev(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_first(ErtsProcList **);
ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_last(ErtsProcList **);
ERTS_GLB_INLINE int erts_proclist_fetch(ErtsProcList **, ErtsProcList **);
ERTS_GLB_INLINE void erts_proclist_remove(ErtsProcList **, ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_empty(ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_first(ErtsProcList *, ErtsProcList *);
ERTS_GLB_INLINE int erts_proclist_is_last(ErtsProcList *, ErtsProcList *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_proclist_same(ErtsProcList *plp, Process *p)
{
    return (plp->pid == p->common.id
	    && (plp->started_interval
		== p->common.u.alive.started_interval));
}

ERTS_GLB_INLINE void erts_proclist_store_first(ErtsProcList **list,
					       ErtsProcList *element)
{
    if (!*list)
	element->next = element->prev = element;
    else {
	element->prev = (*list)->prev;
	element->next = *list;
	element->prev->next = element;
	element->next->prev = element;
    }
    *list = element;
}

ERTS_GLB_INLINE void erts_proclist_store_last(ErtsProcList **list,
					      ErtsProcList *element)
{
    if (!*list) {
	element->next = element->prev = element;
	*list = element;
    }
    else {
	element->prev = (*list)->prev;
	element->next = *list;
	element->prev->next = element;
	element->next->prev = element;
    }
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_first(ErtsProcList *list)
{
    return list;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_last(ErtsProcList *list)
{
    if (!list)
	return NULL;
    else
	return list->prev;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_next(ErtsProcList *list,
						      ErtsProcList *element)
{
    ErtsProcList *next;
    ASSERT(list && element);
    next = element->next;
    return list == next ? NULL : next;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_peek_prev(ErtsProcList *list,
						      ErtsProcList *element)
{
    ErtsProcList *prev;
    ASSERT(list && element);
    prev = element->prev;
    return list == element ? NULL : prev;
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_first(ErtsProcList **list)
{
    if (!*list)
	return NULL;
    else {
	ErtsProcList *res = *list;
	if (res->next == *list)
	    *list = NULL;
	else
	    *list = res->next;
	res->next->prev = res->prev;
	res->prev->next = res->next;
	return res;
    }
}

ERTS_GLB_INLINE ErtsProcList *erts_proclist_fetch_last(ErtsProcList **list)
{
    if (!*list)
	return NULL;
    else {
	ErtsProcList *res = (*list)->prev;
	if (res == *list)
	    *list = NULL;
	res->next->prev = res->prev;
	res->prev->next = res->next;
	return res;
    }
}

ERTS_GLB_INLINE int erts_proclist_fetch(ErtsProcList **list_first,
					ErtsProcList **list_last)
{
    if (!*list_first) {
	if (list_last)
	    *list_last = NULL;
	return 0;
    }
    else {
	if (list_last)
	    *list_last = (*list_first)->prev;
	(*list_first)->prev->next = NULL;
	(*list_first)->prev = NULL;
	return !0;
    }
}

ERTS_GLB_INLINE void erts_proclist_remove(ErtsProcList **list,
					  ErtsProcList *element)
{
    ASSERT(list && *list);
    if (*list == element) {
	*list = element->next;
	if (*list == element)
	    *list = NULL;
    }
    element->next->prev = element->prev;
    element->prev->next = element->next;
}

ERTS_GLB_INLINE int erts_proclist_is_empty(ErtsProcList *list)
{
    return list == NULL;
}

ERTS_GLB_INLINE int erts_proclist_is_first(ErtsProcList *list,
					   ErtsProcList *element)
{
    ASSERT(list && element);
    return list == element;
}

ERTS_GLB_INLINE int erts_proclist_is_last(ErtsProcList *list,
					  ErtsProcList *element)
{
    ASSERT(list && element);
    return list->prev == element;
}

#endif

int erts_sched_set_wakeup_other_threshold(ErtsSchedType sched_type, char *str);
int erts_sched_set_wakeup_other_type(ErtsSchedType sched_type, char *str);
int erts_sched_set_busy_wait_threshold(ErtsSchedType sched_type, char *str);
int erts_sched_set_wake_cleanup_threshold(char *);

void erts_schedule_thr_prgr_later_op(void (*)(void *),
				     void *,
				     ErtsThrPrgrLaterOp *);
void erts_schedule_thr_prgr_later_cleanup_op(void (*)(void *),
					     void *,
					     ErtsThrPrgrLaterOp *,
					     UWord);
void erts_schedule_complete_off_heap_message_queue_change(Eterm pid);
struct db_fixation;
void erts_schedule_ets_free_fixation(Eterm pid, struct db_fixation*);
void erts_schedule_flush_trace_messages(Process *proc, int force_on_proc);
int erts_flush_trace_messages(Process *c_p, ErtsProcLocks locks);
int erts_sig_prio(Eterm pid, int prio);

#if defined(ERTS_ENABLE_LOCK_CHECK)
int erts_dbg_check_halloc_lock(Process *p);
#endif
void
erts_schedulers_state(Uint *, Uint *, Uint *, Uint *, Uint *, Uint *, Uint *, Uint *);
ErtsSchedSuspendResult
erts_set_schedulers_online(Process *p,
			   ErtsProcLocks plocks,
			   Sint new_no,
			   Sint *old_no,
			   int dirty_only);
ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *, ErtsProcLocks, int, int, int);
int erts_is_multi_scheduling_blocked(void);
Eterm erts_multi_scheduling_blockers(Process *, int);
void erts_start_schedulers(void);
void erts_alloc_notify_delayed_dealloc(int);
void erts_alloc_ensure_handle_delayed_dealloc_call(int);
void erts_notify_canceled_timer(ErtsSchedulerData *, int);
void erts_notify_check_async_ready_queue(void *);
void erts_notify_code_ix_activation(Process* p, ErtsThrPrgrVal later);
void erts_notify_finish_breakpointing(Process* p);
void erts_schedule_misc_aux_work(int sched_id,
				 void (*func)(void *),
				 void *arg);
void erts_schedule_multi_misc_aux_work(int ignore_self,
				       int max_sched,
				       void (*func)(void *),
				       void *arg);
erts_aint32_t erts_set_aux_work_timeout(int, erts_aint32_t, int);
void erts_aux_work_timeout_late_init(ErtsSchedulerData *esdp);
void erts_sched_notify_check_cpu_bind(void);
Uint erts_active_schedulers(void);
void erts_init_process(int, int, int);
Eterm erts_process_state2status(erts_aint32_t);
Eterm erts_process_status(Process *, Eterm);
Uint erts_run_queues_len(Uint *, int, int, int);
void erts_add_to_runq(Process *);
Eterm erts_bound_schedulers_term(Process *c_p);
Eterm erts_get_cpu_topology_term(Process *c_p, Eterm which);
Eterm erts_get_schedulers_binds(Process *c_p);
Eterm erts_set_cpu_topology(Process *c_p, Eterm term);
Eterm erts_bind_schedulers(Process *c_p, Eterm how);
ErtsRunQueue *erts_schedid2runq(Uint);
Process *erts_schedule(ErtsSchedulerData *, Process*, int);
void erts_schedule_misc_op(void (*)(void *), void *);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_set_self_exiting(Process *, Eterm);
void erts_do_exit_process(Process*, Eterm);
void erts_continue_exit_process(Process *);
void erts_proc_exit_link(Process *, ErtsLink *, Uint16, Eterm, Eterm);
/* Begin System profile */
Uint erts_runnable_process_count(void);
/* End System profile */
void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);
#ifdef DEBUG
void erts_debug_verify_clean_empty_process(Process* p);
#endif
void erts_stack_dump(fmtfn_t to, void *to_arg, Process *);
void erts_limited_stack_trace(fmtfn_t to, void *to_arg, Process *);
void erts_program_counter_info(fmtfn_t to, void *to_arg, Process *);
void erts_print_scheduler_info(fmtfn_t to, void *to_arg, ErtsSchedulerData *esdp);
void erts_print_run_queue_info(fmtfn_t, void *to_arg, ErtsRunQueue*);
void erts_dump_extended_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg);
void erts_dump_process_state(fmtfn_t to, void *to_arg, erts_aint32_t psflg);

typedef struct {
    Process *c_p;
    Eterm reason;
} ErtsProcExitContext;
void erts_proc_exit_handle_monitor(ErtsMonitor *mon, void *vctxt);
void erts_proc_exit_handle_link(ErtsLink *lnk, void *vctxt);

Eterm erts_get_process_priority(erts_aint32_t state);
Eterm erts_set_process_priority(Process *p, Eterm prio);

Uint erts_get_total_context_switches(void);
void erts_get_total_reductions(Uint *, Uint *);
void erts_get_exact_total_reductions(Process *, Uint *, Uint *);

Eterm erts_fake_scheduler_bindings(Process *p, Eterm how);

void erts_sched_stat_modify(int what);
Eterm erts_sched_stat_term(Process *p, int total);

void erts_free_proc(Process *);

void erts_suspend(Process*, ErtsProcLocks, Port*);
void erts_resume(Process*, ErtsProcLocks);
int erts_resume_processes(ErtsProcList *);

void erts_deep_process_dump(fmtfn_t, void *);

Eterm erts_get_reader_groups_map(Process *c_p);
Eterm erts_debug_reader_groups_map(Process *c_p, int groups);

Uint erts_debug_nbalance(void);

#define ERTS_DEBUG_WAIT_COMPLETED_DEALLOCATIONS		(1 << 0)
#define ERTS_DEBUG_WAIT_COMPLETED_TIMER_CANCELLATIONS	(1 << 1)

int erts_debug_wait_completed(Process *c_p, int flags);

Uint erts_process_memory(Process *c_p, int incl_msg_inq);

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(P)					\
do {										\
    ErtsSchedulerData *esdp__ = ((P)						\
				 ? erts_proc_sched_data((Process *) (P))	\
				 : erts_get_scheduler_data());			\
    if (esdp__ && !ERTS_SCHEDULER_IS_DIRTY(esdp__))				\
	esdp__->verify_unused_temp_alloc(					\
	    esdp__->verify_unused_temp_alloc_data);				\
} while (0)
#else
#  define ERTS_VERIFY_UNUSED_TEMP_ALLOC(ESDP)
#endif

ErtsSchedulerData *erts_get_scheduler_data(void);

void erts_schedule_process(Process *, erts_aint32_t, ErtsProcLocks);
erts_aint32_t erts_proc_sys_schedule(Process *p, erts_aint32_t state,
                                     erts_aint32_t enable_flag);

ERTS_GLB_INLINE void erts_proc_notify_new_message(Process *p, ErtsProcLocks locks);
ERTS_GLB_INLINE void erts_schedule_dirty_sys_execution(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void
erts_proc_notify_new_message(Process *p, ErtsProcLocks locks)
{
    /* No barrier needed, due to msg lock */
    erts_aint32_t state = erts_atomic32_read_nob(&p->state);
    if (!(state & ERTS_PSFLG_ACTIVE))
	erts_schedule_process(p, state, locks);
}

ERTS_GLB_INLINE void
erts_schedule_dirty_sys_execution(Process *c_p)
{
    erts_aint32_t a, n, e;

    a = erts_atomic32_read_nob(&c_p->state);

    /*
     * Only a currently executing process schedules
     * itself for dirty-sys execution...
     */

    ASSERT(a & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS));

    /* Don't set dirty-active-sys if we are about to exit... */

    while (!(a & (ERTS_PSFLG_DIRTY_ACTIVE_SYS
                  | ERTS_PSFLG_EXITING))) {
        e = a;
        n = a | ERTS_PSFLG_DIRTY_ACTIVE_SYS;
        a = erts_atomic32_cmpxchg_mb(&c_p->state, n, e);
        if (a == e)
            break; /* dirty-active-sys set */
    }
}

#endif

#if defined(ERTS_ENABLE_LOCK_CHECK)

#define ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__
#include "erl_process_lock.h"
#undef ERTS_PROCESS_LOCK_ONLY_LOCK_CHECK_PROTO__

#define ERTS_LC_CHK_RUNQ_LOCK(RQ, L)				\
do {									\
    if ((L))								\
	ERTS_LC_ASSERT(erts_lc_runq_is_locked((RQ)));		\
    else								\
	ERTS_LC_ASSERT(!erts_lc_runq_is_locked((RQ)));		\
} while (0)
#else
#define ERTS_LC_CHK_RUNQ_LOCK(RQ, L)
#endif

void *erts_psd_set_init(Process *p, int ix, void *data);

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix);
ERTS_GLB_INLINE void *
erts_psd_set(Process *p, int ix, void *new);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_psd_get(Process *p, int ix)
{
    ErtsPSD *psd;
#if defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].get_locks)
	ERTS_LC_ASSERT(locks || erts_thr_progress_is_blocking());
    else {
	locks &= erts_psd_required_locks[ix].get_locks;
	ERTS_LC_ASSERT(erts_psd_required_locks[ix].get_locks == locks
			   || erts_thr_progress_is_blocking());
    }
#endif

    psd = (ErtsPSD *) erts_atomic_read_nob(&p->psd);
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    if (!psd)
	return NULL;
    ERTS_THR_DATA_DEPENDENCY_READ_MEMORY_BARRIER;
    return psd->data[ix];
}

ERTS_GLB_INLINE void *
erts_psd_set(Process *p, int ix, void *data)
{
    ErtsPSD *psd;
#if defined(ERTS_ENABLE_LOCK_CHECK)
    ErtsProcLocks locks = erts_proc_lc_my_proc_locks(p);
    erts_aint32_t state = state = erts_atomic32_read_nob(&p->state);
    if (!(state & ERTS_PSFLG_FREE)) {
	if (ERTS_LC_PSD_ANY_LOCK == erts_psd_required_locks[ix].set_locks)
	    ERTS_LC_ASSERT(locks || erts_thr_progress_is_blocking());
	else {
	    locks &= erts_psd_required_locks[ix].set_locks;
	    ERTS_LC_ASSERT(erts_psd_required_locks[ix].set_locks == locks
			       || erts_thr_progress_is_blocking());
	}
    }
#endif
    psd = (ErtsPSD *) erts_atomic_read_nob(&p->psd);
    ASSERT(0 <= ix && ix < ERTS_PSD_SIZE);
    if (psd) {
	void *old;
#ifdef ETHR_ORDERED_READ_DEPEND
	ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
#else
	ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreStore);
#endif
	old = psd->data[ix];
	psd->data[ix] = data;
	return old;
    }

    if (!data)
	return NULL;

    return erts_psd_set_init(p, ix, data);
}

#endif

#define ERTS_PROC_SCHED_ID(P, ID) \
  ((UWord) erts_psd_set((P), ERTS_PSD_SCHED_ID, (void *) (ID)))

#define ERTS_PROC_GET_SAVED_CALLS_BUF(P) \
  ((struct saved_calls *) erts_psd_get((P), ERTS_PSD_SAVED_CALLS_BUF))
#define ERTS_PROC_SET_SAVED_CALLS_BUF(P, SCB) \
  ((struct saved_calls *) erts_psd_set((P), ERTS_PSD_SAVED_CALLS_BUF, (void *) (SCB)))

#define ERTS_PROC_GET_CALL_TIME(P) \
  ((process_breakpoint_time_t *) erts_psd_get((P), ERTS_PSD_CALL_TIME_BP))
#define ERTS_PROC_SET_CALL_TIME(P, PBT) \
  ((process_breakpoint_time_t *) erts_psd_set((P), ERTS_PSD_CALL_TIME_BP, (void *) (PBT)))

#define ERTS_PROC_GET_DELAYED_GC_TASK_QS(P) \
    ((ErtsProcSysTaskQs *) erts_psd_get((P), ERTS_PSD_DELAYED_GC_TASK_QS))
#define ERTS_PROC_SET_DELAYED_GC_TASK_QS(P, PBT) \
    ((ErtsProcSysTaskQs *) erts_psd_set((P), ERTS_PSD_DELAYED_GC_TASK_QS, (void *) (PBT)))

#define ERTS_PROC_GET_NIF_TRAP_EXPORT(P) \
    erts_psd_get((P), ERTS_PSD_NIF_TRAP_EXPORT)
#define ERTS_PROC_SET_NIF_TRAP_EXPORT(P, NTE) \
    erts_psd_set((P), ERTS_PSD_NIF_TRAP_EXPORT, (void *) (NTE))

#define ERTS_PROC_GET_DIST_ENTRY(P) \
    ((DistEntry *) erts_psd_get((P), ERTS_PSD_DIST_ENTRY))
#define ERTS_PROC_SET_DIST_ENTRY(P, DE) \
    ((DistEntry *) erts_psd_set((P), ERTS_PSD_DIST_ENTRY, (void *) (DE)))

#ifdef HIPE
#define ERTS_PROC_GET_SUSPENDED_SAVED_CALLS_BUF(P) \
  ((struct saved_calls *) erts_psd_get((P), ERTS_PSD_SUSPENDED_SAVED_CALLS_BUF))
#define ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(P, SCB) \
  ((struct saved_calls *) erts_psd_set((P), ERTS_PSD_SUSPENDED_SAVED_CALLS_BUF, (void *) (SCB)))
#endif

ERTS_GLB_INLINE Eterm erts_proc_get_error_handler(Process *p);
ERTS_GLB_INLINE Eterm erts_proc_set_error_handler(Process *p, Eterm handler);

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
erts_proc_set_error_handler(Process *p, Eterm handler)
{
    void *old_val;
    void *new_val;
    ASSERT(is_atom(handler));
    new_val = (handler == am_error_handler) ? NULL : (void *) (UWord) handler;
    old_val = erts_psd_set(p, ERTS_PSD_ERROR_HANDLER, new_val);
    if (!old_val)
	return am_error_handler;
    else {
	ASSERT(is_atom(((Eterm) (UWord) old_val)));
	return (Eterm) (UWord) old_val;
    }
}

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS


#include "erl_thr_progress.h"

extern erts_atomic_t erts_migration_paths;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
int erts_get_sched_util(ErtsRunQueue *rq,
			int initially_locked,
			int short_interval);
#endif


ERTS_GLB_INLINE ErtsMigrationPaths *erts_get_migration_paths_managed(void);
ERTS_GLB_INLINE ErtsMigrationPaths *erts_get_migration_paths(void);
ERTS_GLB_INLINE ErtsRunQueue *erts_check_emigration_need(ErtsRunQueue *c_rq,
							 int prio);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsMigrationPaths *
erts_get_migration_paths_managed(void)
{
    return (ErtsMigrationPaths *) erts_atomic_read_ddrb(&erts_migration_paths);
}

ERTS_GLB_INLINE ErtsMigrationPaths *
erts_get_migration_paths(void)
{
    if (erts_thr_progress_is_managed_thread())
	return erts_get_migration_paths_managed();
    else
	return NULL;
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_check_emigration_need(ErtsRunQueue *c_rq, int prio)
{
    ErtsMigrationPaths *mps = erts_get_migration_paths();
    ErtsMigrationPath *mp;
    Uint32 flags;

    if (!mps)
	return NULL;

    mp = &mps->mpath[c_rq->ix];
    flags = mp->flags;

    if (ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, prio)) {
	int len;

	if (ERTS_CHK_RUNQ_FLG_EVACUATE(flags, prio)) {
	    /* force emigration */
	    return mp->prio[prio].runq;
	}

	if (flags & ERTS_RUNQ_FLG_INACTIVE) {
	    /*
	     * Run queue was inactive at last balance. Verify that
	     * it still is before forcing emigration.
	     */
	    if (ERTS_RUNQ_FLGS_GET(c_rq) & ERTS_RUNQ_FLG_INACTIVE)
		return mp->prio[prio].runq;
	}

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	if (mp->sched_util) {
	    ErtsRunQueue *rq = mp->prio[prio].runq;
	    /* No migration if other is non-empty */
	    if (!(ERTS_RUNQ_FLGS_GET(rq) & ERTS_RUNQ_FLG_NONEMPTY)
		&& erts_get_sched_util(rq, 0, 1) < mp->prio[prio].limit.other
		&& erts_get_sched_util(c_rq, 0, 1) > mp->prio[prio].limit.this) {
		return rq;
	    }
	}
	else
#endif
	{

	    if (prio == ERTS_PORT_PRIO_LEVEL)
		len = RUNQ_READ_LEN(&c_rq->ports.info.len);
	    else
		len = RUNQ_READ_LEN(&c_rq->procs.prio_info[prio].len);

	    if (len > mp->prio[prio].limit.this) {
		ErtsRunQueue *n_rq = mp->prio[prio].runq;
		if (n_rq) {
		    if (prio == ERTS_PORT_PRIO_LEVEL)
			len = RUNQ_READ_LEN(&n_rq->ports.info.len);
		    else
			len = RUNQ_READ_LEN(&n_rq->procs.prio_info[prio].len);

		    if (len < mp->prio[prio].limit.other)
			return n_rq;
		}
	    }
	}
    }
    return NULL;
}

#endif


#endif

ERTS_GLB_INLINE ErtsSchedulerData *erts_proc_sched_data(Process *c_p);
ERTS_GLB_INLINE int erts_is_scheduler_bound(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE Process *erts_get_current_process(void);
ERTS_GLB_INLINE Eterm erts_get_current_pid(void);
ERTS_GLB_INLINE Uint erts_get_scheduler_id(void);
ERTS_GLB_INLINE void erts_init_runq_proc(Process *p, ErtsRunQueue *rq, int bnd);
ERTS_GLB_INLINE ErtsRunQueue *erts_set_runq_proc(Process *p, ErtsRunQueue *rq, int *boundp);
ERTS_GLB_INLINE int erts_try_change_runq_proc(Process *p, ErtsRunQueue *rq);
ERTS_GLB_INLINE ErtsRunQueue *erts_bind_runq_proc(Process *p, int bind);
ERTS_GLB_INLINE int erts_proc_runq_is_bound(Process *p);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_proc(Process *p, int *boundp);
ERTS_GLB_INLINE ErtsRunQueue *erts_get_runq_current(ErtsSchedulerData *esdp);
ERTS_GLB_INLINE void erts_runq_lock(ErtsRunQueue *rq);
ERTS_GLB_INLINE int erts_runq_trylock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_runq_unlock(ErtsRunQueue *rq);
ERTS_GLB_INLINE void erts_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq);
ERTS_GLB_INLINE void erts_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);
ERTS_GLB_INLINE void erts_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2);

ERTS_GLB_INLINE ErtsMessage *erts_alloc_message_heap_state(Process *pp,
							   erts_aint32_t *psp,
							   ErtsProcLocks *plp,
							   Uint sz,
							   Eterm **hpp,
							   ErlOffHeap **ohpp);
ERTS_GLB_INLINE ErtsMessage *erts_alloc_message_heap(Process *pp,
						     ErtsProcLocks *plp,
						     Uint sz,
						     Eterm **hpp,
						     ErlOffHeap **ohpp);

ERTS_GLB_INLINE void erts_shrink_message_heap(ErtsMessage **msgpp, Process *pp,
					      Eterm *start_hp, Eterm *used_hp, Eterm *end_hp,
					      Eterm *brefs, Uint brefs_size);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsSchedulerData *erts_proc_sched_data(Process *c_p)
{
    ErtsSchedulerData *esdp;
    ASSERT(c_p);
    esdp = c_p->scheduler_data;
    if (esdp) {
	ASSERT(esdp == erts_get_scheduler_data());
	ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
    }
    else {
	esdp = erts_get_scheduler_data();
	ASSERT(esdp);
	ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
    }
    ASSERT(esdp);
    return esdp;
}

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
    return proc ? proc->common.id : THE_NON_VALUE;
}

ERTS_GLB_INLINE
Uint erts_get_scheduler_id(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (esdp && ERTS_SCHEDULER_IS_DIRTY(esdp))
	return 0;
    else
	return esdp ? esdp->no : (Uint) 0;
}

/**
 * Init run-queue of process.
 *
 * @param p[in,out]     Process
 * @param rq[in]        Run-queue that process will be assigned to
 * @param bnd[in,out]   If non-zero binds process to run-queue.
 */

ERTS_GLB_INLINE void
erts_init_runq_proc(Process *p, ErtsRunQueue *rq, int bnd)
{
    erts_aint_t rqint = (erts_aint_t) rq;
    if (bnd)
        rqint |= ERTS_RUNQ_BOUND_FLAG;
    erts_atomic_init_nob(&p->run_queue, rqint);
}

/**
 * Forcibly set run-queue of process.
 *
 * @param p[in,out]     Process
 * @param rq[in]        Run-queue that process will be assigned to
 * @param bndp[in,out]  Pointer to integer. On input non-zero
 *                      value causes the process to be bound to
 *                      the run-queue. On output, indicating
 *                      wether process previously was bound or
 *                      not.
 * @return              Previous run-queue.
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_set_runq_proc(Process *p, ErtsRunQueue *rq, int *bndp)
{
    erts_aint_t rqint = (erts_aint_t) rq;
    ASSERT(bndp);
    ASSERT(rq);
    if (*bndp)
        rqint |= ERTS_RUNQ_BOUND_FLAG;
    rqint = erts_atomic_xchg_nob(&p->run_queue, rqint);
    *bndp = (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
    return (ErtsRunQueue *) (rqint & ERTS_RUNQ_POINTER_MASK);
}

/**
 * Try to change run-queue assignment of a process.
 *
 * @param p[in,out]     Process
 * @param rq[int]       Run-queue that process will be assigned to
 * @return              Non-zero if the run-queue assignment was
 *                      successfully changed.
 */

ERTS_GLB_INLINE int
erts_try_change_runq_proc(Process *p, ErtsRunQueue *rq)
{
    erts_aint_t old_rqint, new_rqint;

    ASSERT(rq);

    new_rqint = (erts_aint_t) rq;
    old_rqint = (erts_aint_t) erts_atomic_read_nob(&p->run_queue);
    while (1) {
        erts_aint_t act_rqint;

        if (old_rqint & ERTS_RUNQ_BOUND_FLAG)
            return 0;

        act_rqint = erts_atomic_cmpxchg_nob(&p->run_queue,
                                            new_rqint,
                                            old_rqint);
        if (act_rqint == old_rqint)
            return !0;
    }
}

/**
 *
 * Bind or unbind process to/from currently used run-queue.
 *
 * @param p             Process
 * @param bind          Bind if non-zero; otherwise unbind
 * @return              Pointer to previously bound run-queue,
 *                      or NULL if previously unbound
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_bind_runq_proc(Process *p, int bind)
{
    erts_aint_t rqint;
    if (bind)
        rqint = erts_atomic_read_bor_nob(&p->run_queue,
                                         ERTS_RUNQ_BOUND_FLAG);
    else
        rqint = erts_atomic_read_band_nob(&p->run_queue,
                                          ~ERTS_RUNQ_BOUND_FLAG);
    if (rqint & ERTS_RUNQ_BOUND_FLAG)
        return (ErtsRunQueue *) (rqint & ERTS_RUNQ_POINTER_MASK);
    else
        return NULL;
}

/**
 * Determine wether a process is bound to a run-queue or not.
 *
 * @return              Returns a non-zero value if bound,
 *                      and zero of not bound.
 */

ERTS_GLB_INLINE int
erts_proc_runq_is_bound(Process *p)
{
    erts_aint_t rqint = erts_atomic_read_nob(&p->run_queue);
    return (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
}

/**
 * Set run-queue of process.
 *
 * @param p[in,out]     Process
 * @param bndp[out]     Pointer to integer. If non-NULL pointer,
 *                      the integer will be set to a non-zero
 *                      value if the process is bound to the
 *                      run-queue.
 * @return              Pointer to the normal run-queue that
 *                      the process currently is assigend to.
 *                      A process is always assigned to a
 *                      normal run-queue.
 */

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_proc(Process *p, int *bndp)
{
    erts_aint_t rqint = erts_atomic_read_nob(&p->run_queue);
    ErtsRunQueue *rq;
    if (bndp)
        *bndp = (int) (rqint & ERTS_RUNQ_BOUND_FLAG);
    rqint &= ERTS_RUNQ_POINTER_MASK;
    rq = (ErtsRunQueue *) rqint;
    ASSERT(rq);
    return rq;
}

ERTS_GLB_INLINE ErtsRunQueue *
erts_get_runq_current(ErtsSchedulerData *esdp)
{
    ASSERT(!esdp || esdp == erts_get_scheduler_data());
    if (!esdp)
	esdp = erts_get_scheduler_data();
    return esdp->run_queue;
}

ERTS_GLB_INLINE void
erts_runq_lock(ErtsRunQueue *rq)
{
    erts_mtx_lock(&rq->mtx);
}

ERTS_GLB_INLINE int
erts_runq_trylock(ErtsRunQueue *rq)
{
    return erts_mtx_trylock(&rq->mtx);
}

ERTS_GLB_INLINE void
erts_runq_unlock(ErtsRunQueue *rq)
{
    erts_mtx_unlock(&rq->mtx);
}

ERTS_GLB_INLINE void
erts_xrunq_lock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&rq->mtx));
    if (xrq != rq) {
	if (erts_mtx_trylock(&xrq->mtx) == EBUSY) {
	    if (rq < xrq)
		erts_mtx_lock(&xrq->mtx);
	    else {
		erts_mtx_unlock(&rq->mtx);
		erts_mtx_lock(&xrq->mtx);
		erts_mtx_lock(&rq->mtx);
	    }
	}
    }
}

ERTS_GLB_INLINE void
erts_xrunq_unlock(ErtsRunQueue *rq, ErtsRunQueue *xrq)
{
    if (xrq != rq)
	erts_mtx_unlock(&xrq->mtx);
}

ERTS_GLB_INLINE void
erts_runqs_lock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
    ASSERT(rq1 && rq2);
    if (rq1 == rq2)
	erts_mtx_lock(&rq1->mtx);
    else if (rq1 < rq2) {
	erts_mtx_lock(&rq1->mtx);
	erts_mtx_lock(&rq2->mtx);
    }
    else {
	erts_mtx_lock(&rq2->mtx);
	erts_mtx_lock(&rq1->mtx);
    }
}

ERTS_GLB_INLINE void
erts_runqs_unlock(ErtsRunQueue *rq1, ErtsRunQueue *rq2)
{
    ASSERT(rq1 && rq2);
    erts_mtx_unlock(&rq1->mtx);
    if (rq1 != rq2)
	erts_mtx_unlock(&rq2->mtx);
}

ERTS_GLB_INLINE ErtsMessage *
erts_alloc_message_heap_state(Process *pp,
			      erts_aint32_t *psp,
			      ErtsProcLocks *plp,
			      Uint sz,
			      Eterm **hpp,
			      ErlOffHeap **ohpp)
{
    int on_heap;
    ErtsMessage *mp;

    if ((*psp) & ERTS_PSFLG_OFF_HEAP_MSGQ) {
	mp = erts_alloc_message(sz, hpp);
	*ohpp = sz == 0 ? NULL : &mp->hfrag.off_heap;
	return mp;
    }

    mp = erts_try_alloc_message_on_heap(pp, psp, plp, sz, hpp, ohpp, &on_heap);
    ASSERT(pp || !on_heap);
    return mp;
}

ERTS_GLB_INLINE ErtsMessage *
erts_alloc_message_heap(Process *pp,
			ErtsProcLocks *plp,
			Uint sz,
			Eterm **hpp,
			ErlOffHeap **ohpp)
{
    erts_aint32_t state = pp ? erts_atomic32_read_nob(&pp->state) : 0;
    return erts_alloc_message_heap_state(pp, &state, plp, sz, hpp, ohpp);
}

ERTS_GLB_INLINE void
erts_shrink_message_heap(ErtsMessage **msgpp, Process *pp,
			 Eterm *start_hp, Eterm *used_hp, Eterm *end_hp,
			 Eterm *brefs, Uint brefs_size)
{
    ASSERT(start_hp <= used_hp && used_hp <= end_hp);
    if ((*msgpp)->data.attached == ERTS_MSG_COMBINED_HFRAG)
	*msgpp = erts_shrink_message(*msgpp, used_hp - start_hp,
				     brefs, brefs_size);
    else if (!(*msgpp)->data.attached) {
	ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN
			   & erts_proc_lc_my_proc_locks(pp));
	HRelease(pp, end_hp, used_hp);
    }
    else {
	ErlHeapFragment *hfrag = (*msgpp)->data.heap_frag;
	if (start_hp != used_hp)
	    hfrag = erts_resize_message_buffer(hfrag, used_hp - start_hp,
					       brefs, brefs_size);
	else {
	    free_message_buffer(hfrag);
	    hfrag = NULL;
	}
	(*msgpp)->data.heap_frag = hfrag;
    }
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

ERTS_GLB_INLINE ErtsAtomCacheMap *erts_get_atom_cache_map(Process *c_p);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE ErtsAtomCacheMap *
erts_get_atom_cache_map(Process *c_p)
{
    ErtsSchedulerData *esdp = (c_p
			       ? erts_proc_sched_data(c_p)
			       : erts_get_scheduler_data());
    ASSERT(esdp);
    return &esdp->atom_cache_map;
}
#endif

#ifdef __WIN32__
/*
 * Don't want erts_time2reds() inlined in beam_emu.c on windows since
 * it is compiled with gcc which fails on it. Implementation is in
 * erl_process.c on windows.
 */
#  define ERTS_TIME2REDS_IMPL__ erts_time2reds__
#else
#  define ERTS_TIME2REDS_IMPL__ erts_time2reds
#endif

ERTS_GLB_INLINE Sint64 ERTS_TIME2REDS_IMPL__(ErtsMonotonicTime start,
					     ErtsMonotonicTime end);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Sint64
ERTS_TIME2REDS_IMPL__(ErtsMonotonicTime start, ErtsMonotonicTime end)
{
    ErtsMonotonicTime time = end - start;
    ASSERT(time >= 0);
    time = ERTS_MONOTONIC_TO_USEC(time);
    if (time == 0)
	return (Sint64) 1; /* At least one reduction */
    /* Currently two reductions per micro second */
    time *= (CONTEXT_REDS-1)/1000 + 1;
    return (Sint64) time;
}
#endif

Process *erts_try_lock_sig_free_proc(Eterm pid,
                                     ErtsProcLocks locks);

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
#define ERTS_ASSERT_IS_NOT_EXITING(P) \
    do { ASSERT(!ERTS_PROC_IS_EXITING((P))); } while (0)
#else
#define ERTS_ASSERT_IS_NOT_EXITING(P)
#endif


#define ERTS_PROC_IS_EXITING(P) \
    (ERTS_PSFLG_EXITING & erts_atomic32_read_acqb(&(P)->state))


/* Minimum NUMBER of processes for a small system to start */
#define ERTS_MIN_PROCESSES		1024
#if ERTS_MIN_PROCESSES < ERTS_NO_OF_PIX_LOCKS
#undef ERTS_MIN_PROCESSES
#define ERTS_MIN_PROCESSES		ERTS_NO_OF_PIX_LOCKS
#endif

void erts_notify_inc_runq(ErtsRunQueue *runq);

void erts_sched_finish_poke(ErtsSchedulerSleepInfo *, erts_aint32_t);
ERTS_GLB_INLINE void erts_sched_poke(ErtsSchedulerSleepInfo *ssi);
void erts_aux_thread_poke(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_sched_poke(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t flags;
    ERTS_THR_MEMORY_BARRIER;
    flags = erts_atomic32_read_nob(&ssi->flags);
    if (flags & ERTS_SSI_FLG_SLEEPING) {
	flags = erts_atomic32_read_band_nob(&ssi->flags, ~ERTS_SSI_FLGS_SLEEP);
	erts_sched_finish_poke(ssi, flags);
    }
}


#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#include "erl_process_lock.h"

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS

#endif


void erts_halt(int code);
extern erts_atomic32_t erts_halt_progress;
extern int erts_halt_code;
