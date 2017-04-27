/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#define ERL_PROCESS_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_instrument.h"
#include "erl_threads.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_cpu_topology.h"
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_async.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"
#include "erl_ptab.h"
#include "erl_bif_unique.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"
#include "erl_nfunc_sched.h"

#define ERTS_CHECK_TIME_REDS CONTEXT_REDS
#define ERTS_DELAYED_WAKEUP_INFINITY (~(Uint64) 0)
#define ERTS_DELAYED_WAKEUP_REDUCTIONS ((Uint64) CONTEXT_REDS/2)

#define ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED (2000*CONTEXT_REDS)
#define ERTS_RUNQ_CALL_CHECK_BALANCE_REDS \
  (ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED/2)

#define ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST (CONTEXT_REDS/10)

#define ERTS_SCHED_SPIN_UNTIL_YIELD 100

#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_LONG 40
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_LONG 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_LONG 20
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_LONG 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM 10
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_MEDIUM 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_SHORT 10
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_SHORT 0
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_SHORT 5
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_SHORT 0
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_NONE 0
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_NONE 0

#define ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT 1000
#define ERTS_SCHED_SUSPEND_SLEEP_SPINCOUNT 0

#if 0 || defined(DEBUG)
#define ERTS_FAKE_SCHED_BIND_PRINT_SORTED_CPU_DATA
#endif

#if defined(DEBUG) && 0
#define HARDDEBUG
#else
#undef HARDDEBUG
#endif

#ifdef HARDDEBUG
#define HARDDEBUG_RUNQS
#endif

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_init_process() */
#include "hipe_signal.h"	/* for hipe_thread_signal_init() */
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#define MAX_BIT       (1 << PRIORITY_MAX)
#define HIGH_BIT      (1 << PRIORITY_HIGH)
#define NORMAL_BIT    (1 << PRIORITY_NORMAL)
#define LOW_BIT       (1 << PRIORITY_LOW)
#define PORT_BIT      (1 << ERTS_PORT_PRIO_LEVEL)

#define ERTS_IS_RUNQ_EMPTY_FLGS(FLGS) \
    (!((FLGS) & (ERTS_RUNQ_FLGS_QMASK|ERTS_RUNQ_FLG_MISC_OP)))

#define ERTS_IS_RUNQ_EMPTY_PORTS_FLGS(FLGS) \
    (!((FLGS) & (PORT_BIT|ERTS_RUNQ_FLG_MISC_OP)))

#define ERTS_EMPTY_RUNQ(RQ) \
    ERTS_IS_RUNQ_EMPTY_FLGS(ERTS_RUNQ_FLGS_GET_NOB((RQ)))

#define ERTS_EMPTY_RUNQ_PORTS(RQ) \
    ERTS_IS_RUNQ_EMPTY_FLGS(ERTS_RUNQ_FLGS_GET_NOB((RQ)))

static ERTS_INLINE int
runq_got_work_to_execute_flags(Uint32 flags)
{
    if (flags & ERTS_RUNQ_FLG_HALTING)
        return !ERTS_IS_RUNQ_EMPTY_PORTS_FLGS(flags);
    return !ERTS_IS_RUNQ_EMPTY_FLGS(flags);
}

#ifdef ERTS_SMP
static ERTS_INLINE int
runq_got_work_to_execute(ErtsRunQueue *rq)
{
    return runq_got_work_to_execute_flags(ERTS_RUNQ_FLGS_GET_NOB(rq));
}
#endif

#undef RUNQ_READ_RQ
#undef RUNQ_SET_RQ
#define RUNQ_READ_RQ(X) ((ErtsRunQueue *) erts_smp_atomic_read_nob((X)))
#define RUNQ_SET_RQ(X, RQ) erts_smp_atomic_set_nob((X), (erts_aint_t) (RQ))

#ifdef DEBUG
#  if defined(ARCH_64)
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
    (RUNQ_SET_RQ((RQP), (0xdeadbeefdead0003LL | ((N) << 4)))
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP) \
do { \
    ASSERT((RQP) != NULL); \
    ASSERT(((((Uint) (RQP)) & ((Uint) 0x3))) == ((Uint) 0)); \
    ASSERT((((Uint) (RQP)) & ~((Uint) 0xffff)) != ((Uint) 0xdeadbeefdead0000LL));\
} while (0)
#  else
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
    (RUNQ_SET_RQ((RQP), (0xdead0003 | ((N) << 4))))
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

const Process erts_invalid_process = {{ERTS_INVALID_PID}};

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];

int ERTS_WRITE_UNLIKELY(erts_default_spo_flags) = SPO_ON_HEAP_MSGQ;
int ERTS_WRITE_UNLIKELY(erts_eager_check_io) = 1;
int ERTS_WRITE_UNLIKELY(erts_sched_compact_load);
int ERTS_WRITE_UNLIKELY(erts_sched_balance_util) = 0;
Uint ERTS_WRITE_UNLIKELY(erts_no_schedulers);
Uint ERTS_WRITE_UNLIKELY(erts_no_total_schedulers);
Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_cpu_schedulers) = 0;
Uint ERTS_WRITE_UNLIKELY(erts_no_dirty_io_schedulers) = 0;

static char *erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_NO_FLAGS] = {0};
int erts_aux_work_no_flags = ERTS_SSI_AUX_WORK_NO_FLAGS;

#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_LAZY		(4*1024*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_LAZY			(512*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_MEDIUM			(64*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_EAGER			(16*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_EAGER		(1024)

static UWord thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_MEDIUM;

ErtsPTab erts_proc erts_align_attribute(ERTS_CACHE_LINE_SIZE);

int erts_sched_thread_suggested_stack_size = -1;
#ifdef ERTS_DIRTY_SCHEDULERS
int erts_dcpu_sched_thread_suggested_stack_size = -1;
int erts_dio_sched_thread_suggested_stack_size = -1;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];
#endif

static struct {
    int aux_work;
    int tse;
    int sys_schedule;
} sched_busy_wait;

#ifdef ERTS_SMP
int erts_disable_proc_not_running_opt;

static ErtsAuxWorkData *aux_thread_aux_work_data;

#define ERTS_SCHDLR_SSPND_CHNG_NMSB		(((erts_aint32_t) 1) << 0)
#define ERTS_SCHDLR_SSPND_CHNG_MSB		(((erts_aint32_t) 1) << 1)
#define ERTS_SCHDLR_SSPND_CHNG_ONLN		(((erts_aint32_t) 1) << 2)
#define ERTS_SCHDLR_SSPND_CHNG_DCPU_ONLN	(((erts_aint32_t) 1) << 3)

typedef struct {
    int ongoing;
    ErtsProcList *blckrs;
    ErtsProcList *chngq;
} ErtsMultiSchedulingBlock;

typedef struct {
    Uint32 normal;
#ifdef ERTS_DIRTY_SCHEDULERS
    Uint32 dirty_cpu;
    Uint32 dirty_io;
#endif
} ErtsSchedTypeCounters;

static struct {
    erts_smp_mtx_t mtx;
    ErtsSchedTypeCounters online;
    ErtsSchedTypeCounters curr_online;
    ErtsSchedTypeCounters active;
    erts_smp_atomic32_t changing;
    ErtsProcList *chngq;
    Eterm changer;
    ErtsMultiSchedulingBlock nmsb; /* Normal multi Scheduling Block */
    ErtsMultiSchedulingBlock msb; /* Multi Scheduling Block */
#ifdef ERTS_DIRTY_SCHEDULERS
    ErtsSchedType last_msb_dirty_type;
#endif
} schdlr_sspnd;

static void init_scheduler_suspend(void);

static ERTS_INLINE Uint32
schdlr_sspnd_eq_nscheds(ErtsSchedTypeCounters *val1p, ErtsSchedTypeCounters *val2p)
{
    int res = val1p->normal == val2p->normal;
#ifdef ERTS_DIRTY_SCHEDULERS
    res &= val1p->dirty_cpu == val2p->dirty_cpu;
    res &= val1p->dirty_io == val2p->dirty_io;
#endif
    return res;
}

static ERTS_INLINE Uint32
schdlr_sspnd_get_nscheds(ErtsSchedTypeCounters *valp,
                         ErtsSchedType type)
{
    switch (type) {
    case ERTS_SCHED_NORMAL:
        return valp->normal;
#ifdef ERTS_DIRTY_SCHEDULERS
    case ERTS_SCHED_DIRTY_CPU:
        return valp->dirty_cpu;
    case ERTS_SCHED_DIRTY_IO:
        return valp->dirty_io;
#else
    case ERTS_SCHED_DIRTY_CPU:
    case ERTS_SCHED_DIRTY_IO:
        return 0;
#endif
    default:
	ERTS_INTERNAL_ERROR("Invalid scheduler type");
	return 0;
    }
}

#ifdef DEBUG
static ERTS_INLINE Uint32
schdlr_sspnd_get_nscheds_tot(ErtsSchedTypeCounters *valp)
{
    Uint32 res = valp->normal;
#ifdef ERTS_DIRTY_SCHEDULERS
    res += valp->dirty_cpu;
    res += valp->dirty_io;
#endif
    return res;
}
#endif

static ERTS_INLINE void
schdlr_sspnd_dec_nscheds(ErtsSchedTypeCounters *valp,
                         ErtsSchedType type)
{
    ASSERT(schdlr_sspnd_get_nscheds(valp, type) > 0);

    switch (type) {
    case ERTS_SCHED_NORMAL:
        valp->normal--;
	break;
#ifdef ERTS_DIRTY_SCHEDULERS
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu--;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io--;
	break;
#endif
    default:
	ERTS_INTERNAL_ERROR("Invalid scheduler type");
    }
}

static ERTS_INLINE void
schdlr_sspnd_inc_nscheds(ErtsSchedTypeCounters *valp,
                         ErtsSchedType type)
{
    switch (type) {
    case ERTS_SCHED_NORMAL:
        valp->normal++;
	break;
#ifdef ERTS_DIRTY_SCHEDULERS
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu++;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io++;
	break;
#endif
    default:
	ERTS_INTERNAL_ERROR("Invalid scheduler type");
    }
}

static ERTS_INLINE void
schdlr_sspnd_set_nscheds(ErtsSchedTypeCounters *valp,
                         ErtsSchedType type, Uint32 no)
{
    switch (type) {
    case ERTS_SCHED_NORMAL:
        valp->normal = no;
	break;
#ifdef ERTS_DIRTY_SCHEDULERS
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu = no;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io = no;
	break;
#endif
    default:
	ERTS_INTERNAL_ERROR("Invalid scheduler type");
    }
}

static struct {
    erts_smp_mtx_t update_mtx;
    erts_smp_atomic32_t no_runqs;
    int last_active_runqs;
    int forced_check_balance;
    erts_smp_atomic32_t checking_balance;
    int halftime;
    int full_reds_history_index;
    struct {
	int active_runqs;
	int reds;
	erts_aint32_t max_len;
    } prev_rise;
    Uint n;
} balance_info;

#define ERTS_BLNCE_SAVE_RISE(ACTIVE, MAX_LEN, REDS)	\
do {							\
    balance_info.prev_rise.active_runqs = (ACTIVE);	\
    balance_info.prev_rise.max_len = (MAX_LEN);		\
    balance_info.prev_rise.reds = (REDS);		\
} while (0)

#endif

erts_sched_stat_t erts_sched_stat;

#ifdef USE_THREADS
static erts_tsd_key_t ERTS_WRITE_UNLIKELY(sched_data_key);
#endif

static erts_smp_atomic32_t function_calls;

#ifdef ERTS_SMP
static erts_smp_atomic32_t doing_sys_schedule;
static erts_smp_atomic32_t no_empty_run_queues;
long erts_runq_supervision_interval = 0;
static ethr_event runq_supervision_event;
static erts_tid_t runq_supervisor_tid;
static erts_atomic_t runq_supervisor_sleeping;
#else /* !ERTS_SMP */
ErtsSchedulerData *erts_scheduler_data;
#endif

ErtsAlignedRunQueue *erts_aligned_run_queues;
Uint erts_no_run_queues;

ErtsAlignedSchedulerData *erts_aligned_scheduler_data;
#ifdef ERTS_DIRTY_SCHEDULERS
ErtsAlignedSchedulerData *erts_aligned_dirty_cpu_scheduler_data;
ErtsAlignedSchedulerData *erts_aligned_dirty_io_scheduler_data;
typedef union {
    Process dsp;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(Process))];
} ErtsAlignedDirtyShadowProcess;
#endif

typedef union {
    ErtsSchedulerSleepInfo ssi;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerSleepInfo))];
} ErtsAlignedSchedulerSleepInfo;

static ErtsAlignedSchedulerSleepInfo *aligned_sched_sleep_info;
#ifdef ERTS_DIRTY_SCHEDULERS
#ifdef ERTS_SMP
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_cpu_sched_sleep_info;
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_io_sched_sleep_info;
#endif
#endif

static Uint last_reductions;
static Uint last_exact_reductions;
Eterm ERTS_WRITE_UNLIKELY(erts_system_monitor);
Eterm ERTS_WRITE_UNLIKELY(erts_system_monitor_long_gc);
Uint ERTS_WRITE_UNLIKELY(erts_system_monitor_long_schedule);
Eterm ERTS_WRITE_UNLIKELY(erts_system_monitor_large_heap);
struct erts_system_monitor_flags_t erts_system_monitor_flags;

/* system performance monitor */
Eterm erts_system_profile;
struct erts_system_profile_flags_t erts_system_profile_flags;
int erts_system_profile_ts_type = ERTS_TRACE_FLG_NOW_TIMESTAMP;

#if ERTS_MAX_PROCESSES > 0x7fffffff
#error "Need to store process_count in another type"
#endif

typedef enum {
    ERTS_PSTT_GC_MAJOR,	/* Garbage Collect: Fullsweep */
    ERTS_PSTT_GC_MINOR,	/* Garbage Collect: Generational */
    ERTS_PSTT_CPC,	/* Check Process Code */
    ERTS_PSTT_CLA,	/* Copy Literal Area */
    ERTS_PSTT_COHMQ,    /* Change off heap message queue */
    ERTS_PSTT_FTMQ,     /* Flush trace msg queue */
    ERTS_PSTT_ETS_FREE_FIXATION
} ErtsProcSysTaskType;

#define ERTS_MAX_PROC_SYS_TASK_ARGS 2

struct ErtsProcSysTask_ {
    ErtsProcSysTask *next;
    ErtsProcSysTask *prev;
    ErtsProcSysTaskType type;
    Eterm requester;
    Eterm reply_tag;
    Eterm req_id;
    Uint req_id_sz;
    Eterm arg[ERTS_MAX_PROC_SYS_TASK_ARGS];
    ErlOffHeap off_heap;
    Eterm heap[1];
};

#define ERTS_PROC_SYS_TASK_SIZE(HSz) \
    (sizeof(ErtsProcSysTask) - sizeof(Eterm) + sizeof(Eterm)*(HSz))

struct ErtsProcSysTaskQs_ {
    int qmask;
    int ncount;
    ErtsProcSysTask *q[ERTS_NO_PROC_PRIO_LEVELS];
};

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(proc_sys_task_queues,
				 ErtsProcSysTaskQs,
				 50,
				 ERTS_ALC_T_PROC_SYS_TSK_QS)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(misc_op_list,
				 ErtsMiscOpList,
				 10,
				 ERTS_ALC_T_MISC_OP_LIST)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(proclist,
				 ErtsProcList,
				 200,
				 ERTS_ALC_T_PROC_LIST)

#define ERTS_SCHED_SLEEP_INFO_IX(IX)					\
    (ASSERT(-1 <= ((int) (IX))					        \
		 && ((int) (IX)) < ((int) erts_no_schedulers)),		\
     &aligned_sched_sleep_info[(IX)].ssi)
#ifdef ERTS_DIRTY_SCHEDULERS
#define ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_cpu_schedulers)),	\
     &aligned_dirty_cpu_sched_sleep_info[(IX)].ssi)
#define ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_io_schedulers)),	\
     &aligned_dirty_io_sched_sleep_info[(IX)].ssi)
#endif

#define ERTS_FOREACH_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++) {			\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_smp_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_FOREACH_OP_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    int online__ = (int) schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,	\
						  ERTS_SCHED_NORMAL);	\
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&schdlr_sspnd.mtx));	\
    for (ix__ = 0; ix__ < online__; ix__++) {				\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_smp_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, DO, DOX)			\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++) {			\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
    }									\
    { DOX; }								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++)			\
	erts_smp_runq_unlock(ERTS_RUNQ_IX(ix__));			\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ(RQVAR, DO) \
  ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, DO, )
/*
 * Local functions.
 */

static void exec_misc_ops(ErtsRunQueue *);
static void print_function_from_pc(fmtfn_t to, void *to_arg, BeamInstr* x);
static int stack_element_dump(fmtfn_t to, void *to_arg, Eterm* sp, int yreg);

static void aux_work_timeout(void *unused);
static void aux_work_timeout_early_init(int no_schedulers);
static void setup_aux_work_timer(ErtsSchedulerData *esdp);

static int execute_sys_tasks(Process *c_p,
			     erts_aint32_t *statep,
			     int in_reds);
static int cleanup_sys_tasks(Process *c_p,
			     erts_aint32_t in_state,
			     int in_reds);


#if defined(DEBUG) || 0
#define ERTS_DBG_CHK_AUX_WORK_VAL(V) dbg_chk_aux_work_val((V))
static void
dbg_chk_aux_work_val(erts_aint32_t value)
{
    erts_aint32_t valid = 0;

    valid |= ERTS_SSI_AUX_WORK_SET_TMO;
    valid |= ERTS_SSI_AUX_WORK_MISC;
    valid |= ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM;
    valid |= ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC;
#if ERTS_USE_ASYNC_READY_Q
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY;
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
#endif
#ifdef ERTS_SMP
    valid |= ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP;
    valid |= ERTS_SSI_AUX_WORK_MISC_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_DD;
    valid |= ERTS_SSI_AUX_WORK_DD_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_CNCLD_TMRS;
    valid |= ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP;
    valid |= ERTS_SSI_AUX_WORK_PENDING_EXITERS;
#endif
#if HAVE_ERTS_MSEG
    valid |= ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK;
#endif
#ifdef ERTS_SSI_AUX_WORK_REAP_PORTS
    valid |= ERTS_SSI_AUX_WORK_REAP_PORTS;
#endif
    valid |= ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED;
    valid |= ERTS_SSI_AUX_WORK_YIELD;

    if (~valid & value)
	erts_exit(ERTS_ABORT_EXIT,
		 "Invalid aux_work value found: 0x%x\n",
		 ~valid & value);
}
#define ERTS_DBG_CHK_SSI_AUX_WORK(SSI) \
  ERTS_DBG_CHK_AUX_WORK_VAL(erts_atomic32_read_nob(&(SSI)->aux_work))
#else
#define ERTS_DBG_CHK_AUX_WORK_VAL(V)
#define ERTS_DBG_CHK_SSI_AUX_WORK(SSI)
#endif

#ifdef ERTS_SMP
static void do_handle_pending_exiters(ErtsProcList *);
static void wake_scheduler(ErtsRunQueue *rq);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_smp_lc_runq_is_locked(ErtsRunQueue *runq)
{
    return erts_smp_lc_mtx_is_locked(&runq->mtx);
}
#endif


static ERTS_INLINE Uint64
ensure_later_proc_interval(Uint64 interval)
{
    return erts_smp_ensure_later_interval_nob(erts_ptab_interval(&erts_proc), interval);
}

Uint64
erts_get_proc_interval(void)
{
    return erts_smp_current_interval_nob(erts_ptab_interval(&erts_proc));
}

Uint64
erts_ensure_later_proc_interval(Uint64 interval)
{
    return ensure_later_proc_interval(interval);
}

Uint64
erts_step_proc_interval(void)
{
    return erts_smp_step_interval_nob(erts_ptab_interval(&erts_proc));
}

void
erts_pre_init_process(void)
{
#ifdef USE_THREADS
    erts_tsd_key_create(&sched_data_key, "erts_sched_data_key");
#endif

    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP_IX]
	= "DELAYED_AW_WAKEUP";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_DD_IX]
	= "DD";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_DD_THR_PRGR_IX]
	= "DD_THR_PRGR";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC_IX]
	= "FIX_ALLOC_DEALLOC";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM_IX]
	= "FIX_ALLOC_LOWER_LIM";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP_IX]
	= "THR_PRGR_LATER_OP";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_CNCLD_TMRS_IX]
	= "CNCLD_TMRS";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR_IX]
	= "CNCLD_TMRS_THR_PRGR";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_ASYNC_READY_IX]
	= "ASYNC_READY";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN_IX]
	= "ASYNC_READY_CLEAN";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_MISC_THR_PRGR_IX]
	= "MISC_THR_PRGR";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_MISC_IX]
	= "MISC";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_PENDING_EXITERS_IX]
	= "PENDING_EXITERS";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_SET_TMO_IX]
	= "SET_TMO";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK_IX]
	= "MSEG_CACHE_CHECK";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_YIELD_IX]
	= "YIELD";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_REAP_PORTS_IX]
	= "REAP_PORTS";
    erts_aux_work_flag_descr[ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED_IX]
	= "DEBUG_WAIT_COMPLETED";

#ifdef ERTS_ENABLE_LOCK_CHECK

    erts_psd_required_locks[ERTS_PSD_ERROR_HANDLER].get_locks
	= ERTS_PSD_ERROR_HANDLER_BUF_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_ERROR_HANDLER].set_locks
	= ERTS_PSD_ERROR_HANDLER_BUF_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_SAVED_CALLS_BUF].get_locks
	= ERTS_PSD_SAVED_CALLS_BUF_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_SAVED_CALLS_BUF].set_locks
	= ERTS_PSD_SAVED_CALLS_BUF_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_SCHED_ID].get_locks
	= ERTS_PSD_SCHED_ID_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_SCHED_ID].set_locks
	= ERTS_PSD_SCHED_ID_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_CALL_TIME_BP].get_locks
	= ERTS_PSD_CALL_TIME_BP_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_CALL_TIME_BP].set_locks
	= ERTS_PSD_CALL_TIME_BP_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_DELAYED_GC_TASK_QS].get_locks
	= ERTS_PSD_DELAYED_GC_TASK_QS_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_DELAYED_GC_TASK_QS].set_locks
	= ERTS_PSD_DELAYED_GC_TASK_QS_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_NIF_TRAP_EXPORT].get_locks
	= ERTS_PSD_NIF_TRAP_EXPORT_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_NIF_TRAP_EXPORT].set_locks
	= ERTS_PSD_NIF_TRAP_EXPORT_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_ETS_OWNED_TABLES].get_locks
        = ERTS_PSD_ETS_OWNED_TABLES_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_ETS_OWNED_TABLES].set_locks
        = ERTS_PSD_ETS_OWNED_TABLES_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_ETS_FIXED_TABLES].get_locks
        = ERTS_PSD_ETS_FIXED_TABLES_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_ETS_FIXED_TABLES].set_locks
        = ERTS_PSD_ETS_FIXED_TABLES_SET_LOCKS;
#endif
}

static void
release_process(void *vproc)
{
    erts_proc_dec_refc((Process *) vproc);
}

/* initialize the scheduler */
void
erts_init_process(int ncpu, int proc_tab_size, int legacy_proc_tab)
{

#ifdef ERTS_SMP
    erts_disable_proc_not_running_opt = 0;
    erts_init_proc_lock(ncpu);
#endif

    init_proclist_alloc();

    erts_ptab_init_table(&erts_proc,
			 ERTS_ALC_T_PROC_TABLE,
			 release_process,
			 (ErtsPTabElementCommon *) &erts_invalid_process.common,
			 proc_tab_size,
			 sizeof(Process),
			 "process_table",
			 legacy_proc_tab,
#ifdef ERTS_SMP
			 1
#else
			 0
#endif
	);

    last_reductions = 0;
    last_exact_reductions = 0;
}

void
erts_late_init_process(void)
{
    int ix;

    erts_smp_spinlock_init(&erts_sched_stat.lock, "sched_stat");
    for (ix = 0; ix < ERTS_NO_PRIO_LEVELS; ix++) {
	Eterm atom;
	char *atom_str;
	switch (ix) {
	case PRIORITY_MAX:
	    atom_str = "process_max";
	    break;
	case PRIORITY_HIGH:
	    atom_str = "process_high";
	    break;
	case PRIORITY_NORMAL:
	    atom_str = "process_normal";
	    break;
	case PRIORITY_LOW:
	    atom_str = "process_low";
	    break;
	case ERTS_PORT_PRIO_LEVEL:
	    atom_str = "port";
	    break;
	default:
	    atom_str = "bad_prio";
	    ASSERT(!"bad prio");
	    break;
	}
	atom = am_atom_put(atom_str, sys_strlen(atom_str));
	erts_sched_stat.prio[ix].name = atom;
	erts_sched_stat.prio[ix].total_executed = 0;
	erts_sched_stat.prio[ix].executed = 0;
	erts_sched_stat.prio[ix].total_migrated = 0;
	erts_sched_stat.prio[ix].migrated = 0;
    }

}

#define ERTS_SCHED_WTIME_IDLE ~((Uint64) 0)

static void
init_sched_wall_time(ErtsSchedulerData *esdp, Uint64 time_stamp)
{
#ifdef ERTS_DIRTY_SCHEDULERS
    if (esdp->type != ERTS_SCHED_NORMAL) {
        erts_atomic32_init_nob(&esdp->sched_wall_time.u.mod, 0);
        esdp->sched_wall_time.enabled = 1;
        esdp->sched_wall_time.start = time_stamp;
        esdp->sched_wall_time.working.total = 0;
        esdp->sched_wall_time.working.start = ERTS_SCHED_WTIME_IDLE;
    }
    else
#endif
    {
        esdp->sched_wall_time.u.need = erts_sched_balance_util;
        esdp->sched_wall_time.enabled = 0;
        esdp->sched_wall_time.start = 0;
        esdp->sched_wall_time.working.total = 0;
        esdp->sched_wall_time.working.start = 0;
    }
}

static ERTS_INLINE Uint64
sched_wall_time_ts(void)
{
#ifdef ERTS_HAVE_OS_MONOTONIC_TIME_SUPPORT
    return (Uint64) erts_os_monotonic_time();
#else
    Uint64 res;
    SysTimeval tv;
    sys_gettimeofday(&tv);
    res = (Uint64) tv.tv_sec*1000000;
    res += (Uint64) tv.tv_usec;
    return res;
#endif
}

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT

static ERTS_INLINE Uint64
aschedtime_read(ErtsAtomicSchedTime *var)
{
    return (Uint64) erts_atomic64_read_nob((erts_atomic64_t *) var);
}

static ERTS_INLINE void
aschedtime_set(ErtsAtomicSchedTime *var, Uint64 val)
{
    erts_atomic64_set_nob((erts_atomic64_t *) var, (erts_aint64_t) val);
}

static ERTS_INLINE void
aschedtime_init(ErtsAtomicSchedTime *var)
{
    erts_atomic64_init_nob((erts_atomic64_t *) var, (erts_aint64_t) 0);
}

#define ERTS_GET_AVG_MAX_UNLOCKED_TRY 50
#define ERTS_SCHED_AVG_UTIL_WRITE_MARKER (~((Uint64) 0))

/* Intervals in nanoseconds */
#define ERTS_SCHED_UTIL_SHORT_INTERVAL ((Uint64) 1*1000*1000*1000)
#define ERTS_SCHED_UTIL_LONG_INTERVAL ((Uint64) 10*1000*1000*1000)


#define ERTS_SCHED_UTIL_IGNORE_IMBALANCE_DIFF 5000 /* ppm */

static ERTS_INLINE Uint64
calc_sched_worktime(int is_working, Uint64 now, Uint64 last,
		    Uint64 interval, Uint64 old_worktime)
{
    Uint64 worktime;
    Uint64 new;

    if (now <= last)
	return old_worktime;

    new = now - last;

    if (new >= interval)
	return is_working ? interval : (Uint64) 0;


    /*
     * Division by 1000 in order to avoid
     * overflow. If changed update assertions
     * in init_runq_sched_util().
     */
    worktime = old_worktime;
    worktime *= (interval - new)/1000;
    worktime /= (interval/1000);
    if (is_working)
	worktime += new;

    ASSERT(0 <= worktime && worktime <= interval);

    return worktime;
}

static ERTS_INLINE void
update_avg_sched_util(ErtsSchedulerData *esdp, Uint64 now, int is_working)
{
    ErtsRunQueue *rq;
    int worked;
    Uint64 swt, lwt, last;

    rq = esdp->run_queue;
    last = aschedtime_read(&rq->sched_util.last);

    if (now <= last) {
	ASSERT(last == ERTS_SCHED_AVG_UTIL_WRITE_MARKER);
	return;
    }

    ASSERT(now >= last);

    worked = rq->sched_util.is_working;

    swt = calc_sched_worktime(worked, now, last, ERTS_SCHED_UTIL_SHORT_INTERVAL,
			      rq->sched_util.worktime.short_interval);
    lwt = calc_sched_worktime(worked, now, last, ERTS_SCHED_UTIL_LONG_INTERVAL,
			      rq->sched_util.worktime.long_interval);

    aschedtime_set(&rq->sched_util.last, ERTS_SCHED_AVG_UTIL_WRITE_MARKER);
    ERTS_THR_WRITE_MEMORY_BARRIER;
    rq->sched_util.is_working = is_working;
    rq->sched_util.worktime.short_interval = swt;
    rq->sched_util.worktime.long_interval = lwt;
    ERTS_THR_WRITE_MEMORY_BARRIER;
    aschedtime_set(&rq->sched_util.last, now);
}

int
erts_get_sched_util(ErtsRunQueue *rq, int initially_locked, int short_interval)
{
    /* Average scheduler utilization in ppm */
    int util, is_working, try = 0, locked = initially_locked;
    Uint64 worktime, old_worktime, now, last, interval, *old_worktimep;

    if (short_interval) {
	old_worktimep = &rq->sched_util.worktime.short_interval;
	interval = ERTS_SCHED_UTIL_SHORT_INTERVAL;
    }
    else {
	old_worktimep = &rq->sched_util.worktime.long_interval;
	interval = ERTS_SCHED_UTIL_LONG_INTERVAL;
    }

    while (1) {
	Uint64 chk_last;
	last = aschedtime_read(&rq->sched_util.last);
	ERTS_THR_READ_MEMORY_BARRIER;
	is_working = rq->sched_util.is_working;
	old_worktime = *old_worktimep;
	ERTS_THR_READ_MEMORY_BARRIER;
	chk_last = aschedtime_read(&rq->sched_util.last);
	if (chk_last == last)
	    break;
	if (!locked) {
	    if (++try >= ERTS_GET_AVG_MAX_UNLOCKED_TRY) {
		/* Writer will eventually block on runq-lock */
		erts_smp_runq_lock(rq);
		locked = 1;
	    }
	}
    }

    if (!initially_locked && locked)
	erts_smp_runq_unlock(rq);

    now = sched_wall_time_ts();
    worktime = calc_sched_worktime(is_working, now, last, interval, old_worktime);

    util = (int) ((worktime * 1000000)/interval);

    ASSERT(0 <= util && util <= 1000000);

    return util;
}

static void
init_runq_sched_util(ErtsRunQueueSchedUtil *rqsu, int enabled)
{
    aschedtime_init(&rqsu->last);
    if (!enabled)
	aschedtime_set(&rqsu->last, ERTS_SCHED_AVG_UTIL_WRITE_MARKER);
    rqsu->is_working = 0;
    rqsu->worktime.short_interval = (Uint64) 0;
    rqsu->worktime.long_interval = (Uint64) 0;

#ifdef DEBUG
    {
	Uint64 intrvl;
	/*
	 * If one of these asserts fail we may have
	 * overflow in calc_sched_worktime(). Which
	 * have to be fixed either by shrinking
	 * interval size, or fix calculation of
	 * worktime in calc_sched_worktime().
	 */
	intrvl = ERTS_SCHED_UTIL_SHORT_INTERVAL;
	ASSERT(intrvl*(intrvl/1000) > intrvl);
	intrvl = ERTS_SCHED_UTIL_LONG_INTERVAL;
	ASSERT(intrvl*(intrvl/1000) > intrvl);
    }
#endif
}

#endif /* ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT */

#ifdef ERTS_DIRTY_SCHEDULERS

typedef struct {
    Uint64 working;
    Uint64 total;
} ErtsDirtySchedWallTime;

static void
read_dirty_sched_wall_time(ErtsSchedulerData *esdp, ErtsDirtySchedWallTime *info)
{
    erts_aint32_t mod1;
    Uint64 working, start, ts;

    mod1 = erts_atomic32_read_nob(&esdp->sched_wall_time.u.mod);

    while (1) {
        erts_aint32_t mod2;

        /* Spin until values are not written... */
        while (1) {
            if ((mod1 & 1) == 0)
                break;
            ERTS_SPIN_BODY;
            mod1 = erts_atomic32_read_nob(&esdp->sched_wall_time.u.mod);
        }

        ERTS_THR_READ_MEMORY_BARRIER;

        working = esdp->sched_wall_time.working.total;
        start = esdp->sched_wall_time.working.start;

        ERTS_THR_READ_MEMORY_BARRIER;

        mod2 = erts_atomic32_read_nob(&esdp->sched_wall_time.u.mod);
        if (mod1 == mod2)
            break;
        mod1 = mod2;
    }

    ts = sched_wall_time_ts();
    ts -= esdp->sched_wall_time.start;

    info->total = ts;

    if (start == ERTS_SCHED_WTIME_IDLE || ts < start)
        info->working = working;
    else
        info->working = working + (ts - start);

    if (info->working > info->total)
        info->working = info->total;
}

#endif

#ifdef ERTS_SMP

static void
dirty_sched_wall_time_change(ErtsSchedulerData *esdp, int working)
{
    erts_aint32_t mod;
    Uint64 ts = sched_wall_time_ts();

    ts -= esdp->sched_wall_time.start;

    /*
     * This thread is the only thread writing in
     * this sched_wall_time struct. We set 'mod' to
     * an odd value while writing...
     */
    mod = erts_atomic32_read_dirty(&esdp->sched_wall_time.u.mod);
    ASSERT((mod & 1) == 0);
    mod++;

    erts_atomic32_set_nob(&esdp->sched_wall_time.u.mod, mod);
    ERTS_THR_WRITE_MEMORY_BARRIER;

    if (working) {
        ASSERT(esdp->sched_wall_time.working.start
               == ERTS_SCHED_WTIME_IDLE);

        esdp->sched_wall_time.working.start = ts;

    }
    else {
        Uint64 total;

        ASSERT(esdp->sched_wall_time.working.start
               != ERTS_SCHED_WTIME_IDLE);

        total = esdp->sched_wall_time.working.total;
        total += ts - esdp->sched_wall_time.working.start;

        esdp->sched_wall_time.working.total = total;
        esdp->sched_wall_time.working.start = ERTS_SCHED_WTIME_IDLE;


    }

    ERTS_THR_WRITE_MEMORY_BARRIER;
    mod++;
    erts_atomic32_set_nob(&esdp->sched_wall_time.u.mod, mod);

#if 0
    if (!working) {
        ERTS_MSACC_SET_STATE_M_X(ERTS_MSACC_STATE_BUSY_WAIT);
    } else {
        ERTS_MSACC_SET_STATE_M_X(ERTS_MSACC_STATE_OTHER);
    }
#endif
}

#endif /* ERTS_SMP */

static void
sched_wall_time_change(ErtsSchedulerData *esdp, int working)
{
    if (esdp->sched_wall_time.u.need) {
	Uint64 ts = sched_wall_time_ts();
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
        update_avg_sched_util(esdp, ts, working);
#endif
	if (esdp->sched_wall_time.enabled) {
	    if (working) {
                ASSERT(esdp->sched_wall_time.working.start
                       == ERTS_SCHED_WTIME_IDLE);
		ts -= esdp->sched_wall_time.start;
		esdp->sched_wall_time.working.start = ts;
	    }
	    else {
                ASSERT(esdp->sched_wall_time.working.start
                       != ERTS_SCHED_WTIME_IDLE);
		ts -= esdp->sched_wall_time.start;
		ts -= esdp->sched_wall_time.working.start;
		esdp->sched_wall_time.working.total += ts;
#ifdef DEBUG
                esdp->sched_wall_time.working.start
                    = ERTS_SCHED_WTIME_IDLE;
#endif
	    }
	}
    }
    if (!working) {
        ERTS_MSACC_SET_STATE_M_X(ERTS_MSACC_STATE_BUSY_WAIT);
    } else {
        ERTS_MSACC_SET_STATE_M_X(ERTS_MSACC_STATE_OTHER);
    }

}

typedef struct {
    int set;
    int enable;
    Process *proc;
    Eterm ref;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Uint req_sched;
    erts_smp_atomic32_t refc;
#ifdef ERTS_DIRTY_SCHEDULERS
    int want_dirty_cpu;
    int want_dirty_io;
#endif
} ErtsSchedWallTimeReq;

typedef struct {
    Process *proc;
    Eterm ref;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Uint req_sched;
    erts_smp_atomic32_t refc;
} ErtsSystemCheckReq;


ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(swtreq,
				 ErtsSchedWallTimeReq,
				 5,
				 ERTS_ALC_T_SCHED_WTIME_REQ)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(screq,
				 ErtsSystemCheckReq,
				 5,
				 ERTS_ALC_T_SYS_CHECK_REQ)


static void
reply_sched_wall_time(void *vswtrp)
{
    Uint64 working = 0, total = 0;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsSchedWallTimeReq *swtrp = (ErtsSchedWallTimeReq *) vswtrp;
    ErtsProcLocks rp_locks = (swtrp->req_sched == esdp->no
			      ? ERTS_PROC_LOCK_MAIN
			      : 0);
    Process *rp = swtrp->proc;
    Eterm ref_copy = NIL, msg;
    Eterm *hp = NULL;
    Eterm **hpp;
    Uint sz, *szp;
    ErlOffHeap *ohp = NULL;
    ErtsMessage *mp = NULL;

    ASSERT(esdp);
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
#endif
    if (swtrp->set) {
	if (!swtrp->enable && esdp->sched_wall_time.enabled) {
	    esdp->sched_wall_time.u.need = erts_sched_balance_util;
	    esdp->sched_wall_time.enabled = 0;
	}
	else if (swtrp->enable && !esdp->sched_wall_time.enabled) {
	    Uint64 ts = sched_wall_time_ts();
	    esdp->sched_wall_time.u.need = 1;
	    esdp->sched_wall_time.enabled = 1;
	    esdp->sched_wall_time.start = ts;
	    esdp->sched_wall_time.working.total = 0;
	    esdp->sched_wall_time.working.start = 0;
	}
    }

    if (esdp->sched_wall_time.enabled) {
	Uint64 ts = sched_wall_time_ts();
        ASSERT(esdp->sched_wall_time.working.start
               != ERTS_SCHED_WTIME_IDLE);
	ts -= esdp->sched_wall_time.start;
	total = ts;
	ts -= esdp->sched_wall_time.working.start;
	working = esdp->sched_wall_time.working.total + ts;
    }

    sz = 0;
    hpp = NULL;
    szp = &sz;

#ifdef ERTS_DIRTY_SCHEDULERS
    if (esdp->sched_wall_time.enabled
        && swtrp->req_sched == esdp->no
        && (swtrp->want_dirty_cpu || swtrp->want_dirty_io)) {
        /* Reply with info about this scheduler and all dirty schedulers... */
        ErtsDirtySchedWallTime *dswt;
        int ix, no_dirty_scheds, want_dcpu, want_dio, soffset;

        want_dcpu = swtrp->want_dirty_cpu;
        want_dio = swtrp->want_dirty_io;

        no_dirty_scheds = 0;
        if (want_dcpu)
            no_dirty_scheds += erts_no_dirty_cpu_schedulers;
        if (want_dio)
            no_dirty_scheds += erts_no_dirty_io_schedulers;

        ASSERT(no_dirty_scheds);

        dswt = erts_alloc(ERTS_ALC_T_TMP,
                          sizeof(ErtsDirtySchedWallTime)
                          * no_dirty_scheds);

        for (ix = 0; ix < no_dirty_scheds; ix++) {
            ErtsSchedulerData *esdp;
            if (want_dcpu && ix < erts_no_dirty_cpu_schedulers)
                esdp = &erts_aligned_dirty_cpu_scheduler_data[ix].esd;
            else {
                int dio_ix = ix - erts_no_dirty_cpu_schedulers;
                esdp = &erts_aligned_dirty_io_scheduler_data[dio_ix].esd;
            }
            read_dirty_sched_wall_time(esdp, &dswt[ix]);
        }

        soffset = erts_no_schedulers + 1;

        if (!want_dcpu) {
            ASSERT(want_dio);
            soffset += erts_no_dirty_cpu_schedulers;
        }

        while (1) {
            if (hpp)
                ref_copy = STORE_NC(hpp, ohp, swtrp->ref);
            else
                *szp += ERTS_REF_THING_SIZE;

            ASSERT(!swtrp->set);

            /* info about dirty schedulers... */
            msg = NIL;
            for (ix = no_dirty_scheds-1; ix >= 0; ix--) {
                msg = erts_bld_cons(hpp, szp,
                                    erts_bld_tuple(hpp, szp, 3,
                                                   make_small(ix+soffset),
                                                   erts_bld_uint64(hpp, szp,
                                                                   dswt[ix].working),
                                                   erts_bld_uint64(hpp, szp,
                                                                   dswt[ix].total)),
                                    msg);
            }
            /* info about this scheduler... */
            msg = erts_bld_cons(hpp, szp,
                                erts_bld_tuple(hpp, szp, 3,
                                               make_small(esdp->no),
                                               erts_bld_uint64(hpp, szp, working),
                                               erts_bld_uint64(hpp, szp, total)),
                                msg);

            msg = erts_bld_tuple(hpp, szp, 2, ref_copy, msg);

            if (hpp)
                break;

            mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
            szp = NULL;
            hpp = &hp;
        }

        erts_free(ERTS_ALC_T_TMP, dswt);
    }
    else
#endif
    {
        /* Reply with info about this scheduler only... */

        while (1) {
            if (hpp)
                ref_copy = STORE_NC(hpp, ohp, swtrp->ref);
            else
                *szp += ERTS_REF_THING_SIZE;

            if (swtrp->set)
                msg = ref_copy;
            else {
                msg = (!esdp->sched_wall_time.enabled
                       ? am_undefined
                       : erts_bld_tuple(hpp, szp, 3,
                                        make_small(esdp->no),
                                        erts_bld_uint64(hpp, szp, working),
                                        erts_bld_uint64(hpp, szp, total)));

                msg = erts_bld_tuple(hpp, szp, 2, ref_copy, msg);
            }
            if (hpp)
                break;

            mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
            szp = NULL;
            hpp = &hp;
        }
    }

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (swtrp->req_sched == esdp->no)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;
 
    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    if (erts_smp_atomic32_dec_read_nob(&swtrp->refc) == 0)
	swtreq_free(vswtrp);
}

Eterm
erts_sched_wall_time_request(Process *c_p, int set, int enable,
                             int want_dirty_cpu, int want_dirty_io)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    Eterm ref;
    ErtsSchedWallTimeReq *swtrp;
    Eterm *hp;

    if (!set && !esdp->sched_wall_time.enabled)
	return THE_NON_VALUE;
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
#endif

    swtrp = swtreq_alloc();
    ref = erts_make_ref(c_p);
    hp = &swtrp->ref_heap[0];

    swtrp->set = set;
    swtrp->enable = enable;
    swtrp->proc = c_p;
    swtrp->ref = STORE_NC(&hp, NULL, ref);
    swtrp->req_sched = esdp->no;
#ifdef ERTS_DIRTY_SCHEDULERS
    swtrp->want_dirty_cpu = want_dirty_cpu;
    swtrp->want_dirty_io = want_dirty_io;
#endif
    erts_smp_atomic32_init_nob(&swtrp->refc,
			       (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint32) erts_no_schedulers);

#ifdef ERTS_SMP
    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_sched_wall_time,
					  (void *) swtrp);
#endif

    reply_sched_wall_time((void *) swtrp);

    return ref;
}

static void
reply_system_check(void *vscrp)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsSystemCheckReq *scrp = (ErtsSystemCheckReq *) vscrp;
    ErtsProcLocks rp_locks = (scrp->req_sched == esdp->no ? ERTS_PROC_LOCK_MAIN : 0);
    Process *rp = scrp->proc;
    Eterm msg;
    Eterm *hp = NULL;
    Eterm **hpp;
    Uint sz;
    ErlOffHeap *ohp = NULL;
    ErtsMessage *mp = NULL;

    ASSERT(esdp);
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
#endif

    sz = ERTS_REF_THING_SIZE;
    mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
    hpp = &hp;
    msg = STORE_NC(hpp, ohp, scrp->ref);

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (scrp->req_sched == esdp->no)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;

    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    if (erts_smp_atomic32_dec_read_nob(&scrp->refc) == 0)
	screq_free(vscrp);
}


Eterm erts_system_check_request(Process *c_p) {
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    Eterm ref;
    ErtsSystemCheckReq *scrp;
    Eterm *hp;

    scrp = screq_alloc();
    ref = erts_make_ref(c_p);
    hp = &scrp->ref_heap[0];

    scrp->proc = c_p;
    scrp->ref = STORE_NC(&hp, NULL, ref);
    scrp->req_sched = esdp->no;
    erts_smp_atomic32_init_nob(&scrp->refc, (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint) erts_no_schedulers);

#ifdef ERTS_SMP
    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_system_check,
					  (void *) scrp);
#endif

    reply_system_check((void *) scrp);

    return ref;
}

static ERTS_INLINE ErtsProcList *
proclist_create(Process *p)
{
    ErtsProcList *plp = proclist_alloc();
    ensure_later_proc_interval(p->common.u.alive.started_interval);
    plp->pid = p->common.id;
    plp->started_interval = p->common.u.alive.started_interval;
    return plp;
}

static ERTS_INLINE ErtsProcList *
proclist_copy(ErtsProcList *plp0)
{
    ErtsProcList *plp1 = proclist_alloc();
    plp1->pid = plp0->pid;
    plp1->started_interval = plp0->started_interval;
    return plp1;
}

static ERTS_INLINE void
proclist_destroy(ErtsProcList *plp)
{
    proclist_free(plp);
}

ErtsProcList *
erts_proclist_copy(ErtsProcList *plp)
{
    return proclist_copy(plp);
}

ErtsProcList *
erts_proclist_create(Process *p)
{
    return proclist_create(p);
}

void
erts_proclist_destroy(ErtsProcList *plp)
{
    proclist_destroy(plp);
}

void *
erts_psd_set_init(Process *p, int ix, void *data)
{
    void *old;
    ErtsPSD *psd, *new_psd;
    int i;

    new_psd = erts_alloc(ERTS_ALC_T_PSD, sizeof(ErtsPSD));
    for (i = 0; i < ERTS_PSD_SIZE; i++)
	new_psd->data[i] = NULL;

    psd = (ErtsPSD *) erts_smp_atomic_cmpxchg_mb(&p->psd,
						 (erts_aint_t) new_psd,
						 (erts_aint_t) NULL);
    if (psd)
	erts_free(ERTS_ALC_T_PSD, new_psd);
    else
	psd = new_psd;
    old = psd->data[ix];
    psd->data[ix] = data;
    return old;
}

#ifdef ERTS_SMP

void
erts_sched_finish_poke(ErtsSchedulerSleepInfo *ssi, erts_aint32_t flags)
{
    switch (flags & ERTS_SSI_FLGS_SLEEP_TYPE) {
    case ERTS_SSI_FLG_POLL_SLEEPING:
	erts_sys_schedule_interrupt(1);
	break;
    case ERTS_SSI_FLG_POLL_SLEEPING|ERTS_SSI_FLG_TSE_SLEEPING:
	/*
	 * Thread progress blocking while poll sleeping; need
	 * to signal on both...
	 */
	erts_sys_schedule_interrupt(1);
	/* fall through */
    case ERTS_SSI_FLG_TSE_SLEEPING:
	erts_tse_set(ssi->event);
	break;
    case 0:
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error\n",
		 __FILE__, __LINE__);
	break;
    }
}

#endif

static ERTS_INLINE void
set_aux_work_flags_wakeup_nob(ErtsSchedulerSleepInfo *ssi,
			      erts_aint32_t flgs)
{
    erts_aint32_t old_flgs;

    ERTS_DBG_CHK_SSI_AUX_WORK(ssi);

    old_flgs = erts_atomic32_read_nob(&ssi->aux_work);
    if ((old_flgs & flgs) != flgs) {

	old_flgs = erts_atomic32_read_bor_nob(&ssi->aux_work, flgs);

	if ((old_flgs & flgs) != flgs) {
#ifdef ERTS_SMP
	    erts_sched_poke(ssi);
#else
	    erts_sys_schedule_interrupt(1);
#endif
	}
    }
}

static ERTS_INLINE void
set_aux_work_flags_wakeup_relb(ErtsSchedulerSleepInfo *ssi,
			       erts_aint32_t flgs)
{
    erts_aint32_t old_flgs;

    ERTS_DBG_CHK_SSI_AUX_WORK(ssi);

    old_flgs = erts_atomic32_read_bor_relb(&ssi->aux_work, flgs);

    if ((old_flgs & flgs) != flgs) {
#ifdef ERTS_SMP
	erts_sched_poke(ssi);
#else
	erts_sys_schedule_interrupt(1);
#endif
    }
}

static ERTS_INLINE erts_aint32_t
set_aux_work_flags(ErtsSchedulerSleepInfo *ssi, erts_aint32_t flgs)
{
    return erts_atomic32_read_bor_nob(&ssi->aux_work, flgs);
}

static ERTS_INLINE erts_aint32_t
unset_aux_work_flags(ErtsSchedulerSleepInfo *ssi, erts_aint32_t flgs)
{
    return erts_atomic32_read_band_nob(&ssi->aux_work, ~flgs);
}

#ifdef ERTS_SMP

static ERTS_INLINE void
haw_chk_later_cleanup_op_wakeup(ErtsAuxWorkData *awdp, ErtsThrPrgrVal val)
{
    if (awdp->later_op.first
	&& erts_thr_progress_cmp(val, awdp->later_op.thr_prgr) >= 0) {
	awdp->later_op.size = thr_prgr_later_cleanup_op_threshold;
    }
}

static ERTS_INLINE void
haw_thr_prgr_wakeup(ErtsAuxWorkData *awdp, ErtsThrPrgrVal val)
{
    int cmp = erts_thr_progress_cmp(val, awdp->latest_wakeup);
    if (cmp != 0) {
	if (cmp > 0) {
	    awdp->latest_wakeup = val;
	    haw_chk_later_cleanup_op_wakeup(awdp, val);
	}
	erts_thr_progress_wakeup(awdp->esdp, val);
    }
}

static ERTS_INLINE void
haw_thr_prgr_soft_wakeup(ErtsAuxWorkData *awdp, ErtsThrPrgrVal val)
{
    if (erts_thr_progress_cmp(val, awdp->latest_wakeup) > 0) {
	awdp->latest_wakeup = val;
	haw_chk_later_cleanup_op_wakeup(awdp, val);
	erts_thr_progress_wakeup(awdp->esdp, val);
    }
}

static ERTS_INLINE void
haw_thr_prgr_later_cleanup_op_wakeup(ErtsAuxWorkData *awdp, ErtsThrPrgrVal val, UWord size)
{
    if (erts_thr_progress_cmp(val, awdp->latest_wakeup) > 0) {
	awdp->later_op.thr_prgr = val;
	if (awdp->later_op.size > size)
	    awdp->later_op.size -= size;
	else {
	    awdp->latest_wakeup = val;
	    awdp->later_op.size = thr_prgr_later_cleanup_op_threshold;
	    erts_thr_progress_wakeup(awdp->esdp, val);
	}
    }
}

static ERTS_INLINE void
haw_thr_prgr_current_reset(ErtsAuxWorkData *awdp)
{
    awdp->current_thr_prgr = ERTS_THR_PRGR_INVALID;
}

static ERTS_INLINE ErtsThrPrgrVal
haw_thr_prgr_current(ErtsAuxWorkData *awdp)
{
    ErtsThrPrgrVal current = awdp->current_thr_prgr;
    if (current == ERTS_THR_PRGR_INVALID) {
	current = erts_thr_progress_current();
	awdp->current_thr_prgr = current;
    }
    return current;
}

static ERTS_INLINE void
haw_thr_prgr_current_check_progress(ErtsAuxWorkData *awdp)
{
    ErtsThrPrgrVal current = awdp->current_thr_prgr;
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    if (current != ERTS_THR_PRGR_INVALID
	&& !erts_thr_progress_equal(current, erts_thr_progress_current())) {
	/*
	 * We have used a previouly read current value that isn't the
	 * latest; need to poke ourselfs in order to guarantee no loss
	 * of wakeups.
	 */
	erts_sched_poke(awdp->ssi);
    }
}

static ERTS_INLINE erts_aint32_t
handle_delayed_aux_work_wakeup(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    int jix, max_jix;

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif

    ASSERT(awdp->delayed_wakeup.next != ERTS_DELAYED_WAKEUP_INFINITY);

    if (!waiting && awdp->delayed_wakeup.next > awdp->esdp->reductions)
	return aux_work;

    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP);

    ERTS_THR_MEMORY_BARRIER;

    max_jix = awdp->delayed_wakeup.jix;
    awdp->delayed_wakeup.jix = -1;
    for (jix = 0; jix <= max_jix; jix++) {
	int sched = awdp->delayed_wakeup.job[jix].sched;
	erts_aint32_t aux_work = awdp->delayed_wakeup.job[jix].aux_work;

	ASSERT(awdp->delayed_wakeup.sched2jix[sched] == jix);
	awdp->delayed_wakeup.sched2jix[sched] = -1;
	set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(sched-1),
				      aux_work);
    }
    awdp->delayed_wakeup.next = ERTS_DELAYED_WAKEUP_INFINITY;
    return aux_work & ~ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP;
}

static ERTS_INLINE void
schedule_aux_work_wakeup(ErtsAuxWorkData *awdp,
			 int sched,
			 erts_aint32_t aux_work)
{
    int jix = awdp->delayed_wakeup.sched2jix[sched];
    if (jix >= 0) {
	ASSERT(awdp->delayed_wakeup.job[jix].sched == sched);
	awdp->delayed_wakeup.job[jix].aux_work |= aux_work;
    }
    else {
	jix = ++awdp->delayed_wakeup.jix;
	awdp->delayed_wakeup.sched2jix[sched] = jix;
	awdp->delayed_wakeup.job[jix].sched = sched;
	awdp->delayed_wakeup.job[jix].aux_work = aux_work;
    }

    if (awdp->delayed_wakeup.next != ERTS_DELAYED_WAKEUP_INFINITY) {
	ASSERT(erts_atomic32_read_nob(&awdp->ssi->aux_work)
	       & ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP);
    }
    else {
	awdp->delayed_wakeup.next = (awdp->esdp->reductions
				     + ERTS_DELAYED_WAKEUP_REDUCTIONS);

	ASSERT(!(erts_atomic32_read_nob(&awdp->ssi->aux_work)
		 & ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP));
	set_aux_work_flags_wakeup_nob(awdp->ssi,
				      ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP);
    }
}

#endif

typedef struct erts_misc_aux_work_t_ erts_misc_aux_work_t;
struct erts_misc_aux_work_t_ {
    void (*func)(void *);
    void *arg;
};

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(misc_aux_work,
				 erts_misc_aux_work_t,
				 200,
				 ERTS_ALC_T_MISC_AUX_WORK)

typedef union {
    ErtsThrQ_t q;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsThrQ_t))];
} erts_algnd_misc_aux_work_q_t;

static erts_algnd_misc_aux_work_q_t *misc_aux_work_queues;

static void
notify_aux_work(void *vssi)
{
    set_aux_work_flags_wakeup_nob((ErtsSchedulerSleepInfo *) vssi,
				  ERTS_SSI_AUX_WORK_MISC);
}

static void
init_misc_aux_work(void)
{
    int ix;
    ErtsThrQInit_t qinit = ERTS_THR_Q_INIT_DEFAULT;
    qinit.notify = notify_aux_work;

    init_misc_aux_work_alloc();

    misc_aux_work_queues = 
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_MISC_AUX_WORK_Q,
					   sizeof(erts_algnd_misc_aux_work_q_t)
					   * (erts_no_schedulers+1));

#ifdef ERTS_SMP
    ix = 0; /* aux_thread + schedulers */
#else
    ix = 1; /* scheduler only */
#endif

    for (; ix <= erts_no_schedulers; ix++) {
	qinit.arg = (void *) ERTS_SCHED_SLEEP_INFO_IX(ix-1);
	erts_thr_q_initialize(&misc_aux_work_queues[ix].q, &qinit);
    }
}

static erts_aint32_t
misc_aux_work_clean(ErtsThrQ_t *q,
		    ErtsAuxWorkData *awdp,
		    erts_aint32_t aux_work)
{
    switch (erts_thr_q_clean(q)) {
    case ERTS_THR_Q_DIRTY:
	set_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC);
	return aux_work | ERTS_SSI_AUX_WORK_MISC;
    case ERTS_THR_Q_NEED_THR_PRGR:
#ifdef ERTS_SMP
	set_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC_THR_PRGR);
	haw_thr_prgr_soft_wakeup(awdp, erts_thr_q_need_thr_progress(q));
#endif
    case ERTS_THR_Q_CLEAN:
	break;
    }
    return aux_work;
}

static ERTS_INLINE erts_aint32_t
handle_misc_aux_work(ErtsAuxWorkData *awdp,
		     erts_aint32_t aux_work,
		     int waiting)
{
    ErtsThrQ_t *q = &misc_aux_work_queues[awdp->sched_id].q;

    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC);
    while (1) {
	erts_misc_aux_work_t *mawp = erts_thr_q_dequeue(q);
	if (!mawp)
	    break;
	mawp->func(mawp->arg);
	misc_aux_work_free(mawp);
    }

    return misc_aux_work_clean(q, awdp, aux_work & ~ERTS_SSI_AUX_WORK_MISC);
}

#ifdef ERTS_SMP

static ERTS_INLINE erts_aint32_t
handle_misc_aux_work_thr_prgr(ErtsAuxWorkData *awdp,
			      erts_aint32_t aux_work,
			      int waiting)
{
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    if (!erts_thr_progress_has_reached_this(haw_thr_prgr_current(awdp),
					    awdp->misc.thr_prgr))
	return aux_work & ~ERTS_SSI_AUX_WORK_MISC_THR_PRGR;

    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC_THR_PRGR);

    return misc_aux_work_clean(&misc_aux_work_queues[awdp->sched_id].q,
			       awdp,
			       aux_work & ~ERTS_SSI_AUX_WORK_MISC_THR_PRGR);
}

#endif

static ERTS_INLINE void
schedule_misc_aux_work(int sched_id,
		       void (*func)(void *),
		       void *arg)
{
    ErtsThrQ_t *q;
    erts_misc_aux_work_t *mawp;

#ifdef ERTS_SMP
    ASSERT(0 <= sched_id && sched_id <= erts_no_schedulers);
#else
    ASSERT(sched_id == 1);
#endif

    q = &misc_aux_work_queues[sched_id].q;
    mawp = misc_aux_work_alloc();
    mawp->func = func;
    mawp->arg = arg;
    erts_thr_q_enqueue(q, mawp);
}

void
erts_schedule_misc_aux_work(int sched_id,
			    void (*func)(void *),
			    void *arg)
{
    schedule_misc_aux_work(sched_id, func, arg);
}

void
erts_schedule_multi_misc_aux_work(int ignore_self,
				  int max_sched,
				  void (*func)(void *),
				  void *arg)
{
    int id, self = 0;

    if (ignore_self) {
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
#ifdef ERTS_DIRTY_SCHEDULERS
	ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
#endif
	if (esdp)
	    self = (int) esdp->no;
    }

    ASSERT(0 < max_sched && max_sched <= erts_no_schedulers);

    for (id = 1; id <= max_sched; id++) {
	if (id == self)
	    continue;
	schedule_misc_aux_work(id, func, arg);
   }
}

#if ERTS_USE_ASYNC_READY_Q

void
erts_notify_check_async_ready_queue(void *vno)
{
    int ix = ((int) (SWord) vno) -1;
    set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(ix),
				  ERTS_SSI_AUX_WORK_ASYNC_READY);
}

static ERTS_INLINE erts_aint32_t
handle_async_ready(ErtsAuxWorkData *awdp,
		   erts_aint32_t aux_work,
		   int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY);
    if (erts_check_async_ready(awdp->async_ready.queue)) {
	if (set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY)
	    & ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN) {
	    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
	    aux_work &= ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
	}
	return aux_work;
    }
#ifdef ERTS_SMP
    awdp->async_ready.need_thr_prgr = 0;
#endif
    set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
    return ((aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY)
	    | ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
}

static ERTS_INLINE erts_aint32_t
handle_async_ready_clean(ErtsAuxWorkData *awdp,
			 erts_aint32_t aux_work,
			 int waiting)
{
    void *thr_prgr_p;

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
#ifdef ERTS_SMP
    if (awdp->async_ready.need_thr_prgr
	&& !erts_thr_progress_has_reached_this(haw_thr_prgr_current(awdp),
					       awdp->async_ready.thr_prgr)) {
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
    }

    awdp->async_ready.need_thr_prgr = 0;
    thr_prgr_p = (void *) &awdp->async_ready.thr_prgr;
#else
    thr_prgr_p = NULL;
#endif

    switch (erts_async_ready_clean(awdp->async_ready.queue, thr_prgr_p)) {
    case ERTS_ASYNC_READY_CLEAN:
	unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
#ifdef ERTS_SMP
    case ERTS_ASYNC_READY_NEED_THR_PRGR:
	haw_thr_prgr_soft_wakeup(awdp, awdp->async_ready.thr_prgr);
	awdp->async_ready.need_thr_prgr = 1;
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
#endif
    default:
	return aux_work;
    }
}

#endif /* ERTS_USE_ASYNC_READY_Q */


static ERTS_INLINE erts_aint32_t
handle_fix_alloc(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    erts_aint32_t res;

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    unset_aux_work_flags(ssi, (ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
			       | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC));
    aux_work &= ~(ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
		  | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC);
    res = erts_alloc_fix_alloc_shrink(awdp->sched_id, aux_work);
    if (res) {
	set_aux_work_flags(ssi, res);
	aux_work |= res;
    }

    return aux_work;
}

#ifdef ERTS_SMP

void
erts_alloc_notify_delayed_dealloc(int ix)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    if (esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp))
	schedule_aux_work_wakeup(&esdp->aux_work_data,
				 ix,
				 ERTS_SSI_AUX_WORK_DD);
    else
	set_aux_work_flags_wakeup_relb(ERTS_SCHED_SLEEP_INFO_IX(ix-1),
				       ERTS_SSI_AUX_WORK_DD);
}

void
erts_alloc_ensure_handle_delayed_dealloc_call(int ix)
{
#ifdef DEBUG
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ASSERT(!esdp || (ERTS_SCHEDULER_IS_DIRTY(esdp) || ix == (int) esdp->no));
#endif
    set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(ix-1),
				  ERTS_SSI_AUX_WORK_DD);
}

static ERTS_INLINE erts_aint32_t
handle_delayed_dealloc(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    int need_thr_progress = 0;
    ErtsThrPrgrVal wakeup = ERTS_THR_PRGR_INVALID;
    int more_work = 0;
    ERTS_MSACC_PUSH_STATE_M_X();
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD);
    ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_ALLOC);
    erts_alloc_scheduler_handle_delayed_dealloc((void *) awdp->esdp,
						&need_thr_progress,
						&wakeup,
						&more_work);
    ERTS_MSACC_POP_STATE_M_X();
    if (more_work) {
	if (set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD)
	    & ERTS_SSI_AUX_WORK_DD_THR_PRGR) {
	    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	    aux_work &= ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;
	}
	return aux_work;
    }

    if (need_thr_progress) {
	if (wakeup == ERTS_THR_PRGR_INVALID)
	    wakeup = erts_thr_progress_later(awdp->esdp);
	awdp->dd.thr_prgr = wakeup;
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	awdp->dd.thr_prgr = wakeup;
	haw_thr_prgr_soft_wakeup(awdp, wakeup);
    }
    return aux_work & ~ERTS_SSI_AUX_WORK_DD;
}

static ERTS_INLINE erts_aint32_t
handle_delayed_dealloc_thr_prgr(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi;
    int need_thr_progress;
    int more_work;
    ErtsThrPrgrVal wakeup = ERTS_THR_PRGR_INVALID;
    ErtsThrPrgrVal current = haw_thr_prgr_current(awdp);

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    if (!erts_thr_progress_has_reached_this(current, awdp->dd.thr_prgr))
	return aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;

    ssi = awdp->ssi;
    need_thr_progress = 0;
    more_work = 0;

    erts_alloc_scheduler_handle_delayed_dealloc((void *) awdp->esdp,
						&need_thr_progress,
						&wakeup,
						&more_work);
    if (more_work) {
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD);
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	return ((aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR)
		| ERTS_SSI_AUX_WORK_DD);
    }

    if (need_thr_progress) {
	if (wakeup == ERTS_THR_PRGR_INVALID)
	    wakeup = erts_thr_progress_later(awdp->esdp);
	awdp->dd.thr_prgr = wakeup;
	haw_thr_prgr_soft_wakeup(awdp, wakeup);
    }
    else {
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
    }

    return aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;
}

/*
 * Canceled timers
 */

void
erts_notify_canceled_timer(ErtsSchedulerData *esdp, int rsid)
{
    ASSERT(esdp && esdp == erts_get_scheduler_data());
    if (esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp))
	schedule_aux_work_wakeup(&esdp->aux_work_data,
				 rsid,
				 ERTS_SSI_AUX_WORK_CNCLD_TMRS);
    else
	set_aux_work_flags_wakeup_relb(ERTS_SCHED_SLEEP_INFO_IX(rsid-1),
				       ERTS_SSI_AUX_WORK_CNCLD_TMRS);
}

static ERTS_INLINE erts_aint32_t
handle_canceled_timers(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    int need_thr_progress = 0;
    ErtsThrPrgrVal wakeup = ERTS_THR_PRGR_INVALID;
    int more_work = 0;

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS);
    erts_handle_canceled_timers((void *) awdp->esdp,
				&need_thr_progress,
				&wakeup,
				&more_work);
    if (more_work) {
	if (set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS)
	    & ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR) {
	    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
	    aux_work &= ~ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR;
	}
	return aux_work;
    }

    if (need_thr_progress) {
	if (wakeup == ERTS_THR_PRGR_INVALID)
	    wakeup = erts_thr_progress_later(awdp->esdp);
	awdp->cncld_tmrs.thr_prgr = wakeup;
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
	haw_thr_prgr_soft_wakeup(awdp, wakeup);
    }
    return aux_work & ~ERTS_SSI_AUX_WORK_CNCLD_TMRS;
}

static ERTS_INLINE erts_aint32_t
handle_canceled_timers_thr_prgr(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi;
    int need_thr_progress;
    int more_work;
    ErtsThrPrgrVal wakeup = ERTS_THR_PRGR_INVALID;
    ErtsThrPrgrVal current = haw_thr_prgr_current(awdp);

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    if (!erts_thr_progress_has_reached_this(current, awdp->cncld_tmrs.thr_prgr))
	return aux_work & ~ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR;

    ssi = awdp->ssi;
    need_thr_progress = 0;
    more_work = 0;

    erts_handle_canceled_timers((void *) awdp->esdp,
				&need_thr_progress,
				&wakeup,
				&more_work);
    if (more_work) {
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS);
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
	return ((aux_work & ~ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR)
		| ERTS_SSI_AUX_WORK_CNCLD_TMRS);
    }

    if (need_thr_progress) {
	if (wakeup == ERTS_THR_PRGR_INVALID)
	    wakeup = erts_thr_progress_later(awdp->esdp);
	awdp->cncld_tmrs.thr_prgr = wakeup;
	haw_thr_prgr_soft_wakeup(awdp, wakeup);
    }
    else {
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
    }

    return aux_work & ~ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR;
}

/*
 * Handle scheduled thread progress later operations.
 */
#define ERTS_MAX_THR_PRGR_LATER_OPS 50

static ERTS_INLINE erts_aint32_t
handle_thr_prgr_later_op(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    int lops;
    ErtsThrPrgrVal current = haw_thr_prgr_current(awdp);

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif
    for (lops = 0; lops < ERTS_MAX_THR_PRGR_LATER_OPS; lops++) {
	ErtsThrPrgrLaterOp *lop = awdp->later_op.first;

	if (!erts_thr_progress_has_reached_this(current, lop->later))
	    return aux_work & ~ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP;
	awdp->later_op.first = lop->next;
	if (!awdp->later_op.first) {
	    awdp->later_op.last = NULL;
	}
	lop->func(lop->data);
	if (!awdp->later_op.first) {
	    awdp->later_op.size = thr_prgr_later_cleanup_op_threshold;
	    awdp->later_op.last = NULL;
	    unset_aux_work_flags(awdp->ssi,
				 ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP);
	    return aux_work & ~ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP;
	}
    }

    return aux_work;
}

static ERTS_INLINE ErtsThrPrgrVal
enqueue_later_op(ErtsSchedulerData *esdp,
		 void (*later_func)(void *),
		 void *later_data,
		 ErtsThrPrgrLaterOp *lop)
{
    ErtsThrPrgrVal later = erts_thr_progress_later(esdp);
    ASSERT(esdp);

    lop->func = later_func;
    lop->data = later_data;
    lop->later = later;
    lop->next = NULL;
    if (!esdp->aux_work_data.later_op.last)
	esdp->aux_work_data.later_op.first = lop;
    else
	esdp->aux_work_data.later_op.last->next = lop;
    esdp->aux_work_data.later_op.last = lop;
    set_aux_work_flags_wakeup_nob(esdp->ssi,
				  ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP);
    return later;
}

#endif /* ERTS_SMP */

void
erts_schedule_thr_prgr_later_op(void (*later_func)(void *),
				void *later_data,
				ErtsThrPrgrLaterOp *lop)
{
#ifndef ERTS_SMP
    later_func(later_data);
#else
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsThrPrgrVal later = enqueue_later_op(esdp, later_func, later_data, lop);
    haw_thr_prgr_wakeup(&esdp->aux_work_data, later);
#endif
}

void
erts_schedule_thr_prgr_later_cleanup_op(void (*later_func)(void *),
					void *later_data,
					ErtsThrPrgrLaterOp *lop,
					UWord size)
{
#ifndef ERTS_SMP
    later_func(later_data);
#else
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsThrPrgrVal later = enqueue_later_op(esdp, later_func, later_data, lop);
    haw_thr_prgr_later_cleanup_op_wakeup(&esdp->aux_work_data, later, size);
#endif
}

static ERTS_INLINE erts_aint32_t
handle_debug_wait_completed(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    erts_aint32_t saved_aux_work, flags;

#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#endif

    flags = awdp->debug.wait_completed.flags;

    if (aux_work & flags)
	return aux_work;

    saved_aux_work = erts_atomic32_read_acqb(&ssi->aux_work);

    if (saved_aux_work & flags)
	return aux_work & ~ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED;

    awdp->debug.wait_completed.callback(awdp->debug.wait_completed.arg);

    awdp->debug.wait_completed.flags = 0;
    awdp->debug.wait_completed.callback = NULL;
    awdp->debug.wait_completed.arg = NULL;

    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED);

    return aux_work & ~ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED;
}

static erts_atomic32_t debug_wait_completed_count;
static int debug_wait_completed_flags;

static void
thr_debug_wait_completed(void *vproc)
{
    if (erts_atomic32_dec_read_mb(&debug_wait_completed_count) == 0) {
	erts_resume((Process *) vproc, (ErtsProcLocks) 0);
	erts_proc_dec_refc((Process *) vproc);
    }
}

static void
setup_thr_debug_wait_completed(void *vproc)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsAuxWorkData *awdp;
    erts_aint32_t wait_flags, aux_work_flags;
#ifdef ERTS_SMP
    awdp = esdp ? &esdp->aux_work_data : aux_thread_aux_work_data;
#else
    awdp = &esdp->aux_work_data;
#endif

    wait_flags = 0;
    aux_work_flags = ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED;

    if (debug_wait_completed_flags & ERTS_DEBUG_WAIT_COMPLETED_DEALLOCATIONS) {
	erts_alloc_fix_alloc_shrink(awdp->sched_id, 0);
	wait_flags |= (ERTS_SSI_AUX_WORK_DD
		       | ERTS_SSI_AUX_WORK_DD_THR_PRGR);
#ifdef ERTS_SMP
	aux_work_flags |= ERTS_SSI_AUX_WORK_DD;
#endif
    }

    if (debug_wait_completed_flags & ERTS_DEBUG_WAIT_COMPLETED_TIMER_CANCELLATIONS) {
	wait_flags |= (ERTS_SSI_AUX_WORK_CNCLD_TMRS
		       | ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
#ifdef ERTS_SMP
	if (awdp->esdp && !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp))
	    aux_work_flags |= ERTS_SSI_AUX_WORK_CNCLD_TMRS;
#endif
    }

    set_aux_work_flags_wakeup_nob(awdp->ssi, aux_work_flags);

    awdp->debug.wait_completed.flags = wait_flags;
    awdp->debug.wait_completed.callback = thr_debug_wait_completed;
    awdp->debug.wait_completed.arg = vproc;
}

struct debug_lop {
    ErtsThrPrgrLaterOp lop;
    Process *proc;
};

static void later_thr_debug_wait_completed(void *vlop)
{
    struct debug_lop *lop = vlop;
    erts_aint32_t count = (erts_aint32_t) erts_no_schedulers;
#ifdef ERTS_SMP
    count += 1; /* aux thread */
#endif
    if (erts_atomic32_dec_read_mb(&debug_wait_completed_count) == count) {
        /* scheduler threads */
        erts_schedule_multi_misc_aux_work(0,
                                          erts_no_schedulers,
                                          setup_thr_debug_wait_completed,
                                          lop->proc);
#ifdef ERTS_SMP
        /* aux_thread */
        erts_schedule_misc_aux_work(0,
                                    setup_thr_debug_wait_completed,
                                    lop->proc);
#endif
    }
    erts_free(ERTS_ALC_T_DEBUG, lop);
}


static void
init_thr_debug_wait_completed(void *vproc)
{
    struct debug_lop* lop = erts_alloc(ERTS_ALC_T_DEBUG,
                                       sizeof(struct debug_lop));
    lop->proc = vproc;
    erts_schedule_thr_prgr_later_op(later_thr_debug_wait_completed, lop, &lop->lop);
}


int
erts_debug_wait_completed(Process *c_p, int flags)
{
    /* Only one process at a time can do this */
    erts_aint32_t count = (erts_aint32_t) (2*erts_no_schedulers);
#ifdef ERTS_SMP
    count += 1; /* aux thread */
#endif
    if (0 == erts_atomic32_cmpxchg_mb(&debug_wait_completed_count,
				      count,
				      0)) {
	debug_wait_completed_flags = flags;
	erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
	erts_proc_inc_refc(c_p);

        /* First flush later-ops on all scheduler threads */
	erts_schedule_multi_misc_aux_work(0,
					  erts_no_schedulers,
					  init_thr_debug_wait_completed,
					  (void *) c_p);
	return 1;
    }
    return 0;
}


static void
notify_reap_ports_relb(void)
{
    int i;
    for (i = 0; i < erts_no_schedulers; i++) {
	set_aux_work_flags_wakeup_relb(ERTS_SCHED_SLEEP_INFO_IX(i),
				       ERTS_SSI_AUX_WORK_REAP_PORTS);
    }
}

erts_smp_atomic32_t erts_halt_progress;
int erts_halt_code;

static ERTS_INLINE erts_aint32_t
handle_reap_ports(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_REAP_PORTS);
    ERTS_RUNQ_FLGS_SET(awdp->esdp->run_queue, ERTS_RUNQ_FLG_HALTING);

    if (erts_smp_atomic32_dec_read_acqb(&erts_halt_progress) == 0) {
	int i, max = erts_ptab_max(&erts_port);
	erts_smp_atomic32_set_nob(&erts_halt_progress, 1);
	for (i = 0; i < max; i++) {
	    erts_aint32_t state;
	    Port *prt = erts_pix2port(i);
	    if (!prt)
		continue;
	    state = erts_atomic32_read_acqb(&prt->state);
	    if (state & (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
			 | ERTS_PORT_SFLG_HALT))
		continue;

	    /* We need to set the halt flag - get the port lock */

	    erts_smp_port_lock(prt);

	    state = erts_atomic32_read_nob(&prt->state);
	    if (!(state & (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
			   | ERTS_PORT_SFLG_HALT))) {
		state = erts_atomic32_read_bor_relb(&prt->state,
						    ERTS_PORT_SFLG_HALT);
		erts_smp_atomic32_inc_nob(&erts_halt_progress);
		if (!(state & (ERTS_PORT_SFLG_EXITING|ERTS_PORT_SFLG_CLOSING)))
		    erts_deliver_port_exit(prt, prt->common.id, am_killed, 0, 1);
	    }

	    erts_port_release(prt);
	}
	if (erts_smp_atomic32_dec_read_nob(&erts_halt_progress) == 0) {
	    erts_flush_async_exit(erts_halt_code, "");
	}
    }
    return aux_work & ~ERTS_SSI_AUX_WORK_REAP_PORTS;
}

void
erts_notify_new_aux_yield_work(ErtsSchedulerData *esdp)
{
    ASSERT(esdp == erts_get_scheduler_data());
    /* Always called by the scheduler itself... */
    set_aux_work_flags_wakeup_nob(esdp->ssi, ERTS_SSI_AUX_WORK_YIELD);
}

static ERTS_INLINE erts_aint32_t
handle_yield(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    int yield = 0;
    /*
     * Yield operations are always requested by the scheduler itself.
     *
     * The following handlers should *not* set the ERTS_SSI_AUX_WORK_YIELD
     * flag in order to indicate more work. They should instead return
     * information so this "main handler" can manipulate the flag...
     *
     * The following handlers should be able to handle being called
     * even though no work is to be done...
     */

    /* Various yielding operations... */

    yield |= erts_handle_yielded_ets_all_request(awdp->esdp,
                                                 &awdp->yield.ets_all);

    /*
     * Other yielding operations...
     *
     */

    if (!yield) {
        unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_YIELD);
        return aux_work & ~ERTS_SSI_AUX_WORK_YIELD;
    }

    return aux_work;
}


#if HAVE_ERTS_MSEG

static ERTS_INLINE erts_aint32_t
handle_mseg_cache_check(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK);
    erts_mseg_cache_check();
    return aux_work & ~ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK;
}

#endif

#ifdef ERTS_SMP

static ERTS_INLINE erts_aint32_t
handle_pending_exiters(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsProcList *pnd_xtrs;
    ErtsRunQueue *rq;

    rq = awdp->esdp->run_queue;
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_PENDING_EXITERS);

    erts_smp_runq_lock(rq);
    pnd_xtrs = rq->procs.pending_exiters;
    rq->procs.pending_exiters = NULL;
    erts_smp_runq_unlock(rq);

    if (erts_proclist_fetch(&pnd_xtrs, NULL))
	do_handle_pending_exiters(pnd_xtrs);

    return aux_work & ~ERTS_SSI_AUX_WORK_PENDING_EXITERS;
}

#endif

static ERTS_INLINE erts_aint32_t
handle_setup_aux_work_timer(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_SET_TMO);
    setup_aux_work_timer(awdp->esdp);
    return aux_work & ~ERTS_SSI_AUX_WORK_SET_TMO;
}

static erts_aint32_t
handle_aux_work(ErtsAuxWorkData *awdp, erts_aint32_t orig_aux_work, int waiting)
{
#undef HANDLE_AUX_WORK
#define HANDLE_AUX_WORK(FLG, HNDLR) \
    ignore |= FLG; \
    if (aux_work & FLG) { \
	aux_work = HNDLR(awdp, aux_work, waiting);   \
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work); \
	if (!(aux_work & ~ignore)) { \
	    ERTS_DBG_CHK_AUX_WORK_VAL(aux_work); \
            ERTS_MSACC_UPDATE_CACHE();       \
            ERTS_MSACC_POP_STATE_M();              \
	    return aux_work; \
	} \
    }

    erts_aint32_t aux_work = orig_aux_work;
    erts_aint32_t ignore = 0;
    ERTS_MSACC_PUSH_AND_SET_STATE_M(ERTS_MSACC_STATE_AUX);

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));
#ifdef ERTS_SMP
    haw_thr_prgr_current_reset(awdp);
#endif

    ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    ASSERT(aux_work);

    /*
     * Handlers are *only* allowed to modify flags in return value
     * and ssi flags that are explicity handled by the handler.
     * Handlers are, e.g., not allowed to read the ssi flag field and
     * then unconditionally return that value.
     *
     * Flag field returned should only contain flags for work that
     * can continue immediately.
     */

    /*
     * Keep ERTS_SSI_AUX_WORK flags in expected frequency order relative
     * eachother. Most frequent first.
     */
#ifdef ERTS_SMP
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP,
		    handle_delayed_aux_work_wakeup);
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DD,
		    handle_delayed_dealloc);
    /* DD must be before DD_THR_PRGR */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DD_THR_PRGR,
		    handle_delayed_dealloc_thr_prgr);
#endif

    HANDLE_AUX_WORK((ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
		     | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
		    handle_fix_alloc);

#ifdef ERTS_SMP
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP,
		    handle_thr_prgr_later_op);
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_CNCLD_TMRS,
		    handle_canceled_timers);
    /* CNCLD_TMRS must be before CNCLD_TMRS_THR_PRGR */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR,
		    handle_canceled_timers_thr_prgr);
#endif

#if ERTS_USE_ASYNC_READY_Q
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_ASYNC_READY,
		    handle_async_ready);
    /* ASYNC_READY must be before ASYNC_READY_CLEAN */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN,
		    handle_async_ready_clean);
#endif

#ifdef ERTS_SMP
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_MISC_THR_PRGR,
		    handle_misc_aux_work_thr_prgr);
#endif
    /* MISC_THR_PRGR must be before MISC */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_MISC,
		    handle_misc_aux_work);

#ifdef ERTS_SMP
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_PENDING_EXITERS,
		    handle_pending_exiters);
#endif

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_SET_TMO,
		    handle_setup_aux_work_timer);

#if HAVE_ERTS_MSEG
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK,
		    handle_mseg_cache_check);
#endif

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_YIELD,
		    handle_yield);

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_REAP_PORTS,
		    handle_reap_ports);

    /*
     * ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED *need* to be
     * the last flag checked!
     */

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED,
		    handle_debug_wait_completed);

    ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);

#ifdef ERTS_SMP
    if (waiting && !aux_work)
	haw_thr_prgr_current_check_progress(awdp);
#endif

    ERTS_MSACC_UPDATE_CACHE();
    ERTS_MSACC_POP_STATE_M();
    return aux_work;

#undef HANDLE_AUX_WORK

}

typedef struct {
    union {
	ErtsTWheelTimer data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsTWheelTimer))];
    } timer;

    int initialized;
    erts_atomic32_t refc;
#ifdef DEBUG
    erts_atomic32_t used;
#endif
    erts_atomic32_t type[1];
} ErtsAuxWorkTmo;

static ErtsAuxWorkTmo *aux_work_tmo;

static ERTS_INLINE void
start_aux_work_timer(ErtsSchedulerData *esdp)
{
    ErtsMonotonicTime tmo = erts_get_monotonic_time(esdp);
#ifdef DEBUG
    Uint no = (Uint) erts_atomic32_xchg_mb(&aux_work_tmo->used,
                                           (erts_aint32_t) esdp->no);
    ASSERT(esdp->type == ERTS_SCHED_NORMAL);
    ASSERT(!no);
#endif

    tmo = ERTS_MONOTONIC_TO_CLKTCKS(tmo-1);
    tmo += ERTS_MSEC_TO_CLKTCKS(1000) + 1;
    erts_twheel_init_timer(&aux_work_tmo->timer.data);
    ASSERT(esdp);
    erts_twheel_set_timer(esdp->timer_wheel,
			  &aux_work_tmo->timer.data,
			  aux_work_timeout,
			  (void *) esdp,
			  tmo);
}

static void
aux_work_timeout_early_init(int no_schedulers)
{
    int i;
    UWord p;

    /*
     * This is done really early. Our own allocators have
     * not been started yet.
     */

    p = (UWord) malloc((sizeof(ErtsAuxWorkTmo)
			+ sizeof(erts_atomic32_t)*(no_schedulers+1))
		       + ERTS_CACHE_LINE_SIZE-1);
    if (!p) {
        ERTS_INTERNAL_ERROR("malloc failed to allocate memory!");
    }
    if (p & ERTS_CACHE_LINE_MASK)
	p = (p & ~ERTS_CACHE_LINE_MASK) + ERTS_CACHE_LINE_SIZE;
    ASSERT((p & ERTS_CACHE_LINE_MASK) == 0);

    aux_work_tmo = (ErtsAuxWorkTmo *) p;
    aux_work_tmo->initialized = 0;
    erts_atomic32_init_nob(&aux_work_tmo->refc, 0);
#ifdef DEBUG
    erts_atomic32_init_nob(&aux_work_tmo->used, 0);
#endif
    for (i = 0; i <= no_schedulers; i++)
	erts_atomic32_init_nob(&aux_work_tmo->type[i], 0);
}

void
erts_aux_work_timeout_late_init(ErtsSchedulerData *esdp)
{
    aux_work_tmo->initialized = 1;
    if (erts_atomic32_read_acqb(&aux_work_tmo->refc))
	start_aux_work_timer(esdp);
}

static void
aux_work_timeout(void *vesdp)
{
    erts_aint32_t refc;
    int i;
#ifdef DEBUG
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    Uint no = (Uint) erts_atomic32_xchg_mb(&aux_work_tmo->used, 0);
    ASSERT(no == esdp->no);
    ASSERT(esdp == (ErtsSchedulerData *) vesdp);
#endif

#ifdef ERTS_SMP
    i = 0;
#else
    i = 1;
#endif

    for (; i <= erts_no_schedulers; i++) {
	erts_aint32_t type;
	type = erts_atomic32_read_acqb(&aux_work_tmo->type[i]);
	if (type)
	    set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(i-1),
					  type);
    }

    refc = erts_atomic32_read_nob(&aux_work_tmo->refc);
    ASSERT(refc >= 1);
    if (refc != 1
	|| 1 != erts_atomic32_cmpxchg_relb(&aux_work_tmo->refc, 0, 1)) {
	/* Setup next timeout... */
	start_aux_work_timer((ErtsSchedulerData *) vesdp);
    }
}

static void
setup_aux_work_timer(ErtsSchedulerData *esdp)
{
    if (!esdp || !esdp->timer_wheel)
	set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(0),
				      ERTS_SSI_AUX_WORK_SET_TMO);
    else
	start_aux_work_timer(esdp);
}

erts_aint32_t
erts_set_aux_work_timeout(int ix, erts_aint32_t type, int enable)
{
    erts_aint32_t old, refc;

#ifndef ERTS_SMP
    ix = 1;
#endif

    ERTS_DBG_CHK_AUX_WORK_VAL(type);
    ERTS_DBG_CHK_AUX_WORK_VAL(erts_atomic32_read_nob(&aux_work_tmo->type[ix]));
    /* erts_fprintf(stderr, "t(%d, 0x%x, %d)\n", ix, type, enable); */

    if (!enable) {
	old = erts_atomic32_read_band_mb(&aux_work_tmo->type[ix], ~type);
	ERTS_DBG_CHK_AUX_WORK_VAL(erts_atomic32_read_nob(&aux_work_tmo->type[ix]));
	if (old != 0 && (old & ~type) == 0)
	    erts_atomic32_dec_relb(&aux_work_tmo->refc);
	return old;
    }

    old = erts_atomic32_read_bor_mb(&aux_work_tmo->type[ix], type);
    ERTS_DBG_CHK_AUX_WORK_VAL(erts_atomic32_read_nob(&aux_work_tmo->type[ix]));
    if (old == 0 && type != 0) {
	refc = erts_atomic32_inc_read_acqb(&aux_work_tmo->refc);
	if (refc == 1) {
	    erts_atomic32_inc_acqb(&aux_work_tmo->refc);
	    if (aux_work_tmo->initialized) 
		setup_aux_work_timer(erts_get_scheduler_data());
	}
    }
    return old;
}



static ERTS_INLINE void
sched_waiting_sys(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    ASSERT(rq->waiting >= 0);
    (void) ERTS_RUNQ_FLGS_SET(rq, (ERTS_RUNQ_FLG_OUT_OF_WORK
				   | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK));
    rq->waiting++;
    rq->waiting *= -1;
    rq->woken = 0;
    if (erts_system_profile_flags.scheduler)
	profile_scheduler(make_small(no), am_inactive);
}

static ERTS_INLINE void
sched_active_sys(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!ERTS_RUNQ_IX_IS_DIRTY(rq->ix));
#endif
    ASSERT(rq->waiting < 0);
    rq->waiting *= -1;
    rq->waiting--;
    if (erts_system_profile_flags.scheduler)
	profile_scheduler(make_small(no), am_active);
}

Uint
erts_active_schedulers(void)
{
    Uint as = erts_no_schedulers;

    ERTS_ATOMIC_FOREACH_RUNQ(rq, as -= abs(rq->waiting));

    return as;
}

#ifdef ERTS_SMP

static ERTS_INLINE void
clear_sys_scheduling(void)
{
    erts_smp_atomic32_set_mb(&doing_sys_schedule, 0);
}

static ERTS_INLINE int
try_set_sys_scheduling(void)
{
    return 0 == erts_smp_atomic32_cmpxchg_acqb(&doing_sys_schedule, 1, 0);
}

#endif

static ERTS_INLINE int
prepare_for_sys_schedule(int non_blocking)
{
    if (non_blocking && erts_eager_check_io) {
#ifdef ERTS_SMP
	return try_set_sys_scheduling();
#else
	return 1;
#endif
    }
    else {
#ifdef ERTS_SMP
	while (!erts_port_task_have_outstanding_io_tasks()
	       && try_set_sys_scheduling()) {
	    if (!erts_port_task_have_outstanding_io_tasks())
		return 1;
	    clear_sys_scheduling();
	}
	return 0;
#else
	return !erts_port_task_have_outstanding_io_tasks();
#endif
    }
}

#ifdef ERTS_SMP

static ERTS_INLINE void
sched_change_waiting_sys_to_waiting(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(!ERTS_RUNQ_IX_IS_DIRTY(rq->ix));
#endif
    ASSERT(rq->waiting < 0);
    rq->waiting *= -1;
}

static ERTS_INLINE void
sched_waiting(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    (void) ERTS_RUNQ_FLGS_SET(rq, (ERTS_RUNQ_FLG_OUT_OF_WORK
				   | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK));
    if (rq->waiting < 0)
	rq->waiting--;
    else
	rq->waiting++;
    rq->woken = 0;
    if (!ERTS_RUNQ_IX_IS_DIRTY(rq->ix) && erts_system_profile_flags.scheduler)
	profile_scheduler(make_small(no), am_inactive);
}

static ERTS_INLINE void
sched_active(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    if (rq->waiting < 0)
	rq->waiting++;
    else
	rq->waiting--;
    if (!ERTS_RUNQ_IX_IS_DIRTY(rq->ix) && erts_system_profile_flags.scheduler)
	profile_scheduler(make_small(no), am_active);
}

static ERTS_INLINE void
empty_runq_aux(ErtsRunQueue *rq, Uint32 old_flags)
{
    if (!ERTS_RUNQ_IX_IS_DIRTY(rq->ix) && old_flags & ERTS_RUNQ_FLG_NONEMPTY) {
#ifdef DEBUG
	erts_aint32_t empty = erts_smp_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 <= empty && empty < 2*erts_no_run_queues);
#endif
	if (!erts_runq_supervision_interval)
	    erts_smp_atomic32_inc_relb(&no_empty_run_queues);
	else {
	    erts_smp_atomic32_inc_mb(&no_empty_run_queues);
	    if (erts_atomic_read_nob(&runq_supervisor_sleeping))
		ethr_event_set(&runq_supervision_event);
	}
    }
}

static ERTS_INLINE void
empty_runq(ErtsRunQueue *rq)
{
    Uint32 old_flags = ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_NONEMPTY|ERTS_RUNQ_FLG_PROTECTED);
    empty_runq_aux(rq, old_flags);
}

static ERTS_INLINE Uint32
empty_protected_runq(ErtsRunQueue *rq)
{
    Uint32 old_flags = ERTS_RUNQ_FLGS_BSET(rq,
					   ERTS_RUNQ_FLG_NONEMPTY|ERTS_RUNQ_FLG_PROTECTED,
					   ERTS_RUNQ_FLG_PROTECTED);
    empty_runq_aux(rq, old_flags);
    return old_flags;
}

static ERTS_INLINE void
non_empty_runq(ErtsRunQueue *rq)
{
    Uint32 old_flags = ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_NONEMPTY);
    if (!ERTS_RUNQ_IX_IS_DIRTY(rq->ix) && (!(old_flags & ERTS_RUNQ_FLG_NONEMPTY))) {
#ifdef DEBUG
	erts_aint32_t empty = erts_smp_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 < empty && empty <= 2*erts_no_run_queues);
#endif
	if (!erts_runq_supervision_interval)
	    erts_smp_atomic32_dec_relb(&no_empty_run_queues);
	else {
	    erts_aint32_t no;
	    no = erts_smp_atomic32_dec_read_mb(&no_empty_run_queues);
	    if (no > 0 && erts_atomic_read_nob(&runq_supervisor_sleeping))
		ethr_event_set(&runq_supervision_event);
	}
    }
}

void
erts_empty_runq(ErtsRunQueue *rq)
{
    empty_runq(rq);
}

void
erts_non_empty_runq(ErtsRunQueue *rq)
{
    non_empty_runq(rq);
}

static erts_aint32_t
sched_prep_spin_wait(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs;
    erts_aint32_t xflgs = 0;

    do {
        nflgs = (xflgs & ERTS_SSI_FLG_MSB_EXEC);
        nflgs |= ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING;
	oflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return nflgs;
	xflgs = oflgs;
    } while (!(oflgs & ERTS_SSI_FLG_SUSPENDED));
    return oflgs;
}

static erts_aint32_t
sched_prep_cont_spin_wait(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING);
    erts_aint32_t xflgs = ERTS_SSI_FLG_WAITING;

    do {
	oflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return nflgs;
	xflgs = oflgs;
	nflgs |= oflgs & (ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC);
    } while (oflgs & ERTS_SSI_FLG_WAITING);
    return oflgs;
}

static erts_aint32_t
sched_spin_wait(ErtsSchedulerSleepInfo *ssi, int spincount)
{
    int until_yield = ERTS_SCHED_SPIN_UNTIL_YIELD;
    int sc = spincount;
    erts_aint32_t flgs;

    do {
	flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
	if ((flgs & (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING))
	    != (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING)) {
	    break;
	}
	ERTS_SPIN_BODY;
	if (--until_yield == 0) {
	    until_yield = ERTS_SCHED_SPIN_UNTIL_YIELD;
	    erts_thr_yield();
	}
    } while (--sc > 0);
    return flgs;
}

static erts_aint32_t
sched_set_sleeptype(ErtsSchedulerSleepInfo *ssi, erts_aint32_t sleep_type)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING|sleep_type;
    erts_aint32_t xflgs = ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING;

    if (sleep_type == ERTS_SSI_FLG_TSE_SLEEPING)
	erts_tse_reset(ssi->event);
    else {
	ASSERT(sleep_type == ERTS_SSI_FLG_POLL_SLEEPING);
	erts_sys_schedule_interrupt(0);
    }

    while (1) {
	oflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return nflgs;
	if ((oflgs & (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING))
	    != (ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING)) {
	    return oflgs;
	}
	xflgs = oflgs;
	nflgs |= oflgs & (ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC);
    }
}

#define ERTS_SCHED_WAIT_WOKEN(FLGS)				\
  (((FLGS) & (ERTS_SSI_FLG_WAITING|ERTS_SSI_FLG_SUSPENDED))	\
   != ERTS_SSI_FLG_WAITING)


static void
thr_prgr_wakeup(void *vssi)
{
    erts_sched_poke((ErtsSchedulerSleepInfo *) vssi);
}

static void
thr_prgr_prep_wait(void *vssi)
{
    ErtsSchedulerSleepInfo *ssi = (ErtsSchedulerSleepInfo *) vssi;
    erts_smp_atomic32_read_bor_acqb(&ssi->flags,
				    ERTS_SSI_FLG_SLEEPING);
}

static void
thr_prgr_wait(void *vssi)
{
    ErtsSchedulerSleepInfo *ssi = (ErtsSchedulerSleepInfo *) vssi;
    erts_aint32_t xflgs = ERTS_SSI_FLG_SLEEPING;

    erts_tse_reset(ssi->event);

    while (1) {
	erts_aint32_t aflgs, nflgs;
	nflgs = xflgs | ERTS_SSI_FLG_TSE_SLEEPING;
	aflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (aflgs == xflgs) {
	    erts_tse_wait(ssi->event);
	    break;
	}
	if ((aflgs & ERTS_SSI_FLG_SLEEPING) == 0)
	    break;
	xflgs = aflgs;
    }
}

static void
thr_prgr_fin_wait(void *vssi)
{
    ErtsSchedulerSleepInfo *ssi = (ErtsSchedulerSleepInfo *) vssi;
    erts_smp_atomic32_read_band_nob(&ssi->flags,
				    ~(ERTS_SSI_FLG_SLEEPING
				      | ERTS_SSI_FLG_TSE_SLEEPING));
}

static void init_aux_work_data(ErtsAuxWorkData *awdp, ErtsSchedulerData *esdp, char *dawwp);

void
erts_interupt_aux_thread_timed(ErtsMonotonicTime timeout_time)
{
    /* TODO only poke when needed (based on timeout_time) */
    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(-1));
}

static void *
aux_thread(void *unused)
{
    ErtsAuxWorkData *awdp = aux_thread_aux_work_data;
    ErtsSchedulerSleepInfo *ssi = ERTS_SCHED_SLEEP_INFO_IX(-1);
    erts_aint32_t aux_work;
    ErtsThrPrgrCallbacks callbacks;
    int thr_prgr_active = 1;

#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[] = "aux_thread";
	erts_lc_set_thread_name(buf);
    }
#endif

    ssi->event = erts_tse_fetch();

    erts_msacc_init_thread("aux", 1, 1);

    callbacks.arg = (void *) ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

    erts_thr_progress_register_managed_thread(NULL, &callbacks, 1);
    init_aux_work_data(awdp, NULL, NULL);
    awdp->ssi = ssi;


    sched_prep_spin_wait(ssi);

    while (1) {
	erts_aint32_t flgs;

	aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	if (aux_work) {
	    if (!thr_prgr_active)
		erts_thr_progress_active(NULL, thr_prgr_active = 1);
	    aux_work = handle_aux_work(awdp, aux_work, 1);
	    if (aux_work && erts_thr_progress_update(NULL))
		erts_thr_progress_leader_update(NULL);
	}

	if (!aux_work) {
	    if (thr_prgr_active)
		erts_thr_progress_active(NULL, thr_prgr_active = 0);
	    erts_thr_progress_prepare_wait(NULL);

	    flgs = sched_spin_wait(ssi, 0);

	    if (flgs & ERTS_SSI_FLG_SLEEPING) {
		ASSERT(flgs & ERTS_SSI_FLG_WAITING);
		flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_TSE_SLEEPING);
		if (flgs & ERTS_SSI_FLG_SLEEPING) {
		    int res;
		    ASSERT(flgs & ERTS_SSI_FLG_TSE_SLEEPING);
		    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
		    do {
			res = erts_tse_wait(ssi->event);
		    } while (res == EINTR);
		}
	    }
	    erts_thr_progress_finalize_wait(NULL);
	}

	flgs = sched_prep_spin_wait(ssi);
    }
    return NULL;
}

static void suspend_scheduler(ErtsSchedulerData *esdp);

#endif /* ERTS_SMP */

static void
scheduler_wait(int *fcalls, ErtsSchedulerData *esdp, ErtsRunQueue *rq)
{
    int working = 1;
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    int spincount;
    erts_aint32_t aux_work = 0;
#ifdef ERTS_SMP
    int thr_prgr_active = 1;
    erts_aint32_t flgs;
#endif
    ERTS_MSACC_PUSH_STATE_M();
#ifdef ERTS_SMP

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

#ifdef ERTS_DIRTY_SCHEDULERS
    if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix))
	erts_smp_spin_lock(&rq->sleepers.lock);
#endif
    flgs = sched_prep_spin_wait(ssi);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {
	/* Go suspend instead... */
#ifdef ERTS_DIRTY_SCHEDULERS
	if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix))
	    erts_smp_spin_unlock(&rq->sleepers.lock);
#endif
	return;
    }

#ifdef ERTS_DIRTY_SCHEDULERS
    if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix)) {
	ssi->prev = NULL;
	ssi->next = rq->sleepers.list;
	if (rq->sleepers.list)
	    rq->sleepers.list->prev = ssi;
	rq->sleepers.list = ssi;
	erts_smp_spin_unlock(&rq->sleepers.lock);
    }
#endif

    /*
     * If all schedulers are waiting, one of them *should*
     * be waiting in erl_sys_schedule()
     */

    if (ERTS_SCHEDULER_IS_DIRTY(esdp) || !prepare_for_sys_schedule(0)) {

	sched_waiting(esdp->no, rq);

	erts_smp_runq_unlock(rq);

	spincount = sched_busy_wait.tse;

    tse_wait:

	if (ERTS_SCHEDULER_IS_DIRTY(esdp))
	    dirty_sched_wall_time_change(esdp, working = 0);
        else if (thr_prgr_active != working)
	    sched_wall_time_change(esdp, working = thr_prgr_active);

	while (1) {
	    ErtsMonotonicTime current_time = 0;

	    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	    if (aux_work && !ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		if (!thr_prgr_active) {
		    erts_thr_progress_active(esdp, thr_prgr_active = 1);
		    sched_wall_time_change(esdp, 1);
		}
		aux_work = handle_aux_work(&esdp->aux_work_data, aux_work, 1);
                ERTS_MSACC_UPDATE_CACHE();
		if (aux_work && erts_thr_progress_update(esdp))
		    erts_thr_progress_leader_update(esdp);
	    }

	    if (aux_work) {
		if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		    flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
		    current_time = erts_get_monotonic_time(esdp);
		    if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref)) {
			if (!thr_prgr_active) {
			    erts_thr_progress_active(esdp, thr_prgr_active = 1);
			    sched_wall_time_change(esdp, 1);
			}
			erts_bump_timers(esdp->timer_wheel, current_time);
		    }
		}
	    }
	    else {
		ErtsMonotonicTime timeout_time;
		int do_timeout = 0;
		if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		    timeout_time = erts_check_next_timeout_time(esdp);
		    current_time = erts_get_monotonic_time(esdp);
		    do_timeout = (current_time >= timeout_time);
		} else {
		    current_time = 0;
		    timeout_time = ERTS_MONOTONIC_TIME_MAX;
		}
		if (do_timeout) {
		    if (!thr_prgr_active) {
			erts_thr_progress_active(esdp, thr_prgr_active = 1);
			sched_wall_time_change(esdp, 1);
		    }
		}
		else {
		    if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
			if (thr_prgr_active) {
			    erts_thr_progress_active(esdp, thr_prgr_active = 0);
			    sched_wall_time_change(esdp, 0);
			}
			erts_thr_progress_prepare_wait(esdp);
		    }

		    flgs = sched_spin_wait(ssi, spincount);
		    if (flgs & ERTS_SSI_FLG_SLEEPING) {
			ASSERT(flgs & ERTS_SSI_FLG_WAITING);
			flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_TSE_SLEEPING);
			if (flgs & ERTS_SSI_FLG_SLEEPING) {
			    int res;
			    ASSERT(flgs & ERTS_SSI_FLG_TSE_SLEEPING);
			    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
			    current_time = ERTS_SCHEDULER_IS_DIRTY(esdp) ? 0 :
				erts_get_monotonic_time(esdp);
			    do {
				Sint64 timeout;
				if (current_time >= timeout_time)
				    break;
				if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
				    timeout = ERTS_MONOTONIC_TO_NSEC(timeout_time
								     - current_time
								     - 1) + 1;
				} else
				    timeout = -1;
                                ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_SLEEP);
				res = erts_tse_twait(ssi->event, timeout);
                                ERTS_MSACC_POP_STATE_M();
				current_time = ERTS_SCHEDULER_IS_DIRTY(esdp) ? 0 :
				    erts_get_monotonic_time(esdp);
			    } while (res == EINTR);
			}
		    }
		    if (!ERTS_SCHEDULER_IS_DIRTY(esdp))
			erts_thr_progress_finalize_wait(esdp);
		}
		if (!ERTS_SCHEDULER_IS_DIRTY(esdp) && current_time >= timeout_time)
		    erts_bump_timers(esdp->timer_wheel, current_time);
	    }

	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		break;
	    }

	    flgs = sched_prep_cont_spin_wait(ssi);
	    spincount = sched_busy_wait.aux_work;

	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		break;
	    }

	}

	if (flgs & ~(ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC))
	    erts_smp_atomic32_read_band_nob(&ssi->flags,
                                            (ERTS_SSI_FLG_SUSPENDED
                                             | ERTS_SSI_FLG_MSB_EXEC));

	if (ERTS_SCHEDULER_IS_DIRTY(esdp))
	    dirty_sched_wall_time_change(esdp, working = 1);
        else if (!thr_prgr_active) {
	    erts_thr_progress_active(esdp, thr_prgr_active = 1);
	    sched_wall_time_change(esdp, 1);
        }

	erts_smp_runq_lock(rq);
	sched_active(esdp->no, rq);

    }
    else
#endif
    {

	erts_smp_atomic32_set_relb(&function_calls, 0);
	*fcalls = 0;

#ifdef ERTS_DIRTY_SCHEDULERS
	ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
#endif
	sched_waiting_sys(esdp->no, rq);

	erts_smp_runq_unlock(rq);

	ASSERT(working);
	sched_wall_time_change(esdp, working = 0);

	spincount = sched_busy_wait.sys_schedule;
	if (spincount == 0)
	    goto sys_aux_work;

	while (spincount-- > 0) {
	    ErtsMonotonicTime current_time;

	sys_poll_aux_work:

	    if (working)
		sched_wall_time_change(esdp, working = 0);

	    ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_CHECK_IO);

	    ASSERT(!erts_port_task_have_outstanding_io_tasks());
            LTTNG2(scheduler_poll, esdp->no, 1);
	    erl_sys_schedule(1); /* Might give us something to do */

	    ERTS_MSACC_POP_STATE_M();

	    if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		current_time = erts_get_monotonic_time(esdp);
		if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref))
		    erts_bump_timers(esdp->timer_wheel, current_time);
	    }

	sys_aux_work:
#ifndef ERTS_SMP
	    erts_sys_schedule_interrupt(0);
#endif

	    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	    if (aux_work && !ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		if (!working)
		    sched_wall_time_change(esdp, working = 1);
#ifdef ERTS_SMP
		if (!thr_prgr_active)
		    erts_thr_progress_active(esdp, thr_prgr_active = 1);
#endif
		aux_work = handle_aux_work(&esdp->aux_work_data, aux_work, 1);
                ERTS_MSACC_UPDATE_CACHE();
#ifdef ERTS_SMP
		if (aux_work && erts_thr_progress_update(esdp))
		    erts_thr_progress_leader_update(esdp);
#endif
	    }

#ifndef ERTS_SMP
	    if (erts_smp_atomic32_read_dirty(&rq->len) != 0 || rq->misc.start)
		goto sys_woken;
#else
	    flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		goto sys_woken;
	    }

	    /*
	     * If we got new I/O tasks we aren't allowed to
	     * call erl_sys_schedule() until it is handled.
	     */
	    if (erts_port_task_have_outstanding_io_tasks()) {
		clear_sys_scheduling();
		/*
		 * Got to check that we still got I/O tasks; otherwise
		 * we have to continue checking for I/O...
		 */
		if (!prepare_for_sys_schedule(0)) {
		    spincount *= ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT;
		    goto tse_wait;
		}
	    }
#endif
	}

	erts_smp_runq_lock(rq);

#ifdef ERTS_SMP
	/*
	 * If we got new I/O tasks we aren't allowed to
	 * sleep in erl_sys_schedule().
	 */
	if (erts_port_task_have_outstanding_io_tasks()) {
	    clear_sys_scheduling();

	    /*
	     * Got to check that we still got I/O tasks; otherwise
	     * we have to wait in erl_sys_schedule() after all...
	     */
	    if (!prepare_for_sys_schedule(0)) {
		/*
		 * Not allowed to wait in erl_sys_schedule;
		 * do tse wait instead...
		 */
		sched_change_waiting_sys_to_waiting(esdp->no, rq);
		erts_smp_runq_unlock(rq);
		spincount = 0;
		goto tse_wait;
	    }
	}
#endif
	if (aux_work) {
	    erts_smp_runq_unlock(rq);
	    goto sys_poll_aux_work;
	}
#ifdef ERTS_SMP
	flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_POLL_SLEEPING);
	if (!(flgs & ERTS_SSI_FLG_SLEEPING)) {
	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		goto sys_locked_woken;
	    }
	    erts_smp_runq_unlock(rq);
	    flgs = sched_prep_cont_spin_wait(ssi);
	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		goto sys_woken;
	    }
	    ASSERT(!erts_port_task_have_outstanding_io_tasks());
	    goto sys_poll_aux_work;
	}

	ASSERT(flgs & ERTS_SSI_FLG_POLL_SLEEPING);
	ASSERT(flgs & ERTS_SSI_FLG_WAITING);
#endif

	erts_smp_runq_unlock(rq);

	if (working)
	    sched_wall_time_change(esdp, working = 0);

#ifdef ERTS_SMP
	if (thr_prgr_active)
	    erts_thr_progress_active(esdp, thr_prgr_active = 0);
#endif

	ASSERT(!erts_port_task_have_outstanding_io_tasks());

	ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_CHECK_IO);
        LTTNG2(scheduler_poll, esdp->no, 0);

	erl_sys_schedule(0);

	ERTS_MSACC_POP_STATE_M();

	if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
	    ErtsMonotonicTime current_time = erts_get_monotonic_time(esdp);
	    if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref))
		erts_bump_timers(esdp->timer_wheel, current_time);
	}

#ifndef ERTS_SMP
	if (erts_smp_atomic32_read_dirty(&rq->len) == 0 && !rq->misc.start)
	    goto sys_aux_work;
    sys_woken:
#else
	flgs = sched_prep_cont_spin_wait(ssi);
	if (flgs & ERTS_SSI_FLG_WAITING)
	    goto sys_aux_work;

    sys_woken:
	if (!thr_prgr_active)
	    erts_thr_progress_active(esdp, thr_prgr_active = 1);
	erts_smp_runq_lock(rq);
    sys_locked_woken:
	if (!thr_prgr_active) {
	    erts_smp_runq_unlock(rq);
	    erts_thr_progress_active(esdp, thr_prgr_active = 1);
	    erts_smp_runq_lock(rq);
	}
	clear_sys_scheduling();
	if (flgs & ~(ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC))
	    erts_smp_atomic32_read_band_nob(&ssi->flags,
                                            (ERTS_SSI_FLG_SUSPENDED
                                             | ERTS_SSI_FLG_MSB_EXEC));
#endif
	if (!working)
	    sched_wall_time_change(esdp, working = 1);
	sched_active_sys(esdp->no, rq);
    }

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
}

#ifdef ERTS_SMP

static ERTS_INLINE erts_aint32_t
ssi_flags_set_wake(ErtsSchedulerSleepInfo *ssi)
{
    /* reset all flags but suspended */
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = 0;
    erts_aint32_t xflgs = ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING;
    while (1) {
	oflgs = erts_smp_atomic32_cmpxchg_relb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return oflgs;
	nflgs = oflgs & (ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC);
	xflgs = oflgs;
    }
}

static ERTS_INLINE void
ssi_wake(ErtsSchedulerSleepInfo *ssi)
{
    erts_sched_finish_poke(ssi, ssi_flags_set_wake(ssi));
}

#ifdef ERTS_DIRTY_SCHEDULERS

static void
dcpu_sched_ix_suspend_wake(Uint ix)
{
    ErtsSchedulerSleepInfo* ssi = ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix);
    erts_smp_atomic32_read_bor_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);
    ssi_wake(ssi);
}

static void
dio_sched_ix_suspend_wake(Uint ix)
{
    ErtsSchedulerSleepInfo* ssi = ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(ix);
    erts_smp_atomic32_read_bor_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);
    ssi_wake(ssi);
}

static void
dcpu_sched_ix_wake(Uint ix)
{
    ssi_wake(ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix));
}

#if 0
static void
dio_sched_ix_wake(Uint ix)
{
    ssi_wake(ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(ix));
}
#endif

#endif

static void
wake_scheduler(ErtsRunQueue *rq)
{
    /*
     * The unlocked run queue is not strictly necessary
     * from a thread safety or deadlock prevention
     * perspective. It will, however, cost us performance
     * if it is locked during wakup of another scheduler,
     * so all code *should* handle this without having
     * the lock on the run queue.
     */
    ERTS_SMP_LC_ASSERT(!erts_smp_lc_runq_is_locked(rq)
		       || ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    ssi_wake(rq->scheduler->ssi);
}

#ifdef ERTS_DIRTY_SCHEDULERS
static void
wake_dirty_schedulers(ErtsRunQueue *rq, int one)
{
    ErtsSchedulerSleepInfo *ssi;
    ErtsSchedulerSleepList *sl;

    ASSERT(ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    sl = &rq->sleepers;
    erts_smp_spin_lock(&sl->lock);
    ssi = sl->list;
    if (!ssi) {
	erts_smp_spin_unlock(&sl->lock);
	if (one)
	    wake_scheduler(rq);
    } else if (one) {
	erts_aint32_t flgs;
	if (ssi->prev)
	    ssi->prev->next = ssi->next;
	else {
	    ASSERT(sl->list == ssi);
	    sl->list = ssi->next;
	}
	if (ssi->next)
	    ssi->next->prev = ssi->prev;

	erts_smp_spin_unlock(&sl->lock);

	ERTS_THR_MEMORY_BARRIER;
	flgs = ssi_flags_set_wake(ssi);
	erts_sched_finish_poke(ssi, flgs);
    } else {
	sl->list = NULL;
	erts_smp_spin_unlock(&sl->lock);

	ERTS_THR_MEMORY_BARRIER;
	do {
	    ErtsSchedulerSleepInfo *wake_ssi = ssi;
	    ssi = ssi->next;
	    erts_sched_finish_poke(wake_ssi, ssi_flags_set_wake(wake_ssi));
	} while (ssi);
    }
}

static void
wake_dirty_scheduler(ErtsRunQueue *rq)
{
    wake_dirty_schedulers(rq, 1);
}

#endif

#define ERTS_NO_USED_RUNQS_SHIFT 16
#define ERTS_NO_RUNQS_MASK 0xffffU

#if ERTS_MAX_NO_OF_SCHEDULERS > ERTS_NO_RUNQS_MASK
#  error "Too large amount of schedulers allowed"
#endif

static ERTS_INLINE void
init_no_runqs(int active, int used)
{
    erts_aint32_t no_runqs = (erts_aint32_t) (active & ERTS_NO_RUNQS_MASK);
    no_runqs |= (erts_aint32_t) ((used & ERTS_NO_RUNQS_MASK) << ERTS_NO_USED_RUNQS_SHIFT);
    erts_smp_atomic32_init_nob(&balance_info.no_runqs, no_runqs);
}

static ERTS_INLINE void
get_no_runqs(int *active, int *used)
{
    erts_aint32_t no_runqs = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
    if (active)
	*active = (int) (no_runqs & ERTS_NO_RUNQS_MASK);
    if (used)
	*used = (int) ((no_runqs >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK);
}

static ERTS_INLINE void
set_no_used_runqs(int used)
{
    erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
    while (1) {
	erts_aint32_t act, new;
	new = (used & ERTS_NO_RUNQS_MASK) << ERTS_NO_USED_RUNQS_SHIFT;
	new |= exp & ERTS_NO_RUNQS_MASK;
	act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
	if (act == exp)
	    break;
	exp = act;
    }
}

static ERTS_INLINE void
set_no_active_runqs(int active)
{
    erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
    while (1) {
	erts_aint32_t act, new;
	if ((exp & ERTS_NO_RUNQS_MASK) == active)
	    break;
	new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
	new |= active & ERTS_NO_RUNQS_MASK;
	act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
	if (act == exp)
	    break;
	exp = act;
    }
}

static ERTS_INLINE int
try_inc_no_active_runqs(int active)
{
    erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
    if (((exp >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK) < active)
	return 0;
    if ((exp & ERTS_NO_RUNQS_MASK) + 1 == active) {
	erts_aint32_t new, act;
	new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
	new |= active & ERTS_NO_RUNQS_MASK;
	act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
	if (act == exp)
	    return 1;
    }
    return 0;
}

static ERTS_INLINE int
chk_wake_sched(ErtsRunQueue *crq, int ix, int activate)
{
    Uint32 flags;
    ErtsRunQueue *wrq;
    if (crq->ix == ix)
	return 0;
    wrq = ERTS_RUNQ_IX(ix);
    flags = ERTS_RUNQ_FLGS_GET(wrq);
    if (activate && !(flags & ERTS_RUNQ_FLG_SUSPENDED)) {
	if (try_inc_no_active_runqs(ix+1))
	    (void) ERTS_RUNQ_FLGS_UNSET(wrq, ERTS_RUNQ_FLG_INACTIVE);
    }
    if (!(flags & (ERTS_RUNQ_FLG_SUSPENDED|ERTS_RUNQ_FLG_NONEMPTY))) {
	wake_scheduler(wrq);
	return 1;
    }
    return 0;
}

static void
wake_scheduler_on_empty_runq(ErtsRunQueue *crq)
{
    int ix = crq->ix;
    int stop_ix = ix;
    int active_ix, balance_ix;

    get_no_runqs(&active_ix, &balance_ix);

    if (active_ix > balance_ix)
	active_ix = balance_ix;

    if (ix >= active_ix)
	stop_ix = ix = active_ix;

    /* Try to wake a scheduler on an active run queue */
    while (1) {
	ix--;
	if (ix < 0) {
	    if (active_ix == stop_ix)
		break;
	    ix = active_ix - 1;
	}
	if (ix == stop_ix)
	    break;
	if (chk_wake_sched(crq, ix, 0))
	    return;
    }

    if (active_ix < balance_ix) {
	/* Try to activate a new run queue and wake its scheduler */
	(void) chk_wake_sched(crq, active_ix, 1);
    }
}

#endif /* ERTS_SMP */

static ERTS_INLINE void
smp_notify_inc_runq(ErtsRunQueue *runq)
{
#ifdef ERTS_SMP
    if (runq) {
#ifdef ERTS_DIRTY_SCHEDULERS
	if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
	    wake_dirty_scheduler(runq);
	else
#endif
	    wake_scheduler(runq);
    }
#endif
}

void
erts_smp_notify_inc_runq(ErtsRunQueue *runq)
{
    smp_notify_inc_runq(runq);
}

void
erts_sched_notify_check_cpu_bind(void)
{
#ifdef ERTS_SMP
    int ix;
    for (ix = 0; ix < erts_no_run_queues; ix++) {
	ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
	(void) ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_CHK_CPU_BIND);
	wake_scheduler(rq);
    }
#else
    erts_sched_check_cpu_bind(erts_get_scheduler_data());
#endif
}


static ERTS_INLINE void
enqueue_process(ErtsRunQueue *runq, int prio, Process *p)
{
    ErtsRunPrioQueue *rpq;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    erts_smp_inc_runq_len(runq, &runq->procs.prio_info[prio], prio);

    if (prio == PRIORITY_LOW) {
	p->schedule_count = RESCHEDULE_LOW;
	rpq = &runq->procs.prio[PRIORITY_NORMAL];
    }
    else {
	p->schedule_count = 1;
	rpq = &runq->procs.prio[prio];
    }

    p->next = NULL;
    if (rpq->last)
	rpq->last->next = p;
    else
	rpq->first = p;
    rpq->last = p;
}


static ERTS_INLINE void
unqueue_process(ErtsRunQueue *runq,
		ErtsRunPrioQueue *rpq,
		ErtsRunQueueInfo *rqi,
		int prio,
		Process *prev_proc,
		Process *proc)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    if (prev_proc)
	prev_proc->next = proc->next;
    else
	rpq->first = proc->next;
    if (!proc->next)
	rpq->last = prev_proc;

    if (!rpq->first)
	rpq->last = NULL;

    erts_smp_dec_runq_len(runq, rqi, prio);
}


static ERTS_INLINE Process *
dequeue_process(ErtsRunQueue *runq, int prio_q, erts_aint32_t *statep)
{
    erts_aint32_t state;
    int prio;
    ErtsRunPrioQueue *rpq;
    ErtsRunQueueInfo *rqi;
    Process *p;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    ASSERT(PRIORITY_NORMAL == prio_q
	   || PRIORITY_HIGH == prio_q
	   || PRIORITY_MAX == prio_q);

    rpq = &runq->procs.prio[prio_q];
    p = rpq->first;
    if (!p)
	return NULL;

    ERTS_SMP_DATA_DEPENDENCY_READ_MEMORY_BARRIER;

    state = erts_smp_atomic32_read_nob(&p->state);
    if (statep)
	*statep = state;

    prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);

    rqi = &runq->procs.prio_info[prio];

    if (p)
	unqueue_process(runq, rpq, rqi, prio, NULL, p);

    return p;
}

static ERTS_INLINE int
check_requeue_process(ErtsRunQueue *rq, int prio_q)
{
    ErtsRunPrioQueue *rpq = &rq->procs.prio[prio_q];
    Process *p = rpq->first;
    if (--p->schedule_count > 0 && p != rpq->last) {
	/* reschedule */
	rpq->first = p->next;
	rpq->last->next = p;
	rpq->last = p;
	p->next = NULL;
	return 1;
    }
    return 0;
}

static ERTS_INLINE void
free_proxy_proc(Process *proxy)
{
    ASSERT(erts_smp_atomic32_read_nob(&proxy->state) & ERTS_PSFLG_PROXY);
    erts_free(ERTS_ALC_T_PROC, proxy);
}

#ifdef ERTS_SMP

static ErtsRunQueue *
check_immigration_need(ErtsRunQueue *c_rq, ErtsMigrationPath *mp, int prio)
{
    int len;
    Uint32 f_flags, f_rq_flags;
    ErtsRunQueue *f_rq;

    f_flags = mp->prio[prio].flags;

    ASSERT(ERTS_CHK_RUNQ_FLG_IMMIGRATE(mp->flags, prio));

    f_rq = mp->prio[prio].runq;
    if (!f_rq)
	return NULL;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    if (mp->sched_util)
	return NULL;
#endif

    f_rq_flags = ERTS_RUNQ_FLGS_GET(f_rq);
    if (f_rq_flags & ERTS_RUNQ_FLG_PROTECTED)
	return NULL;

    if (ERTS_CHK_RUNQ_FLG_EVACUATE(f_flags, prio))
	return f_rq;

    if (f_rq_flags & ERTS_RUNQ_FLG_INACTIVE)
	return f_rq;

    if (prio == ERTS_PORT_PRIO_LEVEL)
	len = RUNQ_READ_LEN(&c_rq->ports.info.len);
    else
	len = RUNQ_READ_LEN(&c_rq->procs.prio_info[prio].len);

    if (len < mp->prio[prio].limit.this) {
	if (prio == ERTS_PORT_PRIO_LEVEL)
	    len = RUNQ_READ_LEN(&f_rq->ports.info.len);
	else
	    len = RUNQ_READ_LEN(&f_rq->procs.prio_info[prio].len);

	if (len > mp->prio[prio].limit.other)
	    return f_rq;
    }
    return NULL;
}

static void
immigrate(ErtsRunQueue *c_rq, ErtsMigrationPath *mp)
{
    Uint32 iflags, iflag;
    erts_smp_runq_unlock(c_rq);

    ASSERT(erts_thr_progress_is_managed_thread());

    iflags = mp->flags & ERTS_RUNQ_FLGS_IMMIGRATE_QMASK;

    iflag = iflags & -iflags;

    while (iflag) {
	ErtsRunQueue *rq;
	int prio;

	switch (iflag) {
	case (MAX_BIT << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT):
	    prio = PRIORITY_MAX;
	    break;
	case (HIGH_BIT << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT):
	    prio = PRIORITY_HIGH;
	    break;
	case (NORMAL_BIT << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT):
	    prio = PRIORITY_NORMAL;
	    break;
	case (LOW_BIT << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT):
	    prio = PRIORITY_LOW;
	    break;
	case (PORT_BIT << ERTS_RUNQ_FLGS_IMMIGRATE_SHFT):
	    prio = ERTS_PORT_PRIO_LEVEL;
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT,
		     "%s:%d:%s(): Invalid immigrate queue mask",
		     __FILE__, __LINE__, __func__);
	    prio = 0;
	    break;
	}

	iflags &= ~iflag;
	iflag = iflags & -iflags;

	rq = check_immigration_need(c_rq, mp, prio);
	if (rq) {
	    erts_smp_runq_lock(rq);
	    if (prio == ERTS_PORT_PRIO_LEVEL) {
		Port *prt;
		prt = erts_dequeue_port(rq);
		if (prt)
		    RUNQ_SET_RQ(&prt->run_queue, c_rq);
		erts_smp_runq_unlock(rq);
		if (prt) {
		    /* port might terminate while we have no lock... */
		    rq = erts_port_runq(prt);
		    if (rq) {
			if (rq != c_rq)
			    erts_exit(ERTS_ABORT_EXIT,
				     "%s:%d:%s(): Internal error",
				     __FILE__, __LINE__, __func__);
			erts_enqueue_port(c_rq, prt);
			if (!iflag)
			    return; /* done */
			erts_smp_runq_unlock(c_rq);
		    }
		}
	    }
	    else {
		ErtsRunPrioQueue *rpq = &rq->procs.prio[prio == PRIORITY_LOW
							? PRIORITY_NORMAL
							: prio];
		Process *prev_proc = NULL;
		Process *proc = rpq->first;
		int rq_locked = 1;

		while (proc) {
		    erts_aint32_t state;
		    state = erts_smp_atomic32_read_acqb(&proc->state);
		    if (!(ERTS_PSFLG_BOUND & state)
			&& (prio == (int) ERTS_PSFLGS_GET_PRQ_PRIO(state))) {
			ErtsRunQueueInfo *rqi = &rq->procs.prio_info[prio];
			unqueue_process(rq, rpq, rqi, prio, prev_proc, proc);
			erts_smp_runq_unlock(rq);
			RUNQ_SET_RQ(&proc->run_queue, c_rq);
			rq_locked = 0;

			erts_smp_runq_lock(c_rq);
			enqueue_process(c_rq, prio, proc);
			if (!iflag)
			    return; /* done */
			erts_smp_runq_unlock(c_rq);
			break;
		    }
		    prev_proc = proc;
		    proc = proc->next;
		}
		if (rq_locked)
		    erts_smp_runq_unlock(rq);
	    }
	}
    }

    erts_smp_runq_lock(c_rq);
}

static ERTS_INLINE void
suspend_run_queue(ErtsRunQueue *rq)
{
    erts_smp_atomic32_read_bor_nob(&rq->scheduler->ssi->flags,
				   ERTS_SSI_FLG_SUSPENDED);
    (void) ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_SUSPENDED);

    wake_scheduler(rq);
}

static void nrml_sched_ix_resume_wake(Uint ix);

static ERTS_INLINE void
resume_run_queue(ErtsRunQueue *rq)
{
    int pix;
    Uint32 oflgs;

    ASSERT(!ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    erts_smp_runq_lock(rq);

    oflgs = ERTS_RUNQ_FLGS_READ_BSET(rq,
				     (ERTS_RUNQ_FLG_OUT_OF_WORK
				      | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK
				      | ERTS_RUNQ_FLG_SUSPENDED
                                      | ERTS_RUNQ_FLG_MSB_EXEC),
				     (ERTS_RUNQ_FLG_OUT_OF_WORK
				      | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK));

    if (oflgs & (ERTS_RUNQ_FLG_SUSPENDED|ERTS_RUNQ_FLG_MSB_EXEC)) {
	erts_aint32_t len;

	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
	    len = erts_smp_atomic32_read_dirty(&rq->procs.prio_info[pix].len);
	    rq->procs.prio_info[pix].max_len = len;
	    rq->procs.prio_info[pix].reds = 0;
	}
	len = erts_smp_atomic32_read_dirty(&rq->ports.info.len);
	rq->ports.info.max_len = len;
	rq->ports.info.reds = 0;
	len = erts_smp_atomic32_read_dirty(&rq->len);
	rq->max_len = len;

    }

    erts_smp_runq_unlock(rq);

    nrml_sched_ix_resume_wake(rq->ix);
}

typedef struct {
    Process *first;
    Process *last;
} ErtsStuckBoundProcesses;

static void
schedule_bound_processes(ErtsRunQueue *rq,
			 ErtsStuckBoundProcesses *sbpp)
{
    Process *proc, *next;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

    proc = sbpp->first;
    while (proc) {
	erts_aint32_t state = erts_smp_atomic32_read_acqb(&proc->state);
	next = proc->next;
	enqueue_process(rq, (int) ERTS_PSFLGS_GET_PRQ_PRIO(state), proc);
	proc = next;
    }
}

#ifdef ERTS_DIRTY_SCHEDULERS

static ERTS_INLINE void
clear_proc_dirty_queue_bit(Process *p, ErtsRunQueue *rq, int prio_bit)
{
#ifdef DEBUG
    erts_aint32_t old;
#endif
    erts_aint32_t qb = prio_bit;
    if (rq == ERTS_DIRTY_CPU_RUNQ)
	qb <<= ERTS_PDSFLGS_IN_CPU_PRQ_MASK_OFFSET;
    else {
	ASSERT(rq == ERTS_DIRTY_IO_RUNQ);
	qb <<= ERTS_PDSFLGS_IN_IO_PRQ_MASK_OFFSET;
    }
#ifdef DEBUG
    old = (int)
#else
	(void)
#endif
	erts_smp_atomic32_read_band_mb(&p->dirty_state, ~qb);
    ASSERT(old & qb);
}

#endif /* ERTS_DIRTY_SCHEDULERS */


static void
evacuate_run_queue(ErtsRunQueue *rq,
		   ErtsStuckBoundProcesses *sbpp)
{
    int prio_q;
    ErtsRunQueue *to_rq;
    ErtsMigrationPaths *mps;
    ErtsMigrationPath *mp;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

    (void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);

    ASSERT(!ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    mps = erts_get_migration_paths_managed();
    mp = &mps->mpath[rq->ix];

    /* Evacuate scheduled misc ops */

    if (rq->misc.start) {
	ErtsMiscOpList *start, *end;

	to_rq = mp->misc_evac_runq;
	if (!to_rq)
	    return;

	start = rq->misc.start;
	end = rq->misc.end;
	rq->misc.start = NULL;
	rq->misc.end = NULL;
        ERTS_RUNQ_FLGS_UNSET_NOB(rq, ERTS_RUNQ_FLG_MISC_OP);
	erts_smp_runq_unlock(rq);

	erts_smp_runq_lock(to_rq);
	if (to_rq->misc.end)
	    to_rq->misc.end->next = start;
	else
	    to_rq->misc.start = start;

	to_rq->misc.end = end;

	non_empty_runq(to_rq);

	erts_smp_runq_unlock(to_rq);
	smp_notify_inc_runq(to_rq);
	erts_smp_runq_lock(to_rq);
    }

    if (rq->ports.start) {
	Port *prt;

	to_rq = mp->prio[ERTS_PORT_PRIO_LEVEL].runq;
	if (!to_rq)
	    return;

	/* Evacuate scheduled ports */
	prt = rq->ports.start;
	while (prt) {
	    ErtsRunQueue *prt_rq;
	    prt = erts_dequeue_port(rq);
	    RUNQ_SET_RQ(&prt->run_queue, to_rq);
	    erts_smp_runq_unlock(rq);
	    /*
	     * The port might terminate while
	     * we have no lock on it...
	     */
	    prt_rq = erts_port_runq(prt);
	    if (prt_rq) {
		if (prt_rq != to_rq)
		    erts_exit(ERTS_ABORT_EXIT,
			     "%s:%d:%s() internal error\n",
			     __FILE__, __LINE__, __func__);
		erts_enqueue_port(to_rq, prt);
		erts_smp_runq_unlock(to_rq);
	    }
	    erts_smp_runq_lock(rq);
	    prt = rq->ports.start;
	}
	smp_notify_inc_runq(to_rq);
    }

    /* Evacuate scheduled processes */
    for (prio_q = 0; prio_q < ERTS_NO_PROC_PRIO_QUEUES; prio_q++) {
	erts_aint32_t state;
	Process *proc;
	int notify = 0;
	to_rq = NULL;

        if (!mp->prio[prio_q].runq)
            return;
        if (prio_q == PRIORITY_NORMAL && !mp->prio[PRIORITY_LOW].runq)
            return;

	proc = dequeue_process(rq, prio_q, &state);
	while (proc) {
	    Process *real_proc;
	    int prio;
	    erts_aint32_t max_qbit, qbit, real_state;

	    prio = ERTS_PSFLGS_GET_PRQ_PRIO(state);
	    qbit = ((erts_aint32_t) 1) << prio;

	    if (!(state & ERTS_PSFLG_PROXY)) {
		real_proc = proc;
		real_state = state;
	    }
	    else {
		real_proc = erts_proc_lookup_raw(proc->common.id);
		if (!real_proc) {
		    free_proxy_proc(proc);
		    goto handle_next_proc;
		}
		real_state =  erts_smp_atomic32_read_acqb(&real_proc->state);
	    }

	    max_qbit = (state >> ERTS_PSFLGS_IN_PRQ_MASK_OFFSET);
	    max_qbit &= ERTS_PSFLGS_QMASK;
	    max_qbit |= 1 << ERTS_PSFLGS_QMASK_BITS;
	    max_qbit &= -max_qbit;

	    if (qbit > max_qbit) {
		/* Process already queued with higher prio; drop it... */
		if (real_proc != proc)
		    free_proxy_proc(proc);
		else {
		    erts_aint32_t clr_bits;
#ifdef DEBUG
		    erts_aint32_t old;
#endif

		    clr_bits = ERTS_PSFLG_IN_RUNQ;
		    clr_bits |= qbit << ERTS_PSFLGS_IN_PRQ_MASK_OFFSET;

#ifdef DEBUG
		    old =
#else
			(void)
#endif
			erts_smp_atomic32_read_band_mb(&proc->state,
						       ~clr_bits);
			ASSERT((old & clr_bits) == clr_bits);

		}

		goto handle_next_proc;
	    }

	    if (ERTS_PSFLG_BOUND & real_state) {
		/* Bound processes get stuck here... */
		proc->next = NULL;
		if (sbpp->last)
		    sbpp->last->next = proc;
		else
		    sbpp->first = proc;
		sbpp->last = proc;
	    }
	    else {
		int prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);
		erts_smp_runq_unlock(rq);

                to_rq = mp->prio[prio].runq;
		RUNQ_SET_RQ(&proc->run_queue, to_rq);

		erts_smp_runq_lock(to_rq);
		enqueue_process(to_rq, prio, proc);
		erts_smp_runq_unlock(to_rq);
		notify = 1;

		erts_smp_runq_lock(rq);
	    }

	handle_next_proc:
	    proc = dequeue_process(rq, prio_q, &state);
	}
	if (notify)
	    smp_notify_inc_runq(to_rq);
    }
}

static int
try_steal_task_from_victim(ErtsRunQueue *rq, int *rq_lockedp, ErtsRunQueue *vrq, Uint32 flags)
{
    Uint32 procs_qmask = flags & ERTS_RUNQ_FLGS_PROCS_QMASK;
    int max_prio_bit;
    ErtsRunPrioQueue *rpq;

    if (*rq_lockedp) {
	erts_smp_runq_unlock(rq);
	*rq_lockedp = 0;
    }

    ERTS_SMP_LC_ASSERT(!erts_smp_lc_runq_is_locked(rq));

    erts_smp_runq_lock(vrq);

    if (ERTS_RUNQ_FLGS_GET_NOB(rq) & ERTS_RUNQ_FLG_HALTING)
	goto no_procs;

    /*
     * Check for a runnable process to steal...
     */

    while (procs_qmask) {
	Process *prev_proc;
	Process *proc;

	max_prio_bit = procs_qmask & -procs_qmask;
	switch (max_prio_bit) {
	case MAX_BIT:
	    rpq = &vrq->procs.prio[PRIORITY_MAX];
	    break;
	case HIGH_BIT:
	    rpq = &vrq->procs.prio[PRIORITY_HIGH];
	    break;
	case NORMAL_BIT:
	case LOW_BIT:
	    rpq = &vrq->procs.prio[PRIORITY_NORMAL];
	    break;
	case 0:
	    goto no_procs;
	default:
	    ASSERT(!"Invalid queue mask");
	    goto no_procs;
	}

	prev_proc = NULL;
	proc = rpq->first;

	while (proc) {
	    erts_aint32_t state = erts_smp_atomic32_read_acqb(&proc->state);
	    if (!(ERTS_PSFLG_BOUND & state)) {
		/* Steal process */
		int prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);
		ErtsRunQueueInfo *rqi = &vrq->procs.prio_info[prio];
		unqueue_process(vrq, rpq, rqi, prio, prev_proc, proc);
		erts_smp_runq_unlock(vrq);
		RUNQ_SET_RQ(&proc->run_queue, rq);

		erts_smp_runq_lock(rq);
		*rq_lockedp = 1;
		enqueue_process(rq, prio, proc);
		return !0;
	    }
	    prev_proc = proc;
	    proc = proc->next;
	}

	procs_qmask &= ~max_prio_bit;
    }

no_procs:

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(vrq));

    /*
     * Check for a runnable port to steal...
     */

    if (vrq->ports.start) {
	ErtsRunQueue *prt_rq;
	Port *prt = erts_dequeue_port(vrq);
	RUNQ_SET_RQ(&prt->run_queue, rq);
	erts_smp_runq_unlock(vrq);

	/*
	 * The port might terminate while
	 * we have no lock on it...
	 */

	prt_rq = erts_port_runq(prt);
	if (!prt_rq)
	    return 0;
	else {
	    if (prt_rq != rq)
		erts_exit(ERTS_ABORT_EXIT,
			 "%s:%d:%s() internal error\n",
			 __FILE__, __LINE__, __func__);
	    *rq_lockedp = 1;
	    erts_enqueue_port(rq, prt);
	    return !0;
	}
    }

    erts_smp_runq_unlock(vrq);

    return 0;
}


static ERTS_INLINE int
check_possible_steal_victim(ErtsRunQueue *rq, int *rq_lockedp, int vix)
{
    ErtsRunQueue *vrq = ERTS_RUNQ_IX(vix);
    Uint32 flags = ERTS_RUNQ_FLGS_GET(vrq);
    if (runq_got_work_to_execute_flags(flags) & (!(flags & ERTS_RUNQ_FLG_PROTECTED)))
	return try_steal_task_from_victim(rq, rq_lockedp, vrq, flags);
    else
	return 0;
}


static int
try_steal_task(ErtsRunQueue *rq)
{
    int res, rq_locked, vix, active_rqs, blnc_rqs;
    Uint32 flags;

    /* Protect jobs we steal from getting stolen from us... */
    flags = empty_protected_runq(rq);
    if (flags & ERTS_RUNQ_FLG_SUSPENDED)
	return 0; /* go suspend instead... */

    res = 0;
    rq_locked = 1;

    ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, rq_locked);

    get_no_runqs(&active_rqs, &blnc_rqs);

    if (active_rqs > blnc_rqs)
	active_rqs = blnc_rqs;

    if (rq->ix < active_rqs) {

	/* First try to steal from an inactive run queue... */
	if (active_rqs < blnc_rqs) {
	    int no = blnc_rqs - active_rqs;
	    int stop_ix = vix = active_rqs + rq->ix % no;
	    while (erts_smp_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
		res = check_possible_steal_victim(rq, &rq_locked, vix);
		if (res)
		    goto done;
		vix++;
		if (vix >= blnc_rqs)
		    vix = active_rqs;
		if (vix == stop_ix)
		    break;
	    }
	}

	vix = rq->ix;

	/* ... then try to steal a job from another active queue... */
	while (erts_smp_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
	    vix++;
	    if (vix >= active_rqs)
		vix = 0;
	    if (vix == rq->ix)
		break;

	    res = check_possible_steal_victim(rq, &rq_locked, vix);
	    if (res)
		goto done;
	}

    }

 done:

    if (!rq_locked)
	erts_smp_runq_lock(rq);

    if (res)
        return res;
    return runq_got_work_to_execute(rq);
}

/* Run queue balancing */

typedef struct {
    Uint32 flags;
    struct {
	int max_len;
	int avail;
	int reds;
	int migration_limit;
	int emigrate_to;
	int immigrate_from;
    } prio[ERTS_NO_PRIO_LEVELS];
    int reds;
    int full_reds;
    int full_reds_history_sum;
    int full_reds_history_change;
    int oowc;
    int max_len;
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    int sched_util;
#endif
} ErtsRunQueueBalance;
static ErtsRunQueueBalance *run_queue_info;

typedef struct {
    int qix;
    int len;
} ErtsRunQueueCompare;
static ErtsRunQueueCompare *run_queue_compare;

static int
rqc_len_cmp(const void *x, const void *y)
{
    return ((ErtsRunQueueCompare *) x)->len - ((ErtsRunQueueCompare *) y)->len;
}

#define ERTS_PERCENT(X, Y) \
  ((Y) == 0 \
   ? ((X) == 0 ? 100 : INT_MAX) \
   : ((100*(X))/(Y)))

#define ERTS_UPDATE_FULL_REDS(QIX, LAST_REDS)				\
do {									\
    run_queue_info[(QIX)].full_reds					\
	= run_queue_info[(QIX)].full_reds_history_sum;			\
    run_queue_info[(QIX)].full_reds += (LAST_REDS);			\
    run_queue_info[(QIX)].full_reds					\
	>>= ERTS_FULL_REDS_HISTORY_AVG_SHFT;				\
    run_queue_info[(QIX)].full_reds_history_sum				\
	-= run_queue_info[(QIX)].full_reds_history_change;		\
    run_queue_info[(QIX)].full_reds_history_sum += (LAST_REDS);		\
    run_queue_info[(QIX)].full_reds_history_change = (LAST_REDS);	\
} while (0)

#define ERTS_DBG_CHK_FULL_REDS_HISTORY(RQ)				\
do {									\
    int sum__ = 0;							\
    int rix__;								\
    for (rix__ = 0; rix__ < ERTS_FULL_REDS_HISTORY_SIZE; rix__++)	\
	sum__ += (RQ)->full_reds_history[rix__];			\
    ASSERT(sum__ == (RQ)->full_reds_history_sum);			\
} while (0);

#define ERTS_PRE_ALLOCED_MPATHS 8

erts_atomic_t erts_migration_paths;

static struct {
    size_t size;
    ErtsMigrationPaths *freelist;
    struct {
	ErtsMigrationPaths *first;
	ErtsMigrationPaths *last;
    } retired;
} mpaths;

static void
init_migration_paths(void)
{
    int qix, i;
    char *p;
    ErtsMigrationPaths *mps;

    mpaths.size = sizeof(ErtsMigrationPaths);
    mpaths.size += sizeof(ErtsMigrationPath)*(erts_no_schedulers-1);
    mpaths.size = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(mpaths.size);

    p = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_LL_MPATHS,
					   (mpaths.size
					    * ERTS_PRE_ALLOCED_MPATHS));
    mpaths.freelist = NULL;
    for (i = 0; i < ERTS_PRE_ALLOCED_MPATHS-1; i++) {
	mps = (ErtsMigrationPaths *) p;
	mps->next = mpaths.freelist;
	mpaths.freelist = mps;
	p += mpaths.size;
    }

    mps = (ErtsMigrationPaths *) p;
    mps->block = NULL;
    for (qix = 0; qix < erts_no_run_queues; qix++) {
	int pix;
	mps->mpath[qix].flags = 0;
	mps->mpath[qix].misc_evac_runq = NULL;
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    mps->mpath[qix].prio[pix].limit.this = -1;
	    mps->mpath[qix].prio[pix].limit.other = -1;
	    mps->mpath[qix].prio[pix].runq = NULL;
	    mps->mpath[qix].prio[pix].flags = 0;
	}
    }
    erts_atomic_init_wb(&erts_migration_paths, (erts_aint_t) mps);
}

static ERTS_INLINE ErtsMigrationPaths *
alloc_mpaths(void)
{
    void *block;
    ErtsMigrationPaths *res;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&balance_info.update_mtx));

    res = mpaths.freelist;
    if (res) {
	mpaths.freelist = res->next;
	res->block = NULL;
	return res;
    }
    res = erts_alloc(ERTS_ALC_T_SL_MPATHS,
		     mpaths.size+ERTS_CACHE_LINE_SIZE);
    block = (void *) res;
    if (((UWord) res) & ERTS_CACHE_LINE_MASK)
	res = (ErtsMigrationPaths *) ((((UWord) res) & ~ERTS_CACHE_LINE_MASK)
				      + ERTS_CACHE_LINE_SIZE);
    res->block = block;
    return res;
}

static ERTS_INLINE void
retire_mpaths(ErtsMigrationPaths *mps)
{
    ErtsThrPrgrVal current;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&balance_info.update_mtx));

    current = erts_thr_progress_current();

    while (mpaths.retired.first) {
	ErtsMigrationPaths *tmp = mpaths.retired.first;
	if (!erts_thr_progress_has_reached_this(current, tmp->thr_prgr))
	    break;
	mpaths.retired.first = tmp->next;
	if (tmp->block) {
	    erts_free(ERTS_ALC_T_SL_MPATHS, tmp->block);
	}
	else {
	    tmp->next = mpaths.freelist;
	    mpaths.freelist = tmp;
	}
    }

    if (!mpaths.retired.first)
	mpaths.retired.last = NULL;

    mps->thr_prgr = erts_thr_progress_later(NULL);
    mps->next = NULL;

    if (mpaths.retired.last)
	mpaths.retired.last->next = mps;
    else
	mpaths.retired.first = mps;
    mpaths.retired.last = mps;
}

static void
check_balance(ErtsRunQueue *c_rq)
{
#if ERTS_MAX_PROCESSES >= (1 << 27)
#  error check_balance() assumes ERTS_MAX_PROCESS < (1 << 27)
#endif
    ErtsMigrationPaths *new_mpaths, *old_mpaths;
    ErtsRunQueueBalance avg = {0};
    Sint64 scheds_reds, full_scheds_reds;
    int forced, active, current_active, oowc, half_full_scheds, full_scheds,
	mmax_len, blnc_no_rqs, qix, pix, freds_hist_ix;
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    int sched_util_balancing;
#endif

    if (erts_smp_atomic32_xchg_nob(&balance_info.checking_balance, 1)) {
	c_rq->check_balance_reds = INT_MAX;
	return;
    }

    get_no_runqs(NULL, &blnc_no_rqs);
    if (blnc_no_rqs == 1) {
	c_rq->check_balance_reds = INT_MAX;
	erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
	return;
    }

    erts_smp_runq_unlock(c_rq);

    if (balance_info.halftime) {	
	balance_info.halftime = 0;
	erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
	ERTS_FOREACH_RUNQ(rq,
	{
	    if (rq->waiting)
		(void) ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
	    else
		(void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
	    rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	});

	erts_smp_runq_lock(c_rq);
	return;
    }

    /*
     * check_balance() is never called in more threads
     * than one at a time, i.e., we will normally never
     * get any conflicts on the balance_info.update_mtx.
     * However, when blocking multi scheduling (which performance
     * critical applications do *not* do) migration information
     * is manipulated. Such updates of the migration information
     * might clash with balancing.
     */
    erts_smp_mtx_lock(&balance_info.update_mtx);

    forced = balance_info.forced_check_balance;
    balance_info.forced_check_balance = 0;

    get_no_runqs(&current_active, &blnc_no_rqs);

    if (blnc_no_rqs == 1) {
	erts_smp_mtx_unlock(&balance_info.update_mtx);
	erts_smp_runq_lock(c_rq);
	c_rq->check_balance_reds = INT_MAX;
	erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
	return;
    }

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    sched_util_balancing = 0;
#endif

    freds_hist_ix = balance_info.full_reds_history_index;
    balance_info.full_reds_history_index++;
    if (balance_info.full_reds_history_index >= ERTS_FULL_REDS_HISTORY_SIZE)
	balance_info.full_reds_history_index = 0;

    /* Read balance information for all run queues */
    for (qix = 0; qix < blnc_no_rqs; qix++) {
	ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);
	erts_smp_runq_lock(rq);

	run_queue_info[qix].flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
	for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
	    run_queue_info[qix].prio[pix].max_len
		= rq->procs.prio_info[pix].max_len;
	    run_queue_info[qix].prio[pix].reds
		= rq->procs.prio_info[pix].reds;
	}
	run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].max_len
	    = rq->ports.info.max_len;
	run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].reds
	    = rq->ports.info.reds;

	run_queue_info[qix].full_reds_history_sum
	    = rq->full_reds_history_sum;
	run_queue_info[qix].full_reds_history_change
	    = rq->full_reds_history[freds_hist_ix];

	run_queue_info[qix].oowc = rq->out_of_work_count;
	run_queue_info[qix].max_len = rq->max_len;
	rq->check_balance_reds = INT_MAX;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	if (erts_sched_balance_util)
	    run_queue_info[qix].sched_util = erts_get_sched_util(rq, 1, 0);
#endif

	erts_smp_runq_unlock(rq);
    }

    full_scheds = 0;
    half_full_scheds = 0;
    full_scheds_reds = 0;
    scheds_reds = 0;
    oowc = 0;
    mmax_len = 0;

    /* Calculate availability for each priority in each run queues */
    for (qix = 0; qix < blnc_no_rqs; qix++) {
	int treds = 0;

	if (run_queue_info[qix].flags & ERTS_RUNQ_FLG_OUT_OF_WORK) {
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		run_queue_info[qix].prio[pix].avail = 100;
		treds += run_queue_info[qix].prio[pix].reds;
	    }
	    if (!(run_queue_info[qix].flags & ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK))
		half_full_scheds++;
	    ERTS_UPDATE_FULL_REDS(qix, ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED);
	}
	else {
	    ASSERT(!(run_queue_info[qix].flags & ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK));
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++)
		treds += run_queue_info[qix].prio[pix].reds;
	    if (treds == 0) {
		for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++)
		    run_queue_info[qix].prio[pix].avail = 0;
	    }
	    else {
		Sint64 xreds = 0;
		Sint64 procreds = treds;
		procreds -= 
		    ((Sint64)
		     run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].reds);

		for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
		    Sint64 av;

		    if (xreds == 0)
			av = 100;
		    else if (procreds == xreds)
			av = 0;
		    else {
			av = (100*(procreds - xreds)) / procreds;
			if (av == 0)
			    av = 1;
		    }
		    run_queue_info[qix].prio[pix].avail = (int) av;
		    ASSERT(run_queue_info[qix].prio[pix].avail >= 0);
		    if (pix < PRIORITY_NORMAL) /* ie., max or high */
			xreds += (Sint64) run_queue_info[qix].prio[pix].reds;
		}
		run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].avail = 100;
	    }
	    ERTS_UPDATE_FULL_REDS(qix, treds);
	    full_scheds_reds += run_queue_info[qix].full_reds;
	    full_scheds++;
	    half_full_scheds++;
	}
	run_queue_info[qix].reds = treds;
	scheds_reds += treds;
	oowc += run_queue_info[qix].oowc;
	if (mmax_len < run_queue_info[qix].max_len)
	    mmax_len = run_queue_info[qix].max_len;
    }

    if (!erts_sched_compact_load) {
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	if (erts_sched_balance_util && full_scheds < blnc_no_rqs) {
	    int avg_util = 0;

	    for (qix = 0; qix < blnc_no_rqs; qix++)
		avg_util += run_queue_info[qix].sched_util;

	    avg_util /= blnc_no_rqs; /* in ppm */

	    sched_util_balancing = 1;
	    /*
	     * In order to avoid renaming a large amount of fields
	     * we write utilization values instead of length values
	     * in the 'max_len' and 'migration_limit' fields...
	     */
	    for (qix = 0; qix < blnc_no_rqs; qix++) {
		run_queue_info[qix].flags = 0; /* Reset for later use... */
		for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		    run_queue_info[qix].prio[pix].emigrate_to = -1;
		    run_queue_info[qix].prio[pix].immigrate_from = -1;
		    run_queue_info[qix].prio[pix].avail = 100;
		    run_queue_info[qix].prio[pix].max_len = run_queue_info[qix].sched_util;
		    run_queue_info[qix].prio[pix].migration_limit = avg_util;
		}
	    }
	    active = blnc_no_rqs;
	    goto setup_migration_paths;
	}
#endif
	goto all_active;
    }

    if (!forced && half_full_scheds != blnc_no_rqs) {
	int min = 1;
	if (min < half_full_scheds)
	    min = half_full_scheds;
	if (full_scheds) {
	    active = (scheds_reds - 1)/ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED+1;
	}
	else {
	    active = balance_info.last_active_runqs - 1;
	}

	if (balance_info.last_active_runqs < current_active) {
	    ERTS_BLNCE_SAVE_RISE(current_active, mmax_len, scheds_reds);
	    active = current_active;
	}
	else if (active < balance_info.prev_rise.active_runqs) {
	    if (ERTS_PERCENT(mmax_len,
			     balance_info.prev_rise.max_len) >= 90
		&& ERTS_PERCENT(scheds_reds,
				balance_info.prev_rise.reds) >= 90) {
		active = balance_info.prev_rise.active_runqs;
	    }
	}

	if (active < min)
	    active = min;
	else if (active > blnc_no_rqs)
	    active = blnc_no_rqs;

	if (active == blnc_no_rqs)
	    goto all_active;

	for (qix = 0; qix < active; qix++) {
	    run_queue_info[qix].flags = 0;
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		run_queue_info[qix].prio[pix].emigrate_to = -1;
		run_queue_info[qix].prio[pix].immigrate_from = -1;
		run_queue_info[qix].prio[pix].migration_limit = 0;
	    }
	}
	for (qix = active; qix < blnc_no_rqs; qix++) {
	    run_queue_info[qix].flags = ERTS_RUNQ_FLG_INACTIVE;
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		int tix = qix % active;
		ERTS_SET_RUNQ_FLG_EMIGRATE(run_queue_info[qix].flags, pix);
		run_queue_info[qix].prio[pix].emigrate_to = tix;
		run_queue_info[qix].prio[pix].immigrate_from = -1;
		run_queue_info[qix].prio[pix].migration_limit = 0;
	    }
	}
    }
    else {
	if (balance_info.last_active_runqs < current_active)
	    ERTS_BLNCE_SAVE_RISE(current_active, mmax_len, scheds_reds);
    all_active:

	active = blnc_no_rqs;

	for (qix = 0; qix < blnc_no_rqs; qix++) {

	    if (full_scheds_reds > 0) {
		/* Calculate availability compared to other schedulers */
		if (!(run_queue_info[qix].flags & ERTS_RUNQ_FLG_OUT_OF_WORK)) {
		    Sint64 tmp = ((Sint64) run_queue_info[qix].full_reds
				  * (Sint64) full_scheds);
		    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
			Sint64 avail = run_queue_info[qix].prio[pix].avail;
			avail = (avail*tmp)/full_scheds_reds;
			ASSERT(avail >= 0);
			run_queue_info[qix].prio[pix].avail = (int) avail;
		    }
		}
	    }

	    /* Calculate average max length */
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		run_queue_info[qix].prio[pix].emigrate_to = -1;
		run_queue_info[qix].prio[pix].immigrate_from = -1;
		avg.prio[pix].max_len += run_queue_info[qix].prio[pix].max_len;
		avg.prio[pix].avail += run_queue_info[qix].prio[pix].avail;
	    }

	}

	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    int max_len = avg.prio[pix].max_len;
	    if (max_len != 0) {
		int avail = avg.prio[pix].avail;
		if (avail != 0) {
		    max_len = (int) ((100*((Sint64) max_len) - 1)
				     / ((Sint64) avail)) + 1;
		    avg.prio[pix].max_len = max_len;
		    ASSERT(max_len >= 0);
		}
	    }
	}

	/* Calculate migration limits for all priority queues in all
	   run queues */
	for (qix = 0; qix < blnc_no_rqs; qix++) {
	    run_queue_info[qix].flags = 0; /* Reset for later use... */
	    for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		int limit;
		if (avg.prio[pix].max_len == 0
		    || run_queue_info[qix].prio[pix].avail == 0)
		    limit = 0;
		else
		    limit = (int) (((((Sint64) avg.prio[pix].max_len)
				     * ((Sint64) run_queue_info[qix].prio[pix].avail))
				    - 1)
				   / 100 + 1);
		run_queue_info[qix].prio[pix].migration_limit = limit;
	    }
	}

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    setup_migration_paths:
#endif

	/* Setup migration paths for all priorities */
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    int low = 0, high = 0;
	    for (qix = 0; qix < blnc_no_rqs; qix++) {
		int len_diff = run_queue_info[qix].prio[pix].max_len;
		len_diff -= run_queue_info[qix].prio[pix].migration_limit;

#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "%d ", len_diff);
#endif

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
		if (sched_util_balancing
		    && -ERTS_SCHED_UTIL_IGNORE_IMBALANCE_DIFF <= len_diff
		    && len_diff <= ERTS_SCHED_UTIL_IGNORE_IMBALANCE_DIFF) {
		    /* ignore minor imbalance */
		    len_diff = 0;
		}
#endif
		    
		run_queue_compare[qix].qix = qix;
		run_queue_compare[qix].len = len_diff;
		if (len_diff != 0) {
		    if (len_diff < 0)
			low++;
		    else
			high++;
		}
	    }
#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "\n");
#endif
	    if (low && high) {
		int from_qix;
		int to_qix;
		int eof = 0;
		int eot = 0;
		int tix = 0;
		int fix = blnc_no_rqs-1;
		qsort(run_queue_compare,
		      blnc_no_rqs,
		      sizeof(ErtsRunQueueCompare),
		      rqc_len_cmp);

		while (1) {
		    if (run_queue_compare[fix].len <= 0)
			eof = 1;
		    if (run_queue_compare[tix].len >= 0)
			eot = 1;
		    if (eof || eot)
			break;
		    from_qix = run_queue_compare[fix].qix;
		    to_qix = run_queue_compare[tix].qix;
		    if (run_queue_info[from_qix].prio[pix].avail == 0) {
			ERTS_SET_RUNQ_FLG_EVACUATE(run_queue_info[from_qix].flags,
						   pix);
			ERTS_SET_RUNQ_FLG_EVACUATE(run_queue_info[to_qix].flags,
						   pix);
		    }
		    ERTS_SET_RUNQ_FLG_EMIGRATE(run_queue_info[from_qix].flags, pix);
		    ERTS_SET_RUNQ_FLG_IMMIGRATE(run_queue_info[to_qix].flags, pix);
		    run_queue_info[from_qix].prio[pix].emigrate_to = to_qix;
		    run_queue_info[to_qix].prio[pix].immigrate_from = from_qix;
		    tix++;
		    fix--;

#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "%d >--> %d\n", from_qix, to_qix);
#endif
		}

		if (!eot && eof) {
		    if (fix < blnc_no_rqs-1)
			fix++;

		    if (run_queue_compare[fix].len > 0) {
			int fix2 = -1;
			while (tix < fix) {
			    if (run_queue_compare[tix].len >= 0)
				break;
			    if (fix2 < fix)
				fix2 = blnc_no_rqs-1;
			    from_qix = run_queue_compare[fix2].qix;
			    to_qix = run_queue_compare[tix].qix;
			    ASSERT(to_qix != from_qix);
			    if (run_queue_info[from_qix].prio[pix].avail == 0)
				ERTS_SET_RUNQ_FLG_EVACUATE(run_queue_info[to_qix].flags,
							   pix);
			    ERTS_SET_RUNQ_FLG_IMMIGRATE(run_queue_info[to_qix].flags, pix);
			    run_queue_info[to_qix].prio[pix].immigrate_from = from_qix;
			    tix++;
			    fix2--;
#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "%d  --> %d\n", from_qix, to_qix);
#endif
			}
		    }
		}
		else if (!eof && eot) {
		    if (tix > 0)
			tix--;
		    if (run_queue_compare[tix].len < 0) {
			int tix2 = 0;
			while (tix < fix) {
			    if (run_queue_compare[fix].len <= 0)
				break;
			    if (tix2 > tix)
				tix2 = 0;
			    from_qix = run_queue_compare[fix].qix;
			    to_qix = run_queue_compare[tix2].qix;
			    ASSERT(to_qix != from_qix);
			    if (run_queue_info[from_qix].prio[pix].avail == 0)
				ERTS_SET_RUNQ_FLG_EVACUATE(run_queue_info[from_qix].flags,
							   pix);
			    ERTS_SET_RUNQ_FLG_EMIGRATE(run_queue_info[from_qix].flags, pix);
			    run_queue_info[from_qix].prio[pix].emigrate_to = to_qix;
			    fix--;
			    tix2++;
#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "%d >--  %d\n", from_qix, to_qix);
#endif

			}
		    }
		}
	    }
	}

#ifdef DBG_PRINT
erts_fprintf(stderr, "--------------------------------\n");
#endif
    }

    balance_info.last_active_runqs = active;
    set_no_active_runqs(active);

    balance_info.halftime = 1;
    new_mpaths = alloc_mpaths();

    /* Write migration paths */

    for (qix = 0; qix < blnc_no_rqs; qix++) {
	int mqix;
	Uint32 flags = run_queue_info[qix].flags;
	ErtsMigrationPath *mp = &new_mpaths->mpath[qix];

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	mp->sched_util = sched_util_balancing;
#endif
	mp->flags = flags;
	mp->misc_evac_runq = NULL;

	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    if (!(ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)
		  | ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix))) {
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0);
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0);
		mp->prio[pix].limit.this = -1;
		mp->prio[pix].limit.other = -1;
		mp->prio[pix].runq = NULL;
		mp->prio[pix].flags = 0;
	    }
	    else if (ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)) {
		ASSERT(!ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix));
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0);
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to >= 0);

		mqix = run_queue_info[qix].prio[pix].emigrate_to;
		mp->prio[pix].limit.this
		    = run_queue_info[qix].prio[pix].migration_limit;
		mp->prio[pix].limit.other
		    = run_queue_info[mqix].prio[pix].migration_limit;
		mp->prio[pix].runq = ERTS_RUNQ_IX(mqix);
		mp->prio[pix].flags = run_queue_info[mqix].flags;
	    }
	    else {
		ASSERT(ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix));
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0);
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from >= 0);

		mqix = run_queue_info[qix].prio[pix].immigrate_from;
		mp->prio[pix].limit.this
		    = run_queue_info[qix].prio[pix].migration_limit;
		mp->prio[pix].limit.other
		    = run_queue_info[mqix].prio[pix].migration_limit;
		mp->prio[pix].runq = ERTS_RUNQ_IX(mqix);
		mp->prio[pix].flags = run_queue_info[mqix].flags;
	    }
	}
    }

    old_mpaths = erts_get_migration_paths_managed();

    /* Keep offline run-queues as is */
    for (qix = blnc_no_rqs; qix < erts_no_schedulers; qix++) {
	ErtsMigrationPath *nmp = &new_mpaths->mpath[qix];
	ErtsMigrationPath *omp = &old_mpaths->mpath[qix];

	nmp->flags = omp->flags;
	nmp->misc_evac_runq = omp->misc_evac_runq;

	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    nmp->prio[pix].limit.this = omp->prio[pix].limit.this;
	    nmp->prio[pix].limit.other = omp->prio[pix].limit.other;
	    nmp->prio[pix].runq = omp->prio[pix].runq;
	    nmp->prio[pix].flags = omp->prio[pix].flags;
	}
    }


    /* Publish new migration paths... */
    erts_atomic_set_wb(&erts_migration_paths, (erts_aint_t) new_mpaths);

    /* Reset balance statistics in all online queues */
    for (qix = 0; qix < blnc_no_rqs; qix++) {
	Uint32 flags = run_queue_info[qix].flags;
	ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);

	erts_smp_runq_lock(rq);
	ASSERT(!(flags & ERTS_RUNQ_FLG_OUT_OF_WORK));
	if (rq->waiting)
	    flags |= ERTS_RUNQ_FLG_OUT_OF_WORK;

	rq->full_reds_history_sum
	    = run_queue_info[qix].full_reds_history_sum;
	rq->full_reds_history[freds_hist_ix]
	    = run_queue_info[qix].full_reds_history_change;

	ERTS_DBG_CHK_FULL_REDS_HISTORY(rq);

	rq->out_of_work_count = 0;
	(void) ERTS_RUNQ_FLGS_READ_BSET(rq, ERTS_RUNQ_FLGS_MIGRATION_INFO, flags);

	rq->max_len = erts_smp_atomic32_read_dirty(&rq->len);
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    ErtsRunQueueInfo *rqi;
	    rqi = (pix == ERTS_PORT_PRIO_LEVEL
		   ? &rq->ports.info
		   : &rq->procs.prio_info[pix]);
	    erts_smp_reset_max_len(rq, rqi);
	    rqi->reds = 0;
	}

	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	erts_smp_runq_unlock(rq);
    }

    erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);

    balance_info.n++;
    retire_mpaths(old_mpaths);
    erts_smp_mtx_unlock(&balance_info.update_mtx);

    erts_smp_runq_lock(c_rq);
}

static void
change_no_used_runqs(int used)
{
    ErtsMigrationPaths *new_mpaths, *old_mpaths;
    int qix;
    erts_smp_mtx_lock(&balance_info.update_mtx);
    set_no_used_runqs(used);

    old_mpaths = erts_get_migration_paths_managed();
    new_mpaths = alloc_mpaths();

    /* Write migration paths... */

    for (qix = 0; qix < used; qix++) {
	int pix;
	ErtsMigrationPath *omp = &old_mpaths->mpath[qix];
	ErtsMigrationPath *nmp = &new_mpaths->mpath[qix];

	nmp->flags = omp->flags & ~ERTS_RUNQ_FLGS_MIGRATION_QMASKS;
	nmp->misc_evac_runq = NULL;

	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    nmp->prio[pix].limit.this = -1;
	    nmp->prio[pix].limit.other = -1;
	    nmp->prio[pix].runq = NULL;
	    nmp->prio[pix].flags = 0;
	}
    }
    for (qix = used; qix < erts_no_run_queues; qix++) {
	int pix;
	ErtsRunQueue *to_rq = ERTS_RUNQ_IX(qix % used);
	ErtsMigrationPath *nmp = &new_mpaths->mpath[qix];

	nmp->flags = (ERTS_RUNQ_FLGS_EMIGRATE_QMASK
		      | ERTS_RUNQ_FLGS_EVACUATE_QMASK);
	nmp->misc_evac_runq = to_rq;
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    nmp->prio[pix].limit.this = -1;
	    nmp->prio[pix].limit.other = -1;
	    nmp->prio[pix].runq = to_rq;
	    nmp->prio[pix].flags = 0;
	}
    }

    /* ... and publish them. */
    erts_atomic_set_wb(&erts_migration_paths, (erts_aint_t) new_mpaths);

    retire_mpaths(old_mpaths);

    /* Make sure that we balance soon... */
    balance_info.forced_check_balance = 1;

    erts_smp_mtx_unlock(&balance_info.update_mtx);

    erts_smp_runq_lock(ERTS_RUNQ_IX(0));
    ERTS_RUNQ_IX(0)->check_balance_reds = 0;
    erts_smp_runq_unlock(ERTS_RUNQ_IX(0));
}


#endif /* #ifdef ERTS_SMP */

Uint
erts_debug_nbalance(void)
{
#ifdef ERTS_SMP
    Uint n;
    erts_smp_mtx_lock(&balance_info.update_mtx);
    n = balance_info.n;
    erts_smp_mtx_unlock(&balance_info.update_mtx);
    return n;
#else
    return 0;
#endif
}

/* Wakeup other schedulers */

typedef enum {
    ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH,
    ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH,
    ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM,
    ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW,
    ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW
} ErtsSchedWakeupOtherThreshold;

typedef enum {
    ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT,
    ERTS_SCHED_WAKEUP_OTHER_TYPE_LEGACY
} ErtsSchedWakeupOtherType;

/* Default */

#define ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH (200*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_HIGH (50*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_MEDIUM (10*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_LOW (CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW (CONTEXT_REDS/10)

#define ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_HIGH 3
#define ERTS_WAKEUP_OTHER_DEC_SHIFT_HIGH 1
#define ERTS_WAKEUP_OTHER_DEC_SHIFT_MEDIUM 0
#define ERTS_WAKEUP_OTHER_DEC_SHIFT_LOW -2
#define ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_LOW -5 

#define ERTS_WAKEUP_OTHER_DEC_SHIFT 2
#define ERTS_WAKEUP_OTHER_FIXED_INC (CONTEXT_REDS/10)

/* Legacy */

#define ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH_LEGACY (200*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_HIGH_LEGACY (50*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_MEDIUM_LEGACY (10*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_LOW_LEGACY (CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW_LEGACY (CONTEXT_REDS/10)

#define ERTS_WAKEUP_OTHER_DEC_LEGACY 10
#define ERTS_WAKEUP_OTHER_FIXED_INC_LEGACY (CONTEXT_REDS/10)

#ifdef ERTS_SMP

static struct {
    ErtsSchedWakeupOtherThreshold threshold;
    ErtsSchedWakeupOtherType type;
    int limit;
    int dec_shift;
    int dec_mask;
    void (*check)(ErtsRunQueue *rq, Uint32 flags);
} wakeup_other;

static void
wakeup_other_check(ErtsRunQueue *rq, Uint32 flags)
{
    int wo_reds = rq->wakeup_other_reds;
    if (wo_reds) {
	int left_len = erts_smp_atomic32_read_dirty(&rq->len) - 1;
	if (left_len < 1) {
	    int wo_reduce = wo_reds << wakeup_other.dec_shift;
	    wo_reduce &= wakeup_other.dec_mask;
	    rq->wakeup_other -= wo_reduce;
	    if (rq->wakeup_other < 0)
		rq->wakeup_other = 0;
	}
	else {
	    rq->wakeup_other += (left_len*wo_reds
				 + ERTS_WAKEUP_OTHER_FIXED_INC);
	    if (rq->wakeup_other > wakeup_other.limit) {
#ifdef ERTS_DIRTY_SCHEDULERS
		if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix)) {
		    if (rq->waiting) {
			wake_dirty_scheduler(rq);
		    }
		} else
#endif
		{
		    int empty_rqs =
			erts_smp_atomic32_read_acqb(&no_empty_run_queues);
		    if (flags & ERTS_RUNQ_FLG_PROTECTED)
			(void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);
		    if (empty_rqs != 0)
			wake_scheduler_on_empty_runq(rq);
		    rq->wakeup_other = 0;
		}
	    }
	}
	rq->wakeup_other_reds = 0;
    }
}

static void
wakeup_other_set_limit(void)
{
    switch (wakeup_other.threshold) {
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH;
	wakeup_other.dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_HIGH;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_HIGH;
	wakeup_other.dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_HIGH;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM;
	wakeup_other.dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_MEDIUM;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_LOW;
	wakeup_other.dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_LOW;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW;
	wakeup_other.dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_LOW;
	break;
    }
    if (wakeup_other.dec_shift < 0)
	wakeup_other.dec_mask = (1 << (sizeof(wakeup_other.dec_mask)*8
				       + wakeup_other.dec_shift)) - 1;
    else {
	wakeup_other.dec_mask = 0;
	wakeup_other.dec_mask = ~wakeup_other.dec_mask;
    }
}

static void
wakeup_other_check_legacy(ErtsRunQueue *rq, Uint32 flags)
{
    int wo_reds = rq->wakeup_other_reds;
    if (wo_reds) {
	erts_aint32_t len = erts_smp_atomic32_read_dirty(&rq->len);
	if (len < 2) {
	    rq->wakeup_other -= ERTS_WAKEUP_OTHER_DEC_LEGACY*wo_reds;
	    if (rq->wakeup_other < 0)
		rq->wakeup_other = 0;
	}
	else if (rq->wakeup_other < wakeup_other.limit)
	    rq->wakeup_other += len*wo_reds + ERTS_WAKEUP_OTHER_FIXED_INC_LEGACY;
	else {
	    if (flags & ERTS_RUNQ_FLG_PROTECTED)
		(void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);
	    if (erts_smp_atomic32_read_acqb(&no_empty_run_queues) != 0) {
		wake_scheduler_on_empty_runq(rq);
		rq->wakeup_other = 0;
	    }
	    rq->wakeup_other = 0;
	}
    }
    rq->wakeup_other_reds = 0;
}

static void
wakeup_other_set_limit_legacy(void)
{
    switch (wakeup_other.threshold) {
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_HIGH_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_LOW_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW:
	wakeup_other.limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW_LEGACY;
	break;
    }
}

static void
set_wakeup_other_data(void)
{
    switch (wakeup_other.type) {
    case ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT:
	wakeup_other.check = wakeup_other_check;
	wakeup_other_set_limit();
	break;
    case ERTS_SCHED_WAKEUP_OTHER_TYPE_LEGACY:
	wakeup_other.check = wakeup_other_check_legacy;
	wakeup_other_set_limit_legacy();
	break;
    }
}

static int
no_runqs_to_supervise(void)
{
    int used;
    erts_aint32_t nerq = erts_smp_atomic32_read_acqb(&no_empty_run_queues);
    if (nerq <= 0)
	return 0;
    get_no_runqs(NULL, &used);
    if (nerq >= used)
	return 0;
    return used;
}

static void *
runq_supervisor(void *unused)
{
    while (1) {
	int ix, no_rqs;

	erts_milli_sleep(erts_runq_supervision_interval);
	no_rqs = no_runqs_to_supervise();
	if (!no_rqs) {
	    erts_atomic_set_nob(&runq_supervisor_sleeping, 1);
	    while (1) {
		ethr_event_reset(&runq_supervision_event);
		no_rqs = no_runqs_to_supervise();
		if (no_rqs) {
		    erts_atomic_set_nob(&runq_supervisor_sleeping, 0);
		    break;
		}
		ethr_event_wait(&runq_supervision_event);
	    }
	}

	for (ix = 0; ix < no_rqs; ix++) {
	    ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
	    if (ERTS_RUNQ_FLGS_GET(rq) & ERTS_RUNQ_FLG_NONEMPTY) {
		erts_smp_runq_lock(rq);
		if (erts_smp_atomic32_read_dirty(&rq->len) != 0)
		    wake_scheduler_on_empty_runq(rq); /* forced wakeup... */
		erts_smp_runq_unlock(rq);
	    }
	}
    }
    return NULL;
}

#endif

void
erts_early_init_scheduling(int no_schedulers)
{
    aux_work_timeout_early_init(no_schedulers);
#ifdef ERTS_SMP
    wakeup_other.threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM;
    wakeup_other.type = ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT;
#endif
    sched_busy_wait.sys_schedule = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM;
    sched_busy_wait.tse = (ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM
			   * ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT);
    sched_busy_wait.aux_work = (ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM
				* ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_MEDIUM);
}

int
erts_sched_set_wakeup_other_thresold(char *str)
{
#ifdef ERTS_SMP
    ErtsSchedWakeupOtherThreshold threshold;
    if (sys_strcmp(str, "very_high") == 0)
	threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH;
    else if (sys_strcmp(str, "high") == 0)
	threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH;
    else if (sys_strcmp(str, "medium") == 0)
	threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM;
    else if (sys_strcmp(str, "low") == 0)
	threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW;
    else if (sys_strcmp(str, "very_low") == 0)
	threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW;
    else
	return EINVAL;
    wakeup_other.threshold = threshold;
    set_wakeup_other_data();
    return 0;
#else
    if (sys_strcmp(str, "very_high") == 0 || sys_strcmp(str, "high") == 0 ||
	sys_strcmp(str, "medium") == 0 || sys_strcmp(str, "low") == 0 ||
	sys_strcmp(str, "very_low") == 0) {
	return 0;
    } 
    return EINVAL;
#endif
}

int
erts_sched_set_wakeup_other_type(char *str)
{
#ifdef ERTS_SMP
    ErtsSchedWakeupOtherType type;
    if (sys_strcmp(str, "default") == 0)
	type = ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT;
    else if (sys_strcmp(str, "legacy") == 0)
	type = ERTS_SCHED_WAKEUP_OTHER_TYPE_LEGACY;
    else
	return EINVAL;
    wakeup_other.type = type;
    return 0;
#else
    if (sys_strcmp(str, "default") == 0 || sys_strcmp(str, "legacy") == 0) {
	return 0;
    } 
    return EINVAL;
#endif
}

int
erts_sched_set_busy_wait_threshold(char *str)
{
    int sys_sched;
    int aux_work_fact;

    if (sys_strcmp(str, "very_long") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_LONG;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_LONG;
    }
    else if (sys_strcmp(str, "long") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_LONG;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_LONG;
    }
    else if (sys_strcmp(str, "medium") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_MEDIUM;
    }
    else if (sys_strcmp(str, "short") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_SHORT;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_SHORT;
    }
    else if (sys_strcmp(str, "very_short") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_SHORT;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_SHORT;
    }
    else if (sys_strcmp(str, "none") == 0) {
	sys_sched = ERTS_SCHED_SYS_SLEEP_SPINCOUNT_NONE;
	aux_work_fact = ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_NONE;
    }
    else {
	return EINVAL;
    }

    sched_busy_wait.sys_schedule = sys_sched;
    sched_busy_wait.tse = sys_sched*ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT;
    sched_busy_wait.aux_work = sys_sched*aux_work_fact;

    return 0;
}

int
erts_sched_set_wake_cleanup_threshold(char *str)
{
    if (sys_strcmp(str, "very_lazy") == 0)
	thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_LAZY;
    else if (sys_strcmp(str, "lazy") == 0)
	thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_LAZY;
    else if (sys_strcmp(str, "medium") == 0)
	thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_MEDIUM;
    else if (sys_strcmp(str, "eager") == 0)
	thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_EAGER;
    else if (sys_strcmp(str, "very_eager") == 0)
	thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_EAGER;
    else
	return EINVAL;
    return 0;
}

static void
init_aux_work_data(ErtsAuxWorkData *awdp, ErtsSchedulerData *esdp, char *dawwp)
{
    int id = 0;
    if (esdp) {
        switch (esdp->type) {
        case ERTS_SCHED_NORMAL:
            id = (int) esdp->no;
            break;
#ifdef ERTS_DIRTY_SCHEDULERS
        case ERTS_SCHED_DIRTY_CPU:
            id = (int) erts_no_schedulers;
            id += (int) esdp->dirty_no;
            break;
        case ERTS_SCHED_DIRTY_IO:
            id = (int) erts_no_schedulers;
            id += (int) erts_no_dirty_cpu_schedulers;
            id += (int) esdp->dirty_no;
            break;
#endif
        default:
            ERTS_INTERNAL_ERROR("Invalid scheduler type");
            break;
        }
    }

    awdp->sched_id = id;
    awdp->esdp = esdp;
    awdp->ssi = esdp ? esdp->ssi : NULL;
#ifdef ERTS_SMP
    awdp->latest_wakeup = ERTS_THR_PRGR_VAL_FIRST;
    awdp->misc.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->dd.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->cncld_tmrs.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->later_op.thr_prgr = ERTS_THR_PRGR_VAL_FIRST;
    awdp->later_op.size = 0;
    awdp->later_op.first = NULL;
    awdp->later_op.last = NULL;
#endif
#ifdef ERTS_USE_ASYNC_READY_Q
#ifdef ERTS_SMP
    awdp->async_ready.need_thr_prgr = 0;
    awdp->async_ready.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
#endif
    awdp->async_ready.queue = NULL;
#endif
#ifdef ERTS_SMP
    awdp->delayed_wakeup.next = ERTS_DELAYED_WAKEUP_INFINITY;
    if (!dawwp) {
	awdp->delayed_wakeup.job = NULL;
	awdp->delayed_wakeup.sched2jix = NULL;
	awdp->delayed_wakeup.jix = -1;
    }
    else {
	int i;
	awdp->delayed_wakeup.job = (ErtsDelayedAuxWorkWakeupJob *) dawwp;
	dawwp += sizeof(ErtsDelayedAuxWorkWakeupJob)*(erts_no_schedulers+1);
	awdp->delayed_wakeup.sched2jix = (int *) dawwp;
	awdp->delayed_wakeup.jix = -1;
	for (i = 0; i <= erts_no_schedulers; i++)
	    awdp->delayed_wakeup.sched2jix[i] = -1;
    }
#endif
    awdp->debug.wait_completed.flags = 0;
    awdp->debug.wait_completed.callback = NULL;
    awdp->debug.wait_completed.arg = NULL;
}

static void
init_scheduler_data(ErtsSchedulerData* esdp, int num,
		    ErtsSchedulerSleepInfo* ssi,
		    ErtsRunQueue* runq,
		    char** daww_ptr, size_t daww_sz,
		    Process *shadow_proc,
                    Uint64 time_stamp)
{
    esdp->timer_wheel = NULL;
#ifdef ERTS_SMP
    erts_bits_init_state(&esdp->erl_bits_state);
    esdp->match_pseudo_process = NULL;
    esdp->free_process = NULL;
#endif
    esdp->x_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   ERTS_X_REGS_ALLOCATED *
					   sizeof(Eterm));
    esdp->f_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   MAX_REG * sizeof(FloatDef));
#ifdef ERTS_DIRTY_SCHEDULERS
    esdp->run_queue = runq;
    if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix)) {
	esdp->no = 0;
        if (runq == ERTS_DIRTY_CPU_RUNQ)
            esdp->type = ERTS_SCHED_DIRTY_CPU;
        else {
            ASSERT(runq == ERTS_DIRTY_IO_RUNQ);
            esdp->type = ERTS_SCHED_DIRTY_IO;
        }
        esdp->dirty_no = (Uint) num;
        if (num == 1) {
            /*
             * Multi-scheduling block functionality depends
             * on finding dirty scheduler number 1 here...
             */
            runq->scheduler = esdp;
        }
    }
    else {
        esdp->type = ERTS_SCHED_NORMAL;
	esdp->no = (Uint) num;
	esdp->dirty_no = 0;
        runq->scheduler = esdp;
    }
    esdp->dirty_shadow_process = shadow_proc;
    if (shadow_proc) {
	erts_init_empty_process(shadow_proc);
	erts_smp_atomic32_init_nob(&shadow_proc->state,
				   (ERTS_PSFLG_ACTIVE
				    | ERTS_PSFLG_DIRTY_RUNNING
				    | ERTS_PSFLG_PROXY));
	shadow_proc->static_flags = ERTS_STC_FLG_SHADOW_PROC;
    }
#else
    runq->scheduler = esdp;
    esdp->run_queue = runq;
    esdp->no = (Uint) num;
    esdp->type = ERTS_SCHED_NORMAL;
#endif

    esdp->ssi = ssi;
    esdp->current_process = NULL;
    esdp->current_port = NULL;

    esdp->virtual_reds = 0;
    esdp->cpu_id = -1;

    erts_init_atom_cache_map(&esdp->atom_cache_map);

    esdp->last_monotonic_time = 0;
    esdp->check_time_reds = 0;

    esdp->thr_id = (Uint32) num;
    erts_sched_bif_unique_init(esdp);

    esdp->io.out = (Uint64) 0;
    esdp->io.in = (Uint64) 0;

    if (daww_ptr) {
	init_aux_work_data(&esdp->aux_work_data, esdp, *daww_ptr);
#ifdef ERTS_SMP
	*daww_ptr += daww_sz;
#endif
    }

    esdp->reductions = 0;

    init_sched_wall_time(esdp, time_stamp);
    erts_port_task_handle_init(&esdp->nosuspend_port_task_handle);
}

void
erts_init_scheduling(int no_schedulers, int no_schedulers_online
#ifdef ERTS_DIRTY_SCHEDULERS
		     , int no_dirty_cpu_schedulers, int no_dirty_cpu_schedulers_online,
		     int no_dirty_io_schedulers
#endif
		     )
{
    int ix, n, no_ssi;
    char *daww_ptr;
    size_t daww_sz;
    size_t size_runqs;
#ifdef ERTS_SMP
    erts_aint32_t set_schdlr_sspnd_change_flags;
#endif

    init_misc_op_list_alloc();
    init_proc_sys_task_queues_alloc();

#ifdef ERTS_SMP
    set_wakeup_other_data();
#endif

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    if (erts_sched_balance_util)
	erts_sched_compact_load = 0;
#endif

    ASSERT(no_schedulers_online <= no_schedulers);
    ASSERT(no_schedulers_online >= 1);
    ASSERT(no_schedulers >= 1);
#ifdef ERTS_DIRTY_SCHEDULERS
    ASSERT(no_dirty_cpu_schedulers <= no_schedulers);
    ASSERT(no_dirty_cpu_schedulers >= 1);
    ASSERT(no_dirty_cpu_schedulers_online <= no_schedulers_online);
    ASSERT(no_dirty_cpu_schedulers_online >= 1);
#endif

    /* Create and initialize run queues */

    n = no_schedulers;
    size_runqs = sizeof(ErtsAlignedRunQueue) * (n + ERTS_NUM_DIRTY_RUNQS);
    erts_aligned_run_queues =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_RUNQS, size_runqs);
#ifdef ERTS_SMP
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_aligned_run_queues += ERTS_NUM_DIRTY_RUNQS;
#endif
    erts_smp_atomic32_init_nob(&no_empty_run_queues, 0);
#endif

    erts_no_run_queues = n;

    for (ix = -(ERTS_NUM_DIRTY_RUNQS); ix < n; ix++) {
	int pix, rix;
#ifdef ERTS_DIRTY_SCHEDULERS
	ErtsRunQueue *rq = ERTS_RUNQ_IX_IS_DIRTY(ix) ?
	    ERTS_DIRTY_RUNQ_IX(ix) : ERTS_RUNQ_IX(ix);
#else
	ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
#endif

	rq->ix = ix;

	/* make sure that the "extra" id correponds to the schedulers
	 * id if the esdp->no <-> ix+1 mapping change.
	 */

	erts_smp_mtx_init_x(&rq->mtx, "run_queue", make_small(ix + 1));
	erts_smp_cnd_init(&rq->cnd);

#ifdef ERTS_DIRTY_SCHEDULERS
#ifdef ERTS_SMP
	if (ERTS_RUNQ_IX_IS_DIRTY(ix))
	    erts_smp_spinlock_init(&rq->sleepers.lock, "dirty_run_queue_sleep_list");
	rq->sleepers.list = NULL;
#endif
#endif

	rq->waiting = 0;
	rq->woken = 0;
	ERTS_RUNQ_FLGS_INIT(rq, ERTS_RUNQ_FLG_NONEMPTY);
	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	rq->full_reds_history_sum = 0;
	for (rix = 0; rix < ERTS_FULL_REDS_HISTORY_SIZE; rix++) {
	    rq->full_reds_history_sum += ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED;
	    rq->full_reds_history[rix] = ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED;
	}
	rq->out_of_work_count = 0;
	rq->max_len = 0;
	erts_smp_atomic32_set_nob(&rq->len, 0);
	rq->wakeup_other = 0;
	rq->wakeup_other_reds = 0;

	rq->procs.pending_exiters = NULL;
	rq->procs.context_switches = 0;
	rq->procs.reductions = 0;

	for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
	    erts_smp_atomic32_init_nob(&rq->procs.prio_info[pix].len, 0);
	    rq->procs.prio_info[pix].max_len = 0;
	    rq->procs.prio_info[pix].reds = 0;
	    if (pix < ERTS_NO_PROC_PRIO_LEVELS - 1) {
		rq->procs.prio[pix].first = NULL;
		rq->procs.prio[pix].last = NULL;
	    }
	}

	rq->misc.start = NULL;
	rq->misc.end = NULL;

	erts_smp_atomic32_init_nob(&rq->ports.info.len, 0);
	rq->ports.info.max_len = 0;
	rq->ports.info.reds = 0;
	rq->ports.start = NULL;
	rq->ports.end = NULL;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	init_runq_sched_util(&rq->sched_util, erts_sched_balance_util);
#endif

    }

#ifdef ERTS_SMP

    if (erts_no_run_queues != 1) {
	run_queue_info = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				    (sizeof(ErtsRunQueueBalance)
				    * erts_no_run_queues));
	run_queue_compare = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				       (sizeof(ErtsRunQueueCompare)
					* erts_no_run_queues));
    }

#endif

    n = (int) no_schedulers;
    erts_no_schedulers = n;
    erts_no_total_schedulers = n;
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_no_dirty_cpu_schedulers = no_dirty_cpu_schedulers;
    erts_no_total_schedulers += no_dirty_cpu_schedulers;
    erts_no_dirty_io_schedulers = no_dirty_io_schedulers;
    erts_no_total_schedulers += no_dirty_io_schedulers;
#endif

    /* Create and initialize scheduler sleep info */
#ifdef ERTS_SMP
    no_ssi = n+1;
#else
    no_ssi = 1;
#endif
    aligned_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_ssi*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_ssi; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_sched_sleep_info[ix].ssi;
#ifdef ERTS_SMP
#if 0 /* no need to initialize these... */
	ssi->next = NULL;
	ssi->prev = NULL;
#endif
	erts_smp_atomic32_init_nob(&ssi->flags, 0);
	ssi->event = NULL; /* initialized in sched_thread_func */
#endif
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }

#ifdef ERTS_SMP
    aligned_sched_sleep_info++;

#ifdef ERTS_DIRTY_SCHEDULERS
    aligned_dirty_cpu_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_dirty_cpu_schedulers*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_dirty_cpu_schedulers; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_dirty_cpu_sched_sleep_info[ix].ssi;
	erts_smp_atomic32_init_nob(&ssi->flags, 0);
	ssi->event = NULL; /* initialized in sched_dirty_cpu_thread_func */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }
    aligned_dirty_io_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_dirty_io_schedulers*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_dirty_io_schedulers; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_dirty_io_sched_sleep_info[ix].ssi;
	erts_smp_atomic32_init_nob(&ssi->flags, 0);
	ssi->event = NULL; /* initialized in sched_dirty_io_thread_func */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }
#endif
#endif

    /* Create and initialize scheduler specific data */

#ifdef ERTS_SMP
    daww_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE((sizeof(ErtsDelayedAuxWorkWakeupJob)
					      + sizeof(int))*(n+1));
    daww_ptr = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
						  daww_sz*n);
#else
    daww_sz = 0;
    daww_ptr = NULL;
#endif

    erts_aligned_scheduler_data = 
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   n*sizeof(ErtsAlignedSchedulerData));

    for (ix = 0; ix < n; ix++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
	init_scheduler_data(esdp, ix+1, ERTS_SCHED_SLEEP_INFO_IX(ix),
			    ERTS_RUNQ_IX(ix), &daww_ptr, daww_sz,
			    NULL, 0);
    }

#ifdef ERTS_DIRTY_SCHEDULERS
    {
        Uint64 ts = sched_wall_time_ts();
	int dirty_scheds = no_dirty_cpu_schedulers + no_dirty_io_schedulers;
	int adspix = 0;
	ErtsAlignedDirtyShadowProcess *adsp =
	    erts_alloc_permanent_cache_aligned(
		ERTS_ALC_T_SCHDLR_DATA,
		dirty_scheds * sizeof(ErtsAlignedDirtyShadowProcess));
						   
	erts_aligned_dirty_cpu_scheduler_data =
	    erts_alloc_permanent_cache_aligned(
		ERTS_ALC_T_SCHDLR_DATA,
		dirty_scheds * sizeof(ErtsAlignedSchedulerData));

	erts_aligned_dirty_io_scheduler_data =
	    &erts_aligned_dirty_cpu_scheduler_data[no_dirty_cpu_schedulers];

	for (ix = 0; ix < no_dirty_cpu_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_CPU_SCHEDULER_IX(ix);
	    init_scheduler_data(esdp, ix+1, ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix),
				ERTS_DIRTY_CPU_RUNQ, NULL, 0,
				&adsp[adspix++].dsp, ts);
	}
	for (ix = 0; ix < no_dirty_io_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_IO_SCHEDULER_IX(ix);
	    init_scheduler_data(esdp, ix+1, ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(ix),
				ERTS_DIRTY_IO_RUNQ, NULL, 0,
				&adsp[adspix++].dsp, ts);
	}
    }
#endif

    init_misc_aux_work();
    init_swtreq_alloc();
    init_screq_alloc();

    erts_atomic32_init_nob(&debug_wait_completed_count, 0); /* debug only */
    debug_wait_completed_flags = 0;

#ifdef ERTS_SMP

    aux_thread_aux_work_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   sizeof(ErtsAuxWorkData));

    init_no_runqs(no_schedulers_online, no_schedulers_online);
    balance_info.last_active_runqs = no_schedulers;
    erts_smp_mtx_init(&balance_info.update_mtx, "migration_info_update");
    balance_info.forced_check_balance = 0;
    balance_info.halftime = 1;
    balance_info.full_reds_history_index = 0;
    erts_smp_atomic32_init_nob(&balance_info.checking_balance, 0);
    balance_info.prev_rise.active_runqs = 0;
    balance_info.prev_rise.max_len = 0;
    balance_info.prev_rise.reds = 0;
    balance_info.n = 0;

    init_migration_paths();

    init_scheduler_suspend();

    set_schdlr_sspnd_change_flags = 0;

    schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
			     ERTS_SCHED_NORMAL,
			     no_schedulers_online);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.curr_online,
			     ERTS_SCHED_NORMAL,
			     no_schedulers);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.active,
			     ERTS_SCHED_NORMAL,
			     no_schedulers);

    if (no_schedulers_online != no_schedulers) {
	ASSERT(no_schedulers_online < no_schedulers);
	set_schdlr_sspnd_change_flags |= ERTS_SCHDLR_SSPND_CHNG_ONLN;
	schdlr_sspnd.changer = am_init;
	change_no_used_runqs(no_schedulers_online);
	for (ix = no_schedulers_online; ix < erts_no_run_queues; ix++)
	    suspend_run_queue(ERTS_RUNQ_IX(ix));
    }

#ifdef ERTS_DIRTY_SCHEDULERS

    schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
			     ERTS_SCHED_DIRTY_CPU,
			     no_dirty_cpu_schedulers_online);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.curr_online,
			     ERTS_SCHED_DIRTY_CPU,
			     no_dirty_cpu_schedulers);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.active,
			     ERTS_SCHED_DIRTY_CPU,
			     no_dirty_cpu_schedulers);

    if (no_dirty_cpu_schedulers_online != no_dirty_cpu_schedulers) {
	ASSERT(no_dirty_cpu_schedulers_online < no_dirty_cpu_schedulers);
	set_schdlr_sspnd_change_flags |= ERTS_SCHDLR_SSPND_CHNG_DCPU_ONLN;
	for (ix = no_dirty_cpu_schedulers_online; ix < no_dirty_cpu_schedulers; ix++) {
	    ErtsSchedulerData* esdp = ERTS_DIRTY_CPU_SCHEDULER_IX(ix);
	    erts_smp_atomic32_read_bor_nob(&esdp->ssi->flags, ERTS_SSI_FLG_SUSPENDED);
	}
    }

    schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
			     ERTS_SCHED_DIRTY_IO,
			     no_dirty_io_schedulers);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.curr_online,
			     ERTS_SCHED_DIRTY_IO,
			     no_dirty_io_schedulers);
    schdlr_sspnd_set_nscheds(&schdlr_sspnd.active,
			     ERTS_SCHED_DIRTY_IO,
			     no_dirty_io_schedulers);

#endif

    if (set_schdlr_sspnd_change_flags)
	erts_smp_atomic32_set_nob(&schdlr_sspnd.changing,
				  set_schdlr_sspnd_change_flags);

    erts_smp_atomic32_init_nob(&doing_sys_schedule, 0);

    init_misc_aux_work();

#else /* !ERTS_SMP */
    {
	ErtsSchedulerData *esdp;
	esdp = ERTS_SCHEDULER_IX(0);
	erts_scheduler_data = esdp;
#ifdef USE_THREADS
	erts_tsd_set(sched_data_key, (void *) esdp);
#endif
    }
    erts_no_dirty_cpu_schedulers = 0;
    erts_no_dirty_io_schedulers = 0;
#endif

    erts_smp_atomic32_init_nob(&function_calls, 0);

    /* init port tasks */
    erts_port_task_init();

#ifndef ERTS_SMP
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_scheduler_data->verify_unused_temp_alloc
	= erts_alloc_get_verify_unused_temp_alloc(
	    &erts_scheduler_data->verify_unused_temp_alloc_data);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif
#endif

    erts_smp_atomic32_init_relb(&erts_halt_progress, -1);
    erts_halt_code = 0;

#if !defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    erts_lc_set_thread_name("scheduler 1");
#endif

}

ErtsRunQueue *
erts_schedid2runq(Uint id)
{
    int ix;
    ix = (int) id - 1;
    ASSERT(0 <= ix && ix < erts_no_run_queues);
    return ERTS_RUNQ_IX(ix);
}

#ifdef USE_THREADS

ErtsSchedulerData *
erts_get_scheduler_data(void)
{
    return (ErtsSchedulerData *) erts_tsd_get(sched_data_key);
}

#endif

static Process *
make_proxy_proc(Process *prev_proxy, Process *proc, erts_aint32_t prio)
{
    erts_aint32_t state;
    Process *proxy;
#ifdef ERTS_SMP
    ErtsRunQueue *rq = RUNQ_READ_RQ(&proc->run_queue);
#endif

    state = (ERTS_PSFLG_PROXY
	     | ERTS_PSFLG_IN_RUNQ
	     | (((erts_aint32_t) 1) << (prio + ERTS_PSFLGS_IN_PRQ_MASK_OFFSET))
	     | (prio << ERTS_PSFLGS_PRQ_PRIO_OFFSET)
	     | (prio << ERTS_PSFLGS_USR_PRIO_OFFSET)
	     | (prio << ERTS_PSFLGS_ACT_PRIO_OFFSET));

    if (prev_proxy) {
	proxy = prev_proxy;
	ASSERT(erts_smp_atomic32_read_nob(&proxy->state) & ERTS_PSFLG_PROXY);
	erts_smp_atomic32_set_nob(&proxy->state, state);
#ifdef ERTS_SMP
	RUNQ_SET_RQ(&proc->run_queue, rq);
#endif
    }
    else {
	proxy = erts_alloc(ERTS_ALC_T_PROC, sizeof(Process));
#ifdef DEBUG
	{
	    int i;
	    Uint32 *ui32 = (Uint32 *) (char *) proxy;
	    for (i = 0; i < sizeof(Process)/sizeof(Uint32); i++)
		ui32[i] = (Uint32) 0xdeadbeef;
	}
#endif
	erts_smp_atomic32_init_nob(&proxy->state, state);
#ifdef ERTS_SMP
	erts_smp_atomic_init_nob(&proxy->run_queue,
				 erts_smp_atomic_read_nob(&proc->run_queue));
#endif
    }

    proxy->common.id = proc->common.id;

    return proxy;
}

#define ERTS_ENQUEUE_NOT 0
#define ERTS_ENQUEUE_NORMAL_QUEUE 1
#define ERTS_ENQUEUE_DIRTY_CPU_QUEUE 2
#define ERTS_ENQUEUE_DIRTY_IO_QUEUE 3

#ifdef ERTS_DIRTY_SCHEDULERS

static int
check_dirty_enqueue_in_prio_queue(Process *c_p,
				  erts_aint32_t *newp,
				  erts_aint32_t actual,
				  erts_aint32_t aprio,
				  erts_aint32_t qbit)
{
    int queue;
    erts_aint32_t dact, max_qbit;

    /* Do not enqueue free process... */
    if (actual & ERTS_PSFLG_FREE) {
	*newp &= ~ERTS_PSFLGS_DIRTY_WORK;
        return ERTS_ENQUEUE_NOT;
    }

    /* Termination should be done on an ordinary scheduler */
    if ((*newp) & ERTS_PSFLG_EXITING) {
	*newp &= ~ERTS_PSFLGS_DIRTY_WORK;
	return ERTS_ENQUEUE_NORMAL_QUEUE;
    }

    /*
     * If we have system tasks, we enqueue on ordinary run-queue
     * and take care of those system tasks first.
     */
    if ((*newp) & ERTS_PSFLG_ACTIVE_SYS)
	return ERTS_ENQUEUE_NORMAL_QUEUE;

    dact = erts_smp_atomic32_read_mb(&c_p->dirty_state);
    if (actual & (ERTS_PSFLG_DIRTY_ACTIVE_SYS
		  | ERTS_PSFLG_DIRTY_CPU_PROC)) {
	max_qbit = ((dact >> ERTS_PDSFLGS_IN_CPU_PRQ_MASK_OFFSET)
		    & ERTS_PDSFLGS_QMASK);
	queue = ERTS_ENQUEUE_DIRTY_CPU_QUEUE;
    }
    else {
	ASSERT(actual & ERTS_PSFLG_DIRTY_IO_PROC);
	max_qbit = ((dact >> ERTS_PDSFLGS_IN_IO_PRQ_MASK_OFFSET)
		    & ERTS_PDSFLGS_QMASK);
	queue = ERTS_ENQUEUE_DIRTY_IO_QUEUE;
    }

    max_qbit |= 1 << ERTS_PSFLGS_QMASK_BITS;
    max_qbit &= -max_qbit;

    if (qbit >= max_qbit)
	return ERTS_ENQUEUE_NOT; /* Already queued in higher or equal prio */
    if ((actual & (ERTS_PSFLG_IN_RUNQ|ERTS_PSFLGS_USR_PRIO_MASK))
	!= (aprio << ERTS_PSFLGS_USR_PRIO_OFFSET)) {
	/*
	 * Process struct already enqueued, or actual prio not
	 * equal to user prio, i.e., enqueue using proxy.
	 */
	return -1*queue;
    }

    /*
     * Enqueue using process struct.
     */
    *newp &= ~ERTS_PSFLGS_PRQ_PRIO_MASK;
    *newp |= ERTS_PSFLG_IN_RUNQ | (aprio << ERTS_PSFLGS_PRQ_PRIO_OFFSET);
    return queue;
}

static ERTS_INLINE int
fin_dirty_enq_s_change(Process *p,
		       int pstruct_reserved,
		       erts_aint32_t enq_prio,
		       int qmask_offset)
{
    erts_aint32_t qbit = 1 << enq_prio;
    qbit <<= qmask_offset;

    if (qbit & erts_smp_atomic32_read_bor_mb(&p->dirty_state, qbit)) {
	/* Already enqueue by someone else... */
	if (pstruct_reserved) {
	    /* We reserved process struct for enqueue; clear it... */
#ifdef DEBUG
	    erts_aint32_t old =
#else
		(void)
#endif
		erts_smp_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_IN_RUNQ);
	    ASSERT(old & ERTS_PSFLG_IN_RUNQ);
	}
	return 0;
    }

    return !0;
}

#endif /* ERTS_DIRTY_SCHEDULERS */

static ERTS_INLINE int
check_enqueue_in_prio_queue(Process *c_p,
			    erts_aint32_t *prq_prio_p,
			    erts_aint32_t *newp,
			    erts_aint32_t actual)
{
    erts_aint32_t aprio, qbit, max_qbit;

    aprio = (actual >> ERTS_PSFLGS_ACT_PRIO_OFFSET) & ERTS_PSFLGS_PRIO_MASK;
    qbit = 1 << aprio;

    *prq_prio_p = aprio;

#ifdef ERTS_DIRTY_SCHEDULERS
    if (actual & ERTS_PSFLGS_DIRTY_WORK) {
	int res = check_dirty_enqueue_in_prio_queue(c_p, newp, actual,
						    aprio, qbit);
	if (res != ERTS_ENQUEUE_NORMAL_QUEUE)
	    return res;
    }
#endif

    max_qbit = (actual >> ERTS_PSFLGS_IN_PRQ_MASK_OFFSET) & ERTS_PSFLGS_QMASK;
    max_qbit |= 1 << ERTS_PSFLGS_QMASK_BITS;
    max_qbit &= -max_qbit;
    /*
     * max_qbit now either contain bit set for highest prio queue or a bit
     * out of range (which will have a value larger than valid range).
     */

    if (qbit >= max_qbit)
	return ERTS_ENQUEUE_NOT; /* Already queued in higher or equal prio */

    /* Need to enqueue (if already enqueued, it is in lower prio) */
    *newp |= qbit << ERTS_PSFLGS_IN_PRQ_MASK_OFFSET;

    if ((actual & (ERTS_PSFLG_IN_RUNQ|ERTS_PSFLGS_USR_PRIO_MASK))
	!= (aprio << ERTS_PSFLGS_USR_PRIO_OFFSET)) {
	/*
	 * Process struct already enqueued, or actual prio not
	 * equal to user prio, i.e., enqueue using proxy.
	 */
	return -ERTS_ENQUEUE_NORMAL_QUEUE;
    }

    /*
     * Enqueue using process struct.
     */
    *newp &= ~ERTS_PSFLGS_PRQ_PRIO_MASK;
    *newp |= ERTS_PSFLG_IN_RUNQ | (aprio << ERTS_PSFLGS_PRQ_PRIO_OFFSET);
    return ERTS_ENQUEUE_NORMAL_QUEUE;
}

static ERTS_INLINE ErtsRunQueue *
select_enqueue_run_queue(int enqueue, int enq_prio, Process *p, erts_aint32_t state)
{

    switch (enqueue) {

    case ERTS_ENQUEUE_NOT:

	return NULL;

#ifdef ERTS_DIRTY_SCHEDULERS

    case ERTS_ENQUEUE_DIRTY_CPU_QUEUE:
    case -ERTS_ENQUEUE_DIRTY_CPU_QUEUE:

	if (fin_dirty_enq_s_change(p, enqueue > 0, enq_prio,
				   ERTS_PDSFLGS_IN_CPU_PRQ_MASK_OFFSET))
	    return ERTS_DIRTY_CPU_RUNQ;

	return NULL;


    case ERTS_ENQUEUE_DIRTY_IO_QUEUE:
    case -ERTS_ENQUEUE_DIRTY_IO_QUEUE:

	if (fin_dirty_enq_s_change(p, enqueue > 0, enq_prio,
				   ERTS_PDSFLGS_IN_IO_PRQ_MASK_OFFSET))
	    return ERTS_DIRTY_IO_RUNQ;

	return NULL;

#endif

    default: {
	ErtsRunQueue* runq;

	ASSERT(enqueue == ERTS_ENQUEUE_NORMAL_QUEUE
	       || enqueue == -ERTS_ENQUEUE_NORMAL_QUEUE);

	runq = erts_get_runq_proc(p);

#ifdef ERTS_SMP
	if (!(ERTS_PSFLG_BOUND & state)) {
	    ErtsRunQueue *new_runq = erts_check_emigration_need(runq, enq_prio);
	    if (new_runq) {
		RUNQ_SET_RQ(&p->run_queue, new_runq);
		runq = new_runq;
	    }
	}
#endif

	ASSERT(runq);

	return runq;
    }
    }
}


/*
 * schedule_out_process() return with c_rq locked.
 *
 * Return non-zero value if caller should decrease
 * reference count on the process when done with it...
 */
static ERTS_INLINE int
schedule_out_process(ErtsRunQueue *c_rq, erts_aint32_t state, Process *p,
		     Process *proxy, int is_normal_sched)
{
    erts_aint32_t a, e, n, enq_prio = -1, running_flgs;
    int enqueue; /* < 0 -> use proxy */
    ErtsRunQueue* runq;

    if (!is_normal_sched)
	running_flgs = ERTS_PSFLG_DIRTY_RUNNING|ERTS_PSFLG_DIRTY_RUNNING_SYS;
    else {
	running_flgs = ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS;
#ifdef ERTS_DIRTY_SCHEDULERS
        if (state & ERTS_PSFLG_DIRTY_ACTIVE_SYS
            && (p->flags & (F_DELAY_GC|F_DISABLE_GC))) {
            /*
             * Delay dirty GC; will be enabled automatically
             * again by next GC...
             */

            /*
             * No normal execution until dirty CLA or hibernat has
             * been handled...
             */
            ASSERT(!(p->flags & (F_DIRTY_CLA | F_DIRTY_GC_HIBERNATE)));

            state = erts_smp_atomic32_read_band_nob(&p->state,
                                                    ~ERTS_PSFLG_DIRTY_ACTIVE_SYS);
            state &= ~ERTS_PSFLG_DIRTY_ACTIVE_SYS;
        }
#endif
    }

    a = state;

    while (1) {
	n = e = a;

	ASSERT(a & running_flgs);

	enqueue = ERTS_ENQUEUE_NOT;

	n &= ~running_flgs;
	if ((a & (ERTS_PSFLG_ACTIVE_SYS|ERTS_PSFLG_DIRTY_ACTIVE_SYS))
	    || (a & (ERTS_PSFLG_ACTIVE|ERTS_PSFLG_SUSPENDED)) == ERTS_PSFLG_ACTIVE) {
	    enqueue = check_enqueue_in_prio_queue(p, &enq_prio, &n, a);
	}
	a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
    }

    runq = select_enqueue_run_queue(enqueue, enq_prio, p, n);

    if (!runq) {

	if (erts_system_profile_flags.runnable_procs) {

	    /* Status lock prevents out of order "runnable proc" trace msgs */
	    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

	    if (!(a & (ERTS_PSFLG_ACTIVE_SYS|ERTS_PSFLG_DIRTY_ACTIVE_SYS))
		&& (!(a & ERTS_PSFLG_ACTIVE) || (a & ERTS_PSFLG_SUSPENDED))) {
		/* Process inactive */
		profile_runnable_proc(p, am_inactive);
	    }
	}

	if (proxy)
	    free_proxy_proc(proxy);

	erts_smp_runq_lock(c_rq);

#if !defined(ERTS_SMP)
        /* Decrement refc if process struct is free... */
        return !!(n & ERTS_PSFLG_FREE);
#else
        /* Decrement refc if scheduled out from dirty scheduler... */
	return !is_normal_sched;
#endif
    }
    else {
	Process* sched_p;

        ASSERT(!(n & ERTS_PSFLG_FREE));
	ASSERT(!(n & ERTS_PSFLG_SUSPENDED) || (n & (ERTS_PSFLG_ACTIVE_SYS
						    | ERTS_PSFLG_DIRTY_ACTIVE_SYS)));

	if (enqueue < 0)
	    sched_p = make_proxy_proc(proxy, p, enq_prio);
	else {
	    sched_p = p;
	    if (proxy)
		free_proxy_proc(proxy);
	}

	ASSERT(runq);

	erts_smp_runq_lock(runq);

        if (is_normal_sched && sched_p == p && ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
            erts_proc_inc_refc(p); /* Needs to be done before enqueue_process() */

	/* Enqueue the process */
	enqueue_process(runq, (int) enq_prio, sched_p);

	if (runq == c_rq)
	    return 0;

	erts_smp_runq_unlock(runq);

	smp_notify_inc_runq(runq);

	erts_smp_runq_lock(c_rq);

        /*
         * Decrement refc if process is scheduled out by a
         * dirty scheduler, and we have not just scheduled
         * the process using the ordinary process struct
         * on a dirty run-queue again...
         */
        return !is_normal_sched && (sched_p != p
                                    || !ERTS_RUNQ_IX_IS_DIRTY(runq->ix));
    }
}

static ERTS_INLINE void
add2runq(int enqueue, erts_aint32_t prio,
	 Process *proc, erts_aint32_t state,
	 Process **proxy)
{
    ErtsRunQueue *runq;

    runq = select_enqueue_run_queue(enqueue, prio, proc, state);

    if (runq) {
	Process *sched_p;

	if (enqueue > 0) {
	    sched_p = proc;
	    /*
	     * Refc on process struct (i.e. true struct,
	     * not proxy-struct) increased while in a
	     * dirty run-queue or executing on a dirty
	     * scheduler.
	     */
            if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
                erts_proc_inc_refc(proc);
        }
	else {
	    Process *pxy;

	    if (!proxy)
		pxy = NULL;
	    else {
		pxy = *proxy;
		*proxy = NULL;
	    }
	    sched_p = make_proxy_proc(pxy, proc, prio);
	}

	erts_smp_runq_lock(runq);

	/* Enqueue the process */
	enqueue_process(runq, (int) prio, sched_p);

	erts_smp_runq_unlock(runq);
	smp_notify_inc_runq(runq);
    }
}

static ERTS_INLINE int
change_proc_schedule_state(Process *p,
			   erts_aint32_t clear_state_flags,
			   erts_aint32_t set_state_flags,
			   erts_aint32_t *statep,
			   erts_aint32_t *enq_prio_p,
			   ErtsProcLocks locks)
{
    /*
     * NOTE: ERTS_PSFLG_RUNNING, ERTS_PSFLG_RUNNING_SYS,
     *       ERTS_PSFLG_DIRTY_RUNNING, ERTS_PSFLG_DIRTY_RUNNING_SYS
     *       and ERTS_PSFLG_ACTIVE_SYS are not allowed to be
     *       altered by this function!
     */
    erts_aint32_t a = *statep, n;
    int enqueue; /* < 0 -> use proxy */
    unsigned int prof_runnable_procs = erts_system_profile_flags.runnable_procs;
    unsigned int lock_status = (prof_runnable_procs
				&& !(locks & ERTS_PROC_LOCK_STATUS));

    ERTS_SMP_LC_ASSERT(locks == erts_proc_lc_my_proc_locks(p));

    ASSERT(!(a & ERTS_PSFLG_PROXY));
    ASSERT((clear_state_flags & (ERTS_PSFLG_RUNNING
				 | ERTS_PSFLG_RUNNING_SYS
				 | ERTS_PSFLG_DIRTY_RUNNING
				 | ERTS_PSFLG_DIRTY_RUNNING_SYS
				 | ERTS_PSFLG_ACTIVE_SYS)) == 0);
    ASSERT((set_state_flags & (ERTS_PSFLG_RUNNING
			       | ERTS_PSFLG_RUNNING_SYS
			       | ERTS_PSFLG_DIRTY_RUNNING
			       | ERTS_PSFLG_DIRTY_RUNNING_SYS
			       | ERTS_PSFLG_ACTIVE_SYS)) == 0);

    if (lock_status)
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);

    while (1) {
	erts_aint32_t e;
	n = e = a;

	enqueue = ERTS_ENQUEUE_NOT;

	if (a & ERTS_PSFLG_FREE)
	    break; /* We don't want to schedule free processes... */

	if (clear_state_flags)
	    n &= ~clear_state_flags;

	if (set_state_flags)
	    n |= set_state_flags;

	if ((n & (ERTS_PSFLG_SUSPENDED
		  | ERTS_PSFLG_RUNNING
		  | ERTS_PSFLG_RUNNING_SYS
		  | ERTS_PSFLG_DIRTY_RUNNING
		  | ERTS_PSFLG_DIRTY_RUNNING_SYS
		  | ERTS_PSFLG_IN_RUNQ
		  | ERTS_PSFLG_ACTIVE)) == ERTS_PSFLG_ACTIVE
#ifdef ERTS_DIRTY_SCHEDULERS
	    || (n & (ERTS_PSFLG_RUNNING
		     | ERTS_PSFLG_RUNNING_SYS
		     | ERTS_PSFLG_EXITING)) == ERTS_PSFLG_EXITING
#endif
	    ) {
	    /*
	     * Active and seemingly need to be enqueued, but
	     * process may be in a run queue via proxy, need
	     * further inspection...
	     */
	    enqueue = check_enqueue_in_prio_queue(p, enq_prio_p, &n, a);
	}

	a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
	if (enqueue == ERTS_ENQUEUE_NOT && n == a)
	    break;
    }

    if (prof_runnable_procs) {

	/* Status lock prevents out of order "runnable proc" trace msgs */

	if (((n & (ERTS_PSFLG_SUSPENDED
		   | ERTS_PSFLG_ACTIVE)) == ERTS_PSFLG_ACTIVE)
	    && (!(a & (ERTS_PSFLG_ACTIVE_SYS
		       | ERTS_PSFLG_RUNNING
		       | ERTS_PSFLG_RUNNING_SYS
		       | ERTS_PSFLG_DIRTY_RUNNING
		       | ERTS_PSFLG_DIRTY_RUNNING_SYS)
		  && (!(a & ERTS_PSFLG_ACTIVE)
		      || (a & ERTS_PSFLG_SUSPENDED))))) {
	    /* We activated a prevously inactive process */
	    profile_runnable_proc(p, am_active);
	}

	if (lock_status)
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }


    *statep = a;

    return enqueue;
}

static ERTS_INLINE void
schedule_process(Process *p, erts_aint32_t in_state, ErtsProcLocks locks)
{
    erts_aint32_t enq_prio  = -1;
    erts_aint32_t state = in_state;
    int enqueue = change_proc_schedule_state(p,
					     0,
					     ERTS_PSFLG_ACTIVE,
					     &state,
					     &enq_prio,
					     locks);
    add2runq(enqueue, enq_prio, p, state, NULL);
}

void
erts_schedule_process(Process *p, erts_aint32_t state, ErtsProcLocks locks)
{
    schedule_process(p, state, locks);
}

static int
schedule_process_sys_task(Process *p, erts_aint32_t prio, ErtsProcSysTask *st,
			  erts_aint32_t *fail_state_p)
{
    int res;
    int locked;
    ErtsProcSysTaskQs *stqs, *free_stqs;
    erts_aint32_t fail_state, state, a, n, enq_prio;
    int enqueue; /* < 0 -> use proxy */
    unsigned int prof_runnable_procs;

    fail_state = *fail_state_p;

    res = 1; /* prepare for success */
    st->next = st->prev = st; /* Prep for empty prio queue */
    state = erts_smp_atomic32_read_nob(&p->state);
    prof_runnable_procs = erts_system_profile_flags.runnable_procs;
    locked = 0;
    free_stqs = NULL;
    if (state & ERTS_PSFLG_ACTIVE_SYS)
	stqs = NULL;
    else {
    alloc_qs:
	stqs = proc_sys_task_queues_alloc();
	stqs->qmask = 1 << prio;
	stqs->ncount = 0;
	stqs->q[PRIORITY_MAX] = NULL;
	stqs->q[PRIORITY_HIGH] = NULL;
	stqs->q[PRIORITY_NORMAL] = NULL;
	stqs->q[PRIORITY_LOW] = NULL;
	stqs->q[prio] = st;
    }

    if (!locked) {
	locked = 1;
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);

	state = erts_smp_atomic32_read_nob(&p->state);
	if (state & fail_state) {
	    *fail_state_p = (state & fail_state);
	    free_stqs = stqs;
	    res = 0;
	    goto cleanup;
	}
    }

    if (!p->sys_task_qs) {
	if (stqs)
	    p->sys_task_qs = stqs;
	else
	    goto alloc_qs;
    }
    else {
	free_stqs = stqs;
	stqs = p->sys_task_qs;
	if (!stqs->q[prio]) {
	    stqs->q[prio] = st;
	    stqs->qmask |= 1 << prio;
	}
	else {
	    st->next = stqs->q[prio];
	    st->prev = stqs->q[prio]->prev;
	    st->next->prev = st;
	    st->prev->next = st;
	    ASSERT(stqs->qmask & (1 << prio));
	}
    }

    if (ERTS_PSFLGS_GET_ACT_PRIO(state) > prio) {
	erts_aint32_t n, a, e;
	/* Need to elevate actual prio */

	a = state;
	do {
	    if (ERTS_PSFLGS_GET_ACT_PRIO(a) <= prio) {
		n = a;
		break;
	    }
	    n = e = a;
	    n &= ~ERTS_PSFLGS_ACT_PRIO_MASK;
	    n |= (prio << ERTS_PSFLGS_ACT_PRIO_OFFSET);
	    a = erts_smp_atomic32_cmpxchg_nob(&p->state, n, e);    
	} while (a != e);
	state = n;
    }


    a = state;
    enq_prio = -1;

    /* Status lock prevents out of order "runnable proc" trace msgs */
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    if (!prof_runnable_procs) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	locked = 0;
    }

    ASSERT(!(state & ERTS_PSFLG_PROXY));

    while (1) {
	erts_aint32_t e;
	n = e = a;

	if (a & ERTS_PSFLG_FREE)
	    goto cleanup; /* We don't want to schedule free processes... */

	enqueue = ERTS_ENQUEUE_NOT;
	n |= ERTS_PSFLG_ACTIVE_SYS;
	if (!(a & (ERTS_PSFLG_RUNNING
		   | ERTS_PSFLG_RUNNING_SYS
		   | ERTS_PSFLG_DIRTY_RUNNING
		   | ERTS_PSFLG_DIRTY_RUNNING_SYS)))
	    enqueue = check_enqueue_in_prio_queue(p, &enq_prio, &n, a);
	a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
	if (a == n && enqueue == ERTS_ENQUEUE_NOT)
	    goto cleanup;
    }

    if (prof_runnable_procs) {

	if (!(a & (ERTS_PSFLG_ACTIVE_SYS
		   | ERTS_PSFLG_RUNNING
		   | ERTS_PSFLG_RUNNING_SYS
		   | ERTS_PSFLG_DIRTY_RUNNING
		   | ERTS_PSFLG_DIRTY_RUNNING_SYS))
	    && (!(a & ERTS_PSFLG_ACTIVE) || (a & ERTS_PSFLG_SUSPENDED))) {
	    /* We activated a prevously inactive process */
	    profile_runnable_proc(p, am_active);
	}

	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	locked = 0;
    }

    add2runq(enqueue, enq_prio, p, n, NULL);

cleanup:

    if (locked)
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    if (free_stqs)
	proc_sys_task_queues_free(free_stqs);

    ERTS_SMP_LC_ASSERT(!(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p)));

    return res;
}

static ERTS_INLINE int
suspend_process(Process *c_p, Process *p)
{
    erts_aint32_t state;
    int suspended = 0;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    state = erts_smp_atomic32_read_acqb(&p->state);

    if ((state & ERTS_PSFLG_SUSPENDED))
	suspended = -1;
    else {
	if (c_p == p) {
	    state = erts_smp_atomic32_read_bor_relb(&p->state,
						    ERTS_PSFLG_SUSPENDED);
	    ASSERT(state & (ERTS_PSFLG_RUNNING
			    | ERTS_PSFLG_RUNNING_SYS
			    | ERTS_PSFLG_DIRTY_RUNNING
			    | ERTS_PSFLG_DIRTY_RUNNING_SYS));
	    suspended = (state & ERTS_PSFLG_SUSPENDED) ? -1: 1;
	}
	else {
	    while (!(state & (ERTS_PSFLG_RUNNING
			      | ERTS_PSFLG_DIRTY_RUNNING
			      | ERTS_PSFLG_EXITING))) {
		erts_aint32_t n, e;

		n = e = state;
		n |= ERTS_PSFLG_SUSPENDED;
		state = erts_smp_atomic32_cmpxchg_relb(&p->state, n, e);
		if (state == e) {
		    suspended = 1;
		    break;
		}
		if (state & ERTS_PSFLG_SUSPENDED) {
		    suspended = -1;
		    break;
		}
	    }
	}
    }

    if (suspended) {

	if (suspended > 0 && erts_system_profile_flags.runnable_procs) {

	    /* 'state' is before our change... */

	    if ((state & (ERTS_PSFLG_ACTIVE
			  | ERTS_PSFLG_ACTIVE_SYS
			  | ERTS_PSFLG_DIRTY_ACTIVE_SYS
			  | ERTS_PSFLG_RUNNING
			  | ERTS_PSFLG_RUNNING_SYS
			  | ERTS_PSFLG_DIRTY_RUNNING
			  | ERTS_PSFLG_DIRTY_RUNNING_SYS
			  | ERTS_PSFLG_SUSPENDED)) == ERTS_PSFLG_ACTIVE) {
		/* We made process inactive */
		profile_runnable_proc(p, am_inactive);
	    }

	}

	p->rcount++;  /* count number of suspend */
    }

    return suspended;
}

static ERTS_INLINE void
resume_process(Process *p, ErtsProcLocks locks)
{
    erts_aint32_t state, enq_prio = -1;
    int enqueue;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    ASSERT(p->rcount > 0);

    if (--p->rcount > 0)  /* multiple suspend */
	return;

    state = erts_smp_atomic32_read_nob(&p->state);
    enqueue = change_proc_schedule_state(p,
					 ERTS_PSFLG_SUSPENDED,
					 0,
					 &state,
					 &enq_prio,
					 locks);
    add2runq(enqueue, enq_prio, p, state, NULL);
}

#ifdef ERTS_SMP

static ERTS_INLINE void
sched_resume_wake__(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t xflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_TSE_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);
    erts_aint32_t oflgs;
    do {
	oflgs = erts_smp_atomic32_cmpxchg_relb(&ssi->flags, 0, xflgs);
	if (oflgs == xflgs) {
	    erts_sched_finish_poke(ssi, oflgs);
	    break;
	}
	xflgs = oflgs;
    } while (oflgs & (ERTS_SSI_FLG_MSB_EXEC|ERTS_SSI_FLG_SUSPENDED));
}

static void
nrml_sched_ix_resume_wake(Uint ix)
{
    sched_resume_wake__(ERTS_SCHED_SLEEP_INFO_IX(ix));
}

#ifdef ERTS_DIRTY_SCHEDULERS

static void
dcpu_sched_ix_resume_wake(Uint ix)
{
    sched_resume_wake__(ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix));
}

static void
dio_sched_ix_resume_wake(Uint ix)
{
    sched_resume_wake__(ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(ix));
}

#endif

static erts_aint32_t
sched_prep_spin_suspended(ErtsSchedulerSleepInfo *ssi, erts_aint32_t xpct)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);
    erts_aint32_t xflgs = xpct;

    do {
	oflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return nflgs;
	xflgs = oflgs;
    } while (oflgs & ERTS_SSI_FLG_SUSPENDED);

    return oflgs;
}

static erts_aint32_t
sched_spin_suspended(ErtsSchedulerSleepInfo *ssi, int spincount)
{
    int until_yield = ERTS_SCHED_SPIN_UNTIL_YIELD;
    int sc = spincount;
    erts_aint32_t flgs;

    do {
	flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
	if ((flgs & (ERTS_SSI_FLG_SLEEPING
		     | ERTS_SSI_FLG_WAITING
		     | ERTS_SSI_FLG_SUSPENDED))
	    != (ERTS_SSI_FLG_SLEEPING
		| ERTS_SSI_FLG_WAITING
		| ERTS_SSI_FLG_SUSPENDED)) {
	    break;
	}
	ERTS_SPIN_BODY;
	if (--until_yield == 0) {
	    until_yield = ERTS_SCHED_SPIN_UNTIL_YIELD;
	    erts_thr_yield();
	}
    } while (--sc > 0);
    return flgs;
}

static erts_aint32_t
sched_set_suspended_sleeptype(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_TSE_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);
    erts_aint32_t xflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);

    erts_tse_reset(ssi->event);

    while (1) {
	oflgs = erts_smp_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
	if (oflgs == xflgs)
	    return nflgs;
	if ((oflgs & (ERTS_SSI_FLG_SLEEPING
		      | ERTS_SSI_FLG_WAITING
		      | ERTS_SSI_FLG_SUSPENDED))
	    != (ERTS_SSI_FLG_SLEEPING
		| ERTS_SSI_FLG_WAITING
		| ERTS_SSI_FLG_SUSPENDED)) {
	    return oflgs;
	}
	xflgs = oflgs;
    }
}

static void
init_scheduler_suspend(void)
{
    erts_smp_mtx_init(&schdlr_sspnd.mtx, "schdlr_sspnd");
    schdlr_sspnd.online.normal = 1;
    schdlr_sspnd.curr_online.normal = 1;
    schdlr_sspnd.active.normal = 1;
#ifdef ERTS_DIRTY_SCHEDULERS
    schdlr_sspnd.online.dirty_cpu = 0;
    schdlr_sspnd.curr_online.dirty_cpu = 0;
    schdlr_sspnd.active.dirty_cpu = 0;
    schdlr_sspnd.online.dirty_io = 0;
    schdlr_sspnd.curr_online.dirty_io = 0;
    schdlr_sspnd.active.dirty_io = 0;
    schdlr_sspnd.last_msb_dirty_type = ERTS_SCHED_DIRTY_IO;
#endif
    erts_smp_atomic32_init_nob(&schdlr_sspnd.changing, 0);
    schdlr_sspnd.chngq = NULL;
    schdlr_sspnd.changer = am_false;
    schdlr_sspnd.nmsb.ongoing = 0;
    schdlr_sspnd.nmsb.blckrs = NULL;
    schdlr_sspnd.nmsb.chngq = NULL;
    schdlr_sspnd.msb.ongoing = 0;
    schdlr_sspnd.msb.blckrs = NULL;
    schdlr_sspnd.msb.chngq = NULL;
}

typedef struct {
    struct {
	Eterm chngr;
	Eterm nxt;
    } onln;
    struct {
	ErtsProcList *chngrs;
    } msb;
} ErtsSchdlrSspndResume;

static void
schdlr_sspnd_resume_proc(ErtsSchedType sched_type, Eterm pid)
{
    Process *p;
    p = erts_pid2proc_opt(NULL, 0, pid, ERTS_PROC_LOCK_STATUS,
                          (sched_type != ERTS_SCHED_NORMAL
                           ? ERTS_P2P_FLG_INC_REFC
                           : 0));
    if (p) {
	resume_process(p, ERTS_PROC_LOCK_STATUS);
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
        if (sched_type != ERTS_SCHED_NORMAL)
            erts_proc_dec_refc(p);
    }
}

static ERTS_INLINE void
schdlr_sspnd_resume_procs(ErtsSchedType sched_type,
			  ErtsSchdlrSspndResume *resume)
{
    if (is_internal_pid(resume->onln.chngr)) {
	schdlr_sspnd_resume_proc(sched_type,
                                 resume->onln.chngr);
	resume->onln.chngr = NIL;
    }
    if (is_internal_pid(resume->onln.nxt)) {
	schdlr_sspnd_resume_proc(sched_type,
                                 resume->onln.nxt);
	resume->onln.nxt = NIL;
    }
    while (resume->msb.chngrs) {
	ErtsProcList *plp = resume->msb.chngrs;
	resume->msb.chngrs = plp->next;
	schdlr_sspnd_resume_proc(sched_type,
                                 plp->pid);
	proclist_destroy(plp);
    }
}

#ifdef ERTS_DIRTY_SCHEDULERS

static ERTS_INLINE int
have_dirty_work(void)
{
    return !(ERTS_EMPTY_RUNQ(ERTS_DIRTY_CPU_RUNQ)
             | ERTS_EMPTY_RUNQ(ERTS_DIRTY_IO_RUNQ));
}

#define ERTS_MSB_NONE_PRIO_BIT PORT_BIT

static ERTS_INLINE Uint32
msb_runq_prio_bit(Uint32 flgs)
{
    int pbit;

    pbit = (int) (flgs & ERTS_RUNQ_FLGS_PROCS_QMASK);
    if (flgs & PORT_BIT) {
        /* rate ports as proc prio high */
        pbit |= HIGH_BIT;
    }
    if (flgs & ERTS_RUNQ_FLG_MISC_OP) {
        /* rate misc ops as proc prio normal */
        pbit |= NORMAL_BIT;
    }
    if (flgs & LOW_BIT) {
        /* rate low prio as normal (avoid starvation) */
        pbit |= NORMAL_BIT;
    }
    if (!pbit)
        pbit = (int) ERTS_MSB_NONE_PRIO_BIT;
    else
        pbit &= -pbit; /* least significant bit set... */
    ASSERT(pbit);

    /* High prio low value; low prio high value... */
    return (Uint32) pbit;
}

static ERTS_INLINE void
msb_runq_prio_bits(Uint32 *nrmlp, Uint32 *dcpup, Uint32 *diop)
{
    Uint32 flgs = ERTS_RUNQ_FLGS_GET(ERTS_RUNQ_IX(0));
    if (flgs & ERTS_RUNQ_FLG_HALTING) {
        /*
         * Emulator is halting; only execute port jobs
         * on normal scheduler. Ensure that we switch
         * to the normal scheduler.
         */
        *nrmlp = HIGH_BIT;
        *dcpup = ERTS_MSB_NONE_PRIO_BIT;
        *diop = ERTS_MSB_NONE_PRIO_BIT;
    }
    else {
        *nrmlp = msb_runq_prio_bit(flgs);

        flgs = ERTS_RUNQ_FLGS_GET(ERTS_DIRTY_CPU_RUNQ);
        *dcpup = msb_runq_prio_bit(flgs);

        flgs = ERTS_RUNQ_FLGS_GET(ERTS_DIRTY_IO_RUNQ);
        *diop = msb_runq_prio_bit(flgs);
    }
}

static int
msb_scheduler_type_switch(ErtsSchedType sched_type,
                          ErtsSchedulerData *esdp,
                          long no)
{
    Uint32 nrml_prio, dcpu_prio, dio_prio;
    ErtsSchedType exec_type;
    ErtsRunQueue *exec_rq;
#ifdef DEBUG
    erts_aint32_t dbg_val;
#endif

    ASSERT(schdlr_sspnd.msb.ongoing);

    /*
     * This function determines how to switch
     * between scheduler types when multi-scheduling
     * is blocked.
     *
     * If no dirty work exist, we always select
     * execution of normal scheduler. If nothing
     * executes, normal scheduler 1 should be waiting
     * in sys_schedule(), otherwise we cannot react
     * on I/O events.
     *
     * We unconditionally switch back to normal
     * scheduler after executing dirty in order to
     * make sure we check for I/O...
     */

    msb_runq_prio_bits(&nrml_prio, &dcpu_prio, &dio_prio);

    exec_type = ERTS_SCHED_NORMAL;
    if (sched_type == ERTS_SCHED_NORMAL) {

        /*
         * Check priorities of work in the
         * different run-queues and determine
         * run-queue with highest prio job...
         */

        if ((dcpu_prio == ERTS_MSB_NONE_PRIO_BIT)
            & (dio_prio == ERTS_MSB_NONE_PRIO_BIT)) {
            /*
             * No dirty work exist; continue on normal
             * scheduler...
             */
            return 0;
        }

        if (dcpu_prio < nrml_prio) {
            exec_type = ERTS_SCHED_DIRTY_CPU;
            if (dio_prio < dcpu_prio)
                exec_type = ERTS_SCHED_DIRTY_IO;
        }
        else {
            if (dio_prio < nrml_prio)
                exec_type = ERTS_SCHED_DIRTY_IO;
        }

        /*
         * Make sure to alternate between dirty types
         * inbetween normal execution if highest 
         * priorities are equal.
         */

        if (exec_type == ERTS_SCHED_NORMAL) {
            if (dcpu_prio == nrml_prio)
                exec_type = ERTS_SCHED_DIRTY_CPU;
            else if (dio_prio == nrml_prio)
                exec_type = ERTS_SCHED_DIRTY_IO;
            else {
                /*
                 * Normal work has higher prio than
                 * dirty work; continue on normal
                 * scheduler...
                 */
                return 0;
            }
        }

        ASSERT(exec_type != ERTS_SCHED_NORMAL);
        if (dio_prio == dcpu_prio) {
            /* Alter between dirty types... */
            if (schdlr_sspnd.last_msb_dirty_type == ERTS_SCHED_DIRTY_IO)
                exec_type = ERTS_SCHED_DIRTY_CPU;
            else
                exec_type = ERTS_SCHED_DIRTY_IO;
        }
    }

    ASSERT(sched_type != exec_type);

    if (exec_type != ERTS_SCHED_NORMAL)
        schdlr_sspnd.last_msb_dirty_type = exec_type;
    else {
        erts_aint32_t calls;
        /*
         * Going back to normal scheduler after
         * dirty execution; make sure it will check
         * for I/O...
         */
        if (ERTS_USE_MODIFIED_TIMING())
            calls = ERTS_MODIFIED_TIMING_INPUT_REDS + 1;
        else
            calls = INPUT_REDUCTIONS + 1;
        erts_smp_atomic32_set_nob(&function_calls, calls);

        if ((nrml_prio == ERTS_MSB_NONE_PRIO_BIT)
            & ((dcpu_prio != ERTS_MSB_NONE_PRIO_BIT)
               | (dio_prio != ERTS_MSB_NONE_PRIO_BIT))) {
            /*
             * We have dirty work, but an empty
             * normal run-queue.
             *
             * Since the normal run-queue is
             * empty, the normal scheduler will
             * go to sleep when selected for
             * execution. We have dirty work to
             * do, so we only want it to check
             * I/O, and then come back here and
             * switch to dirty execution.
             *
             * To prevent the scheduler from going
             * to sleep we trick it into believing
             * it has work to do...
             */
            ERTS_RUNQ_FLGS_SET_NOB(ERTS_RUNQ_IX(0),
                                   ERTS_RUNQ_FLG_MISC_OP);
        }
    }

    /*
     * Suspend this scheduler and wake up scheduler
     * number one of another type...
     */
#ifdef DEBUG
    dbg_val =
#else
    (void)
#endif
        erts_smp_atomic32_read_bset_mb(&esdp->ssi->flags,
                                       (ERTS_SSI_FLG_SUSPENDED
                                        | ERTS_SSI_FLG_MSB_EXEC),
                                       ERTS_SSI_FLG_SUSPENDED);
    ASSERT(dbg_val & ERTS_SSI_FLG_MSB_EXEC);

    switch (exec_type) {
    case ERTS_SCHED_NORMAL:
        exec_rq = ERTS_RUNQ_IX(0);
        break;
    case ERTS_SCHED_DIRTY_CPU:
        exec_rq = ERTS_DIRTY_CPU_RUNQ;
        break;
    case ERTS_SCHED_DIRTY_IO:
        exec_rq = ERTS_DIRTY_IO_RUNQ;
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid scheduler type");
        exec_rq = NULL;
        break;
    }

#ifdef DEBUG
    dbg_val =
#else
    (void)
#endif
        erts_smp_atomic32_read_bset_mb(&exec_rq->scheduler->ssi->flags,
                                       (ERTS_SSI_FLG_SUSPENDED
                                        | ERTS_SSI_FLG_MSB_EXEC),
                                       ERTS_SSI_FLG_MSB_EXEC);
    ASSERT(dbg_val & ERTS_SSI_FLG_SUSPENDED);

    wake_scheduler(exec_rq);

    return 1; /* suspend this scheduler... */

}

#endif

static void
suspend_scheduler(ErtsSchedulerData *esdp)
{
    erts_aint32_t flgs;
    erts_aint32_t changing;
    long no;
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    int curr_online = 1;
    ErtsSchdlrSspndResume resume = {{NIL, NIL}, {NULL}};
    erts_aint32_t aux_work;
    int thr_prgr_active = 1;
    ErtsStuckBoundProcesses sbp = {NULL, NULL};
    ErtsSchedType sched_type;
    erts_aint32_t online_flag;

    /*
     * Schedulers may be suspended in two different ways:
     * - A scheduler may be suspended since it is not online.
     * - Multi scheduling is blocked. All schedulers except the
     *   scheduler with scheduler id 1 are suspended, and all
     *   dirty CPU and dirty I/O schedulers are suspended.
     *
     * Regardless of why a scheduler is suspended, it ends up here.
     */


#if !defined(ERTS_DIRTY_SCHEDULERS)

    sched_type = ERTS_SCHED_NORMAL;
    online_flag = ERTS_SCHDLR_SSPND_CHNG_ONLN;
    no = esdp->no;
    ASSERT(no != 1);

#else

    sched_type = esdp->type;
    switch (sched_type) {
    case ERTS_SCHED_NORMAL:
        online_flag = ERTS_SCHDLR_SSPND_CHNG_ONLN;
        no = esdp->no;
        break;
    case ERTS_SCHED_DIRTY_CPU:
        online_flag = ERTS_SCHDLR_SSPND_CHNG_DCPU_ONLN;
        no = esdp->dirty_no;
        break;
    case ERTS_SCHED_DIRTY_IO:
        online_flag = 0;
	no = esdp->dirty_no;
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid scheduler type");
        return;
    }

    if (erts_smp_atomic32_read_nob(&ssi->flags) & ERTS_SSI_FLG_MSB_EXEC) {
        ASSERT(no == 1);
        if (!msb_scheduler_type_switch(sched_type, esdp, no))
            return;
        /* Suspend and let scheduler 1 of another type execute... */
    }

#endif

    if (sched_type != ERTS_SCHED_NORMAL) {
	erts_smp_runq_unlock(esdp->run_queue);
        dirty_sched_wall_time_change(esdp, 0);
    }
    else {
        if (no != 1)
            evacuate_run_queue(esdp->run_queue, &sbp);

	erts_smp_runq_unlock(esdp->run_queue);

	erts_sched_check_cpu_bind_prep_suspend(esdp);

	if (erts_system_profile_flags.scheduler)
	    profile_scheduler(make_small(esdp->no), am_inactive);
    }

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    flgs = sched_prep_spin_suspended(ssi, ERTS_SSI_FLG_SUSPENDED);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {

	schdlr_sspnd_dec_nscheds(&schdlr_sspnd.active, sched_type);

	changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);

	while (1) {

	    if (changing & (ERTS_SCHDLR_SSPND_CHNG_NMSB
			    | ERTS_SCHDLR_SSPND_CHNG_MSB)) {
		int i = 0;
		ErtsMultiSchedulingBlock *msb[3] = {0};
		if (changing & ERTS_SCHDLR_SSPND_CHNG_NMSB)
		    msb[i++] = &schdlr_sspnd.nmsb;
		if (changing & ERTS_SCHDLR_SSPND_CHNG_MSB)
		    msb[i++] = &schdlr_sspnd.msb;

		for (i = 0; msb[i]; i++) {
		    erts_aint32_t clr_flg = 0;

		    if (msb[i] == &schdlr_sspnd.nmsb
			&& schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
						    ERTS_SCHED_NORMAL) == 1) {
			clr_flg = ERTS_SCHDLR_SSPND_CHNG_NMSB;
		    }
		    else if (schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                      ERTS_SCHED_NORMAL) == 1
                             && schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                         ERTS_SCHED_DIRTY_CPU) == 0
                             && schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                         ERTS_SCHED_DIRTY_IO) == 0) {
                        clr_flg = ERTS_SCHDLR_SSPND_CHNG_MSB;
		    }

		    if (clr_flg) {
			ErtsProcList *plp, *end_plp;
			changing = erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
								   ~clr_flg);
			changing &= ~clr_flg;
			(void) erts_proclist_fetch(&msb[i]->chngq, &end_plp);
			/* resume processes that initiated the multi scheduling block... */
			plp = msb[i]->chngq;
			if (plp) {
                            ASSERT(end_plp);
                            ASSERT(msb[i]->ongoing);
                            do {
                                erts_proclist_store_last(&msb[i]->blckrs,
                                                         proclist_copy(plp));
                                plp = plp->next;
                            } while (plp);
			    end_plp->next = resume.msb.chngrs;
                            resume.msb.chngrs = msb[i]->chngq;
                            msb[i]->chngq = NULL;
                        }
		    }
		}
	    }

	    if (changing & online_flag) {
		int changed = 0;
		Uint32 st_online;

		st_online = schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
						     sched_type);
		if (no > st_online && curr_online) {
		    schdlr_sspnd_dec_nscheds(&schdlr_sspnd.curr_online,
					     sched_type);
		    curr_online = 0;
		    changed = 1;
		}
		else if (no <= st_online && !curr_online) {
		    schdlr_sspnd_inc_nscheds(&schdlr_sspnd.curr_online,
					     sched_type);
		    curr_online = 1;
		    changed = 1;
		}
		if (changed
		    && (schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
						 sched_type)
			== schdlr_sspnd_get_nscheds(&schdlr_sspnd.curr_online,
						    sched_type))) {
		    ErtsProcList *plp;
		    changing = erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
							       ~online_flag);
		    changing &= ~online_flag;
		    if (sched_type == ERTS_SCHED_NORMAL) {
			ASSERT(is_internal_pid(schdlr_sspnd.changer)
			       || schdlr_sspnd.changer == am_init);
			/* resume process that initiated this change... */
			resume.onln.chngr = schdlr_sspnd.changer;
			plp = erts_proclist_peek_first(schdlr_sspnd.chngq);
			if (!plp)
			    schdlr_sspnd.changer = am_false;
			else {
			    schdlr_sspnd.changer = am_true; /* change right in transit */
			    /* resume process that is queued for next change... */
			    resume.onln.nxt = plp->pid;
			    ASSERT(is_internal_pid(resume.onln.nxt));
			}
		    }
		}
	    }

	    if (curr_online) {
		flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
		if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		    break;
	    }
	    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

	    schdlr_sspnd_resume_procs(sched_type, &resume);

	    while (1) {
		ErtsMonotonicTime current_time;
		erts_aint32_t flgs;

		if (sched_type != ERTS_SCHED_NORMAL)
		    aux_work = 0;
		else {
                    int evacuate = no == 1 ? 0 : !ERTS_EMPTY_RUNQ(esdp->run_queue);

		    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);

		    if (aux_work|evacuate) {
			if (!thr_prgr_active) {
			    erts_thr_progress_active(esdp, thr_prgr_active = 1);
			    sched_wall_time_change(esdp, 1);
			}
			if (aux_work)
			    aux_work = handle_aux_work(&esdp->aux_work_data,
						       aux_work,
						       1);

			if (aux_work && erts_thr_progress_update(esdp))
			    erts_thr_progress_leader_update(esdp);
			if (evacuate) {
			    erts_smp_runq_lock(esdp->run_queue);
			    evacuate_run_queue(esdp->run_queue, &sbp);
			    erts_smp_runq_unlock(esdp->run_queue);
			}
		    }

		}

		if (aux_work) {
		    ASSERT(sched_type == ERTS_SCHED_NORMAL);
		    current_time = erts_get_monotonic_time(esdp);
		    if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref)) {
			if (!thr_prgr_active) {
			    erts_thr_progress_active(esdp, thr_prgr_active = 1);
			    sched_wall_time_change(esdp, 1);
			}
			erts_bump_timers(esdp->timer_wheel, current_time);
		    }
		}
		else {
		    ErtsMonotonicTime timeout_time;
		    int do_timeout;

		    if (sched_type == ERTS_SCHED_NORMAL) {
			timeout_time = erts_check_next_timeout_time(esdp);
			current_time = erts_get_monotonic_time(esdp);
			do_timeout = (current_time >= timeout_time);
		    }
		    else {
			timeout_time = ERTS_MONOTONIC_TIME_MAX;
			current_time = 0;
			do_timeout = 0;
		    }

		    if (do_timeout) {
			ASSERT(sched_type == ERTS_SCHED_NORMAL);
			if (!thr_prgr_active) {
			    erts_thr_progress_active(esdp, thr_prgr_active = 1);
			    sched_wall_time_change(esdp, 1);
			}
		    }
		    else {
			if (sched_type == ERTS_SCHED_NORMAL) {
			    if (thr_prgr_active) {
				erts_thr_progress_active(esdp, thr_prgr_active = 0);
				sched_wall_time_change(esdp, 0);
			    }
			    erts_thr_progress_prepare_wait(esdp);
			}
			flgs = sched_spin_suspended(ssi,
						    ERTS_SCHED_SUSPEND_SLEEP_SPINCOUNT);
			if (flgs == (ERTS_SSI_FLG_SLEEPING
				     | ERTS_SSI_FLG_WAITING
				     | ERTS_SSI_FLG_SUSPENDED)) {
			    flgs = sched_set_suspended_sleeptype(ssi);
			    if (flgs == (ERTS_SSI_FLG_SLEEPING
					 | ERTS_SSI_FLG_TSE_SLEEPING
					 | ERTS_SSI_FLG_WAITING
					 | ERTS_SSI_FLG_SUSPENDED)) {
				int res;

				if (sched_type == ERTS_SCHED_NORMAL)
				    current_time = erts_get_monotonic_time(esdp);
				else
				    current_time = 0;

				do {
				    Sint64 timeout;
				    if (current_time >= timeout_time)
					break;
				    if (sched_type != ERTS_SCHED_NORMAL)
					timeout = -1;
				    else
					timeout = ERTS_MONOTONIC_TO_NSEC(timeout_time
									 - current_time
									 - 1) + 1;
				    res = erts_tse_twait(ssi->event, timeout);

				    if (sched_type == ERTS_SCHED_NORMAL)
					current_time = erts_get_monotonic_time(esdp);
				    else
					current_time = 0;

				} while (res == EINTR);
			    }
			}
			if (sched_type == ERTS_SCHED_NORMAL)
			    erts_thr_progress_finalize_wait(esdp);
		    }

		    if (current_time >= timeout_time) {
			ASSERT(sched_type == ERTS_SCHED_NORMAL);
			erts_bump_timers(esdp->timer_wheel, current_time);
		    }
		}

		flgs = sched_prep_spin_suspended(ssi, (ERTS_SSI_FLG_WAITING
						       | ERTS_SSI_FLG_SUSPENDED));
		if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		    break;
		changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
		if (changing)
		    break;
	    }

	    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
	    changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
	}

	schdlr_sspnd_inc_nscheds(&schdlr_sspnd.active, sched_type);
	changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
        if (changing) {
            if ((changing & ERTS_SCHDLR_SSPND_CHNG_MSB)
                && !schdlr_sspnd.msb.ongoing
                && schdlr_sspnd_eq_nscheds(&schdlr_sspnd.online,
                                           &schdlr_sspnd.active)) {
                erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
                                                ~ERTS_SCHDLR_SSPND_CHNG_MSB);
            }
            if ((changing & ERTS_SCHDLR_SSPND_CHNG_NMSB)
                && !schdlr_sspnd.nmsb.ongoing
                && (schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
                                             ERTS_SCHED_NORMAL)
                    == schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                ERTS_SCHED_NORMAL))) {
                erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
                                                ~ERTS_SCHDLR_SSPND_CHNG_NMSB);
            }
        }
	ASSERT(no <= schdlr_sspnd_get_nscheds(&schdlr_sspnd.online, sched_type));
    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    schdlr_sspnd_resume_procs(sched_type, &resume);

    ASSERT(curr_online);

    if (sched_type != ERTS_SCHED_NORMAL)
        dirty_sched_wall_time_change(esdp, 1);
    else {
        (void) erts_get_monotonic_time(esdp);
        if (erts_system_profile_flags.scheduler)
            profile_scheduler(make_small(esdp->no), am_active);

        if (!thr_prgr_active) {
            erts_thr_progress_active(esdp, thr_prgr_active = 1);
            sched_wall_time_change(esdp, 1);
        }
    }

    erts_smp_runq_lock(esdp->run_queue);
    non_empty_runq(esdp->run_queue);

    if (sched_type == ERTS_SCHED_NORMAL) {
	schedule_bound_processes(esdp->run_queue, &sbp);

	erts_sched_check_cpu_bind_post_suspend(esdp);
    }
}

void
erts_schedulers_state(Uint *total,
		      Uint *online,
		      Uint *active,
		      Uint *dirty_cpu,
		      Uint *dirty_cpu_online,
		      Uint *dirty_cpu_active,
		      Uint *dirty_io,
		      Uint *dirty_io_active)
{
    if (active || online || dirty_cpu_online
	|| dirty_cpu_active || dirty_io_active) {
	erts_smp_mtx_lock(&schdlr_sspnd.mtx);
	if (active)
	    *active = schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
					       ERTS_SCHED_NORMAL);
	if (online)
	    *online = schdlr_sspnd_get_nscheds(&schdlr_sspnd.curr_online,
					       ERTS_SCHED_NORMAL);
	if (dirty_cpu_active)
	    *dirty_cpu_active = schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
							 ERTS_SCHED_DIRTY_CPU);
	if (dirty_cpu_online)
	    *dirty_cpu_online = schdlr_sspnd_get_nscheds(&schdlr_sspnd.curr_online,
							 ERTS_SCHED_DIRTY_CPU);
	if (dirty_io_active)
	    *dirty_io_active = schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
							ERTS_SCHED_DIRTY_IO);
	erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    }

    if (total)
	*total = erts_no_schedulers;
    if (dirty_cpu)
	*dirty_cpu = erts_no_dirty_cpu_schedulers;
    if (dirty_io)
	*dirty_io = erts_no_dirty_io_schedulers;
}

static void
abort_sched_onln_chng_waitq(Process *p)
{
    Eterm resume = NIL;

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

#ifdef DEBUG
    {
	int found_it = 0;
	ErtsProcList *plp = erts_proclist_peek_first(schdlr_sspnd.chngq);
	while (plp) {
	    if (erts_proclist_same(plp, p))
		found_it++;
	    plp = erts_proclist_peek_next(schdlr_sspnd.chngq, plp);
	}
	ASSERT(found_it == !!(p->flags & F_SCHDLR_ONLN_WAITQ));
    }
#endif

    if (p->flags & F_SCHDLR_ONLN_WAITQ) {
	ErtsProcList *plp = NULL;

	plp = erts_proclist_peek_first(schdlr_sspnd.chngq);
	if (plp) {
	    if (erts_proclist_same(plp, p)
		&& schdlr_sspnd.changer == am_true) {
		p->flags &= ~F_SCHDLR_ONLN_WAITQ;
		/*
		 * Change right was in transit to us;
		 * transfer it to the next process by
		 * resuming it...
		 */
		erts_proclist_remove(&schdlr_sspnd.chngq, plp);
		proclist_destroy(plp);
		plp = erts_proclist_peek_first(schdlr_sspnd.chngq);
		if (plp)
		    resume = plp->pid;
		else
		    schdlr_sspnd.changer = am_false;
	    }
	    else {
		do {
		    if (erts_proclist_same(plp, p)) {
			p->flags &= ~F_SCHDLR_ONLN_WAITQ;
			erts_proclist_remove(&schdlr_sspnd.chngq, plp);
			proclist_destroy(plp);
			break;
		    }
		    plp = erts_proclist_peek_next(schdlr_sspnd.chngq, plp);
		} while (plp);
	    }
	}
    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    if (is_internal_pid(resume))
	schdlr_sspnd_resume_proc(ERTS_SCHED_NORMAL, resume);
}

ErtsSchedSuspendResult
erts_set_schedulers_online(Process *p,
			   ErtsProcLocks plocks,
			   Sint new_no,
			   Sint *old_no,
			   int dirty_only)
{
    int resume_proc, ix, res = -1, no, have_unlocked_plocks;
    erts_aint32_t changing = 0, change_flags;
    int online, increase;
    ErtsProcList *plp;
#ifdef ERTS_DIRTY_SCHEDULERS
    int dirty_no, change_dirty, dirty_online;
#else
    ASSERT(!dirty_only);
#endif

    if (new_no < 1)
	return ERTS_SCHDLR_SSPND_EINVAL;
    else if (dirty_only && erts_no_dirty_cpu_schedulers < new_no)
	return ERTS_SCHDLR_SSPND_EINVAL;
    else if (erts_no_schedulers < new_no)
	return ERTS_SCHDLR_SSPND_EINVAL;

#ifdef ERTS_DIRTY_SCHEDULERS
    if (dirty_only)
	resume_proc = 0;
    else
#endif
    {
	resume_proc = 1;
	/*
	 * If we suspend current process we need to suspend before
	 * requesting the change; otherwise, we got a resume/suspend
	 * race...
	 */
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	suspend_process(p, p);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    change_flags = 0;
    have_unlocked_plocks = 0;
    no = (int) new_no;

#ifdef ERTS_DIRTY_SCHEDULERS
    if (!dirty_only)
#endif
    {
	changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
	if (changing & ERTS_SCHDLR_SSPND_CHNG_ONLN) {
	enqueue_wait:
	    p->flags |= F_SCHDLR_ONLN_WAITQ;
	    plp = proclist_create(p);
	    erts_proclist_store_last(&schdlr_sspnd.chngq, plp);
	    resume_proc = 0;
	    res = ERTS_SCHDLR_SSPND_YIELD_RESTART;
	    goto done;
	}
	plp = erts_proclist_peek_first(schdlr_sspnd.chngq);
	if (!plp) {
	    ASSERT(schdlr_sspnd.changer == am_false);
	}
	else {
	    ASSERT(schdlr_sspnd.changer == am_true);
	    if (!erts_proclist_same(plp, p))
		goto enqueue_wait;
	    p->flags &= ~F_SCHDLR_ONLN_WAITQ;
	    erts_proclist_remove(&schdlr_sspnd.chngq, plp);
	    proclist_destroy(plp);
	}
    }

    *old_no = online = schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
						ERTS_SCHED_NORMAL);
#ifndef ERTS_DIRTY_SCHEDULERS
    if (no == online) {
	res = ERTS_SCHDLR_SSPND_DONE;
	goto done;
    }
#else /* ERTS_DIRTY_SCHEDULERS */
    dirty_online = schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
					    ERTS_SCHED_DIRTY_CPU);
    if (dirty_only)
	*old_no = dirty_online;

    ASSERT(dirty_online <= erts_no_dirty_cpu_schedulers);

    if (dirty_only) {
	if (no > online) {
	    res = ERTS_SCHDLR_SSPND_EINVAL;
	    goto done;
	}
	dirty_no = no;
	if (dirty_no == dirty_online) {
	    res = ERTS_SCHDLR_SSPND_DONE;
	    goto done;
	}
	change_dirty = 1;
    } else {
	/*
	 * Adjust the number of dirty CPU schedulers online relative to the
	 * adjustment made to the number of normal schedulers online.
	 */
	int total_pct = erts_no_dirty_cpu_schedulers*100/erts_no_schedulers;
	int onln_pct = no*total_pct/online;
	dirty_no = dirty_online*onln_pct/100;
	if (dirty_no == 0)
	    dirty_no = 1;
	ASSERT(dirty_no <= erts_no_dirty_cpu_schedulers);

	if (no != online)
	    change_dirty = (dirty_no != dirty_online);
	else {
	    dirty_only = 1;
	    if (dirty_no == dirty_online) {
		res = ERTS_SCHDLR_SSPND_DONE;
		goto done;
	    }
	    change_dirty = 1;
	}
    }
    if (change_dirty) {
	change_flags |= ERTS_SCHDLR_SSPND_CHNG_DCPU_ONLN;
	schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
				 ERTS_SCHED_DIRTY_CPU,
				 dirty_no);
    }

    if (dirty_only)
	increase = (dirty_no > dirty_online);
    else
#endif /* ERTS_DIRTY_SCHEDULERS */
    {
	change_flags |= ERTS_SCHDLR_SSPND_CHNG_ONLN;
	schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
				 ERTS_SCHED_NORMAL,
				 no);
	increase = (no > online);
    }

    erts_smp_atomic32_read_bor_nob(&schdlr_sspnd.changing, change_flags);

    res = ERTS_SCHDLR_SSPND_DONE;
    if (increase) {
	int ix;
#ifdef ERTS_DIRTY_SCHEDULERS
	if (change_dirty) {
	    ErtsSchedulerSleepInfo* ssi;
	    if (schdlr_sspnd.msb.ongoing) {
		for (ix = dirty_online; ix < dirty_no; ix++) {
		    ssi = ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix);
		    erts_sched_poke(ssi);
		}
	    } else {
		for (ix = dirty_online; ix < dirty_no; ix++)
		    dcpu_sched_ix_resume_wake(ix);
	    }
	}
	if (!dirty_only)
#endif
	{
	    if (schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing) {
		for (ix = online; ix < no; ix++)
		    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
	    }
	    else {
		if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_smp_proc_unlock(p, plocks);
		}
		change_no_used_runqs(no);

		for (ix = online; ix < no; ix++)
		    resume_run_queue(ERTS_RUNQ_IX(ix));

		for (ix = no; ix < erts_no_run_queues; ix++)
		    suspend_run_queue(ERTS_RUNQ_IX(ix));
	    }
	}
    }
    else /* if decrease */ {
#ifdef ERTS_DIRTY_SCHEDULERS
	if (change_dirty) {
	    if (schdlr_sspnd.msb.ongoing) {
		for (ix = dirty_no; ix < dirty_online; ix++)
		    erts_sched_poke(ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix));
	    }
            else {
		for (ix = dirty_no; ix < dirty_online; ix++)
                    dcpu_sched_ix_suspend_wake(ix);
                /*
                 * Newly suspended scheduler may have just been
                 * about to handle a task. Make sure someone takes
                 * care of such a task...
                 */
                dcpu_sched_ix_wake(0);
	    }
	}
	if (!dirty_only)
#endif
	{
	    if (schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing) {
		for (ix = no; ix < online; ix++)
		    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
	    }
	    else {
		if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_smp_proc_unlock(p, plocks);
		}

		change_no_used_runqs(no);
		for (ix = no; ix < erts_no_run_queues; ix++)
		    suspend_run_queue(ERTS_RUNQ_IX(ix));

		for (ix = no; ix < online; ix++) {
		    ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
		    wake_scheduler(rq);
		}
	    }
	}
    }

    if (change_flags & ERTS_SCHDLR_SSPND_CHNG_ONLN) {
	/* Suspend and wait for requested change to complete... */
	schdlr_sspnd.changer = p->common.id;
	resume_proc = 0;
	res = ERTS_SCHDLR_SSPND_YIELD_DONE;
    }

done:

    ASSERT(schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
				    ERTS_SCHED_DIRTY_CPU)
	   <= schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
				       ERTS_SCHED_NORMAL));

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    if (have_unlocked_plocks)
	erts_smp_proc_lock(p, plocks);

    if (resume_proc) {
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	resume_process(p, plocks|ERTS_PROC_LOCK_STATUS);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    return res;
}

ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *p, ErtsProcLocks plocks, int on, int normal, int all)
{
    int resume_proc, ix, res, have_unlocked_plocks = 0;
    ErtsProcList *plp;
    ErtsMultiSchedulingBlock *msbp;
    erts_aint32_t chng_flg;
    int have_blckd_flg;

    if (normal) {
	chng_flg = ERTS_SCHDLR_SSPND_CHNG_NMSB;
	have_blckd_flg = F_HAVE_BLCKD_NMSCHED;
	msbp = &schdlr_sspnd.nmsb;
    }
    else {
	chng_flg = ERTS_SCHDLR_SSPND_CHNG_MSB;
	have_blckd_flg = F_HAVE_BLCKD_MSCHED;
	msbp = &schdlr_sspnd.msb;
    }

    /*
     * If we suspend current process we need to suspend before
     * requesting the change; otherwise, we got a resume/suspend
     * race...
     */
    if (!on) {
	/* We never suspend current process when unblocking... */
	resume_proc = 0;
    }
    else {
	resume_proc = 1;
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	suspend_process(p, p);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    if (on) { /* ------ BLOCK ------ */
	if (msbp->chngq) {
	    ASSERT(msbp->ongoing);
	    p->flags |= have_blckd_flg;
	    goto wait_until_msb;
	}
	else if (msbp->blckrs || (normal && erts_no_schedulers == 1)) {
	    ASSERT(!msbp->blckrs || msbp->ongoing);
	    msbp->ongoing = 1;
	    plp = proclist_create(p);
	    erts_proclist_store_last(&msbp->blckrs, plp);
	    p->flags |= have_blckd_flg;
	    ASSERT(normal
                   ? 1 == schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                   ERTS_SCHED_NORMAL)
                   : schdlr_sspnd_get_nscheds_tot(&schdlr_sspnd.active) == 1);
	    ASSERT(erts_proc_sched_data(p)->no == 1);
	    if (schdlr_sspnd.msb.ongoing)
		res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
	    else
		res = ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED;
	}
        else {
	    int online = (int) schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
							ERTS_SCHED_NORMAL);
	    ASSERT(!msbp->ongoing);
	    p->flags |= have_blckd_flg;
	    if (plocks) {
		have_unlocked_plocks = 1;
		erts_smp_proc_unlock(p, plocks);
	    }
	    ASSERT(!msbp->ongoing);
	    msbp->ongoing = 1;

            erts_smp_atomic32_read_bor_nob(&schdlr_sspnd.changing,
                                           chng_flg);
            change_no_used_runqs(1);
            for (ix = 1; ix < erts_no_run_queues; ix++)
                suspend_run_queue(ERTS_RUNQ_IX(ix));

            for (ix = 1; ix < online; ix++) {
                ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
                wake_scheduler(rq);
            }

#ifdef ERTS_DIRTY_SCHEDULERS
            if (!normal) {
                ERTS_RUNQ_FLGS_SET_NOB(ERTS_RUNQ_IX(0), ERTS_RUNQ_FLG_MSB_EXEC);
                erts_smp_atomic32_read_bor_nob(&ERTS_RUNQ_IX(0)->scheduler->ssi->flags,
                                               ERTS_SSI_FLG_MSB_EXEC);
                for (ix = 0; ix < erts_no_dirty_cpu_schedulers; ix++)
                    dcpu_sched_ix_suspend_wake(ix);
                for (ix = 0; ix < erts_no_dirty_io_schedulers; ix++)
                    dio_sched_ix_suspend_wake(ix);
            }
#endif

        wait_until_msb:

            ASSERT(chng_flg & erts_smp_atomic32_read_nob(&schdlr_sspnd.changing));

            plp = proclist_create(p);
            erts_proclist_store_last(&msbp->chngq, plp);
            resume_proc = 0;
            if (schdlr_sspnd.msb.ongoing)
                res = ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED;
            else
                res = ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED;
	    ASSERT(erts_proc_sched_data(p));
	}
    }
    else if (!msbp->ongoing) {
	ASSERT(!msbp->blckrs);
	goto unblock_res;
    }
    else {  /* ------ UNBLOCK ------ */
	if (p->flags & have_blckd_flg) {
	    ErtsProcList **plpps[3] = {0};
	    ErtsProcList *plp;

            plpps[0] = &msbp->blckrs;
            if (all)
                plpps[1] = &msbp->chngq;

	    for (ix = 0; plpps[ix]; ix++) {
                plp = erts_proclist_peek_first(*plpps[ix]);
		while (plp) {
		    ErtsProcList *tmp_plp = plp;
		    plp = erts_proclist_peek_next(*plpps[ix], plp);
		    if (erts_proclist_same(tmp_plp, p)) {
			erts_proclist_remove(plpps[ix], tmp_plp);
			proclist_destroy(tmp_plp);
			if (!all)
			    break;
		    }
		}
	    }
	}
	if (!msbp->blckrs && !msbp->chngq) {
	    int online;
	    erts_smp_atomic32_read_bor_nob(&schdlr_sspnd.changing,
					   chng_flg);
	    p->flags &= ~have_blckd_flg;
	    msbp->ongoing = 0;
            if (!(schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing)) {
                if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_smp_proc_unlock(p, plocks);
		}

                online = (int) schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
							ERTS_SCHED_NORMAL);
		change_no_used_runqs(online);

		/* Resume all online run queues */
		for (ix = 1; ix < online; ix++)
		    resume_run_queue(ERTS_RUNQ_IX(ix));

		for (ix = online; ix < erts_no_run_queues; ix++)
		    suspend_run_queue(ERTS_RUNQ_IX(ix));
	    }
#ifdef ERTS_DIRTY_SCHEDULERS
            if (!schdlr_sspnd.msb.ongoing) {
                /* Get rid of msb-exec flag in run-queue of scheduler 1 */
                resume_run_queue(ERTS_RUNQ_IX(0));
		online = (int) schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
							ERTS_SCHED_DIRTY_CPU);
		for (ix = 0; ix < online; ix++)
		    dcpu_sched_ix_resume_wake(ix);
		for (ix = 0; ix < erts_no_dirty_io_schedulers; ix++)
		    dio_sched_ix_resume_wake(ix);
	    }
#endif
	}

    unblock_res:
	if (schdlr_sspnd.msb.ongoing)
	    res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
	else if (schdlr_sspnd.nmsb.ongoing)
	    res = ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED;
	else
	    res = ERTS_SCHDLR_SSPND_DONE;
    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    if (have_unlocked_plocks)
	erts_smp_proc_lock(p, plocks);

    if (resume_proc) {
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	resume_process(p, plocks|ERTS_PROC_LOCK_STATUS);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    return res;
}

int
erts_is_multi_scheduling_blocked(void)
{
    int res;
    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    if (schdlr_sspnd.msb.blckrs)
	res = 1;
    else if (schdlr_sspnd.nmsb.blckrs)
	res = -1;
    else
	res = 0;
    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    return res;
}

Eterm
erts_multi_scheduling_blockers(Process *p, int normal)
{
    Eterm res = NIL;
    ErtsMultiSchedulingBlock *msbp;

    msbp = normal ? &schdlr_sspnd.nmsb : &schdlr_sspnd.msb;

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    if (!erts_proclist_is_empty(msbp->blckrs)) {
	Eterm *hp, *hp_end;
	ErtsProcList *plp1, *plp2;
	Uint max_size = 0;

	for (plp1 = erts_proclist_peek_first(msbp->blckrs);
	     plp1;
	     plp1 = erts_proclist_peek_next(msbp->blckrs, plp1)) {
	    max_size += 2;
	}
	ASSERT(max_size);
	hp = HAlloc(p, max_size);
	hp_end = hp + max_size;
	for (plp1 = erts_proclist_peek_first(msbp->blckrs);
	     plp1;
	     plp1 = erts_proclist_peek_next(msbp->blckrs, plp1)) {
	    for (plp2 = erts_proclist_peek_first(msbp->blckrs);
		 plp2->pid != plp1->pid;
		 plp2 = erts_proclist_peek_next(msbp->blckrs, plp2));
	    if (plp2 == plp1) {
		res = CONS(hp, plp1->pid, res);
		hp += 2;
	    }
	    /* else: already in result list */
	}
	HRelease(p, hp_end, hp);
    }
    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    return res;
}

static void *
sched_thread_func(void *vesdp)
{
    ErtsThrPrgrCallbacks callbacks;
    ErtsSchedulerData *esdp = vesdp;
    Uint no = esdp->no;
#ifdef ERTS_SMP
    erts_tse_t *tse;
#endif

    erts_sched_init_time_sup(esdp);

    if (no == 1)
        erts_aux_work_timeout_late_init(esdp);

    (void) ERTS_RUNQ_FLGS_SET_NOB(esdp->run_queue,
				  ERTS_RUNQ_FLG_EXEC);

#ifdef ERTS_SMP
    tse = erts_tse_fetch();
    erts_tse_prepare_timed(tse);
    ERTS_SCHED_SLEEP_INFO_IX(no - 1)->event = tse;
    callbacks.arg = (void *) esdp->ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

    erts_msacc_init_thread("scheduler", no, 1);

    erts_thr_progress_register_managed_thread(esdp, &callbacks, 0);
    erts_alloc_register_scheduler(vesdp);
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "scheduler %beu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
    erts_tsd_set(sched_data_key, vesdp);
#ifdef ERTS_SMP
#if HAVE_ERTS_MSEG
    erts_mseg_late_init();
#endif
#if ERTS_USE_ASYNC_READY_Q
    esdp->aux_work_data.async_ready.queue = erts_get_async_ready_queue(no);
#endif

    erts_sched_init_check_cpu_bind(esdp);

    erts_proc_lock_prepare_proc_lock_waiter();
#endif

#ifdef HIPE
    hipe_thread_signal_init();
#endif
    erts_thread_init_float();

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    esdp->verify_unused_temp_alloc
	= erts_alloc_get_verify_unused_temp_alloc(
	    &esdp->verify_unused_temp_alloc_data);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif

    erts_ets_sched_spec_data_init(esdp);

    process_main(esdp->x_reg_array, esdp->f_reg_array);

    /* No schedulers should *ever* terminate */
    erts_exit(ERTS_ABORT_EXIT,
	     "Scheduler thread number %beu terminated\n",
	     no);
    return NULL;
}

#ifdef ERTS_DIRTY_SCHEDULERS
#ifdef ERTS_SMP
static void*
sched_dirty_cpu_thread_func(void *vesdp)
{
    ErtsThrPrgrCallbacks callbacks;
    ErtsSchedulerData *esdp = vesdp;
    Uint no = esdp->dirty_no;
    ASSERT(no != 0);
    ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(no-1)->event = erts_tse_fetch();
    callbacks.arg = (void *) esdp->ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = NULL;
    callbacks.wait = NULL;
    callbacks.finalize_wait = NULL;

    dirty_sched_wall_time_change(esdp, 1);

    esdp->thr_id += erts_no_schedulers;

    erts_msacc_init_thread("dirty_cpu_scheduler", no, 0);

    erts_thr_progress_register_unmanaged_thread(&callbacks);
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "dirty cpu scheduler %beu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
    erts_tsd_set(sched_data_key, vesdp);
#if ERTS_USE_ASYNC_READY_Q
    esdp->aux_work_data.async_ready.queue = NULL;
#endif

    erts_proc_lock_prepare_proc_lock_waiter();

#ifdef HIPE
    hipe_thread_signal_init();
#endif
    erts_thread_init_float();

    erts_dirty_process_main(esdp);
    /* No schedulers should *ever* terminate */
    erts_exit(ERTS_ABORT_EXIT,
	     "Dirty CPU scheduler thread number %beu terminated\n",
	     no);
    return NULL;
}

static void*
sched_dirty_io_thread_func(void *vesdp)
{
    ErtsThrPrgrCallbacks callbacks;
    ErtsSchedulerData *esdp = vesdp;
    Uint no = esdp->dirty_no;
    ASSERT(no != 0);
    ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(no-1)->event = erts_tse_fetch();
    callbacks.arg = (void *) esdp->ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = NULL;
    callbacks.wait = NULL;
    callbacks.finalize_wait = NULL;

    dirty_sched_wall_time_change(esdp, 1);

    esdp->thr_id += erts_no_schedulers + erts_no_dirty_cpu_schedulers;

    erts_msacc_init_thread("dirty_io_scheduler", no, 0);

    erts_thr_progress_register_unmanaged_thread(&callbacks);
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "dirty io scheduler %beu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
    erts_tsd_set(sched_data_key, vesdp);
#if ERTS_USE_ASYNC_READY_Q
    esdp->aux_work_data.async_ready.queue = NULL;
#endif

    erts_proc_lock_prepare_proc_lock_waiter();

#ifdef HIPE
    hipe_thread_signal_init();
#endif
    erts_thread_init_float();

    erts_dirty_process_main(esdp);
    /* No schedulers should *ever* terminate */
    erts_exit(ERTS_ABORT_EXIT,
	     "Dirty I/O scheduler thread number %beu terminated\n",
	     no);
    return NULL;
}
#endif
#endif

static ethr_tid aux_tid;

void
erts_start_schedulers(void)
{
    int res = 0;
    Uint actual;
    Uint wanted = erts_no_schedulers;
    Uint wanted_no_schedulers = erts_no_schedulers;
    char name[16];
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;

    opts.detached = 1;

    opts.name = name;

#ifdef ERTS_SMP
    if (erts_runq_supervision_interval) {
	opts.suggested_stack_size = 16;
        erts_snprintf(opts.name, 16, "runq_supervisor");
	erts_atomic_init_nob(&runq_supervisor_sleeping, 0);
	if (0 != ethr_event_init(&runq_supervision_event))
	    erts_exit(ERTS_ERROR_EXIT, "Failed to create run-queue supervision event\n");
	if (0 != ethr_thr_create(&runq_supervisor_tid,
				 runq_supervisor,
				 NULL,
				 &opts))
	    erts_exit(ERTS_ERROR_EXIT, "Failed to create run-queue supervision thread\n");

    }
#endif

    opts.suggested_stack_size = erts_sched_thread_suggested_stack_size;

    if (wanted < 1)
	wanted = 1;
    if (wanted > ERTS_MAX_NO_OF_SCHEDULERS) {
	wanted = ERTS_MAX_NO_OF_SCHEDULERS;
	res = ENOTSUP;
    }

    for (actual = 0; actual < wanted; actual++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(actual);

	ASSERT(actual == esdp->no - 1);

	erts_snprintf(opts.name, 16, "%lu_scheduler", actual + 1);

	res = ethr_thr_create(&esdp->tid, sched_thread_func, (void*)esdp, &opts);

	if (res != 0) {
           break;
	}
    }
    erts_no_schedulers = actual;

#ifdef ERTS_DIRTY_SCHEDULERS
#ifdef ERTS_SMP
    {
	int ix;
	for (ix = 0; ix < erts_no_dirty_cpu_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_CPU_SCHEDULER_IX(ix);
	    erts_snprintf(opts.name, 16, "%d_dirty_cpu_scheduler", ix + 1);
            opts.suggested_stack_size = erts_dcpu_sched_thread_suggested_stack_size;
	    res = ethr_thr_create(&esdp->tid,sched_dirty_cpu_thread_func,(void*)esdp,&opts);
	    if (res != 0)
		erts_exit(ERTS_ERROR_EXIT, "Failed to create dirty cpu scheduler thread %d\n", ix);
	}
	for (ix = 0; ix < erts_no_dirty_io_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_IO_SCHEDULER_IX(ix);
	    erts_snprintf(opts.name, 16, "%d_dirty_io_scheduler", ix + 1);
            opts.suggested_stack_size = erts_dio_sched_thread_suggested_stack_size;
	    res = ethr_thr_create(&esdp->tid,sched_dirty_io_thread_func,(void*)esdp,&opts);
	    if (res != 0)
		erts_exit(ERTS_ERROR_EXIT, "Failed to create dirty io scheduler thread %d\n", ix);
	}
    }
#endif
#endif

    ERTS_THR_MEMORY_BARRIER;

    erts_snprintf(opts.name, 16, "aux");

    res = ethr_thr_create(&aux_tid, aux_thread, NULL, &opts);
    if (res != 0)
	erts_exit(ERTS_ERROR_EXIT, "Failed to create aux thread\n");

    if (actual < 1)
	erts_exit(ERTS_ERROR_EXIT,
		 "Failed to create any scheduler-threads: %s (%d)\n",
		 erl_errno_id(res),
		 res);
    if (res != 0) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	ASSERT(actual != wanted_no_schedulers);
	erts_dsprintf(dsbufp,
		      "Failed to create %beu scheduler-threads (%s:%d); "
		      "only %beu scheduler-thread%s created.\n",
		      wanted_no_schedulers, erl_errno_id(res), res,
		      actual, actual == 1 ? " was" : "s were");
	erts_send_error_to_logger_nogl(dsbufp);
    }
}

#endif /* ERTS_SMP */

#ifdef ERTS_SMP

static void
add_pend_suspend(Process *suspendee,
		 Eterm originator_pid,
		 void (*handle_func)(Process *,
				     ErtsProcLocks,
				     int,
				     Eterm))
{
    ErtsPendingSuspend *psp = erts_alloc(ERTS_ALC_T_PEND_SUSPEND,
					 sizeof(ErtsPendingSuspend));
    psp->next = NULL;
#ifdef DEBUG
#if defined(ARCH_64)
    psp->end = (ErtsPendingSuspend *) 0xdeaddeaddeaddead;
#else
    psp->end = (ErtsPendingSuspend *) 0xdeaddead;
#endif
#endif
    psp->pid = originator_pid;
    psp->handle_func = handle_func;

    if (suspendee->pending_suspenders)
	suspendee->pending_suspenders->end->next = psp;
    else
	suspendee->pending_suspenders = psp;
    suspendee->pending_suspenders->end = psp;
}

static void
handle_pending_suspend(Process *p, ErtsProcLocks p_locks)
{
    ErtsPendingSuspend *psp;
    int is_alive = !ERTS_PROC_IS_EXITING(p);

    ERTS_SMP_LC_ASSERT(p_locks & ERTS_PROC_LOCK_STATUS);

    /*
     * New pending suspenders might appear while we are processing
     * (since we may release the status lock on p while processing).
     */
    while (p->pending_suspenders) {
	psp = p->pending_suspenders;
	p->pending_suspenders = NULL;
	while (psp) {
	    ErtsPendingSuspend *free_psp;
	    (*psp->handle_func)(p, p_locks, is_alive, psp->pid);
	    free_psp = psp;
	    psp = psp->next;
	    erts_free(ERTS_ALC_T_PEND_SUSPEND, (void *) free_psp);
	}
    }
    
}

static ERTS_INLINE void
cancel_suspend_of_suspendee(Process *p, ErtsProcLocks p_locks)
{
    if (is_not_nil(p->suspendee)) {
	Process *rp;
	if (!(p_locks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	rp = erts_pid2proc(p, p_locks|ERTS_PROC_LOCK_STATUS,
			   p->suspendee, ERTS_PROC_LOCK_STATUS);
	if (rp) {
	    erts_resume(rp, ERTS_PROC_LOCK_STATUS);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	}
	if (!(p_locks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	p->suspendee = NIL;
    }
}

static void
handle_pend_sync_suspend(Process *suspendee,
			 ErtsProcLocks suspendee_locks,
			 int suspendee_alive,
			 Eterm suspender_pid)
{
    Process *suspender;

    ERTS_SMP_LC_ASSERT(suspendee_locks & ERTS_PROC_LOCK_STATUS);

    suspender = erts_pid2proc(suspendee,
			      suspendee_locks,
			      suspender_pid,
			      ERTS_PROC_LOCK_STATUS);
    if (suspender) {
	ASSERT(is_nil(suspender->suspendee));
	if (suspendee_alive) {
	    erts_suspend(suspendee, suspendee_locks, NULL);
	    suspender->suspendee = suspendee->common.id;
	}
	/* suspender is suspended waiting for suspendee to suspend;
	   resume suspender */
	ASSERT(suspendee != suspender);
	resume_process(suspender, ERTS_PROC_LOCK_STATUS);
	erts_smp_proc_unlock(suspender, ERTS_PROC_LOCK_STATUS);
    }
}

static Process *
pid2proc_not_running(Process *c_p, ErtsProcLocks c_p_locks,
		     Eterm pid, ErtsProcLocks pid_locks, int suspend)
{
    Process *rp;
    int unlock_c_p_status;

    ERTS_SMP_LC_ASSERT(c_p_locks == erts_proc_lc_my_proc_locks(c_p));

    ERTS_SMP_LC_ASSERT(c_p_locks & ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_LC_ASSERT(pid_locks & (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS));

    if (c_p->common.id == pid)
	return erts_pid2proc(c_p, c_p_locks, pid, pid_locks);

    if (c_p_locks & ERTS_PROC_LOCK_STATUS)
	unlock_c_p_status = 0;
    else {
	unlock_c_p_status = 1;
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
    }

    if (c_p->suspendee == pid) {
	/* Process previously suspended by c_p (below)... */
	ErtsProcLocks rp_locks = pid_locks|ERTS_PROC_LOCK_STATUS;
	rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS, pid, rp_locks);
	c_p->suspendee = NIL;
	ASSERT(c_p->flags & F_P2PNR_RESCHED);
	c_p->flags &= ~F_P2PNR_RESCHED;
	if (!suspend && rp)
	    resume_process(rp, rp_locks);
    }
    else {
	rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
			   pid, ERTS_PROC_LOCK_STATUS);

	if (!rp) {
	    c_p->flags &= ~F_P2PNR_RESCHED;
	    goto done;
	}

	ASSERT(!(c_p->flags & F_P2PNR_RESCHED));

	/*
	 * Suspend the other process in order to prevent
	 * it from being selected for normal execution.
	 * This will however not prevent it from being
	 * selected for execution of a system task. If
	 * it is selected for execution of a system task
	 * we might be blocked for quite a while if the
	 * try-lock below fails. That is, there is room
	 * for improvement here...
	 */

	if (!suspend_process(c_p, rp)) {
	    /* Other process running */

	    ASSERT((ERTS_PSFLG_RUNNING | ERTS_PSFLG_DIRTY_RUNNING)
		   & erts_smp_atomic32_read_nob(&rp->state));

#ifdef ERTS_DIRTY_SCHEDULERS
	    if (!suspend
		&& (erts_smp_atomic32_read_nob(&rp->state)
		    & ERTS_PSFLG_DIRTY_RUNNING)) {
		ErtsProcLocks need_locks = pid_locks & ~ERTS_PROC_LOCK_STATUS;
		if (need_locks && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		    rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
				       pid, pid_locks|ERTS_PROC_LOCK_STATUS);
		}
		goto done;
	    }
#endif

	running:

	    /*
	     * If we got pending suspenders and suspend ourselves waiting
	     * to suspend another process we might deadlock.
	     * In this case we have to yield, be suspended by
	     * someone else and then do it all over again.
	     */
	    if (!c_p->pending_suspenders) {
		/* Mark rp pending for suspend by c_p */
		add_pend_suspend(rp, c_p->common.id, handle_pend_sync_suspend);
		ASSERT(is_nil(c_p->suspendee));

		/* Suspend c_p; when rp is suspended c_p will be resumed. */
		suspend_process(c_p, c_p);
		c_p->flags |= F_P2PNR_RESCHED;
	    }
	    /* Yield (caller is assumed to yield immediately in bif). */
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	    rp = ERTS_PROC_LOCK_BUSY;
	}
	else {
	    ErtsProcLocks need_locks = pid_locks & ~ERTS_PROC_LOCK_STATUS;
	    if (need_locks && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		if ((ERTS_PSFLG_RUNNING_SYS|ERTS_PSFLG_DIRTY_RUNNING_SYS)
		    & erts_smp_atomic32_read_nob(&rp->state)) {
		    /* Executing system task... */
		    resume_process(rp, ERTS_PROC_LOCK_STATUS);
		    goto running;
		}
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		/*
		 * If we are unlucky, the process just got selected for
		 * execution of a system task. In this case we may be
		 * blocked here for quite a while... Execution of system
		 * tasks are fortunately quite rare events. We try to
		 * avoid this by checking if it is in a state executing
		 * system tasks (above), but it will not prevent all
		 * scenarios for a long block here...
		 */
		rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
				   pid, pid_locks|ERTS_PROC_LOCK_STATUS);
		if (!rp)
		    goto done;
	    }

	    /*
	     * The previous suspend has prevented the process
	     * from being selected for normal execution regardless
	     * of locks held or not held on it...
	     */
#ifdef DEBUG
	    {
		erts_aint32_t state;
		state = erts_smp_atomic32_read_nob(&rp->state);
		ASSERT((state & ERTS_PSFLG_PENDING_EXIT)
		       || !(state & ERTS_PSFLG_RUNNING));
	    }
#endif

	    if (!suspend)
		resume_process(rp, pid_locks|ERTS_PROC_LOCK_STATUS);
	}
    }

 done:
	
    if (rp && rp != ERTS_PROC_LOCK_BUSY && !(pid_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
    if (unlock_c_p_status)
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
    return rp;
}


/*
 * Like erts_pid2proc() but:
 *
 * * At least ERTS_PROC_LOCK_MAIN have to be held on c_p.
 * * At least ERTS_PROC_LOCK_MAIN have to be taken on pid.
 * * It also waits for proc to be in a state != running and garbing.
 * * If ERTS_PROC_LOCK_BUSY is returned, the calling process has to
 *   yield (ERTS_BIF_YIELD[0-3]()). c_p might in this case have been
 *   suspended.
 */
Process *
erts_pid2proc_not_running(Process *c_p, ErtsProcLocks c_p_locks,
			  Eterm pid, ErtsProcLocks pid_locks)
{
    return pid2proc_not_running(c_p, c_p_locks, pid, pid_locks, 0);
}

/*
 * Like erts_pid2proc_not_running(), but hands over the process
 * in a suspended state unless (c_p is looked up).
 */
Process *
erts_pid2proc_suspend(Process *c_p, ErtsProcLocks c_p_locks,
		      Eterm pid, ErtsProcLocks pid_locks)
{
    return pid2proc_not_running(c_p, c_p_locks, pid, pid_locks, 1);
}

/*
 * erts_pid2proc_nropt() is normally the same as
 * erts_pid2proc_not_running(). However it is only
 * to be used when 'not running' is a pure optimization,
 * not a requirement.
 */

Process *
erts_pid2proc_nropt(Process *c_p, ErtsProcLocks c_p_locks,
		    Eterm pid, ErtsProcLocks pid_locks)
{
    if (erts_disable_proc_not_running_opt)
	return erts_pid2proc(c_p, c_p_locks, pid, pid_locks);
    else
	return erts_pid2proc_not_running(c_p, c_p_locks, pid, pid_locks);
}

static ERTS_INLINE int
do_bif_suspend_process(Process *c_p,
		       ErtsSuspendMonitor *smon,
		       Process *suspendee)
{
    ASSERT(suspendee);
    ASSERT(!ERTS_PROC_IS_EXITING(suspendee));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
		       & erts_proc_lc_my_proc_locks(suspendee));
    if (smon) {
	if (!smon->active) {
	    if (!suspend_process(c_p, suspendee))
		return 0;
	}
	smon->active += smon->pending;
	ASSERT(smon->active);
	smon->pending = 0;
	return 1;
    }
    return 0;
}

static void
handle_pend_bif_sync_suspend(Process *suspendee,
			     ErtsProcLocks suspendee_locks,
			     int suspendee_alive,
			     Eterm suspender_pid)
{
    Process *suspender;

    ERTS_SMP_LC_ASSERT(suspendee_locks & ERTS_PROC_LOCK_STATUS);

    suspender = erts_pid2proc(suspendee,
			      suspendee_locks,
			      suspender_pid,
			      ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
    if (suspender) {
	ASSERT(is_nil(suspender->suspendee));
	if (!suspendee_alive)
	    erts_delete_suspend_monitor(&suspender->suspend_monitors,
					suspendee->common.id);
	else {
#ifdef DEBUG
	    int res;
#endif
	    ErtsSuspendMonitor *smon;
	    smon = erts_lookup_suspend_monitor(suspender->suspend_monitors,
					       suspendee->common.id);
#ifdef DEBUG
	    res =
#endif
		do_bif_suspend_process(suspendee, smon, suspendee);
	    ASSERT(!smon || res != 0);
	    suspender->suspendee = suspendee->common.id;
	}
	/* suspender is suspended waiting for suspendee to suspend;
	   resume suspender */
	ASSERT(suspender != suspendee);
	resume_process(suspender, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
	erts_smp_proc_unlock(suspender,
			     ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
    }
}

static void
handle_pend_bif_async_suspend(Process *suspendee,
			      ErtsProcLocks suspendee_locks,
			      int suspendee_alive,
			      Eterm suspender_pid)
{

    Process *suspender;

    ERTS_SMP_LC_ASSERT(suspendee_locks & ERTS_PROC_LOCK_STATUS);

    suspender = erts_pid2proc(suspendee,
			      suspendee_locks,
			      suspender_pid,
			      ERTS_PROC_LOCK_LINK);
    if (suspender) {
	ASSERT(is_nil(suspender->suspendee));
	if (!suspendee_alive)
	    erts_delete_suspend_monitor(&suspender->suspend_monitors,
					suspendee->common.id);
	else {
#ifdef DEBUG
	    int res;
#endif
	    ErtsSuspendMonitor *smon;
	    smon = erts_lookup_suspend_monitor(suspender->suspend_monitors,
					       suspendee->common.id);
#ifdef DEBUG
	    res =
#endif
	    do_bif_suspend_process(suspendee, smon, suspendee);
	    ASSERT(!smon || res != 0);
	}
	erts_smp_proc_unlock(suspender, ERTS_PROC_LOCK_LINK);
    }
}

#else

/*
 * Non-smp version of erts_pid2proc_suspend().
 */
Process *
erts_pid2proc_suspend(Process *c_p, ErtsProcLocks c_p_locks,
		      Eterm pid, ErtsProcLocks pid_locks)
{
    Process *rp = erts_pid2proc(c_p, c_p_locks, pid, pid_locks);
    if (rp)
	erts_suspend(rp, pid_locks, NULL);
    return rp;
}

#endif /* ERTS_SMP */

/*
 * The erlang:suspend_process/2 BIF
 */

BIF_RETTYPE
suspend_process_2(BIF_ALIST_2)
{
    Eterm res;
    Process* suspendee = NULL;
    ErtsSuspendMonitor *smon;
    ErtsProcLocks xlocks = (ErtsProcLocks) 0;

    /* Options and default values: */
    int asynchronous = 0;
    int unless_suspending = 0;


    if (BIF_P->common.id == BIF_ARG_1)
	goto badarg; /* We are not allowed to suspend ourselves */

    if (is_not_nil(BIF_ARG_2)) {
	/* Parse option list */
	Eterm arg = BIF_ARG_2;

	while (is_list(arg)) {
	    Eterm *lp = list_val(arg);
	    arg = CAR(lp);
	    switch (arg) {
	    case am_unless_suspending:
		unless_suspending = 1;
		break;
	    case am_asynchronous:
		asynchronous = 1;
		break;
	    default:
		goto badarg;
	    }
	    arg = CDR(lp);
	}
	if (is_not_nil(arg))
	    goto badarg;
    }

    xlocks = ERTS_PROC_LOCK_LINK | (asynchronous
				    ? (ErtsProcLocks) 0
				    : ERTS_PROC_LOCK_STATUS);

    erts_smp_proc_lock(BIF_P, xlocks);

    suspendee = erts_pid2proc(BIF_P,
			      ERTS_PROC_LOCK_MAIN|xlocks,
			      BIF_ARG_1,
			      ERTS_PROC_LOCK_STATUS);
    if (!suspendee)
	goto no_suspendee;

    smon = erts_add_or_lookup_suspend_monitor(&BIF_P->suspend_monitors,
					      BIF_ARG_1);
#ifndef ERTS_SMP /* no ERTS_SMP */

    /* This is really a piece of cake without SMP support... */
    if (!smon->active) {
	erts_smp_atomic32_read_bor_nob(&suspendee->state, ERTS_PSFLG_SUSPENDED);
	suspend_process(BIF_P, suspendee);
	smon->active++;
	res = am_true;
    }
    else if (unless_suspending)
	res = am_false;
    else if (smon->active == INT_MAX)
	goto system_limit;
    else {
	smon->active++;
	res = am_true;
    }

#else /* ERTS_SMP */

    /* ... but a little trickier with SMP support ... */

    if (asynchronous) {
	/* --- Asynchronous suspend begin ---------------------------------- */

	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_LINK
			   & erts_proc_lc_my_proc_locks(BIF_P));
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
			   == erts_proc_lc_my_proc_locks(suspendee));

	if (smon->active) {
	    smon->active += smon->pending;
	    smon->pending = 0;
	    if (unless_suspending)
		res = am_false;
	    else if (smon->active == INT_MAX)
		goto system_limit;
	    else {
		smon->active++;
		res = am_true;
	    }
	    /* done */
	}
	else {
	    /* We havn't got any active suspends on the suspendee */
	    if (smon->pending && unless_suspending)
		res = am_false;
	    else {
		if (smon->pending == INT_MAX)
		    goto system_limit;

		smon->pending++;

		if (!do_bif_suspend_process(BIF_P, smon, suspendee))
		    add_pend_suspend(suspendee,
				     BIF_P->common.id,
				     handle_pend_bif_async_suspend);

		res = am_true;
	    }
	    /* done */
	}
	/* --- Asynchronous suspend end ------------------------------------ */
    }
    else /* if (!asynchronous) */ {
	/* --- Synchronous suspend begin ----------------------------------- */

	ERTS_SMP_LC_ASSERT(((ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS)
			    & erts_proc_lc_my_proc_locks(BIF_P))
			   == (ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS));
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
			   == erts_proc_lc_my_proc_locks(suspendee));

	if (BIF_P->suspendee == BIF_ARG_1) {
	    /* We are back after a yield and the suspendee
	       has been suspended on behalf of us. */
	    ASSERT(smon->active >= 1);
	    BIF_P->suspendee = NIL;
	    res = (!unless_suspending || smon->active == 1
		   ? am_true
		   : am_false);
	    /* done */
	}
	else if (smon->active) {
	    if (unless_suspending)
		res = am_false;
	    else {
		smon->active++;
		res = am_true;
	    }
	    /* done */
	}
	else {
	    /* We haven't got any active suspends on the suspendee */

	    /*
	     * If we have pending suspenders and suspend ourselves waiting
	     * to suspend another process, or suspend another process
	     * we might deadlock. In this case we have to yield,
	     * be suspended by someone else, and then do it all over again.
	     */
	    if (BIF_P->pending_suspenders)
		goto yield;

	    if (!unless_suspending && smon->pending == INT_MAX)
		goto system_limit;
	    if (!unless_suspending || smon->pending == 0)
		smon->pending++;

	    if (do_bif_suspend_process(BIF_P, smon, suspendee)) {
		res = (!unless_suspending || smon->active == 1
		       ? am_true
		       : am_false);
		/* done */
	    }
	    else {
		/* Mark suspendee pending for suspend by BIF_P */
		add_pend_suspend(suspendee,
				 BIF_P->common.id,
				 handle_pend_bif_sync_suspend);

		ASSERT(is_nil(BIF_P->suspendee));

		/*
		 * Suspend BIF_P; when suspendee is suspended, BIF_P
		 * will be resumed and this BIF will be called again.
		 * This time with BIF_P->suspendee == BIF_ARG_1 (see
		 * above).
		 */
		suspend_process(BIF_P, BIF_P);
		goto yield;
	    }
	}
	/* --- Synchronous suspend end ------------------------------------- */
    }

#endif /* ERTS_SMP */
#ifdef DEBUG
    {
	erts_aint32_t state = erts_smp_atomic32_read_acqb(&suspendee->state);
	ASSERT((state & ERTS_PSFLG_SUSPENDED)
	       || (asynchronous && smon->pending));
	ASSERT((state & ERTS_PSFLG_SUSPENDED)
	       || !smon->active);
    }
#endif

    erts_smp_proc_unlock(suspendee, ERTS_PROC_LOCK_STATUS);
    erts_smp_proc_unlock(BIF_P, xlocks);
    BIF_RET(res);

 system_limit:
    ERTS_BIF_PREP_ERROR(res, BIF_P, SYSTEM_LIMIT);
    goto do_return;

 no_suspendee:
#ifdef ERTS_SMP
    BIF_P->suspendee = NIL;
#endif
    erts_delete_suspend_monitor(&BIF_P->suspend_monitors, BIF_ARG_1);

 badarg:
    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
#ifdef ERTS_SMP
    goto do_return;

 yield:
    ERTS_BIF_PREP_YIELD2(res, bif_export[BIF_suspend_process_2],
			 BIF_P, BIF_ARG_1, BIF_ARG_2);
#endif

 do_return:
    if (suspendee)
	erts_smp_proc_unlock(suspendee, ERTS_PROC_LOCK_STATUS);
    if (xlocks)
	erts_smp_proc_unlock(BIF_P, xlocks);
    return res;

}


/*
 * The erlang:resume_process/1 BIF
 */

BIF_RETTYPE
resume_process_1(BIF_ALIST_1)
{
    ErtsSuspendMonitor *smon;
    Process *suspendee;
    int is_active;
 
    if (BIF_P->common.id == BIF_ARG_1)
	BIF_ERROR(BIF_P, BADARG);

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);
    smon = erts_lookup_suspend_monitor(BIF_P->suspend_monitors, BIF_ARG_1);

    if (!smon) {
	/* No previous suspend or dead suspendee */
	goto error;
    }
    else if (smon->pending) {
	smon->pending--;
	ASSERT(smon->pending >= 0);
	if (smon->active) {
	    smon->active += smon->pending;
	    smon->pending = 0;
	}
	is_active = smon->active;
    }
    else if (smon->active) {
	smon->active--;
	ASSERT(smon->pending >= 0);
	is_active = 1;
    }
    else {
	/* No previous suspend or dead suspendee */
	goto error;
    }

    if (smon->active || smon->pending || !is_active) {
	/* Leave the suspendee as it is; just verify that it is still alive */
	suspendee = erts_pid2proc(BIF_P,
				  ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
				  BIF_ARG_1,
				  0);
	if (!suspendee)
	    goto no_suspendee;

    }
    else {
	/* Resume */
	suspendee = erts_pid2proc(BIF_P,
				  ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
				  BIF_ARG_1,
				  ERTS_PROC_LOCK_STATUS);
	if (!suspendee)
	    goto no_suspendee;

	ASSERT(ERTS_PSFLG_SUSPENDED
	       & erts_smp_atomic32_read_nob(&suspendee->state));
	ASSERT(BIF_P != suspendee);
	resume_process(suspendee, ERTS_PROC_LOCK_STATUS);

	erts_smp_proc_unlock(suspendee, ERTS_PROC_LOCK_STATUS);
    }

    if (!smon->active && !smon->pending)
	erts_delete_suspend_monitor(&BIF_P->suspend_monitors, BIF_ARG_1);

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

    BIF_RET(am_true);

 no_suspendee:
    /* cleanup */
    erts_delete_suspend_monitor(&BIF_P->suspend_monitors, BIF_ARG_1);

 error:
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE
erts_internal_is_process_executing_dirty_1(BIF_ALIST_1)
{
    if (is_not_internal_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
#ifdef ERTS_DIRTY_SCHEDULERS
    else {
	Process *rp = erts_proc_lookup(BIF_ARG_1);
	if (rp) {
	    erts_aint32_t state = erts_smp_atomic32_read_nob(&rp->state);
	    if (state & (ERTS_PSFLG_DIRTY_RUNNING
			 |ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
		BIF_RET(am_true);
	    }
	}
    }
#endif
    BIF_RET(am_false);
}


Uint
erts_run_queues_len(Uint *qlen, int atomic_queues_read, int incl_active_sched)
{
    int i = 0;
    Uint len = 0;
    if (atomic_queues_read)
	ERTS_ATOMIC_FOREACH_RUNQ(rq,
	 {
	     Sint rq_len = (Sint) erts_smp_atomic32_read_dirty(&rq->len);
	     ASSERT(rq_len >= 0);
	     if (incl_active_sched
		 && (ERTS_RUNQ_FLGS_GET_NOB(rq) & ERTS_RUNQ_FLG_EXEC)) {
		 rq_len++;
	     }
	     if (qlen)
		 qlen[i++] = rq_len;
	     len += (Uint) rq_len;
	 }
	    );
    else {
	for (i = 0; i < erts_no_run_queues; i++) {
	    ErtsRunQueue *rq = ERTS_RUNQ_IX(i);
	    Sint rq_len = (Sint) erts_smp_atomic32_read_nob(&rq->len);
	    ASSERT(rq_len >= 0);
	     if (incl_active_sched
		 && (ERTS_RUNQ_FLGS_GET_NOB(rq) & ERTS_RUNQ_FLG_EXEC)) {
		 rq_len++;
	     }
	    if (qlen)
		qlen[i] = rq_len;
	    len += (Uint) rq_len;
	}

    }
    return len;
}

Eterm
erts_process_state2status(erts_aint32_t state)
{
    if (state & ERTS_PSFLG_FREE)
	return am_free;

    if (state & ERTS_PSFLG_EXITING)
	return am_exiting;

    if (state & ERTS_PSFLG_GC)
	return am_garbage_collecting;

    if (state & ERTS_PSFLG_SUSPENDED)
	return am_suspended;

    if (state & (ERTS_PSFLG_RUNNING
		 | ERTS_PSFLG_RUNNING_SYS
		 | ERTS_PSFLG_DIRTY_RUNNING
		 | ERTS_PSFLG_DIRTY_RUNNING_SYS))
	return am_running;

    if (state & (ERTS_PSFLG_ACTIVE
		 | ERTS_PSFLG_ACTIVE_SYS
		 | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
	return am_runnable;

    return am_waiting;
}

Eterm
erts_process_status(Process *rp, Eterm rpid)
{
    Eterm res = am_undefined;
    Process *p = rp ? rp : erts_proc_lookup_raw(rpid);

    if (p) {
	erts_aint32_t state = erts_smp_atomic32_read_acqb(&p->state);
	res = erts_process_state2status(state);
    }
#ifdef ERTS_SMP
    else {
	int i;
	ErtsSchedulerData *esdp;

	for (i = 0; i < erts_no_schedulers; i++) {
	    esdp = ERTS_SCHEDULER_IX(i);
	    erts_smp_runq_lock(esdp->run_queue);
	    if (esdp->free_process
		&& esdp->free_process->common.id == rpid) {
		res = am_free;
		erts_smp_runq_unlock(esdp->run_queue);
		break;
	    }
	    erts_smp_runq_unlock(esdp->run_queue);
	}
    }
#endif
    return res;
}

/*
** Suspend a currently executing process 
** If we are to suspend on a port the busy_port is the thing
** otherwise busy_port is NIL
*/

void
erts_suspend(Process* c_p, ErtsProcLocks c_p_locks, Port *busy_port)
{
    int suspend;

    ASSERT(c_p == erts_get_current_process());
    ERTS_SMP_LC_ASSERT(c_p_locks == erts_proc_lc_my_proc_locks(c_p));
    if (!(c_p_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    if (busy_port)
	suspend = erts_save_suspend_process_on_port(busy_port, c_p);
    else
	suspend = 1;

    if (suspend) {
#ifdef DEBUG
	int res =
#endif
	    suspend_process(c_p, c_p);
	ASSERT(res);
    }

    if (!(c_p_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    if (suspend && busy_port && erts_system_monitor_flags.busy_port)
	monitor_generic(c_p, am_busy_port, busy_port->common.id);
}

void
erts_resume(Process* process, ErtsProcLocks process_locks)
{
    ERTS_SMP_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(process, ERTS_PROC_LOCK_STATUS);
    resume_process(process, process_locks|ERTS_PROC_LOCK_STATUS);
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(process, ERTS_PROC_LOCK_STATUS);
}

int
erts_resume_processes(ErtsProcList *list)
{
    /* 'list' is expected to have been fetched (i.e. not a ring anymore) */
    int nresumed = 0;
    ErtsProcList *plp = list;

    while (plp) {
	Process *proc;
	ErtsProcList *fplp;
	ASSERT(is_internal_pid(plp->pid));
	proc = erts_pid2proc(NULL, 0, plp->pid, ERTS_PROC_LOCK_STATUS);
	if (proc) {
	    if (erts_proclist_same(plp, proc)) {
		resume_process(proc, ERTS_PROC_LOCK_STATUS);
		nresumed++;
	    }
	    erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_STATUS);
	}
	fplp = plp;
	plp = plp->next;
	proclist_destroy(fplp);
    }
    return nresumed;
}

Eterm
erts_get_process_priority(Process *p)
{
    erts_aint32_t state = erts_smp_atomic32_read_nob(&p->state);
    switch (ERTS_PSFLGS_GET_USR_PRIO(state)) {
    case PRIORITY_MAX:		return am_max;
    case PRIORITY_HIGH:		return am_high;
    case PRIORITY_NORMAL:	return am_normal;
    case PRIORITY_LOW:		return am_low;
    default: ASSERT(0);		return am_undefined;
    }
}

Eterm
erts_set_process_priority(Process *p, Eterm value)
{
    erts_aint32_t a, oprio, nprio;

    switch (value) {
    case am_max:	nprio = (erts_aint32_t) PRIORITY_MAX;		break;
    case am_high:	nprio = (erts_aint32_t) PRIORITY_HIGH;		break;
    case am_normal:	nprio = (erts_aint32_t) PRIORITY_NORMAL;	break;
    case am_low:	nprio = (erts_aint32_t) PRIORITY_LOW;		break;
    default:		return THE_NON_VALUE;				break;
    }

    a = erts_smp_atomic32_read_nob(&p->state);
    if (nprio == ERTS_PSFLGS_GET_USR_PRIO(a))
	oprio = nprio;
    else {
	int slocked = 0;
	erts_aint32_t e, n, aprio;

	if (a & ERTS_PSFLG_ACTIVE_SYS) {
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	    slocked = 1;
	}

	do {
	    oprio = ERTS_PSFLGS_GET_USR_PRIO(a);
	    n = e = a;

	    if (!(a & (ERTS_PSFLG_ACTIVE_SYS|ERTS_PSFLG_DELAYED_SYS)))
		aprio = nprio;
	    else {
		int max_qbit;

		if (!slocked) {
		    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
		    slocked = 1;
		}

		max_qbit = 0;
		if (a & ERTS_PSFLG_ACTIVE_SYS)
		    max_qbit |= p->sys_task_qs->qmask;
		if (a & ERTS_PSFLG_DELAYED_SYS) {
		    ErtsProcSysTaskQs *qs;
		    qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(p);
		    ASSERT(qs);
		    max_qbit |= qs->qmask;
		}
		max_qbit &= -max_qbit;
		switch (max_qbit) {
		case MAX_BIT:
		    aprio = PRIORITY_MAX;
		    break;
		case HIGH_BIT:
		    aprio = PRIORITY_HIGH;
		    break;
		case NORMAL_BIT:
		    aprio = PRIORITY_NORMAL;
		    break;
		case LOW_BIT:
		    aprio = PRIORITY_LOW;
		    break;
		default:
		    ERTS_INTERNAL_ERROR("Invalid qmask");
		    aprio = -1;
		}

		if (aprio > nprio) /* low value -> high prio */
		    aprio = nprio;
	    }

	    n &= ~(ERTS_PSFLGS_USR_PRIO_MASK
		   | ERTS_PSFLGS_ACT_PRIO_MASK);
	    n |= ((nprio << ERTS_PSFLGS_USR_PRIO_OFFSET)
		  | (aprio << ERTS_PSFLGS_ACT_PRIO_OFFSET));

	    a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	} while (a != e);

        if (slocked)
            erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    }

    switch (oprio) {
    case PRIORITY_MAX:		return am_max;
    case PRIORITY_HIGH:		return am_high;
    case PRIORITY_NORMAL:	return am_normal;
    case PRIORITY_LOW:		return am_low;
    default: ASSERT(0);		return am_undefined;
    }
}

#ifdef __WIN32__
Sint64
erts_time2reds(ErtsMonotonicTime start, ErtsMonotonicTime end)
{
    return ERTS_TIME2REDS_IMPL__(start, end);
}
#endif

static int
scheduler_gc_proc(Process *c_p, int reds_left)
{
    int fcalls, reds;
    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
	fcalls = reds_left;
    else
	fcalls = reds_left - CONTEXT_REDS;
    reds = erts_garbage_collect_nobump(c_p, 0, c_p->arg_reg, c_p->arity, fcalls);
    ASSERT(reds_left >= reds);
    return reds;
}

/*
 * schedule() is called from BEAM (process_main()) or HiPE
 * (hipe_mode_switch()) when the current process is to be
 * replaced by a new process. 'calls' is the number of reduction
 * steps the current process consumed.
 * schedule() returns the new process, and the new process'
 * ->fcalls field is initialised with its allowable number of
 * reduction steps.
 *
 * When no process is runnable, or when sufficiently many reduction
 * steps have been made, schedule() calls erl_sys_schedule() to
 * schedule system-level activities.
 *
 * We use the same queue for normal and low prio processes.
 * We reschedule low prio processes a certain number of times 
 * so that normal processes get to run more frequently. 
 */

Process *erts_schedule(ErtsSchedulerData *esdp, Process *p, int calls)
{
    Process *proxy_p = NULL;
    ErtsRunQueue *rq;
    int context_reds;
    int fcalls;
    int input_reductions;
    int actual_reds;
    int reds;
    Uint32 flags;
    erts_aint32_t state = 0; /* Supress warning... */
    int is_normal_sched;

    ERTS_MSACC_DECLARE_CACHE();

#ifdef USE_VM_PROBES
    if (p != NULL && DTRACE_ENABLED(process_unscheduled)) {
        DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, process_buf);
        DTRACE1(process_unscheduled, process_buf);
    }
#endif

    if (ERTS_USE_MODIFIED_TIMING()) {
	context_reds = ERTS_MODIFIED_TIMING_CONTEXT_REDS;
	input_reductions = ERTS_MODIFIED_TIMING_INPUT_REDS;
    }
    else {
	context_reds = CONTEXT_REDS;
	input_reductions = INPUT_REDUCTIONS;
    }

    ERTS_SMP_LC_ASSERT(ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data())
		       || !erts_thr_progress_is_blocking());

    /*
     * Clean up after the process being scheduled out.
     */
    if (!p) {	/* NULL in the very first schedule() call */
#ifdef ERTS_DIRTY_SCHEDULERS
	is_normal_sched = !esdp;
	if (is_normal_sched) {
	    esdp = erts_get_scheduler_data();
	    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
	else {
	    ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
#else
	esdp = erts_get_scheduler_data();
	is_normal_sched = 1;
#endif
	rq = erts_get_runq_current(esdp);
	ASSERT(esdp);
	fcalls = (int) erts_smp_atomic32_read_acqb(&function_calls);
	actual_reds = reds = 0;
	erts_smp_runq_lock(rq);
    } else {
#ifdef ERTS_SMP
#ifdef ERTS_DIRTY_SCHEDULERS
	is_normal_sched = !esdp;
	if (is_normal_sched) {
	    esdp = p->scheduler_data;
	    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
	else {
	    ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
#else
	esdp = p->scheduler_data;
	is_normal_sched = 1;
#endif
	ASSERT(esdp->current_process == p
	       || esdp->free_process == p);
#else
	esdp = erts_scheduler_data;
	ASSERT(esdp->current_process == p);
	is_normal_sched = 1;
#endif

    sched_out_proc:

	ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

	reds = actual_reds = calls - esdp->virtual_reds;

	ASSERT(actual_reds >= 0);
	if (reds < ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST)
	    reds = ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST;
	esdp->virtual_reds = 0;

	fcalls = (int) erts_smp_atomic32_add_read_acqb(&function_calls, reds);
	ASSERT(esdp && esdp == erts_get_scheduler_data());

	rq = erts_get_runq_current(esdp);

	p->reds += actual_reds;

	state = erts_smp_atomic32_read_nob(&p->state);

	if (IS_TRACED(p)) {
	    if (IS_TRACED_FL(p, F_TRACE_CALLS) && !(state & ERTS_PSFLG_FREE))
		erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_OUT);
	    if ((state & (ERTS_PSFLG_FREE|ERTS_PSFLG_EXITING)) == ERTS_PSFLG_EXITING) {
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
		    trace_sched(p, ERTS_PROC_LOCK_MAIN,
                                ((state & ERTS_PSFLG_FREE)
                                 ? am_out_exited
                                 : am_out_exiting));
	    }
	    else {
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED) ||
                    ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
		    trace_sched(p, ERTS_PROC_LOCK_MAIN, am_out);
	    }
	}

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);

#ifdef ERTS_SMP
        if (p->trace_msg_q) {
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_schedule_flush_trace_messages(p, 1);
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
	}
#endif

        /* have to re-read state after taking lock */
        state = erts_smp_atomic32_read_nob(&p->state);

#ifdef ERTS_SMP
	if (is_normal_sched && (state & ERTS_PSFLG_PENDING_EXIT))
	    erts_handle_pending_exit(p, (ERTS_PROC_LOCK_MAIN
					 | ERTS_PROC_LOCK_TRACE
					 | ERTS_PROC_LOCK_STATUS));
	if (p->pending_suspenders) 
	    handle_pending_suspend(p, (ERTS_PROC_LOCK_MAIN
				       | ERTS_PROC_LOCK_TRACE
				       | ERTS_PROC_LOCK_STATUS));
#endif

	esdp->reductions += reds;

        {
            int dec_refc;

            /* schedule_out_process() returns with rq locked! */
            dec_refc = schedule_out_process(rq, state, p,
                                            proxy_p, is_normal_sched);
            proxy_p = NULL;

            ERTS_PROC_REDUCTIONS_EXECUTED(esdp, rq,
                                          (int) ERTS_PSFLGS_GET_USR_PRIO(state),
                                          reds,
                                          actual_reds);

            esdp->current_process = NULL;
#ifdef ERTS_SMP
            if (is_normal_sched)
                p->scheduler_data = NULL;
#endif

            erts_smp_proc_unlock(p, (ERTS_PROC_LOCK_MAIN
                                     | ERTS_PROC_LOCK_STATUS
                                     | ERTS_PROC_LOCK_TRACE));

            ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_OTHER);

#ifdef ERTS_SMP
            if (state & ERTS_PSFLG_FREE) {
                if (!is_normal_sched) {
                    ASSERT(p->flags & F_DELAYED_DEL_PROC);
                }
                else {
                    ASSERT(esdp->free_process == p);
                    esdp->free_process = NULL;
                }
            }
#endif

            if (dec_refc)
                erts_proc_dec_refc(p);
        }

#ifdef ERTS_SMP
	ASSERT(!esdp->free_process);
#endif
	ASSERT(!esdp->current_process);

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	if (is_normal_sched) {
	    if (esdp->check_time_reds >= ERTS_CHECK_TIME_REDS)
		(void) erts_get_monotonic_time(esdp);

	    if (esdp->last_monotonic_time >= erts_next_timeout_time(esdp->next_tmo_ref)) {
		erts_smp_runq_unlock(rq);
		erts_bump_timers(esdp->timer_wheel, esdp->last_monotonic_time);
		erts_smp_runq_lock(rq);
	    }
	}
    }

    ERTS_SMP_LC_ASSERT(!is_normal_sched || !erts_thr_progress_is_blocking());

 check_activities_to_run: {
	erts_aint32_t psflg_running, psflg_running_sys;
#ifdef ERTS_SMP
	ErtsMigrationPaths *mps;
	ErtsMigrationPath *mp;

	if (is_normal_sched) {
	    if (rq->check_balance_reds <= 0)
		check_balance(rq);

	    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());

	    mps = erts_get_migration_paths_managed();
	    mp = &mps->mpath[rq->ix];

	    if (mp->flags & ERTS_RUNQ_FLGS_IMMIGRATE_QMASK)
		immigrate(rq, mp);
	}

	ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    continue_check_activities_to_run:
	flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
    continue_check_activities_to_run_known_flags:
	ASSERT(!is_normal_sched || (flags & ERTS_RUNQ_FLG_NONEMPTY));

	if (!is_normal_sched) {
	    if (erts_smp_atomic32_read_acqb(&esdp->ssi->flags)
		& (ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC)) {
		suspend_scheduler(esdp);
	    }
	}
	else {
	    erts_aint32_t aux_work;
	    int leader_update;

	    ASSERT(is_normal_sched);

	    if (flags & (ERTS_RUNQ_FLG_CHK_CPU_BIND
                         | ERTS_RUNQ_FLG_SUSPENDED
                         | ERTS_RUNQ_FLG_MSB_EXEC)) {
		if (flags & (ERTS_RUNQ_FLG_SUSPENDED|ERTS_RUNQ_FLG_MSB_EXEC)) {
		    (void) ERTS_RUNQ_FLGS_UNSET_NOB(rq, ERTS_RUNQ_FLG_EXEC);
		    suspend_scheduler(esdp);
		    flags = ERTS_RUNQ_FLGS_SET_NOB(rq, ERTS_RUNQ_FLG_EXEC);
		    flags |= ERTS_RUNQ_FLG_EXEC;
		}
		if (flags & ERTS_RUNQ_FLG_CHK_CPU_BIND) {
		    flags = ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_CHK_CPU_BIND);
		    flags &= ~ERTS_RUNQ_FLG_CHK_CPU_BIND;
		    erts_sched_check_cpu_bind(esdp);
		}
	    }

	    leader_update = erts_thr_progress_update(esdp);
	    aux_work = erts_atomic32_read_acqb(&esdp->ssi->aux_work);
	    if (aux_work | leader_update) {
		erts_smp_runq_unlock(rq);
		if (leader_update)
		    erts_thr_progress_leader_update(esdp);
		if (aux_work)
		    handle_aux_work(&esdp->aux_work_data, aux_work, 0);
		erts_smp_runq_lock(rq);
	    }

	    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
	}
	ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

#else /* ERTS_SMP */
	{
	    erts_aint32_t aux_work;
	    aux_work = erts_atomic32_read_acqb(&esdp->ssi->aux_work);
	    if (aux_work)
		handle_aux_work(&esdp->aux_work_data, aux_work, 0);
	}
#endif /* ERTS_SMP */

	flags = ERTS_RUNQ_FLGS_GET_NOB(rq);

	if (!is_normal_sched & !!(flags & ERTS_RUNQ_FLG_HALTING)) {
	    /* Wait for emulator to terminate... */
	    while (1)
		erts_milli_sleep(1000*1000);
	}
	else if (!runq_got_work_to_execute_flags(flags)) {
	    /* Prepare for scheduler wait */
#ifdef ERTS_SMP
	    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

	    rq->wakeup_other = 0;
	    rq->wakeup_other_reds = 0;

	    flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
	    if (flags & ERTS_RUNQ_FLG_SUSPENDED)
		goto continue_check_activities_to_run_known_flags;
	    if (flags & ERTS_RUNQ_FLG_INACTIVE)
		empty_runq(rq);
	    else {
                ASSERT(!runq_got_work_to_execute(rq));
		if (!is_normal_sched) {
                    /* Dirty scheduler */
                    if (erts_smp_atomic32_read_acqb(&esdp->ssi->flags)
                        & (ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC)) {
                        /* Go suspend... */
                        goto continue_check_activities_to_run_known_flags;
                    }
                }
                else {
                    /* Normal scheduler */
                    if (try_steal_task(rq))
                        goto continue_check_activities_to_run;
                    /*
                     * Check for suspend has to be done after trying
                     * to steal a task...
                     */
                    flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
                    if ((flags & ERTS_RUNQ_FLG_SUSPENDED)
#ifdef ERTS_DIRTY_SCHEDULERS
                        /* If multi scheduling block and we have
                         * dirty work, suspend and let dirty
                         * scheduler handle work... */
                        || ((((flags & (ERTS_RUNQ_FLG_HALTING
                                       | ERTS_RUNQ_FLG_MSB_EXEC))
                              == ERTS_RUNQ_FLG_MSB_EXEC))
                            && have_dirty_work())
#endif
                        ) {
                        non_empty_runq(rq);
                        flags |= ERTS_RUNQ_FLG_NONEMPTY;
                        /*
                         * Go suspend...
                         */
                        goto continue_check_activities_to_run_known_flags;
                    }
		}
		empty_runq(rq);
	    }
#endif

	    (void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_EXEC);
	    scheduler_wait(&fcalls, esdp, rq);
	    flags = ERTS_RUNQ_FLGS_SET_NOB(rq, ERTS_RUNQ_FLG_EXEC);
	    flags |= ERTS_RUNQ_FLG_EXEC;
            ERTS_MSACC_UPDATE_CACHE();
#ifdef ERTS_SMP
	    non_empty_runq(rq);
#endif

	    goto check_activities_to_run;
	}
	else if (is_normal_sched
		 && (fcalls > input_reductions
		     && prepare_for_sys_schedule(!0))) {
	    ErtsMonotonicTime current_time;
	    /*
	     * Schedule system-level activities.
	     */

	    ERTS_MSACC_PUSH_STATE_CACHED_M();

	    erts_smp_atomic32_set_relb(&function_calls, 0);
	    fcalls = 0;

#if 0 /* Not needed since we wont wait in sys schedule */
	    erts_sys_schedule_interrupt(0);
#endif
	    erts_smp_runq_unlock(rq);

            ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_CHECK_IO);
            LTTNG2(scheduler_poll, esdp->no, 1);

	    erl_sys_schedule(1);
	    ERTS_MSACC_POP_STATE_M();

	    current_time = erts_get_monotonic_time(esdp);
	    if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref))
		erts_bump_timers(esdp->timer_wheel, current_time);

#ifdef ERTS_SMP
	    erts_smp_runq_lock(rq);
	    clear_sys_scheduling();
	    goto continue_check_activities_to_run;
#else
	    goto check_activities_to_run;
#endif
	}

        if (flags & ERTS_RUNQ_FLG_MISC_OP)
	    exec_misc_ops(rq);

#ifdef ERTS_SMP
	wakeup_other.check(rq, flags);
#endif

	/*
	 * Find a new port to run.
	 */

        flags = ERTS_RUNQ_FLGS_GET_NOB(rq);

	if (flags & PORT_BIT) {
	    int have_outstanding_io;
	    have_outstanding_io = erts_port_task_execute(rq, &esdp->current_port);
	    if ((!erts_eager_check_io
		 && have_outstanding_io
		 && fcalls > 2*input_reductions)
		|| (flags & ERTS_RUNQ_FLG_HALTING)) {
		/*
		 * If we have performed more than 2*INPUT_REDUCTIONS since
		 * last call to erl_sys_schedule() and we still haven't
		 * handled all I/O tasks we stop running processes and
		 * focus completely on ports.
		 *
		 * One could argue that this is a strange behavior. The
		 * reason for doing it this way is that it is similar
		 * to the behavior before port tasks were introduced.
		 * We don't want to change the behavior too much, at
		 * least not at the time of writing. This behavior
		 * might change in the future.
		 *
		 * /rickard
		 */
		goto check_activities_to_run;
	    }
	}

	/*
	 * Find a new process to run.
	 */
    pick_next_process: {
	    erts_aint32_t psflg_band_mask;
	    int prio_q;
	    int qmask, qbit;

	    flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
	    qmask = (int) (flags & ERTS_RUNQ_FLGS_PROCS_QMASK);
	    qbit = qmask & -qmask;
	    switch (qbit) {
	    case MAX_BIT:
		prio_q = PRIORITY_MAX;
		break;
	    case HIGH_BIT:
		prio_q = PRIORITY_HIGH;
		break;
	    case NORMAL_BIT:
	    case LOW_BIT:
		prio_q = PRIORITY_NORMAL;
		if (check_requeue_process(rq, PRIORITY_NORMAL))
		    goto pick_next_process;
		break;
	    case 0:			/* No process at all */
	    default:
		ASSERT(qmask == 0);
                ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_OTHER);
		goto check_activities_to_run;
	    }

	    /*
	     * Take the chosen process out of the queue.
	     */
	    p = dequeue_process(rq, prio_q, &state);

	    ASSERT(p); /* Wrong qmask in rq->flags? */

	    if (is_normal_sched) {
		psflg_running = ERTS_PSFLG_RUNNING;
		psflg_running_sys = ERTS_PSFLG_RUNNING_SYS;
		psflg_band_mask = ~(((erts_aint32_t) 1) << (ERTS_PSFLGS_GET_PRQ_PRIO(state)
							    + ERTS_PSFLGS_IN_PRQ_MASK_OFFSET));
	    }
	    else {
		psflg_running = ERTS_PSFLG_DIRTY_RUNNING;
		psflg_running_sys = ERTS_PSFLG_DIRTY_RUNNING_SYS;
		psflg_band_mask = ~((erts_aint32_t) 0);
	    }

	    if (!(state & ERTS_PSFLG_PROXY))
		psflg_band_mask &= ~ERTS_PSFLG_IN_RUNQ;
	    else {
                Eterm pid = p->common.id;
		proxy_p = p;
		p = (is_normal_sched
                     ? erts_proc_lookup_raw(pid)
                     : erts_pid2proc_opt(NULL, 0, pid, 0,
                                         ERTS_P2P_FLG_INC_REFC));
		if (!p) {
		    free_proxy_proc(proxy_p);
		    proxy_p = NULL;
		    goto pick_next_process;
		}
		state = erts_smp_atomic32_read_nob(&p->state);
	    }

#ifdef ERTS_DIRTY_SCHEDULERS
	    if (!is_normal_sched)
		clear_proc_dirty_queue_bit(p, rq, qbit);
#endif

	    while (1) {
		erts_aint32_t exp, new;
		int run_process;
		new = exp = state;
		new &= psflg_band_mask;
		/*
		 * Run process if not already running (or free)
		 * or exiting and not running on a normal
		 * scheduler, and not suspended (and not in a
		 * state where suspend should be ignored).
		 */
		run_process = (((!(state & (ERTS_PSFLG_RUNNING
					    | ERTS_PSFLG_RUNNING_SYS
					    | ERTS_PSFLG_DIRTY_RUNNING
					    | ERTS_PSFLG_DIRTY_RUNNING_SYS
					    | ERTS_PSFLG_FREE)))
#ifdef ERTS_DIRTY_SCHEDULERS
				| (((state & (ERTS_PSFLG_RUNNING

					      | ERTS_PSFLG_FREE
					      | ERTS_PSFLG_RUNNING_SYS
					      | ERTS_PSFLG_DIRTY_RUNNING_SYS
					      | ERTS_PSFLG_EXITING))
				    == ERTS_PSFLG_EXITING)
				   & (!!is_normal_sched))
#endif
				   )
			       & ((state & (ERTS_PSFLG_SUSPENDED
					    | ERTS_PSFLG_EXITING
					    | ERTS_PSFLG_FREE
					    | ERTS_PSFLG_PENDING_EXIT
					    | ERTS_PSFLG_ACTIVE_SYS
					    | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
				  != ERTS_PSFLG_SUSPENDED)
#ifdef ERTS_DIRTY_SCHEDULERS
                               & (!(state & (ERTS_PSFLG_EXITING
                                             | ERTS_PSFLG_PENDING_EXIT))
                                  | (!!is_normal_sched))
#endif
                    );

		if (run_process) {
		    if (state & (ERTS_PSFLG_ACTIVE_SYS
				 | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
			new |= psflg_running_sys;
		    else
			new |= psflg_running;
		}
		state = erts_smp_atomic32_cmpxchg_relb(&p->state, new, exp);
		if (state == exp) {
		    if (!run_process) {
			if (proxy_p) {
			    free_proxy_proc(proxy_p);
			    proxy_p = NULL;
			}
			else if (state & ERTS_PSFLG_FREE) {
			    /* free and not queued by proxy */
			    erts_proc_dec_refc(p);
			}
                        if (!is_normal_sched)
                            erts_proc_dec_refc(p);
			goto pick_next_process;
		    }
		    state = new;
		    break;
		}
	    }

	    rq->procs.context_switches++;

	    esdp->current_process = p;

	    calls = 0;
	    reds = context_reds;

	    erts_smp_runq_unlock(rq);

	}

        ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_EMULATOR);

#ifdef ERTS_SMP

	if (flags & ERTS_RUNQ_FLG_PROTECTED)
	    (void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);

	state = erts_smp_atomic32_read_nob(&p->state);

	if (erts_sched_stat.enabled) {
	    int prio;
	    UWord old = ERTS_PROC_SCHED_ID(p, (UWord) esdp->no);
	    int migrated = old && old != esdp->no;

	    ASSERT(is_normal_sched);

	    prio = (int) ERTS_PSFLGS_GET_USR_PRIO(state);

	    erts_smp_spin_lock(&erts_sched_stat.lock);
	    erts_sched_stat.prio[prio].total_executed++;
	    erts_sched_stat.prio[prio].executed++;
	    if (migrated) {
		erts_sched_stat.prio[prio].total_migrated++;
		erts_sched_stat.prio[prio].migrated++;
	    }
	    erts_smp_spin_unlock(&erts_sched_stat.lock);
	}

	state = erts_smp_atomic32_read_nob(&p->state);

#ifndef ERTS_DIRTY_SCHEDULERS
	ASSERT(!p->scheduler_data);
	p->scheduler_data = esdp;
#else /* ERTS_DIRTY_SCHEDULERS */
	if (is_normal_sched) {
	    if ((!!(state & ERTS_PSFLGS_DIRTY_WORK))
		& (!(state & ERTS_PSFLG_ACTIVE_SYS))) {
		/* Migrate to dirty scheduler... */
	    sunlock_sched_out_proc:
		erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
		goto sched_out_proc;
	    }
	    ASSERT(!p->scheduler_data);
	    p->scheduler_data = esdp;
	}
	else {
	    if (state & (ERTS_PSFLG_ACTIVE_SYS
			 | ERTS_PSFLG_PENDING_EXIT
			 | ERTS_PSFLG_EXITING)) {
		/*
		 * IMPORTANT! We need to take care of
		 * scheduled check-process-code requests
		 * before continuing with dirty execution!
		 */
		/* Migrate to normal scheduler... */
		goto sunlock_sched_out_proc;
	    }
	    if ((state & ERTS_PSFLG_DIRTY_ACTIVE_SYS)
		&& rq == ERTS_DIRTY_IO_RUNQ) {
		/* Migrate to dirty cpu scheduler... */
		goto sunlock_sched_out_proc;
	    }

	    ASSERT(rq == ERTS_DIRTY_CPU_RUNQ
		   ? (state & (ERTS_PSFLG_DIRTY_CPU_PROC
			       | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
		   : (rq == ERTS_DIRTY_IO_RUNQ
		      && (state & ERTS_PSFLG_DIRTY_IO_PROC)));	    
	}
#endif

	if (state & ERTS_PSFLG_PENDING_EXIT) {
	    erts_handle_pending_exit(p,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    state = erts_smp_atomic32_read_nob(&p->state);
	}

#endif /* ERTS_SMP */

	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

        /* Clear tracer if it has been removed */
	if (IS_TRACED(p) && erts_is_tracer_proc_enabled(
                p, ERTS_PROC_LOCK_MAIN, &p->common)) {

	    if (state & ERTS_PSFLG_EXITING) {
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
		    trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in_exiting);
	    }
	    else {
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED) ||
                    ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
		    trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in);
	    }
	    if (IS_TRACED_FL(p, F_TRACE_CALLS)) {
		erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_IN);
	    }
	}

        if (is_normal_sched) {

            if (state & ERTS_PSFLG_RUNNING_SYS) {
                /*
                 * GC is normally never delayed when a process
                 * is scheduled out, but might be when executing
                 * hand written beam assembly in
                 * prim_eval:'receive'. If GC is delayed we are
                 * not allowed to execute system tasks.
                 */
                if (!(p->flags & F_DELAY_GC)) {
                    int cost = execute_sys_tasks(p, &state, reds);
                    calls += cost;
                    reds -= cost;
                    if (reds <= 0)
                        goto sched_out_proc;
#ifdef ERTS_DIRTY_SCHEDULERS
                    if (state & ERTS_PSFLGS_DIRTY_WORK)
                        goto sched_out_proc;
#endif
                }

                ASSERT(state & psflg_running_sys);
                ASSERT(!(state & psflg_running));

                while (1) {
                    erts_aint32_t n, e;

                    if (((state & (ERTS_PSFLG_SUSPENDED
                                   | ERTS_PSFLG_ACTIVE)) != ERTS_PSFLG_ACTIVE)
                        && !(state & ERTS_PSFLG_EXITING)) {
                        goto sched_out_proc;
                    }

                    n = e = state;
                    n &= ~psflg_running_sys;
                    n |= psflg_running;

                    state = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
                    if (state == e) {
                        state = n;
                        break;
                    }

                    ASSERT(state & psflg_running_sys);
                    ASSERT(!(state & psflg_running));
                }
            }

            if (ERTS_IS_GC_DESIRED(p)) {
                if (!(state & ERTS_PSFLG_EXITING)
                    && !(p->flags & (F_DELAY_GC|F_DISABLE_GC))) {
                    int cost = scheduler_gc_proc(p, reds);
                    calls += cost;
                    reds -= cost;
                    if (reds <= 0)
                        goto sched_out_proc;
#ifdef ERTS_DIRTY_SCHEDULERS
                    if (p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC))
                        goto sched_out_proc;
#endif
                }
            }
        }

	if (proxy_p) {
	    free_proxy_proc(proxy_p);
	    proxy_p = NULL;
	}

	p->fcalls = reds;
	    
	ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

	/* Never run a suspended process */
#ifdef DEBUG
        {
            erts_aint32_t dstate = erts_smp_atomic32_read_nob(&p->state);
            ASSERT(!(ERTS_PSFLG_SUSPENDED & dstate)
                   || (ERTS_PSFLG_DIRTY_RUNNING_SYS & dstate));
        }
#endif

	ASSERT(erts_proc_read_refc(p) > 0);

	if (!(state & ERTS_PSFLG_EXITING) && ERTS_PTMR_IS_TIMED_OUT(p)) {
	    BeamInstr** pi;
#ifdef ERTS_SMP
	    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
#endif
	    pi = (BeamInstr **) p->def_arg_reg;
	    p->i = *pi;
	    p->flags &= ~F_INSLPQUEUE;
	    p->flags |= F_TIMO;
	    ERTS_PTMR_CLEAR(p);
	}

	return p;
    }
}

static int
notify_sys_task_executed(Process *c_p, ErtsProcSysTask *st,
			 Eterm st_result, int normal_sched)
{
    Process *rp;
#ifdef ERTS_DIRTY_SCHEDULERS
    if (!normal_sched)
	rp = erts_pid2proc_opt(c_p, ERTS_PROC_LOCK_MAIN,
			       st->requester, 0,
			       ERTS_P2P_FLG_INC_REFC);
    else
#endif
	rp = erts_proc_lookup(st->requester);
    if (rp) {
	ErtsProcLocks rp_locks;
	ErlOffHeap *ohp;
	ErtsMessage *mp;
	Eterm *hp, msg, req_id, result;
	Uint st_result_sz, hsz;
#ifdef DEBUG
	Eterm *hp_start;
#endif

	rp_locks = (c_p == rp) ? ERTS_PROC_LOCK_MAIN : 0;

	st_result_sz = is_immed(st_result) ? 0 : size_object(st_result);
	hsz = st->req_id_sz + st_result_sz + 4 /* 3-tuple */;

	mp = erts_alloc_message_heap(rp, &rp_locks, hsz, &hp, &ohp);

#ifdef DEBUG
	hp_start = hp;
#endif

	req_id = st->req_id_sz == 0 ? st->req_id : copy_struct(st->req_id,
							       st->req_id_sz,
							       &hp,
							       ohp);

	result = st_result_sz == 0 ? st_result :  copy_struct(st_result,
							      st_result_sz,
							      &hp,
							      ohp);

	ASSERT(is_immed(st->reply_tag));

	msg = TUPLE3(hp, st->reply_tag, req_id, result);

#ifdef DEBUG
	hp += 4;
	ASSERT(hp_start + hsz == hp);
#endif

	erts_queue_message(rp, rp_locks, mp, msg, c_p->common.id);

	if (c_p == rp)
	    rp_locks &= ~ERTS_PROC_LOCK_MAIN;

	if (rp_locks)
	    erts_smp_proc_unlock(rp, rp_locks);

#ifdef ERTS_DIRTY_SCHEDULERS
	if (!normal_sched)
	    erts_proc_dec_refc(rp);
#endif
    }

    erts_cleanup_offheap(&st->off_heap);

    erts_free(ERTS_ALC_T_PROC_SYS_TSK, st);

    return rp ? 1 : 0;
}

static ERTS_INLINE ErtsProcSysTask *
fetch_sys_task(Process *c_p, erts_aint32_t state, int *qmaskp, int *priop)
{
    ErtsProcSysTaskQs *unused_qs = NULL;
    int qbit, qmask;
    ErtsProcSysTask *st, **qp;

    *priop = -1; /* Shut up annoying erroneous warning */

    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    if (!c_p->sys_task_qs) {
	qmask = 0;
	st = NULL;
	goto update_state;
    }

    qmask = c_p->sys_task_qs->qmask;

    if ((state & (ERTS_PSFLG_ACTIVE
		  | ERTS_PSFLG_EXITING
		  | ERTS_PSFLG_SUSPENDED)) == ERTS_PSFLG_ACTIVE) {
	/* No sys tasks if we got exclusively higher prio user work to do */
	st = NULL;
	switch (ERTS_PSFLGS_GET_USR_PRIO(state)) {
	case PRIORITY_MAX:
	    if (!(qmask & MAX_BIT))
		goto done;
	    break;
	case PRIORITY_HIGH:
	    if (!(qmask & (MAX_BIT|HIGH_BIT)))
		goto done;
	    break;
	default:
	    break;
	}
    }

    qbit = qmask & -qmask;
    switch (qbit) {
    case MAX_BIT:
	qp = &c_p->sys_task_qs->q[PRIORITY_MAX];
	*priop = PRIORITY_MAX;
	break;
    case HIGH_BIT:
	qp = &c_p->sys_task_qs->q[PRIORITY_HIGH];
	*priop = PRIORITY_HIGH;
	break;
    case NORMAL_BIT:
	if (!(qmask & PRIORITY_LOW)
	    || ++c_p->sys_task_qs->ncount <= RESCHEDULE_LOW) {
	    qp = &c_p->sys_task_qs->q[PRIORITY_NORMAL];
	    *priop = PRIORITY_NORMAL;
	    break;
	}
	c_p->sys_task_qs->ncount = 0;
	/* Fall through */
    case LOW_BIT:
	qp = &c_p->sys_task_qs->q[PRIORITY_LOW];
	*priop = PRIORITY_LOW;
	break;
    default:
	ERTS_INTERNAL_ERROR("Invalid qmask");
    }

    st = *qp;
    ASSERT(st);
    if (st->next != st) {
	*qp = st->next;
	st->next->prev = st->prev;
	st->prev->next = st->next;
    }
    else {
	erts_aint32_t a, e, n, st_prio, qmask2;

	*qp = NULL;
	qmask &= ~qbit;
	c_p->sys_task_qs->qmask = qmask;

    update_state:

	qmask2 = qmask;

	if (state & ERTS_PSFLG_DELAYED_SYS) {
	    ErtsProcSysTaskQs *qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(c_p);
	    ASSERT(qs);
	    qmask2 |= qs->qmask;
	}

	switch (qmask2 & -qmask2) {
	case MAX_BIT:
	    st_prio = PRIORITY_MAX;
	    break;
	case HIGH_BIT:
	    st_prio = PRIORITY_HIGH;
	    break;
	case NORMAL_BIT:
	    st_prio = PRIORITY_NORMAL;
	    break;
	case LOW_BIT:
	case 0:
	    st_prio = PRIORITY_LOW;
	    break;
	default:
	    ERTS_INTERNAL_ERROR("Invalid qmask");
	}

	if (!qmask) {
	    unused_qs = c_p->sys_task_qs;
	    c_p->sys_task_qs = NULL;
	}

	a = state;
	do {
	    erts_aint32_t prio = ERTS_PSFLGS_GET_USR_PRIO(a);

	    if (prio > st_prio)
		prio = st_prio;

	    n = e = a;

	    n &= ~ERTS_PSFLGS_ACT_PRIO_MASK;
	    n |= (prio << ERTS_PSFLGS_ACT_PRIO_OFFSET);

	    if (!qmask)
		n &= ~ERTS_PSFLG_ACTIVE_SYS;

	    if (a == n)
		break;
	    a = erts_smp_atomic32_cmpxchg_nob(&c_p->state, n, e);
	} while (a != e);
    }

done:

    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    if (unused_qs)
	proc_sys_task_queues_free(unused_qs);

    *qmaskp = qmask;

    return st;
}

static void save_gc_task(Process *c_p, ErtsProcSysTask *st, int prio);
#ifdef ERTS_DIRTY_SCHEDULERS
static void save_dirty_task(Process *c_p, ErtsProcSysTask *st);
#endif

static int
execute_sys_tasks(Process *c_p, erts_aint32_t *statep, int in_reds)
{
    int minor_gc = 0, major_gc = 0;
    erts_aint32_t state = *statep;
    int reds = in_reds;
    int qmask = 0;

    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(c_p)));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    do {
	ErtsProcSysTaskType type;
	ErtsProcSysTask *st;
	int st_prio;
	Eterm st_res;

	if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT)) {
#ifdef ERTS_SMP
	    if (state & ERTS_PSFLG_PENDING_EXIT)
		erts_handle_pending_exit(c_p, ERTS_PROC_LOCK_MAIN);
#endif
	    ASSERT(ERTS_PROC_IS_EXITING(c_p));
	    break;
	}

	st = fetch_sys_task(c_p, state, &qmask, &st_prio);
	if (!st)
	    break;

	type = st->type;

	switch (type) {
        case ERTS_PSTT_GC_MAJOR:
        case ERTS_PSTT_GC_MINOR:
	    if (c_p->flags & F_DISABLE_GC) {
		save_gc_task(c_p, st, st_prio);
		st = NULL;
		reds--;
	    }
	    else {
		if (!minor_gc
		    || (!major_gc && type == ERTS_PSTT_GC_MAJOR)) {
                    if (type == ERTS_PSTT_GC_MAJOR) {
                        FLAGS(c_p) |= F_NEED_FULLSWEEP;
                    }
		    reds -= scheduler_gc_proc(c_p, reds);
#ifdef ERTS_DIRTY_SCHEDULERS
		    if (c_p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)) {
			save_dirty_task(c_p, st);
			st = NULL;
			break;
		    }
#endif
		    if (type == ERTS_PSTT_GC_MAJOR)
			minor_gc = major_gc = 1;
		    else
			minor_gc = 1;
		}
		st_res = am_true;
	    }
	    break;
	case ERTS_PSTT_CPC: {
	    int fcalls;
            int cpc_reds = 0;
	    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
		fcalls = reds;
	    else
		fcalls = reds - CONTEXT_REDS;
	    st_res = erts_check_process_code(c_p,
					     st->arg[0],
					     &cpc_reds,
					     fcalls);
            reds -= cpc_reds;
	    if (is_non_value(st_res)) {
		/* Needed gc, but gc was disabled */
		save_gc_task(c_p, st, st_prio);
		st = NULL;
	    }
	    break;
        }
	case ERTS_PSTT_CLA: {
	    int fcalls;
            int cla_reds = 0;
	    int do_gc;

	    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(c_p))
		fcalls = reds;
	    else
		fcalls = reds - CONTEXT_REDS;
	    do_gc = st->arg[0] == am_true;
	    st_res = erts_proc_copy_literal_area(c_p, &cla_reds,
						 fcalls, do_gc);
            reds -= cla_reds;
	    if (is_non_value(st_res)) {
#ifdef ERTS_DIRTY_SCHEDULERS
		if (c_p->flags & F_DIRTY_CLA) {
		    save_dirty_task(c_p, st);
		    st = NULL;
		    break;
		}
#endif
		/* Needed gc, but gc was disabled */
		save_gc_task(c_p, st, st_prio);
		st = NULL;
		break;
	    }
	    if (do_gc) /* We did a major gc */
		minor_gc = major_gc = 1;
	    break;
        }
	case ERTS_PSTT_COHMQ:
	    reds -= erts_complete_off_heap_message_queue_change(c_p);
	    st_res = am_true;
	    break;
#ifdef ERTS_SMP
        case ERTS_PSTT_FTMQ:
	    reds -= erts_flush_trace_messages(c_p, ERTS_PROC_LOCK_MAIN);
	    st_res = am_true;
	    break;
#endif
#ifdef ERTS_SMP
        case ERTS_PSTT_ETS_FREE_FIXATION:
	    reds -= erts_db_execute_free_fixation(c_p, (DbFixation*)st->arg[0]);
	    st_res = am_true;
	    break;
#endif
	default:
	    ERTS_INTERNAL_ERROR("Invalid process sys task type");
	    st_res = am_false;
	}

	if (st)
	    reds += notify_sys_task_executed(c_p, st, st_res, 1);

	state = erts_smp_atomic32_read_acqb(&c_p->state);
    } while (qmask && reds > 0);

    *statep = state;

    if (in_reds < reds)
	return in_reds;

    return in_reds - reds;
}

static int
cleanup_sys_tasks(Process *c_p, erts_aint32_t in_state, int in_reds)
{
    erts_aint32_t state = in_state;
    int max_reds = in_reds;
    int reds = 0;
    int qmask = 0;

    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    do {
	ErtsProcSysTask *st;
	Eterm st_res;
	int st_prio;

#ifdef ERTS_DIRTY_SCHEDULERS
	if (c_p->dirty_sys_tasks) {
	    st = c_p->dirty_sys_tasks;
	    c_p->dirty_sys_tasks = st->next;
	}
	else
#endif
	{
	    st = fetch_sys_task(c_p, state, &qmask, &st_prio);
	    if (!st)
		break;
	}

	switch (st->type) {
        case ERTS_PSTT_GC_MAJOR:
        case ERTS_PSTT_GC_MINOR:
	case ERTS_PSTT_CPC:
        case ERTS_PSTT_COHMQ:
        case ERTS_PSTT_ETS_FREE_FIXATION:
	    st_res = am_false;
	    break;
	case ERTS_PSTT_CLA:
	    st_res = am_ok;
	    break;
#ifdef ERTS_SMP
        case ERTS_PSTT_FTMQ:
	    reds -= erts_flush_trace_messages(c_p, ERTS_PROC_LOCK_MAIN);
	    st_res = am_true;
	    break;
#endif
	default:
	    ERTS_INTERNAL_ERROR("Invalid process sys task type");
	    st_res = am_false;
	    break;
	}

	reds += notify_sys_task_executed(c_p, st, st_res, 1);

	state = erts_smp_atomic32_read_acqb(&c_p->state);
    } while (qmask && reds < max_reds);

    return reds;
}

#ifdef ERTS_DIRTY_SCHEDULERS

void
erts_execute_dirty_system_task(Process *c_p)
{
    Eterm cla_res = THE_NON_VALUE;
    ErtsProcSysTask *stasks;

    /*
     * If multiple operations, perform them in the following
     * order (in order to avoid unnecessary GC):
     *  1. Copy Literal Area (implies major GC).
     *  2. GC Hibernate (implies major GC if not woken).
     *  3. Major GC (implies minor GC).
     *  4. Minor GC.
     *
     * System task requests are handled after the actual
     * operations have been performed...
     */

    ASSERT(!(c_p->flags & (F_DELAY_GC|F_DISABLE_GC)));

    if (c_p->flags & F_DIRTY_CLA) {
	int cla_reds = 0;
	cla_res = erts_proc_copy_literal_area(c_p, &cla_reds, c_p->fcalls, 1);
	ASSERT(is_value(cla_res));
    }

    if (c_p->flags & F_DIRTY_GC_HIBERNATE) {
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	if (c_p->msg.len)
            c_p->flags &= ~F_DIRTY_GC_HIBERNATE; /* operation aborted... */
        else {
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	    c_p->fvalue = NIL;
	    erts_garbage_collect_hibernate(c_p);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	}
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
    }

    if (c_p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)) {
        if (c_p->flags & F_DIRTY_MAJOR_GC)
            c_p->flags |= F_NEED_FULLSWEEP;
	(void) erts_garbage_collect_nobump(c_p, 0, c_p->arg_reg,
					   c_p->arity, c_p->fcalls);
    }

    ASSERT(!(c_p->flags & (F_DIRTY_CLA
			   | F_DIRTY_GC_HIBERNATE
			   | F_DIRTY_MAJOR_GC
			   | F_DIRTY_MINOR_GC)));

    stasks = c_p->dirty_sys_tasks;
    c_p->dirty_sys_tasks = NULL;
    
    while (stasks) {
	Eterm st_res;
	ErtsProcSysTask *st = stasks;
	stasks = st->next;

	switch (st->type) {
	case ERTS_PSTT_CLA:
	    ASSERT(is_value(cla_res));
	    st_res = cla_res;
	    break;
	case ERTS_PSTT_GC_MAJOR:
	    st_res = am_true;
	    break;
	case ERTS_PSTT_GC_MINOR:
	    st_res = am_true;
	    break;

	default:
	    ERTS_INTERNAL_ERROR("Not supported dirty system task");
	    break;
	}

	(void) notify_sys_task_executed(c_p, st, st_res, 0);

    }

    erts_smp_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_DIRTY_ACTIVE_SYS);
}

static BIF_RETTYPE
dispatch_system_task(Process *c_p, erts_aint_t fail_state,
		     ErtsProcSysTask *st, Eterm target,
		     Eterm prio, Eterm operation)
{
    Process *rp;
    ErtsProcLocks rp_locks = 0;
    ErlOffHeap *ohp;
    ErtsMessage *mp;
    Eterm *hp, msg;
    Uint hsz, osz;
    BIF_RETTYPE ret;

    switch (st->type) {
    case ERTS_PSTT_CPC:
	rp = erts_dirty_process_code_checker;
	ASSERT(fail_state & (ERTS_PSFLG_DIRTY_RUNNING
			     | ERTS_PSFLG_DIRTY_RUNNING_SYS));
	if (c_p == rp) {
	    ERTS_BIF_PREP_RET(ret, am_dirty_execution);
	    return ret;
	}
	break;
    default:
	rp = NULL;
	ERTS_INTERNAL_ERROR("Non-dispatchable system task");
	break;
    }
	
    ERTS_BIF_PREP_RET(ret, am_ok);

    /*
     * Send message on the form: {Requester, Target, Operation}
     */

    ASSERT(is_immed(st->requester));
    ASSERT(is_immed(target));
    ASSERT(is_immed(prio));

    osz = size_object(operation);
    hsz = 5 /* 4-tuple */ + osz;

    mp = erts_alloc_message_heap(rp, &rp_locks, hsz, &hp, &ohp);

    msg = copy_struct(operation, osz, &hp, ohp);
    msg = TUPLE4(hp, st->requester, target, prio, msg);

    erts_queue_message(rp, rp_locks, mp, msg, st->requester);

    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);

    return ret;
}

#endif

static BIF_RETTYPE
request_system_task(Process *c_p, Eterm requester, Eterm target,
		    Eterm priority, Eterm operation)
{
    BIF_RETTYPE ret;
    Process *rp = erts_proc_lookup(target);
    ErtsProcSysTask *st = NULL;
    erts_aint32_t prio, fail_state = ERTS_PSFLG_EXITING;
    Eterm noproc_res, req_type;

    if (!rp && !is_internal_pid(target)) {
	if (!is_external_pid(target))
	    goto badarg;
	if (external_pid_dist_entry(target) != erts_this_dist_entry)
	    goto badarg;
    }

    switch (priority) {
    case am_max:	prio = PRIORITY_MAX;	break;
    case am_high:	prio = PRIORITY_HIGH;	break;
    case am_normal:	prio = PRIORITY_NORMAL;	break;
    case am_low:	prio = PRIORITY_LOW;	break;
    default: goto badarg;
    }

    if (is_not_tuple(operation))
	goto badarg;
    else {
	int i;
	Eterm *tp = tuple_val(operation);
	Uint arity = arityval(*tp);
	Eterm req_id;
	Uint req_id_sz;
	Eterm arg[ERTS_MAX_PROC_SYS_TASK_ARGS];
	Uint arg_sz[ERTS_MAX_PROC_SYS_TASK_ARGS];
	Uint tot_sz;
	Eterm *hp;

	if (arity < 2)
	    goto badarg;
	if (arity > 2 + ERTS_MAX_PROC_SYS_TASK_ARGS)
	    goto badarg;
	req_type = tp[1];
	req_id = tp[2];
	req_id_sz = is_immed(req_id) ? req_id : size_object(req_id);
	tot_sz = req_id_sz;
	for (i = 0; i < ERTS_MAX_PROC_SYS_TASK_ARGS; i++) {
	    int tix = 3 + i;
	    if (tix > arity) {
		arg[i] = THE_NON_VALUE;
		arg_sz[i] = 0;
	    }
	    else {
		arg[i] = tp[tix];
		if (is_immed(arg[i]))
		    arg_sz[i] = 0;
		else {
		    arg_sz[i] = size_object(arg[i]);
		    tot_sz += arg_sz[i];
		}
	    }
	}
	st = erts_alloc(ERTS_ALC_T_PROC_SYS_TSK,
			ERTS_PROC_SYS_TASK_SIZE(tot_sz));
	ERTS_INIT_OFF_HEAP(&st->off_heap);
	hp = &st->heap[0];

	st->requester = requester;
	st->reply_tag = req_type;
	st->req_id_sz = req_id_sz;
	st->req_id = req_id_sz == 0 ? req_id : copy_struct(req_id,
							   req_id_sz,
							   &hp,
							   &st->off_heap);

	for (i = 0; i < ERTS_MAX_PROC_SYS_TASK_ARGS; i++)
	    st->arg[i] = arg_sz[i] == 0 ? arg[i] : copy_struct(arg[i],
							       arg_sz[i],
							       &hp,
							       &st->off_heap);
	ASSERT(&st->heap[0] + tot_sz == hp);
    }

    switch (req_type) {

    case am_garbage_collect:
        switch (st->arg[0]) {
        case am_minor:  st->type = ERTS_PSTT_GC_MINOR; break;
        case am_major:  st->type = ERTS_PSTT_GC_MAJOR; break;
        default: goto badarg;
        }
        noproc_res = am_false;
        if (!rp)
	    goto noproc;
	break;

    case am_check_process_code:
	if (is_not_atom(st->arg[0]))
	    goto badarg;
	noproc_res = am_false;
	st->type = ERTS_PSTT_CPC;
	if (!rp)
	    goto noproc;
#ifdef ERTS_DIRTY_SCHEDULERS
	/*
	 * If the process should start executing dirty
	 * code it is important that this task is
	 * aborted. Therefore this strict fail state...
	 */
	fail_state |= (ERTS_PSFLG_DIRTY_RUNNING
		       | ERTS_PSFLG_DIRTY_RUNNING_SYS);
#endif
	break;

    case am_copy_literals:
	if (st->arg[0] != am_true && st->arg[0] != am_false)
	    goto badarg;
	st->type = ERTS_PSTT_CLA;
	noproc_res = am_ok;
	if (!rp)
	    goto noproc;
	break;

    default:
	goto badarg;
    }

    if (!schedule_process_sys_task(rp, prio, st, &fail_state)) {
	Eterm failure;
	if (fail_state & ERTS_PSFLG_EXITING) {
	noproc:
	    failure = noproc_res;
	}
#ifdef ERTS_DIRTY_SCHEDULERS
	else if (fail_state & (ERTS_PSFLG_DIRTY_RUNNING
			       | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
	    ret = dispatch_system_task(c_p, fail_state, st,
				       target, priority, operation);
	    goto cleanup_return;
	}
#endif
	else {
	    ERTS_INTERNAL_ERROR("Unknown failure schedule_process_sys_task()");
	    failure = am_internal_error;
	}
	notify_sys_task_executed(c_p, st, failure, 1);
    }

    ERTS_BIF_PREP_RET(ret, am_ok);

    return ret;

badarg:

    ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);

#ifdef ERTS_DIRTY_SCHEDULERS
cleanup_return:
#endif

    if (st) {
	erts_cleanup_offheap(&st->off_heap);
	erts_free(ERTS_ALC_T_PROC_SYS_TSK, st);
    }

    return ret;
}

BIF_RETTYPE
erts_internal_request_system_task_3(BIF_ALIST_3)
{
    return request_system_task(BIF_P, BIF_P->common.id,
			       BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

BIF_RETTYPE
erts_internal_request_system_task_4(BIF_ALIST_4)
{
    return request_system_task(BIF_P, BIF_ARG_1,
			       BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);
}

static void
erts_schedule_generic_sys_task(Eterm pid, ErtsProcSysTaskType type, void* arg)
{
    Process *rp = erts_proc_lookup(pid);
    if (rp) {
	ErtsProcSysTask *st;
	erts_aint32_t state, fail_state;

	st = erts_alloc(ERTS_ALC_T_PROC_SYS_TSK,
			ERTS_PROC_SYS_TASK_SIZE(0));
	st->type = type;
	st->requester = NIL;
	st->reply_tag = NIL;
	st->req_id = NIL;
	st->req_id_sz = 0;
        st->arg[0] = (Eterm)arg;
	ERTS_INIT_OFF_HEAP(&st->off_heap);
	state = erts_smp_atomic32_read_nob(&rp->state);

	fail_state = ERTS_PSFLG_EXITING;

	if (!schedule_process_sys_task(rp, ERTS_PSFLGS_GET_USR_PRIO(state),
				       st, &fail_state))
	    erts_free(ERTS_ALC_T_PROC_SYS_TSK, st);
    }
}


void
erts_schedule_complete_off_heap_message_queue_change(Eterm pid)
{
    erts_schedule_generic_sys_task(pid, ERTS_PSTT_COHMQ, NULL);
}

void
erts_schedule_ets_free_fixation(Eterm pid, DbFixation* fix)
{
    erts_schedule_generic_sys_task(pid, ERTS_PSTT_ETS_FREE_FIXATION, fix);
}

#ifdef ERTS_DIRTY_SCHEDULERS

static void
flush_dirty_trace_messages(void *vpid)
{
    Process *proc;
    Eterm pid;
#ifdef ARCH_64
    pid = (Eterm) vpid;
#else
    pid = *((Eterm *) vpid);
    erts_free(ERTS_ALC_T_DIRTY_SL, vpid);
#endif

    proc = erts_proc_lookup(pid);
    if (proc)
	(void) erts_flush_trace_messages(proc, 0);
}

#endif /* ERTS_DIRTY_SCHEDULERS */

void
erts_schedule_flush_trace_messages(Process *proc, int force_on_proc)
{
#ifdef ERTS_SMP
    ErtsThrPrgrDelayHandle dhndl;
#endif
    Eterm pid = proc->common.id;

#ifdef ERTS_DIRTY_SCHEDULERS
    erts_aint32_t state;

    if (!force_on_proc) {
	state = erts_smp_atomic32_read_nob(&proc->state);
	if (state & (ERTS_PSFLG_DIRTY_RUNNING
		     | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
	    goto sched_flush_dirty;
	}
    }
#endif

#ifdef ERTS_SMP
    dhndl = erts_thr_progress_unmanaged_delay();
#endif

    erts_schedule_generic_sys_task(pid, ERTS_PSTT_FTMQ, NULL);

#ifdef ERTS_SMP
    erts_thr_progress_unmanaged_continue(dhndl);
#endif

#ifdef ERTS_DIRTY_SCHEDULERS
    if (!force_on_proc) {
	state = erts_smp_atomic32_read_mb(&proc->state);
	if (state & (ERTS_PSFLG_DIRTY_RUNNING
		     | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
	    void *vargp;

	sched_flush_dirty:
	    /*
	     * We traced 'proc' from another thread than
	     * it is executing on, and it is executing
	     * on a dirty scheduler. It might take a
	     * significant amount of time before it is
	     * scheduled out (where it gets opportunity
	     * to flush messages). We therefore schedule
	     * the flush on the first ordinary scheduler.
	     */

#ifdef ARCH_64
	    vargp = (void *) pid;
#else
	    {
		Eterm *argp = erts_alloc(ERTS_ALC_T_DIRTY_SL, sizeof(Eterm));
		*argp = pid;
		vargp = (void *) argp;
	    }
#endif
	    erts_schedule_misc_aux_work(1, flush_dirty_trace_messages, vargp);
	}
    }
#endif
}

static void
save_gc_task(Process *c_p, ErtsProcSysTask *st, int prio)
{
    erts_aint32_t state;
    ErtsProcSysTaskQs *qs;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(c_p);
    if (!qs) {
	st->next = st->prev = st;
	qs = proc_sys_task_queues_alloc();
	qs->qmask = 1 << prio;
	qs->ncount = 0;
	qs->q[PRIORITY_MAX] = NULL;
	qs->q[PRIORITY_HIGH] = NULL;
	qs->q[PRIORITY_NORMAL] = NULL;
	qs->q[PRIORITY_LOW] = NULL;
	qs->q[prio] = st;
	(void) ERTS_PROC_SET_DELAYED_GC_TASK_QS(c_p, qs);
    }
    else {
	if (!qs->q[prio]) {
	    st->next = st->prev = st;
	    qs->q[prio] = st;
	    qs->qmask |= 1 << prio;
	}
	else {
	    st->next = qs->q[prio];
	    st->prev = qs->q[prio]->prev;
	    st->next->prev = st;
	    st->prev->next = st;
	    ASSERT(qs->qmask & (1 << prio));
	}
    }

    state = erts_smp_atomic32_read_nob(&c_p->state);
    ASSERT((ERTS_PSFLG_RUNNING
	    | ERTS_PSFLG_RUNNING_SYS
	    | ERTS_PSFLG_DIRTY_RUNNING
	    | ERTS_PSFLG_DIRTY_RUNNING_SYS) & state);

    while (!(state & ERTS_PSFLG_DELAYED_SYS)
	   || prio < ERTS_PSFLGS_GET_ACT_PRIO(state)) {
	erts_aint32_t n, e;

	n = e = state;
	n |= ERTS_PSFLG_DELAYED_SYS;
	if (prio < ERTS_PSFLGS_GET_ACT_PRIO(state)) {
	    n &= ~ERTS_PSFLGS_ACT_PRIO_MASK;
	    n |= prio << ERTS_PSFLGS_ACT_PRIO_OFFSET;
	}
	state = erts_smp_atomic32_cmpxchg_relb(&c_p->state, n, e);
	if (state == e)
	    break;
    }
}

#ifdef ERTS_DIRTY_SCHEDULERS
static void
save_dirty_task(Process *c_p, ErtsProcSysTask *st)
{
    st->next = c_p->dirty_sys_tasks;
    c_p->dirty_sys_tasks = st;
}
#endif

int
erts_set_gc_state(Process *c_p, int enable)
{
    ErtsProcSysTaskQs *dgc_tsk_qs;
    ASSERT(c_p == erts_get_current_process());
    ASSERT((ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)
	   & erts_smp_atomic32_read_nob(&c_p->state));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    if (!enable) {
	c_p->flags |= F_DISABLE_GC;
	return 0;
    }

    c_p->flags &= ~F_DISABLE_GC;

    dgc_tsk_qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(c_p);
    if (!dgc_tsk_qs)
	return 0;

    /* Move delayed gc tasks into sys tasks queues. */

    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    if (!c_p->sys_task_qs) {
	c_p->sys_task_qs = dgc_tsk_qs;
	dgc_tsk_qs = NULL;
    }
    else {
	ErtsProcSysTaskQs *stsk_qs;
	int prio;

	/*
	 * We push delayed tasks to the front of the queue
	 * since they have already made it to the front
	 * once and then been delayed after that.
	 */

	stsk_qs = c_p->sys_task_qs;

	while (dgc_tsk_qs->qmask) {
	    int qbit = dgc_tsk_qs->qmask & -dgc_tsk_qs->qmask;
	    dgc_tsk_qs->qmask &= ~qbit;
	    switch (qbit) {
	    case MAX_BIT:
		prio = PRIORITY_MAX;
		break;
	    case HIGH_BIT:
		prio = PRIORITY_HIGH;
		break;
	    case NORMAL_BIT:
		prio = PRIORITY_NORMAL;
		break;
	    case LOW_BIT:
		prio = PRIORITY_LOW;
		break;
	    default:
		ERTS_INTERNAL_ERROR("Invalid qmask");
		prio = -1;
		break;
	    }

	    ASSERT(dgc_tsk_qs->q[prio]);

	    if (!stsk_qs->q[prio]) {
		stsk_qs->q[prio] = dgc_tsk_qs->q[prio];
		stsk_qs->qmask |= 1 << prio;
	    }
	    else {
		ErtsProcSysTask *first1, *last1, *first2, *last2;

		ASSERT(stsk_qs->qmask & (1 << prio));
		first1 = dgc_tsk_qs->q[prio];
		last1 = first1->prev;
		first2 = stsk_qs->q[prio];
		last2 = first1->prev;

		last1->next = first2;
		first2->prev = last1;

		first1->prev = last2;
		last2->next = first1;

		stsk_qs->q[prio] = first1;
	    }

	}
    }

#ifdef DEBUG
    {
	int qmask;
	erts_aint32_t aprio, state =
#endif

	    erts_smp_atomic32_read_bset_nob(&c_p->state,
					    (ERTS_PSFLG_DELAYED_SYS
					     | ERTS_PSFLG_ACTIVE_SYS),
					    ERTS_PSFLG_ACTIVE_SYS);

#ifdef DEBUG
	ASSERT(state & ERTS_PSFLG_DELAYED_SYS);
	qmask = c_p->sys_task_qs->qmask;
	aprio = ERTS_PSFLGS_GET_ACT_PRIO(state);
	ASSERT(ERTS_PSFLGS_GET_USR_PRIO(state) >= aprio);
	ASSERT((qmask & -qmask) >= (1 << aprio));
    }
#endif

    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    (void) ERTS_PROC_SET_DELAYED_GC_TASK_QS(c_p, NULL);

    if (dgc_tsk_qs)
	proc_sys_task_queues_free(dgc_tsk_qs);

    return 1;
}

void
erts_sched_stat_modify(int what)
{
    int ix;
    switch (what) {
    case ERTS_SCHED_STAT_MODIFY_ENABLE:
	erts_smp_thr_progress_block();
	erts_sched_stat.enabled = 1;
	erts_smp_thr_progress_unblock();
	break;
    case ERTS_SCHED_STAT_MODIFY_DISABLE:
	erts_smp_thr_progress_block();
	erts_sched_stat.enabled = 0;
	erts_smp_thr_progress_unblock();
	break;
    case ERTS_SCHED_STAT_MODIFY_CLEAR:
	erts_smp_spin_lock(&erts_sched_stat.lock);
	for (ix = 0; ix < ERTS_NO_PRIO_LEVELS; ix++) {
	    erts_sched_stat.prio[ix].total_executed = 0;
	    erts_sched_stat.prio[ix].executed = 0;
	    erts_sched_stat.prio[ix].total_migrated = 0;
	    erts_sched_stat.prio[ix].migrated = 0;
	}
	erts_smp_spin_unlock(&erts_sched_stat.lock);
	break;
    }
}

Eterm
erts_sched_stat_term(Process *p, int total)
{
    Uint sz;
    Uint *hp;
    Eterm prio[ERTS_NO_PRIO_LEVELS];
    Uint executed[ERTS_NO_PRIO_LEVELS];
    Uint migrated[ERTS_NO_PRIO_LEVELS];

    erts_smp_spin_lock(&erts_sched_stat.lock);
    if (total) {
	int i;
	for (i = 0; i < ERTS_NO_PRIO_LEVELS; i++) {
	    prio[i] = erts_sched_stat.prio[i].name;
	    executed[i] = erts_sched_stat.prio[i].total_executed;
	    migrated[i] = erts_sched_stat.prio[i].total_migrated;
	}
    }
    else {
	int i;
	for (i = 0; i < ERTS_NO_PRIO_LEVELS; i++) {
	    prio[i] = erts_sched_stat.prio[i].name;
	    executed[i] = erts_sched_stat.prio[i].executed;
	    erts_sched_stat.prio[i].executed = 0;
	    migrated[i] = erts_sched_stat.prio[i].migrated;
	    erts_sched_stat.prio[i].migrated = 0;
	}
    }
    erts_smp_spin_unlock(&erts_sched_stat.lock);

    sz = 0;
    (void) erts_bld_atom_2uint_3tup_list(NULL, &sz, ERTS_NO_PRIO_LEVELS,
					 prio, executed, migrated);
    hp = HAlloc(p, sz);
    return erts_bld_atom_2uint_3tup_list(&hp, NULL, ERTS_NO_PRIO_LEVELS,
					 prio, executed, migrated);
}

/*
 * Scheduling of misc stuff
 */

void
erts_schedule_misc_op(void (*func)(void *), void *arg)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsRunQueue *rq = esdp ? esdp->run_queue : ERTS_RUNQ_IX(0);
    ErtsMiscOpList *molp = misc_op_list_alloc();
#ifdef ERTS_SMP
    ErtsMigrationPaths *mpaths = erts_get_migration_paths();

    if (!mpaths)
	rq = ERTS_RUNQ_IX(0);
    else {
	ErtsRunQueue *erq = mpaths->mpath[rq->ix].misc_evac_runq;
	if (erq)
	    rq = erq;
    }
#endif

    erts_smp_runq_lock(rq);

    molp->next = NULL;
    molp->func = func;
    molp->arg = arg;
    if (rq->misc.end)
	rq->misc.end->next = molp;
    else
	rq->misc.start = molp;
    rq->misc.end = molp;

#ifdef ERTS_SMP
    non_empty_runq(rq);
#endif

    ERTS_RUNQ_FLGS_SET_NOB(rq, ERTS_RUNQ_FLG_MISC_OP);

    erts_smp_runq_unlock(rq);

    smp_notify_inc_runq(rq);
}

static void
exec_misc_ops(ErtsRunQueue *rq)
{
    int i;
    ErtsMiscOpList *molp = rq->misc.start;
    ErtsMiscOpList *tmp_molp = molp;

    for (i = 0; i < ERTS_MAX_MISC_OPS-1; i++) {
	if (!tmp_molp) 
	    goto mtq;
	tmp_molp = tmp_molp->next;
    }
    
    if (!tmp_molp) {
    mtq:
	rq->misc.start = NULL;
	rq->misc.end = NULL;
    }
    else {
	rq->misc.start = tmp_molp->next;
	tmp_molp->next = NULL;
	if (!rq->misc.start)
	    rq->misc.end = NULL;
    }

    if (!rq->misc.start)
        ERTS_RUNQ_FLGS_UNSET_NOB(rq, ERTS_RUNQ_FLG_MISC_OP);

    erts_smp_runq_unlock(rq);

    while (molp) {
	tmp_molp = molp;
	(*molp->func)(molp->arg);
	molp = molp->next;
	misc_op_list_free(tmp_molp);
    }

    erts_smp_runq_lock(rq);
}

Uint
erts_get_total_context_switches(void)
{
    Uint res = 0;
    ERTS_ATOMIC_FOREACH_RUNQ(rq, res += rq->procs.context_switches);
    return res;
}

void
erts_get_total_reductions(Uint *redsp, Uint *diffp)
{
    Uint reds = 0;
    ERTS_ATOMIC_FOREACH_RUNQ_X(rq,

			       reds += rq->procs.reductions,

			       if (redsp) *redsp = reds;
			       if (diffp) *diffp = reds - last_reductions;
			       last_reductions = reds);
}

void
erts_get_exact_total_reductions(Process *c_p, Uint *redsp, Uint *diffp)
{
    Uint reds = erts_current_reductions(c_p, c_p);
    int ix;
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    /*
     * Wait for other schedulers to schedule out their processes
     * and update 'reductions'.
     */
    erts_smp_thr_progress_block();
    for (reds = 0, ix = 0; ix < erts_no_run_queues; ix++)
	reds += ERTS_RUNQ_IX(ix)->procs.reductions;
    if (redsp)
	*redsp = reds;
    if (diffp)
	*diffp = reds - last_exact_reductions;
    last_exact_reductions = reds;
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
}

static void delete_process(Process* p);

void
erts_free_proc(Process *p)
{
#ifdef ERTS_SMP
    erts_proc_lock_fin(p);
#endif
    ASSERT(erts_smp_atomic32_read_nob(&p->state) & ERTS_PSFLG_FREE);
    ASSERT(0 == erts_proc_read_refc(p));
    if (p->flags & F_DELAYED_DEL_PROC)
	delete_process(p);
    erts_free(ERTS_ALC_T_PROC, (void *) p);
}

typedef struct {
    Process *proc;
    erts_aint32_t state;
    ErtsRunQueue *run_queue;
} ErtsEarlyProcInit;

static void early_init_process_struct(void *varg, Eterm data)
{
    ErtsEarlyProcInit *arg = (ErtsEarlyProcInit *) varg;
    Process *proc = arg->proc;

    proc->common.id = make_internal_pid(data);
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_atomic32_init_nob(&proc->dirty_state, 0);
    proc->dirty_sys_tasks = NULL;
#endif
    erts_smp_atomic32_init_relb(&proc->state, arg->state);

#ifdef ERTS_SMP
    RUNQ_SET_RQ(&proc->run_queue, arg->run_queue);

    erts_proc_lock_init(proc); /* All locks locked */
#endif

}

/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(ErtsRunQueue *rq, erts_aint32_t state)
{
    ErtsEarlyProcInit init_arg;
    Process *p;

    p = erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;

    init_arg.proc = (Process *) p;
    init_arg.run_queue = rq;
    init_arg.state = state;

    ERTS_CT_ASSERT(offsetof(Process,common) == 0);

    if (!erts_ptab_new_element(&erts_proc,
			       &p->common,
			       (void *) &init_arg,
			       early_init_process_struct)) {
	erts_free(ERTS_ALC_T_PROC, p);
	return NULL;
    }

    ASSERT(erts_proc_read_refc(p) > 0);

    ASSERT(internal_pid_serial(p->common.id) <= ERTS_MAX_PID_SERIAL);
    
    p->approx_started = erts_get_approx_time();
    p->rcount = 0;
    p->heap = NULL;


    ASSERT(p == (Process *) (erts_ptab_pix2intptr_nob(
				 &erts_proc,
				 internal_pid_index(p->common.id))));

    return p;
}

Eterm
erl_create_process(Process* parent, /* Parent of process (default group leader). */
		   Eterm mod,	/* Tagged atom for module. */
		   Eterm func,	/* Tagged atom for function. */
		   Eterm args,	/* Arguments for function (must be well-formed list). */
		   ErlSpawnOpts* so) /* Options for spawn. */
{
    Uint flags = 0;
    ErtsRunQueue *rq = NULL;
    Process *p;
    Sint arity;			/* Number of arguments. */
    Uint arg_size;		/* Size of arguments. */
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
    Eterm res = THE_NON_VALUE;
    erts_aint32_t state = 0;
    erts_aint32_t prio = (erts_aint32_t) PRIORITY_NORMAL;
    ErtsProcLocks locks = ERTS_PROC_LOCKS_ALL;
#ifdef SHCOPY_SPAWN
    erts_shcopy_t info;
    INITIALIZE_SHCOPY(info);
#else
    erts_literal_area_t litarea;
    INITIALIZE_LITERAL_PURGE_AREA(litarea);
#endif

    erts_smp_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);

    /*
     * Check for errors.
     */

    if (is_not_atom(mod) || is_not_atom(func) || ((arity = erts_list_length(args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }

    if (so->flags & SPO_USE_ARGS) {
	if (so->scheduler) {
	    int ix = so->scheduler-1;
	    ASSERT(0 <= ix && ix < erts_no_run_queues);
	    rq = ERTS_RUNQ_IX(ix);
	    /* Unsupported feature... */
	    state |= ERTS_PSFLG_BOUND;
	}
	prio = (erts_aint32_t) so->priority;
    }

    state |= (((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_ACT_PRIO_OFFSET)
	      | ((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_USR_PRIO_OFFSET));

    if (so->flags & SPO_OFF_HEAP_MSGQ) {
	state |= ERTS_PSFLG_OFF_HEAP_MSGQ;
	flags |= F_OFF_HEAP_MSGQ;
    }
    else if (so->flags & SPO_ON_HEAP_MSGQ) {
	state |= ERTS_PSFLG_ON_HEAP_MSGQ;
	flags |= F_ON_HEAP_MSGQ;
    }

    ASSERT((flags & F_ON_HEAP_MSGQ) || (flags & F_OFF_HEAP_MSGQ));

    if (!rq)
	rq = erts_get_runq_proc(parent);

    p = alloc_process(rq, state); /* All proc locks are locked by this thread
				     on success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

    ASSERT((erts_smp_atomic32_read_nob(&p->state)
	    & ERTS_PSFLG_ON_HEAP_MSGQ)
	   || (erts_smp_atomic32_read_nob(&p->state)
	       & ERTS_PSFLG_OFF_HEAP_MSGQ));

#ifdef SHCOPY_SPAWN
    arg_size = copy_shared_calculate(args, &info);
#else
    arg_size = size_object_litopt(args, &litarea);
#endif
    heap_need = arg_size;

    p->flags = flags;

    p->static_flags = 0;
    if (so->flags & SPO_SYSTEM_PROC)
	p->static_flags |= ERTS_STC_FLG_SYSTEM_PROC;
    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size  = so->min_heap_size;
	p->min_vheap_size = so->min_vheap_size;
	p->max_gen_gcs    = so->max_gen_gcs;
        MAX_HEAP_SIZE_SET(p, so->max_heap_size);
        MAX_HEAP_SIZE_FLAGS_SET(p, so->max_heap_flags);
    } else {
	p->min_heap_size  = H_MIN_SIZE;
	p->min_vheap_size = BIN_VH_MIN_SIZE;
        MAX_HEAP_SIZE_SET(p, H_MAX_SIZE);
        MAX_HEAP_SIZE_FLAGS_SET(p, H_MAX_FLAGS);
	p->max_gen_gcs    = (Uint16) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
    }
    p->schedule_count = 0;
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));

    p->u.initial.module = mod;
    p->u.initial.function = func;
    p->u.initial.arity = (Uint) arity;

    /*
     * Must initialize binary lists here before copying binaries to process.
     */
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;

    heap_need +=
	IS_CONST(parent->group_leader) ? 0 : NC_HEAP_SIZE(parent->group_leader);

    if (heap_need < p->min_heap_size) {
	sz = heap_need = p->min_heap_size;
    } else {
	sz = erts_next_heap_size(heap_need, 0);
    }

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif
    p->heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*sz);
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
    p->abandoned_heap = NULL;
    p->live_hf_end = ERTS_INVALID_HFRAG_PTR;
    p->catches = 0;

    p->bin_vheap_sz     = p->min_vheap_size;
    p->bin_old_vheap_sz = p->min_vheap_size;
    p->bin_old_vheap    = 0;

    p->sys_task_qs = NULL;

    /* No need to initialize p->fcalls. */

    p->current = &p->u.initial;

    p->i = (BeamInstr *) beam_apply;
    p->cp = (BeamInstr *) beam_apply+1;

    p->arg_reg = p->def_arg_reg;
    p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
    p->arg_reg[0] = mod;
    p->arg_reg[1] = func;
#ifdef SHCOPY_SPAWN
    p->arg_reg[2] = copy_shared_perform(args, arg_size, &info, &p->htop, &p->off_heap);
    DESTROY_SHCOPY(info);
#else
    p->arg_reg[2] = copy_struct_litopt(args, arg_size, &p->htop, &p->off_heap, &litarea);
#endif
    p->arity = 3;

    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->reds = 0;

    ERTS_PTMR_INIT(p);

    p->common.u.alive.reg = NULL;
    ERTS_P_LINKS(p) = NULL;
    ERTS_P_MONITORS(p) = NULL;
    p->nodes_monitors = NULL;
    p->suspend_monitors = NULL;

    ASSERT(is_pid(parent->group_leader));

    if (parent->group_leader == ERTS_INVALID_PID)
	p->group_leader = p->common.id;
    else {
	/* Needs to be done after the heap has been set up */
	p->group_leader =
	    IS_CONST(parent->group_leader)
	    ? parent->group_leader
	    : STORE_NC(&p->htop, &p->off_heap, parent->group_leader);
    }

    erts_get_default_proc_tracing(&ERTS_TRACE_FLAGS(p), &ERTS_TRACER(p));

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifdef ERTS_SMP
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
#endif
    p->bif_timers = NULL;
    p->mbuf = NULL;
    p->msg_frag = NULL;
    p->mbuf_sz = 0;
    erts_smp_atomic_init_nob(&p->psd, (erts_aint_t) NULL);
    p->dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
#ifdef USE_VM_PROBES
    DT_UTAG(p) = NIL;
    DT_UTAG_FLAGS(p) = 0;
#endif
    p->parent = (parent->common.id == ERTS_INVALID_PID
		 ? NIL
		 : parent->common.id);

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

#ifdef ERTS_SMP
    p->trace_msg_q = NULL;
    p->scheduler_data = NULL;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

    if (IS_TRACED(parent)) {
	if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOS) {
	    ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent) & TRACEE_FLAGS);
            erts_tracer_replace(&p->common, ERTS_TRACER(parent));
	}
        if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOS1) {
	    /* Overrides TRACE_CHILDREN */
	    ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent) & TRACEE_FLAGS);
            erts_tracer_replace(&p->common, ERTS_TRACER(parent));
	    ERTS_TRACE_FLAGS(p) &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	    ERTS_TRACE_FLAGS(parent) &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	}
        if (so->flags & SPO_LINK && ERTS_TRACE_FLAGS(parent) & (F_TRACE_SOL|F_TRACE_SOL1)) {
		ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent)&TRACEE_FLAGS);
                erts_tracer_replace(&p->common, ERTS_TRACER(parent));
		if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOL1) {/*maybe override*/
		    ERTS_TRACE_FLAGS(p) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    ERTS_TRACE_FLAGS(parent) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
        }
        if (ARE_TRACE_FLAGS_ON(parent, F_TRACE_PROCS)) {
            locks &= ~(ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_smp_proc_unlock(parent, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            trace_proc_spawn(parent, am_spawn, p->common.id, mod, func, args);
            if (so->flags & SPO_LINK)
                trace_proc(parent, locks, parent, am_link, p->common.id);
        }
    }

    if (IS_TRACED_FL(p, F_TRACE_PROCS)) {
        if ((locks & (ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE))
              == (ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE)) {
            /* This happens when parent was not traced, but child is */
            locks &= ~(ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_smp_proc_unlock(parent, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
        }
        trace_proc_spawn(p, am_spawned, parent->common.id, mod, func, args);
        if (so->flags & SPO_LINK)
            trace_proc(p, locks, p, am_getting_linked, parent->common.id);
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
#ifdef DEBUG
	int ret;
#endif
#ifdef DEBUG
	ret = erts_add_link(&ERTS_P_LINKS(parent),  LINK_PID, p->common.id);
	ASSERT(ret == 0);
	ret = erts_add_link(&ERTS_P_LINKS(p), LINK_PID, parent->common.id);
	ASSERT(ret == 0);
#else	
	erts_add_link(&ERTS_P_LINKS(parent), LINK_PID, p->common.id);
	erts_add_link(&ERTS_P_LINKS(p), LINK_PID, parent->common.id);
#endif

    }

    /*
     * Test whether this process should be initially monitored by its parent.
     */
    if (so->flags & SPO_MONITOR) {
	Eterm mref;

	mref = erts_make_ref(parent);
	erts_add_monitor(&ERTS_P_MONITORS(parent), MON_ORIGIN, mref, p->common.id, NIL);
	erts_add_monitor(&ERTS_P_MONITORS(p), MON_TARGET, mref, parent->common.id, NIL);
	so->mref = mref;
    }

    erts_smp_proc_unlock(p, locks);

    res = p->common.id;

    /*
     * Schedule process for execution.
     */

    erts_smp_proc_unlock(parent, locks & ERTS_PROC_LOCKS_ALL_MINOR);

    schedule_process(p, state, 0);

    VERBOSE(DEBUG_PROCESSES, ("Created a new process: %T\n",p->common.id));

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_spawn)) {
        ErtsCodeMFA cmfa = {mod, func, arity};
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);

        dtrace_fun_decode(p, &cmfa, process_name, mfa_buf);
        DTRACE2(process_spawn, process_name, mfa_buf);
    }
#endif
    return res;

 error:

    erts_smp_proc_unlock(parent, locks & ERTS_PROC_LOCKS_ALL_MINOR);

    return res;
}

/*
 * Initiates a pseudo process that can be used
 * for arithmetic BIFs.
 */

void erts_init_empty_process(Process *p)
{
    p->htop = NULL;
    p->stop = NULL;
    p->hend = NULL;
    p->heap = NULL;
    p->abandoned_heap = NULL;
    p->live_hf_end = ERTS_INVALID_HFRAG_PTR;
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
    p->min_heap_size = 0;
    p->min_vheap_size = 0;
    p->rcount = 0;
    p->common.id = ERTS_INVALID_PID;
    p->reds = 0;
    ERTS_TRACER(p) = erts_tracer_nil;
    ERTS_TRACE_FLAGS(p) = F_INITIAL_TRACE_FLAGS;
    p->group_leader = ERTS_INVALID_PID;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->fcalls = 0;

    p->bin_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap = 0;
    p->sys_task_qs = NULL;
    ERTS_PTMR_INIT(p);
    p->next = NULL;
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;
    p->common.u.alive.reg = NULL;
    p->heap_sz = 0;
    p->high_water = NULL;
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->msg_frag = NULL;
    p->mbuf_sz = 0;
    erts_smp_atomic_init_nob(&p->psd, (erts_aint_t) NULL);
    ERTS_P_MONITORS(p) = NULL;
    ERTS_P_LINKS(p) = NULL;         /* List of links */
    p->nodes_monitors = NULL;
    p->suspend_monitors = NULL;
    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
    p->bif_timers = NULL;
    p->dictionary = NULL;
    p->seq_trace_clock = 0;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_token = NIL;
    p->u.initial.module = 0;
    p->u.initial.function = 0;
    p->u.initial.arity = 0;
    p->catches = 0;
    p->cp = NULL;
    p->i = NULL;
    p->current = NULL;

    /*
     * Saved x registers.
     */
    p->arity = 0;
    p->arg_reg = NULL;
    p->max_arg_reg = 0;
    p->def_arg_reg[0] = 0;
    p->def_arg_reg[1] = 0;
    p->def_arg_reg[2] = 0;
    p->def_arg_reg[3] = 0;
    p->def_arg_reg[4] = 0;
    p->def_arg_reg[5] = 0;

    p->parent = NIL;
    p->approx_started = 0;
    p->static_flags = 0;

    p->common.u.alive.started_interval = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_atomic32_init_nob(&p->dirty_state, 0);
    p->dirty_sys_tasks = NULL;
#endif
    erts_smp_atomic32_init_nob(&p->state, (erts_aint32_t) PRIORITY_NORMAL);

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
    erts_proc_lock_init(p);
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    RUNQ_SET_RQ(&p->run_queue, ERTS_RUNQ_IX(0));
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

}    

#ifdef DEBUG

void
erts_debug_verify_clean_empty_process(Process* p)
{
    /* Things that erts_cleanup_empty_process() will *not* cleanup... */
    ASSERT(p->htop == NULL);
    ASSERT(p->stop == NULL);
    ASSERT(p->hend == NULL);
    ASSERT(p->abandoned_heap == NULL);
    ASSERT(p->live_hf_end == ERTS_INVALID_HFRAG_PTR);
    ASSERT(p->heap == NULL);
    ASSERT(p->common.id == ERTS_INVALID_PID);
    ASSERT(ERTS_TRACER_IS_NIL(ERTS_TRACER(p)));
    ASSERT(ERTS_TRACE_FLAGS(p) == F_INITIAL_TRACE_FLAGS);
    ASSERT(p->group_leader == ERTS_INVALID_PID);
    ASSERT(p->next == NULL);
    ASSERT(p->common.u.alive.reg == NULL);
    ASSERT(p->heap_sz == 0);
    ASSERT(p->high_water == NULL);
    ASSERT(p->old_hend == NULL);
    ASSERT(p->old_htop == NULL);
    ASSERT(p->old_heap == NULL);

    ASSERT(ERTS_P_MONITORS(p) == NULL);
    ASSERT(ERTS_P_LINKS(p) == NULL);
    ASSERT(p->nodes_monitors == NULL);
    ASSERT(p->suspend_monitors == NULL);
    ASSERT(p->msg.first == NULL);
    ASSERT(p->msg.len == 0);
    ASSERT(p->bif_timers == NULL);
    ASSERT(p->dictionary == NULL);
    ASSERT(p->catches == 0);
    ASSERT(p->cp == NULL);
    ASSERT(p->i == NULL);
    ASSERT(p->current == NULL);

    ASSERT(p->parent == NIL);

#ifdef ERTS_SMP
    ASSERT(p->msg_inq.first == NULL);
    ASSERT(p->msg_inq.len == 0);
    ASSERT(p->suspendee == NIL);
    ASSERT(p->pending_suspenders == NULL);
    ASSERT(p->pending_exit.reason == THE_NON_VALUE);
    ASSERT(p->pending_exit.bp == NULL);
#endif

    /* Thing that erts_cleanup_empty_process() cleans up */

    ASSERT(p->off_heap.first == NULL);
    ASSERT(p->off_heap.overhead == 0);

    ASSERT(p->mbuf == NULL);
}

#endif

void
erts_cleanup_empty_process(Process* p)
{
    /* We only check fields that are known to be used... */

    erts_cleanup_offheap(&p->off_heap);
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;

    if (p->mbuf != NULL) {
	free_message_buffer(p->mbuf);
	p->mbuf = NULL;
    }
#ifdef ERTS_SMP
    erts_proc_lock_fin(p);
#endif
#ifdef DEBUG
    erts_debug_verify_clean_empty_process(p);
#endif
}

static void
delete_process(Process* p)
{
    ErtsPSD *psd;
    struct saved_calls *scb;
    process_breakpoint_time_t *pbt;

    VERBOSE(DEBUG_PROCESSES, ("Removing process: %T\n",p->common.id));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] delete process: %p %p %p %p\n", p->common.id,
                           HEAP_START(p), HEAP_END(p), OLD_HEAP(p), OLD_HEND(p)));

    scb = ERTS_PROC_SET_SAVED_CALLS_BUF(p, NULL);

    if (scb) {
	p->fcalls += CONTEXT_REDS; /* Reduction counting depends on this... */
        erts_free(ERTS_ALC_T_CALLS_BUF, (void *) scb);
    }

    pbt = ERTS_PROC_SET_CALL_TIME(p, NULL);
    if (pbt)
        erts_free(ERTS_ALC_T_BPD, (void *) pbt);

    erts_destroy_nif_export(p);

    /* Cleanup psd */

    psd = (ErtsPSD *) erts_smp_atomic_read_nob(&p->psd);

    if (psd) {
	erts_smp_atomic_set_nob(&p->psd, (erts_aint_t) NULL); /* Reduction counting depends on this... */
	erts_free(ERTS_ALC_T_PSD, psd);
    }

    /* Clean binaries and funs */
    erts_cleanup_offheap(&p->off_heap);

    /*
     * The mso list should not be used anymore, but if it is, make sure that
     * we'll notice.
     */
    p->off_heap.first = (void *) 0x8DEFFACD;

    if (p->arg_reg != p->def_arg_reg) {
	erts_free(ERTS_ALC_T_ARG_REG, p->arg_reg);
    }

    /*
     * Release heaps. Clobber contents in DEBUG build.
     */

#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif

    erts_deallocate_young_generation(p);

    if (p->old_heap != NULL) {

#ifdef DEBUG
	sys_memset(p->old_heap, DEBUG_BAD_BYTE,
                   (p->old_hend-p->old_heap)*sizeof(Eterm));
#endif
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       p->old_heap,
		       (p->old_hend-p->old_heap)*sizeof(Eterm));
    }

    erts_erase_dicts(p);

    /* free all pending messages */
    erts_cleanup_messages(p->msg.first);
    p->msg.first = NULL;

    ASSERT(!p->nodes_monitors);
    ASSERT(!p->suspend_monitors);

    p->fvalue = NIL;
}

static ERTS_INLINE void
set_proc_exiting(Process *p,
		 erts_aint32_t in_state,
		 Eterm reason,
		 ErlHeapFragment *bp)
{
    erts_aint32_t state = in_state, enq_prio = -1;
    int enqueue;
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCKS_ALL);

    enqueue = change_proc_schedule_state(p,
					 (ERTS_PSFLG_SUSPENDED
					  | ERTS_PSFLG_PENDING_EXIT
					  | ERTS_PSFLGS_DIRTY_WORK),
					 ERTS_PSFLG_EXITING|ERTS_PSFLG_ACTIVE,
					 &state,
					 &enq_prio,
					 ERTS_PROC_LOCKS_ALL);

    p->fvalue = reason;
    if (bp)
	erts_link_mbuf_to_proc(p, bp);
    /*
     * We used to set freason to EXC_EXIT here, but there is no need to
     * save the stack trace since this process irreversibly is going to
     * exit.
     */
    p->freason = EXTAG_EXIT;
    KILL_CATCHES(p);
    p->i = (BeamInstr *) beam_exit;

#ifndef ERTS_SMP
    if (state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)
        && !(state & ERTS_PSFLG_GC)) {
	/*
	 * I non smp case:
	 *
	 * Currently executing process might be sent an exit
	 * signal if it is traced by a port that it also is
	 * linked to, and the port terminates during the
	 * trace. In this case we want schedule out the
	 * process as quickly as possible in order to detect
	 * the event as fast as possible.
	 */
	ERTS_VBUMP_ALL_REDS(p);
    }
#endif

    add2runq(enqueue, enq_prio, p, state, NULL);
}

static ERTS_INLINE erts_aint32_t
set_proc_self_exiting(Process *c_p)
{
#ifdef DEBUG
    int enqueue;
#endif
    erts_aint32_t state, enq_prio = -1;

    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCKS_ALL);

    state = erts_smp_atomic32_read_nob(&c_p->state);
    ASSERT(state & (ERTS_PSFLG_RUNNING
		    |ERTS_PSFLG_RUNNING_SYS
		    | ERTS_PSFLG_DIRTY_RUNNING
		    | ERTS_PSFLG_DIRTY_RUNNING_SYS));

#ifdef DEBUG
    enqueue =
#endif
	change_proc_schedule_state(c_p,
				   ERTS_PSFLG_SUSPENDED|ERTS_PSFLG_PENDING_EXIT,
				   ERTS_PSFLG_EXITING|ERTS_PSFLG_ACTIVE,
				   &state,
				   &enq_prio,
				   ERTS_PROC_LOCKS_ALL);

    ASSERT(!enqueue);
    return state;
}

#ifdef ERTS_SMP

void
erts_handle_pending_exit(Process *c_p, ErtsProcLocks locks)
{
    ErtsProcLocks xlocks;
    ASSERT(is_value(c_p->pending_exit.reason));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == locks);
    ERTS_SMP_LC_ASSERT(locks & ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_LC_ASSERT(!((ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE)
			 & erts_smp_atomic32_read_nob(&c_p->state)));

    /* Ensure that all locks on c_p are locked before proceeding... */
    if (locks == ERTS_PROC_LOCKS_ALL)
	xlocks = 0;
    else {
	xlocks = ~locks & ERTS_PROC_LOCKS_ALL;
	if (erts_smp_proc_trylock(c_p, xlocks) == EBUSY) {
	    erts_smp_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }

    set_proc_exiting(c_p,
		     erts_smp_atomic32_read_acqb(&c_p->state),
		     c_p->pending_exit.reason,
		     c_p->pending_exit.bp);
    c_p->pending_exit.reason = THE_NON_VALUE;
    c_p->pending_exit.bp = NULL;

    if (xlocks)
	erts_smp_proc_unlock(c_p, xlocks);
}

static void save_pending_exiter(Process *p, ErtsProcList *plp);

static void
do_handle_pending_exiters(ErtsProcList *pnd_xtrs)
{
    /* 'list' is expected to have been fetched (i.e. not a ring anymore) */
    ErtsProcList *plp = pnd_xtrs;

    while (plp) {
	ErtsProcList *next_plp = plp->next;
	Process *p = erts_proc_lookup(plp->pid);
	if (p) {
	    erts_aint32_t state;
	    /*
	     * If the process is running on a normal scheduler, the
	     * pending exit will soon be detected and handled by the
	     * scheduler running the process (at schedule in/out).
	     */
	    if (erts_smp_proc_trylock(p, ERTS_PROC_LOCKS_ALL) != EBUSY) {
		if (erts_proclist_same(plp, p)) {
		    state = erts_smp_atomic32_read_acqb(&p->state);
		    if (!(state & (ERTS_PSFLG_RUNNING
				   | ERTS_PSFLG_RUNNING_SYS
				   | ERTS_PSFLG_EXITING))) {
			ASSERT(state & ERTS_PSFLG_PENDING_EXIT);
			erts_handle_pending_exit(p, ERTS_PROC_LOCKS_ALL);
		    }
		}
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
	    }
	    else {
		erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
		if (erts_proclist_same(plp, p)) {
		    state = erts_smp_atomic32_read_acqb(&p->state);
		    if (!(state & (ERTS_PSFLG_RUNNING
				   | ERTS_PSFLG_RUNNING_SYS
				   | ERTS_PSFLG_EXITING))) {
			/*
			 * Save process and try to acquire all
			 * locks at a later time...
			 */
			save_pending_exiter(p, plp);
			plp = NULL;
		    }
		}
		erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	    }
	}
	if (plp)
	    proclist_destroy(plp);
	plp = next_plp;
    }
}

static void
save_pending_exiter(Process *p, ErtsProcList *plp)
{
    ErtsSchedulerSleepInfo *ssi;
    ErtsRunQueue *rq;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    rq = RUNQ_READ_RQ(&p->run_queue);
    ASSERT(rq && !ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    if (!plp)
	plp = proclist_create(p);

    erts_smp_runq_lock(rq);

    erts_proclist_store_last(&rq->procs.pending_exiters, plp);

    non_empty_runq(rq);

    ssi = rq->scheduler->ssi;

    erts_smp_runq_unlock(rq);

    set_aux_work_flags_wakeup_nob(ssi, ERTS_SSI_AUX_WORK_PENDING_EXITERS);
}

#endif

/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

static ERTS_INLINE void
send_exit_message(Process *to, ErtsProcLocks *to_locksp,
		  Eterm exit_term, Uint term_size, Eterm token)
{
    ErtsMessage *mp;
    ErlOffHeap *ohp;
    Eterm* hp;
    Eterm mess;
#ifdef SHCOPY_SEND
    erts_shcopy_t info;
#endif

    if (!have_seqtrace(token)) {
#ifdef SHCOPY_SEND
        INITIALIZE_SHCOPY(info);
        term_size = copy_shared_calculate(exit_term, &info);
	mp = erts_alloc_message_heap(to, to_locksp, term_size, &hp, &ohp);
        mess = copy_shared_perform(exit_term, term_size, &info, &hp, ohp);
        DESTROY_SHCOPY(info);
#else
	mp = erts_alloc_message_heap(to, to_locksp, term_size, &hp, &ohp);
	mess = copy_struct(exit_term, term_size, &hp, ohp);
#endif
	erts_queue_message(to, *to_locksp, mp, mess, am_system);
    } else {
	Eterm temp_token;
	Uint sz_token;

	ASSERT(is_tuple(token));
	sz_token = size_object(token);
#ifdef SHCOPY_SEND
        INITIALIZE_SHCOPY(info);
        term_size = copy_shared_calculate(exit_term, &info);
	mp = erts_alloc_message_heap(to, to_locksp, term_size+sz_token, &hp, &ohp);
        mess = copy_shared_perform(exit_term, term_size, &info, &hp, ohp);
        DESTROY_SHCOPY(info);
#else
	mp = erts_alloc_message_heap(to, to_locksp, term_size+sz_token, &hp, &ohp);
	mess = copy_struct(exit_term, term_size, &hp, ohp);
#endif
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, mess, SEQ_TRACE_SEND, to->common.id, to);
	temp_token = copy_struct(token, sz_token, &hp, ohp);
        ERL_MESSAGE_TOKEN(mp) = temp_token;
	erts_queue_message(to, *to_locksp, mp, mess, am_system);
    }
}

/*
 *
 * *** Exit signal behavior ***
 *
 * Exit signals are asynchronous (truly asynchronous in the
 * SMP emulator). When the signal is received the receiver receives an
 * 'EXIT' message if it is trapping exits; otherwise, it will either
 * ignore the signal if the exit reason is normal, or go into an
 * exiting state (ERTS_PSFLG_EXITING). When a process has gone into the
 * exiting state it will not execute any more Erlang code, but it might
 * take a while before it actually exits. The exit signal is being
 * received when the 'EXIT' message is put in the message queue, the
 * signal is dropped, or when it changes state into exiting. The time it
 * is in the exiting state before actually exiting is undefined (it
 * might take a really long time under certain conditions). The
 * receiver of the exit signal does not break links or trigger monitors
 * until it actually exits.
 *
 * Exit signals and other signals, e.g. messages, have to be received
 * by a receiver in the same order as sent by a sender.
 *
 *
 *
 * Exit signal implementation in the SMP emulator:
 *
 * If the receiver is trapping exits, the signal is transformed
 * into an 'EXIT' message and sent as a normal message, if the
 * reason is normal the signal is dropped; otherwise, the process
 * is determined to be exited. The interesting case is when the
 * process is to be exited and this is what is described below.
 *
 * If it is possible, the receiver is set in the exiting state straight
 * away and we are done; otherwise, the sender places the exit reason
 * in the pending_exit field of the process struct and if necessary
 * adds the receiver to the run queue. It is typically not possible
 * to set a scheduled process or a process which we cannot get all locks
 * on without releasing locks on it in an exiting state straight away.
 *
 * The receiver will poll the pending_exit field when it reach certain
 * places during it's execution. When it discovers the pending exit
 * it will change state into the exiting state. If the receiver wasn't
 * scheduled when the pending exit was set, the first scheduler that
 * schedules a new process will set the receiving process in the exiting
 * state just before it schedules next process.
 * 
 * When the exit signal is placed in the pending_exit field, the signal
 * is considered as being in transit on the Erlang level. The signal is
 * actually in some kind of semi transit state, since we have already
 * determined how it should be received. It will exit the process no
 * matter what if it is received (the process may exit by itself before
 * reception of the exit signal). The signal is received when it is
 * discovered in the pending_exit field by the receiver.
 *
 * The receiver have to poll the pending_exit field at least before:
 * - moving messages from the message in queue to the private message
 *   queue. This in order to preserve signal order.
 * - unlink. Otherwise the process might get exited on a link that
 *   have been removed.
 * - changing the trap_exit flag to true. This in order to simplify the
 *   implementation; otherwise, we would have to transform the signal
 *   into an 'EXIT' message when setting the trap_exit flag to true. We
 *   would also have to maintain a queue of exit signals in transit.
 * - being scheduled in or out.
 */

static ERTS_INLINE int
send_exit_signal(Process *c_p,		/* current process if and only
					   if reason is stored on it */
		 Eterm from,		/* Id of sender of signal */
		 Process *rp,		/* receiving process */
		 ErtsProcLocks *rp_locks,/* current locks on receiver */
		 Eterm reason,		/* exit reason */
		 Eterm exit_tuple,	/* Prebuild exit tuple
					   or THE_NON_VALUE */
		 Uint exit_tuple_sz,	/* Size of prebuilt exit tuple
					   (if exit_tuple != THE_NON_VALUE) */
		 Eterm token,		/* token */
		 Process *token_update, /* token updater */
		 Uint32 flags		/* flags */
    )		
{
    erts_aint32_t state = erts_smp_atomic32_read_nob(&rp->state);
    Eterm rsn = reason == am_kill ? am_killed : reason;

    ERTS_SMP_LC_ASSERT(*rp_locks == erts_proc_lc_my_proc_locks(rp));
    ERTS_SMP_LC_ASSERT((*rp_locks & ERTS_PROC_LOCKS_XSIG_SEND)
		       == ERTS_PROC_LOCKS_XSIG_SEND);

    ASSERT(reason != THE_NON_VALUE);

#ifdef USE_VM_PROBES
    if(DTRACE_ENABLED(process_exit_signal) && is_pid(from)) {
        DTRACE_CHARBUF(sender_str, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(receiver_str, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(reason_buf, DTRACE_TERM_BUF_SIZE);

        dtrace_pid_str(from, sender_str);
        dtrace_proc_str(rp, receiver_str);
        erts_snprintf(reason_buf, sizeof(DTRACE_CHARBUF_NAME(reason_buf)) - 1, "%T", reason);
        DTRACE3(process_exit_signal, sender_str, receiver_str, reason_buf);
    }
#endif

    if ((state & ERTS_PSFLG_TRAP_EXIT)
	&& (reason != am_kill || (flags & ERTS_XSIG_FLG_IGN_KILL))) {
        /* have to release the status lock in order to send the exit message */
        erts_smp_proc_unlock(rp, *rp_locks & ERTS_PROC_LOCKS_XSIG_SEND);
        *rp_locks &= ~ERTS_PROC_LOCKS_XSIG_SEND;
        if (have_seqtrace(token) && token_update)
	    seq_trace_update_send(token_update);
	if (is_value(exit_tuple))
	    send_exit_message(rp, rp_locks, exit_tuple, exit_tuple_sz, token);
	else
	    erts_deliver_exit_message(from, rp, rp_locks, rsn, token);
	return 1; /* Receiver will get a message */
    }
    else if (reason != am_normal || (flags & ERTS_XSIG_FLG_NO_IGN_NORMAL)) {
#ifdef ERTS_SMP
	if (!(state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))) {
	    ASSERT(!rp->pending_exit.bp);

	    if (rp == c_p && (*rp_locks & ERTS_PROC_LOCK_MAIN)) {
		/* Ensure that all locks on c_p are locked before
		   proceeding... */
		if (*rp_locks != ERTS_PROC_LOCKS_ALL) {
		    ErtsProcLocks need_locks = (~(*rp_locks)
						& ERTS_PROC_LOCKS_ALL);
		    if (erts_smp_proc_trylock(c_p, need_locks) == EBUSY) {
			erts_smp_proc_unlock(c_p,
					     *rp_locks & ~ERTS_PROC_LOCK_MAIN);
			erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		    }
		    *rp_locks = ERTS_PROC_LOCKS_ALL;
		}
		set_proc_exiting(c_p, state, rsn, NULL);
	    }
	    else if (!(state & (ERTS_PSFLG_RUNNING
                                | ERTS_PSFLG_RUNNING_SYS
                                | ERTS_PSFLG_DIRTY_RUNNING_SYS))) {
		/* Process not running ... */
		ErtsProcLocks need_locks = ~(*rp_locks) & ERTS_PROC_LOCKS_ALL;
		ErlHeapFragment *bp = NULL;
		Eterm rsn_cpy;
		if (need_locks
		    && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		    /* ... but we havn't got all locks on it ... */
		    save_pending_exiter(rp, NULL); 
		    /*
		     * The pending exit will be discovered when next
		     * process is scheduled in
		     */
                    goto set_pending_exit;
		}
                /* ...and we have all locks on it... */
                *rp_locks = ERTS_PROC_LOCKS_ALL;

		state = erts_smp_atomic32_read_nob(&rp->state);

		if (is_immed(rsn))
		    rsn_cpy = rsn;
		else {
		    Eterm *hp;
		    ErlOffHeap *ohp;
		    Uint rsn_sz = size_object(rsn);
#ifdef ERTS_DIRTY_SCHEDULERS
		    if (state & ERTS_PSFLG_DIRTY_RUNNING) {
			bp = new_message_buffer(rsn_sz);
			ohp = &bp->off_heap;
			hp = &bp->mem[0];
		    }
		    else
#endif
		    {
			hp = HAlloc(rp, rsn_sz);
			ohp = &rp->off_heap;
		    }
		    rsn_cpy = copy_struct(rsn, rsn_sz, &hp, ohp);
		}

		set_proc_exiting(rp, state, rsn_cpy, bp);
	    }
	    else { /* Process running... */
 
		/*
		 * The pending exit will be discovered when the process
		 * is scheduled out if not discovered earlier.
		 */

	    set_pending_exit:
		if (is_immed(rsn)) {
		    rp->pending_exit.reason = rsn;
		}
		else {
		    Eterm *hp;
		    Uint sz = size_object(rsn);
		    ErlHeapFragment *bp = new_message_buffer(sz);

		    hp = &bp->mem[0];
		    rp->pending_exit.reason = copy_struct(rsn,
							  sz,
							  &hp,
							  &bp->off_heap);
		    rp->pending_exit.bp = bp;
		}

		/*
		 * If no dirty work has been scheduled, pending exit will
                 * be discovered when the process is scheduled. If dirty work
                 * has been scheduled, we may need to add it to a normal run
                 * queue...
		 */
#ifndef ERTS_DIRTY_SCHEDULERS
                (void) erts_smp_atomic32_read_bor_relb(&rp->state,
                                                       ERTS_PSFLG_PENDING_EXIT);
#else
                {
                    erts_aint32_t a = erts_smp_atomic32_read_nob(&rp->state);
                    while (1) {
                        erts_aint32_t n, e;
                        int dwork;
                        n = e = a;
                        n |= ERTS_PSFLG_PENDING_EXIT;
                        dwork = !!(n & ERTS_PSFLGS_DIRTY_WORK);
                        n &= ~ERTS_PSFLGS_DIRTY_WORK;
                        a = erts_smp_atomic32_cmpxchg_mb(&rp->state, n, e);
                        if (a == e) {
                            if (dwork)
                                erts_schedule_process(rp, n, *rp_locks);
                            break;
                        }
                    }
                }
#endif
	    }
	}
	/* else:
	 *
	 *    The receiver already has a pending exit (or is exiting)
	 *    so we drop this signal.
	 *
	 *    NOTE: dropping this exit signal is based on the assumption
	 *          that the receiver *will* exit; either on the pending
	 *          exit or by itself before seeing the pending exit.
	 */
#else /* !ERTS_SMP */
	erts_aint32_t state = erts_smp_atomic32_read_nob(&rp->state);
	if (!(state & ERTS_PSFLG_EXITING)) {
	    set_proc_exiting(rp,
			     state,
			     (is_immed(rsn) || c_p == rp
			      ? rsn
			      : copy_object(rsn, rp)),
			     NULL);
	}
#endif
	return -1; /* Receiver will exit */
    }

    return 0; /* Receiver unaffected */
}


int
erts_send_exit_signal(Process *c_p,
		      Eterm from,
		      Process *rp,
		      ErtsProcLocks *rp_locks,
		      Eterm reason,
		      Eterm token,
		      Process *token_update,
		      Uint32 flags)
{
    return send_exit_signal(c_p,
			    from,
			    rp,
			    rp_locks,
			    reason,
			    THE_NON_VALUE,
			    0,
			    token,
			    token_update,
			    flags);
}

typedef struct {
    Eterm reason;
    Process *p;
} ExitMonitorContext;

static void doit_exit_monitor(ErtsMonitor *mon, void *vpcontext)
{
    ExitMonitorContext *pcontext = vpcontext;
    DistEntry *dep;
    ErtsMonitor *rmon;

    switch (mon->type) {
    case MON_ORIGIN:
	/* We are monitoring someone else, we need to demonitor that one.. */
	if (is_atom(mon->u.pid)) { /* remote by name */
	    ASSERT(is_node_name_atom(mon->u.pid));
	    dep = erts_sysname_to_connected_dist_entry(mon->u.pid);
	    if (dep) {
		erts_smp_de_links_lock(dep);
		rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		erts_smp_de_links_unlock(dep);
		if (rmon) {
		    ErtsDSigData dsd;
		    int code = erts_dsig_prepare(&dsd, dep, NULL,
						 ERTS_DSP_NO_LOCK, 0);
		    if (code == ERTS_DSIG_PREP_CONNECTED) {
			code = erts_dsig_send_demonitor(&dsd,
							rmon->u.pid,
							mon->name,
							mon->ref,
							1);
			ASSERT(code == ERTS_DSIG_SEND_OK);
		    }
		    erts_destroy_monitor(rmon);
		}
		erts_deref_dist_entry(dep);
	    }
	} else {
            ASSERT(is_pid(mon->u.pid) || is_port(mon->u.pid));
            /* if is local by pid or name */
            if (is_internal_pid(mon->u.pid)) {
                Process *rp = erts_pid2proc(NULL, 0, mon->u.pid, ERTS_PROC_LOCK_LINK);
		if (!rp) {
		    goto done;
		}
		rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
		if (rmon == NULL) {
		    goto done;
		}
		erts_destroy_monitor(rmon);
            } else if (is_internal_port(mon->u.pid)) {
                /* Is a local port */
                Port *prt = erts_port_lookup_raw(mon->u.pid);
                if (!prt) {
                    goto done;
                }
                erts_port_demonitor(pcontext->p,
                                    ERTS_PORT_DEMONITOR_ORIGIN_ON_DEATHBED,
                                    prt, mon->ref, NULL);
            } else { /* remote by pid */
		ASSERT(is_external_pid(mon->u.pid));
		dep = external_pid_dist_entry(mon->u.pid);
		ASSERT(dep != NULL);
		if (dep) {
		    erts_smp_de_links_lock(dep);
		    rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		    erts_smp_de_links_unlock(dep);
		    if (rmon) {
			ErtsDSigData dsd;
			int code = erts_dsig_prepare(&dsd, dep, NULL,
						     ERTS_DSP_NO_LOCK, 0);
			if (code == ERTS_DSIG_PREP_CONNECTED) {
			    code = erts_dsig_send_demonitor(&dsd,
							    rmon->u.pid,
							    mon->u.pid,
							    mon->ref,
							    1);
			    ASSERT(code == ERTS_DSIG_SEND_OK);
			}
			erts_destroy_monitor(rmon);
		    }
		}
	    }
	}
	break;
    case MON_TARGET:
	ASSERT(is_pid(mon->u.pid) || is_internal_port(mon->u.pid));
	if (is_internal_port(mon->u.pid)) {
	    Port *prt = erts_id2port(mon->u.pid);
	    if (prt == NULL) {
		goto done;
	    }
	    erts_fire_port_monitor(prt, mon->ref);
	    erts_port_release(prt); 
	} else if (is_internal_pid(mon->u.pid)) {/* local by name or pid */
	    Eterm watched;
            Process *rp;
	    DeclareTmpHeapNoproc(lhp,3);
	    ErtsProcLocks rp_locks = (ERTS_PROC_LOCK_LINK
				      | ERTS_PROC_LOCKS_MSG_SEND);
	    rp = erts_pid2proc(NULL, 0, mon->u.pid, rp_locks);
	    if (rp == NULL) {
		goto done;
	    }
	    UseTmpHeapNoproc(3);
	    rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
	    if (rmon) {
		erts_destroy_monitor(rmon);
		watched = (is_atom(mon->name)
			   ? TUPLE2(lhp, mon->name, 
				    erts_this_dist_entry->sysname)
			   : pcontext->p->common.id);
		erts_queue_monitor_message(rp, &rp_locks, mon->ref, am_process, 
					   watched, pcontext->reason);
	    }
	    UnUseTmpHeapNoproc(3);
	    /* else: demonitor while we exited, i.e. do nothing... */
	    erts_smp_proc_unlock(rp, rp_locks);
	} else { /* external by pid or name */
	    ASSERT(is_external_pid(mon->u.pid));    
	    dep = external_pid_dist_entry(mon->u.pid);
	    ASSERT(dep != NULL);
	    if (dep) {
		erts_smp_de_links_lock(dep);
		rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		erts_smp_de_links_unlock(dep);
		if (rmon) {
		    ErtsDSigData dsd;
		    int code = erts_dsig_prepare(&dsd, dep, NULL,
						 ERTS_DSP_NO_LOCK, 0);
		    if (code == ERTS_DSIG_PREP_CONNECTED) {
			code = erts_dsig_send_m_exit(&dsd,
						     mon->u.pid,
						     (rmon->name != NIL
						      ? rmon->name
						      : rmon->u.pid),
						     mon->ref,
						     pcontext->reason);
			ASSERT(code == ERTS_DSIG_SEND_OK);
		    }
		    erts_destroy_monitor(rmon);
		}
	    }
	}
	break;
    case MON_NIF_TARGET:
        erts_fire_nif_monitor(mon->u.resource,
                              pcontext->p->common.id,
                              mon->ref);
        break;
    case MON_TIME_OFFSET:
	erts_demonitor_time_offset(mon->ref);
	break;
    default:
	ERTS_INTERNAL_ERROR("Invalid monitor type");
    }
 done:
    /* As the monitors are previously removed from the process, 
       distribution operations will not cause monitors to disappear,
       we can safely delete it. */
       
    erts_destroy_monitor(mon);
}

typedef struct {
    Process *p;
    Eterm reason;
    Eterm exit_tuple;
    Uint exit_tuple_sz;
} ExitLinkContext;

static void doit_exit_link(ErtsLink *lnk, void *vpcontext)
{
    ExitLinkContext *pcontext = vpcontext;
    /* Unpack context, it's readonly */
    Process *p = pcontext->p;
    Eterm reason = pcontext->reason;
    Eterm exit_tuple = pcontext->exit_tuple;
    Uint exit_tuple_sz = pcontext->exit_tuple_sz;
    Eterm item = lnk->pid;
    ErtsLink *rlnk;
    DistEntry *dep;
    Process *rp;


    switch(lnk->type) {
    case LINK_PID:
	if(is_internal_port(item)) {
	    Port *prt = erts_port_lookup(item, ERTS_PORT_SFLGS_INVALID_LOOKUP);
	    if (prt)
		erts_port_exit(NULL,
			       (ERTS_PORT_SIG_FLG_FORCE_SCHED
				| ERTS_PORT_SIG_FLG_BROKEN_LINK),
			       prt,
			       p->common.id,
			       reason,
			       NULL);
	}
	else if(is_external_port(item)) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Erroneous link between %T and external port %T "
			  "found\n",
			  p->common.id,
			  item);
	    erts_send_error_to_logger_nogl(dsbufp);
	    ASSERT(0); /* It isn't possible to setup such a link... */
	}
	else if (is_internal_pid(item)) {
	    ErtsProcLocks rp_locks = (ERTS_PROC_LOCK_LINK
				      | ERTS_PROC_LOCKS_XSIG_SEND);
	    rp = erts_pid2proc(NULL, 0, item, rp_locks);
	    if (rp) {
		rlnk = erts_remove_link(&ERTS_P_LINKS(rp), p->common.id);
		/* If rlnk == NULL, we got unlinked while exiting,
		   i.e., do nothing... */
		if (rlnk) {
		    int xres;
		    erts_destroy_link(rlnk);
		    xres = send_exit_signal(NULL,
					    p->common.id,
					    rp,
					    &rp_locks, 
					    reason,
					    exit_tuple,
					    exit_tuple_sz,
					    SEQ_TRACE_TOKEN(p),
					    p,
					    ERTS_XSIG_FLG_IGN_KILL);
		    if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
			/* We didn't exit the process and it is traced */
			if (IS_TRACED_FL(rp, F_TRACE_PROCS)) {
                            if (rp_locks & ERTS_PROC_LOCKS_XSIG_SEND) {
                                erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_XSIG_SEND);
                                rp_locks &= ~ERTS_PROC_LOCKS_XSIG_SEND;
                            }
			    trace_proc(NULL, 0, rp, am_getting_unlinked, p->common.id);
			}
		    }
		}
		ASSERT(rp != p);
		erts_smp_proc_unlock(rp, rp_locks);
	    }
	}
	else if (is_external_pid(item)) {
	    dep = external_pid_dist_entry(item);
	    if(dep != erts_this_dist_entry) {
		ErtsDSigData dsd;
		int code;
		ErtsDistLinkData dld;
		erts_remove_dist_link(&dld, p->common.id, item, dep);
		erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
		code = erts_dsig_prepare(&dsd, dep, p, ERTS_DSP_NO_LOCK, 0);
		if (code == ERTS_DSIG_PREP_CONNECTED) {
		    code = erts_dsig_send_exit_tt(&dsd, p->common.id, item,
						  reason, SEQ_TRACE_TOKEN(p));
		    ASSERT(code == ERTS_DSIG_SEND_OK);
		}
		erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
		erts_destroy_dist_link(&dld);
	    }
	}
	break;
    case LINK_NODE:
	ASSERT(is_node_name_atom(item));
	dep = erts_sysname_to_connected_dist_entry(item);
	if(dep) {
	    /* dist entries have node links in a separate structure to 
	       avoid confusion */
	    erts_smp_de_links_lock(dep);
	    rlnk = erts_remove_link(&(dep->node_links), p->common.id);
	    erts_smp_de_links_unlock(dep);
	    if (rlnk)
		erts_destroy_link(rlnk);
	    erts_deref_dist_entry(dep);
	}
	break;
	
    default:
	erts_exit(ERTS_ERROR_EXIT, "bad type in link list\n");
	break;
    }
    erts_destroy_link(lnk);
}

static void
resume_suspend_monitor(ErtsSuspendMonitor *smon, void *vc_p)
{
    Process *suspendee = erts_pid2proc((Process *) vc_p, ERTS_PROC_LOCK_MAIN,
				       smon->pid, ERTS_PROC_LOCK_STATUS);
    if (suspendee) {
	ASSERT(suspendee != vc_p);
	if (smon->active)
	    resume_process(suspendee, ERTS_PROC_LOCK_STATUS);
	erts_smp_proc_unlock(suspendee, ERTS_PROC_LOCK_STATUS);
    }
    erts_destroy_suspend_monitor(smon);
}

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
erts_do_exit_process(Process* p, Eterm reason)
{
    p->arity = 0;		/* No live registers */
    p->fvalue = reason;
    

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_exit)) {
        DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(reason_buf, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, process_buf);
        erts_snprintf(reason_buf, DTRACE_TERM_BUF_SIZE - 1, "%T", reason);
        DTRACE2(process_exit, process_buf, reason_buf);
    }
#endif

    if (p->static_flags & ERTS_STC_FLG_SYSTEM_PROC)
	erts_exit(ERTS_DUMP_EXIT, "System process %T terminated: %T\n",
                 p->common.id, reason);

#ifdef ERTS_SMP
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    /* By locking all locks (main lock is already locked) when going
       to exiting state (ERTS_PSFLG_EXITING), it is enough to take any lock when
       looking up a process (erts_pid2proc()) to prevent the looked up
       process from exiting until the lock has been released. */
    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

#ifndef ERTS_SMP
    set_proc_self_exiting(p);
#else
    if (ERTS_PSFLG_PENDING_EXIT & set_proc_self_exiting(p)) {
	/* Process exited before pending exit was received... */
	p->pending_exit.reason = THE_NON_VALUE;
	if (p->pending_exit.bp) {
	    free_message_buffer(p->pending_exit.bp);
	    p->pending_exit.bp = NULL;
	}
    }

    cancel_suspend_of_suspendee(p, ERTS_PROC_LOCKS_ALL); 

    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
#endif

    if (IS_TRACED(p)) {
	if (IS_TRACED_FL(p, F_TRACE_CALLS))
	    erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_EXITING);

    }

    erts_trace_check_exiting(p->common.id);

    ASSERT((ERTS_TRACE_FLAGS(p) & F_INITIAL_TRACE_FLAGS)
	   == F_INITIAL_TRACE_FLAGS);

    ASSERT(erts_proc_read_refc(p) > 0);
    if (ERTS_PTMR_IS_SET(p)) {
	erts_cancel_proc_timer(p);
	ASSERT(erts_proc_read_refc(p) > 0);
    }

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);

    if (IS_TRACED_FL(p,F_TRACE_PROCS))
        trace_proc(p, ERTS_PROC_LOCK_MAIN, p, am_exit, reason);


    /*
     * p->u.initial of this process can *not* be used anymore;
     * will be overwritten by misc termination data.
     */
    p->u.terminate = NULL;

    erts_continue_exit_process(p);
}

void
erts_continue_exit_process(Process *p)
{
    ErtsLink* lnk;
    ErtsMonitor *mon;
    ErtsProcLocks curr_locks = ERTS_PROC_LOCK_MAIN;
    Eterm reason = p->fvalue;
    DistEntry *dep;
    erts_aint32_t state;
    int delay_del_proc = 0;

#ifdef DEBUG
    int yield_allowed = 1;
#endif

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

    ASSERT(ERTS_PROC_IS_EXITING(p));

    ASSERT(erts_proc_read_refc(p) > 0);
    if (p->bif_timers) {
	if (erts_cancel_bif_timers(p, &p->bif_timers, &p->u.terminate)) {
	    ASSERT(erts_proc_read_refc(p) > 0);
	    goto yield;
	}
	ASSERT(erts_proc_read_refc(p) > 0);
	p->bif_timers = NULL;
    }

#ifdef ERTS_SMP
    if (p->flags & F_SCHDLR_ONLN_WAITQ)
	abort_sched_onln_chng_waitq(p);

    if (p->flags & F_HAVE_BLCKD_MSCHED) {
	ErtsSchedSuspendResult ssr;
	ssr = erts_block_multi_scheduling(p, ERTS_PROC_LOCK_MAIN, 0, 0, 1);
	switch (ssr) {
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    goto yield;
	case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_DONE:
	case ERTS_SCHDLR_SSPND_YIELD_DONE:
	    p->flags &= ~F_HAVE_BLCKD_MSCHED;
	    break;
	case ERTS_SCHDLR_SSPND_EINVAL:
	default:
	    erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error: %d\n",
		      __FILE__, __LINE__, (int) ssr);
	}
    }
    if (p->flags & F_HAVE_BLCKD_NMSCHED) {
	ErtsSchedSuspendResult ssr;
	ssr = erts_block_multi_scheduling(p, ERTS_PROC_LOCK_MAIN, 0, 1, 1);
	switch (ssr) {
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    goto yield;
	case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_DONE:
	case ERTS_SCHDLR_SSPND_YIELD_DONE:
	    p->flags &= ~F_HAVE_BLCKD_MSCHED;
	    break;
	case ERTS_SCHDLR_SSPND_EINVAL:
	default:
	    erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error: %d\n",
		     __FILE__, __LINE__, (int) ssr);
	}
    }
#endif

    if (p->flags & F_USING_DB) {
	if (erts_db_process_exiting(p, ERTS_PROC_LOCK_MAIN))
	    goto yield;
	p->flags &= ~F_USING_DB;
    }

    erts_set_gc_state(p, 1);
    state = erts_smp_atomic32_read_acqb(&p->state);
    if (state & ERTS_PSFLG_ACTIVE_SYS) {
	if (cleanup_sys_tasks(p, state, CONTEXT_REDS) >= CONTEXT_REDS/2)
	    goto yield;
    }

    if (p->flags & F_USING_DDLL) {
	erts_ddll_proc_dead(p, ERTS_PROC_LOCK_MAIN);
	p->flags &= ~F_USING_DDLL;
    }

    if (p->nodes_monitors) {
	erts_delete_nodes_monitors(p, ERTS_PROC_LOCK_MAIN);
	p->nodes_monitors = NULL;
    }
	

    if (p->suspend_monitors) {
	erts_sweep_suspend_monitors(p->suspend_monitors,
				    resume_suspend_monitor,
				    p);
	p->suspend_monitors = NULL;
    }

    /*
     * The registered name *should* be the last "erlang resource" to
     * cleanup.
     */
    if (p->common.u.alive.reg) {
	(void) erts_unregister_name(p, ERTS_PROC_LOCK_MAIN, NULL, THE_NON_VALUE);
	ASSERT(!p->common.u.alive.reg);
    }

    if (IS_TRACED_FL(p, F_TRACE_SCHED_EXIT))
        trace_sched(p, curr_locks, am_out_exited);

    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
    curr_locks = ERTS_PROC_LOCKS_ALL;

    /*
     * From this point on we are no longer allowed to yield
     * this process.
     */
#ifdef DEBUG
    yield_allowed = 0;
#endif

    /*
     * Note! The monitor and link fields will be overwritten 
     * by erts_ptab_delete_element() below.
     */
    mon = ERTS_P_MONITORS(p);
    lnk = ERTS_P_LINKS(p);

    {
	/* Do *not* use erts_get_runq_proc() */
	ErtsRunQueue *rq;
	rq = erts_get_runq_current(erts_proc_sched_data(p));

	erts_smp_runq_lock(rq);

#ifdef ERTS_SMP
	ASSERT(p->scheduler_data);
	ASSERT(p->scheduler_data->current_process == p);
	ASSERT(p->scheduler_data->free_process == NULL);

	p->scheduler_data->current_process = NULL;
	p->scheduler_data->free_process = p;
#else
	erts_proc_inc_refc(p); /* Decremented in schedule() */
#endif

	/* Time of death! */
	erts_ptab_delete_element(&erts_proc, &p->common);

	erts_smp_runq_unlock(rq);
    }

    /*
     * All "erlang resources" have to be deallocated before this point,
     * e.g. registered name, so monitoring and linked processes can
     * be sure that all interesting resources have been deallocated
     * when the monitors and/or links hit.
     */

    {
	/* Inactivate and notify free */
	erts_aint32_t n, e, a = erts_smp_atomic32_read_nob(&p->state);
	int refc_inced = 0;
	while (1) {
	    n = e = a;
	    ASSERT(a & ERTS_PSFLG_EXITING);
	    n |= ERTS_PSFLG_FREE;
	    n &= ~ERTS_PSFLG_ACTIVE;
	    if ((n & ERTS_PSFLG_IN_RUNQ) && !refc_inced) {
		erts_proc_inc_refc(p);
		refc_inced = 1;
	    }
	    a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	    if (a == e)
		break;
	}

#ifdef ERTS_DIRTY_SCHEDULERS
        if (a & (ERTS_PSFLG_DIRTY_RUNNING
                 | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
	    p->flags |= F_DELAYED_DEL_PROC;
	    delay_del_proc = 1;
	    /*
	     * The dirty scheduler decrease refc
             * when done with the process...
	     */
	}
#endif

	if (refc_inced && !(n & ERTS_PSFLG_IN_RUNQ))
	    erts_proc_dec_refc(p);
    }
    
    dep = (p->flags & F_DISTRIBUTION) ? erts_this_dist_entry : NULL;

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);

    if (dep) {
	erts_do_net_exits(dep, reason);
    }

    /*
     * Pre-build the EXIT tuple if there are any links.
     */
    if (lnk) {
	DeclareTmpHeap(tmp_heap,4,p);
	Eterm exit_tuple;
	Uint exit_tuple_sz;
	Eterm* hp;

	UseTmpHeap(4,p);
	hp = &tmp_heap[0];

	exit_tuple = TUPLE3(hp, am_EXIT, p->common.id, reason);

	exit_tuple_sz = size_object(exit_tuple);

	{
	    ExitLinkContext context = {p, reason, exit_tuple, exit_tuple_sz};
	    erts_sweep_links(lnk, &doit_exit_link, &context);
	}
	UnUseTmpHeap(4,p);
    }

    {
	ExitMonitorContext context = {reason, p};
	erts_sweep_monitors(mon,&doit_exit_monitor,&context); /* Allocates TmpHeap, but we
								 have none here */
    }

#ifdef ERTS_SMP
    erts_flush_trace_messages(p, 0);
#endif

    ERTS_TRACER_CLEAR(&ERTS_TRACER(p));

    if (!delay_del_proc)
	delete_process(p);

#ifdef ERTS_SMP
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
#endif

    return;

 yield:

#ifdef DEBUG
    ASSERT(yield_allowed);
#endif

    ERTS_SMP_LC_ASSERT(curr_locks == erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & curr_locks);

    p->i = (BeamInstr *) beam_continue_exit;

    if (!(curr_locks & ERTS_PROC_LOCK_STATUS)) {
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	curr_locks |= ERTS_PROC_LOCK_STATUS;
    }

    if (curr_locks != ERTS_PROC_LOCK_MAIN)
	erts_smp_proc_unlock(p, ~ERTS_PROC_LOCK_MAIN & curr_locks);

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

    BUMP_ALL_REDS(p);
}

/*
 * Stack dump functions follow.
 */

void
erts_stack_dump(fmtfn_t to, void *to_arg, Process *p)
{
    Eterm* sp;
    int yreg = -1;

    if (ERTS_TRACE_FLAGS(p) & F_SENSITIVE) {
	return;
    }
    erts_program_counter_info(to, to_arg, p);
    for (sp = p->stop; sp < STACK_START(p); sp++) {
        yreg = stack_element_dump(to, to_arg, sp, yreg);
    }
}

void
erts_program_counter_info(fmtfn_t to, void *to_arg, Process *p)
{
    erts_aint32_t state;
    int i;

    erts_print(to, to_arg, "Program counter: %p (", p->i);
    print_function_from_pc(to, to_arg, p->i);
    erts_print(to, to_arg, ")\n");
    erts_print(to, to_arg, "CP: %p (", p->cp);
    print_function_from_pc(to, to_arg, p->cp);
    erts_print(to, to_arg, ")\n");
    state = erts_smp_atomic32_read_acqb(&p->state);
    if (!(state & (ERTS_PSFLG_RUNNING
		   | ERTS_PSFLG_RUNNING_SYS
		   | ERTS_PSFLG_GC))) {
        erts_print(to, to_arg, "arity = %d\n",p->arity);
	if (!ERTS_IS_CRASH_DUMPING) {
	    /*
	     * Only print the arguments if we are not writing a
	     * crash dump file. The arguments cannot be interpreted
	     * by the crashdump_viewer application and will therefore
	     * only cause problems.
	     */
	    for (i = 0; i < p->arity; i++)
		erts_print(to, to_arg, "   %.*T\n", INT_MAX, p->arg_reg[i]);
	}
    }
}

static void
print_function_from_pc(fmtfn_t to, void *to_arg, BeamInstr* x)
{
    ErtsCodeMFA *cmfa = find_function_from_pc(x);
    if (cmfa == NULL) {
        if (x == beam_exit) {
            erts_print(to, to_arg, "<terminate process>");
        } else if (x == beam_continue_exit) {
            erts_print(to, to_arg, "<continue terminate process>");
        } else if (x == beam_apply+1) {
            erts_print(to, to_arg, "<terminate process normally>");
	} else if (x == 0) {
            erts_print(to, to_arg, "invalid");
        } else {
            erts_print(to, to_arg, "unknown function");
        }
    } else {
	erts_print(to, to_arg, "%T:%T/%d + %d",
		   cmfa->module, cmfa->function, cmfa->arity,
                   (x-(BeamInstr*)cmfa) * sizeof(Eterm));
    }
}

static int
stack_element_dump(fmtfn_t to, void *to_arg, Eterm* sp, int yreg)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erts_print(to, to_arg, "\n%p ", sp);
    } else {
        char sbuf[16];
        erts_snprintf(sbuf, sizeof(sbuf), "y(%d)", yreg);
        erts_print(to, to_arg, "%-8s ", sbuf);
        yreg++;
    }

    if (is_CP(x)) {
        erts_print(to, to_arg, "Return addr %p (", (Eterm *) x);
        print_function_from_pc(to, to_arg, cp_val(x));
        erts_print(to, to_arg, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        erts_print(to, to_arg, "Catch %p (", catch_pc(x));
        print_function_from_pc(to, to_arg, catch_pc(x));
        erts_print(to, to_arg, ")\n");
    } else {
	erts_print(to, to_arg, "%T\n", x);
    }
    return yreg;
}

/*
 * Print scheduler information
 */
void
erts_print_scheduler_info(fmtfn_t to, void *to_arg, ErtsSchedulerData *esdp) {
    int i;
    erts_aint32_t flg;
    Process *p;

    erts_print(to, to_arg, "=scheduler:%u\n", esdp->no);

#ifdef ERTS_SMP
    flg = erts_smp_atomic32_read_dirty(&esdp->ssi->flags);
    erts_print(to, to_arg, "Scheduler Sleep Info Flags: ");
    for (i = 0; i < ERTS_SSI_FLGS_MAX && flg; i++) {
        erts_aint32_t chk = (1 << i);
        if (flg & chk) {
            switch (chk) {
            case ERTS_SSI_FLG_SLEEPING:
                erts_print(to, to_arg, "SLEEPING"); break;
            case ERTS_SSI_FLG_POLL_SLEEPING:
                erts_print(to, to_arg, "POLL_SLEEPING"); break;
            case ERTS_SSI_FLG_TSE_SLEEPING:
                erts_print(to, to_arg, "TSE_SLEEPING"); break;
            case ERTS_SSI_FLG_WAITING:
                erts_print(to, to_arg, "WAITING"); break;
            case ERTS_SSI_FLG_SUSPENDED:
                erts_print(to, to_arg, "SUSPENDED"); break;
            case ERTS_SSI_FLG_MSB_EXEC:
                erts_print(to, to_arg, "MSB_EXEC"); break;
            default:
                erts_print(to, to_arg, "UNKNOWN(%d)", flg); break;
            }
            if (flg > chk)
                erts_print(to, to_arg, " | ");
            flg -= chk;
        }
    }
    erts_print(to, to_arg, "\n");
#endif

    flg = erts_atomic32_read_dirty(&esdp->ssi->aux_work);
    erts_print(to, to_arg, "Scheduler Sleep Info Aux Work: ");
    for (i = 0; i < ERTS_SSI_AUX_WORK_NO_FLAGS && flg; i++) {
        erts_aint32_t chk = (1 << i);
        if (flg & chk) {
	    if (erts_aux_work_flag_descr[i])
                erts_print(to, to_arg, "%s", erts_aux_work_flag_descr[i]);
	    else
                erts_print(to, to_arg, "1<<%d", i);
            if (flg > chk)
                erts_print(to, to_arg, " | ");
            flg -= chk;
        }
    }
    erts_print(to, to_arg, "\n");

    erts_print(to, to_arg, "Current Port: ");
    if (esdp->current_port)
        erts_print(to, to_arg, "%T", esdp->current_port->common.id);
    erts_print(to, to_arg, "\n");

    for (i = 0; i < ERTS_NO_PROC_PRIO_LEVELS; i++) {
        erts_print(to, to_arg, "Run Queue ");
        switch (i) {
        case PRIORITY_MAX:
            erts_print(to, to_arg, "Max ");
            break;
        case PRIORITY_HIGH:
            erts_print(to, to_arg, "High ");
            break;
        case PRIORITY_NORMAL:
            erts_print(to, to_arg, "Normal ");
            break;
        case PRIORITY_LOW:
            erts_print(to, to_arg, "Low ");
            break;
        default:
            erts_print(to, to_arg, "Unknown ");
            break;
        }
        erts_print(to, to_arg, "Length: %d\n",
                   erts_smp_atomic32_read_dirty(&esdp->run_queue->procs.prio_info[i].len));
    }
    erts_print(to, to_arg, "Run Queue Port Length: %d\n",
               erts_smp_atomic32_read_dirty(&esdp->run_queue->ports.info.len));

    flg = erts_smp_atomic32_read_dirty(&esdp->run_queue->flags);
    erts_print(to, to_arg, "Run Queue Flags: ");
    for (i = 0; i < ERTS_RUNQ_FLG_MAX && flg; i++) {
        erts_aint32_t chk = (1 << i);
        if (flg & chk) {
            switch (chk) {
            case (1 << PRIORITY_MAX):
                erts_print(to, to_arg, "NONEMPTY_MAX"); break;
            case (1 << PRIORITY_HIGH):
                erts_print(to, to_arg, "NONEMPTY_HIGH"); break;
            case (1 << PRIORITY_NORMAL):
                erts_print(to, to_arg, "NONEMPTY_NORMAL"); break;
            case (1 << PRIORITY_LOW):
                erts_print(to, to_arg, "NONEMPTY_LOW"); break;
            case (1 << (PRIORITY_MAX + ERTS_RUNQ_FLGS_EMIGRATE_SHFT)):
                erts_print(to, to_arg, "EMIGRATE_MAX"); break;
            case (1 << (PRIORITY_HIGH + ERTS_RUNQ_FLGS_EMIGRATE_SHFT)):
                erts_print(to, to_arg, "EMIGRATE_HIGH"); break;
            case (1 << (PRIORITY_NORMAL + ERTS_RUNQ_FLGS_EMIGRATE_SHFT)):
                erts_print(to, to_arg, "EMIGRATE_NORMAL"); break;
            case (1 << (PRIORITY_LOW + ERTS_RUNQ_FLGS_EMIGRATE_SHFT)):
                erts_print(to, to_arg, "EMIGRATE_LOW"); break;
            case (1 << (PRIORITY_MAX + ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)):
                erts_print(to, to_arg, "IMMIGRATE_MAX"); break;
            case (1 << (PRIORITY_HIGH + ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)):
                erts_print(to, to_arg, "IMMIGRATE_HIGH"); break;
            case (1 << (PRIORITY_NORMAL + ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)):
                erts_print(to, to_arg, "IMMIGRATE_NORMAL"); break;
            case (1 << (PRIORITY_LOW + ERTS_RUNQ_FLGS_IMMIGRATE_SHFT)):
                erts_print(to, to_arg, "IMMIGRATE_LOW"); break;
            case (1 << (PRIORITY_MAX + ERTS_RUNQ_FLGS_EVACUATE_SHFT)):
                erts_print(to, to_arg, "EVACUATE_MAX"); break;
            case (1 << (PRIORITY_HIGH + ERTS_RUNQ_FLGS_EVACUATE_SHFT)):
                erts_print(to, to_arg, "EVACUATE_HIGH"); break;
            case (1 << (PRIORITY_NORMAL + ERTS_RUNQ_FLGS_EVACUATE_SHFT)):
                erts_print(to, to_arg, "EVACUATE_NORMAL"); break;
            case (1 << (PRIORITY_LOW + ERTS_RUNQ_FLGS_EVACUATE_SHFT)):
                erts_print(to, to_arg, "EVACUATE_LOW"); break;
            case ERTS_RUNQ_FLG_OUT_OF_WORK:
                erts_print(to, to_arg, "OUT_OF_WORK"); break;
            case ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK:
                erts_print(to, to_arg, "HALFTIME_OUT_OF_WORK"); break;
            case ERTS_RUNQ_FLG_SUSPENDED:
                erts_print(to, to_arg, "SUSPENDED"); break;
            case ERTS_RUNQ_FLG_CHK_CPU_BIND:
                erts_print(to, to_arg, "CHK_CPU_BIND"); break;
            case ERTS_RUNQ_FLG_INACTIVE:
                erts_print(to, to_arg, "INACTIVE"); break;
            case ERTS_RUNQ_FLG_NONEMPTY:
                erts_print(to, to_arg, "NONEMPTY"); break;
            case ERTS_RUNQ_FLG_PROTECTED:
                erts_print(to, to_arg, "PROTECTED"); break;
            case ERTS_RUNQ_FLG_EXEC:
                erts_print(to, to_arg, "EXEC"); break;
            case ERTS_RUNQ_FLG_MSB_EXEC:
                erts_print(to, to_arg, "MSB_EXEC"); break;
            case ERTS_RUNQ_FLG_MISC_OP:
                erts_print(to, to_arg, "MISC_OP"); break;
            default:
                erts_print(to, to_arg, "UNKNOWN(%d)", flg); break;
            }
            if (flg > chk)
                erts_print(to, to_arg, " | ");
            flg -= chk;
        }
    }
    erts_print(to, to_arg, "\n");

    /* This *MUST* to be the last information in scheduler block */
    p = esdp->current_process;
    erts_print(to, to_arg, "Current Process: ");
    if (esdp->current_process && !(ERTS_TRACE_FLAGS(p) & F_SENSITIVE)) {
	flg = erts_smp_atomic32_read_dirty(&p->state);
	erts_print(to, to_arg, "%T\n", p->common.id);

	erts_print(to, to_arg, "Current Process State: ");
	erts_dump_process_state(to, to_arg, flg);

	erts_print(to, to_arg, "Current Process Internal State: ");
	erts_dump_extended_process_state(to, to_arg, flg);

	erts_print(to, to_arg, "Current Process Program counter: %p (", p->i);
	print_function_from_pc(to, to_arg, p->i);
	erts_print(to, to_arg, ")\n");
	erts_print(to, to_arg, "Current Process CP: %p (", p->cp);
	print_function_from_pc(to, to_arg, p->cp);
	erts_print(to, to_arg, ")\n");

	/* Getting this stacktrace can segfault if we are very very
	   unlucky if called while a process is being garbage collected.
	   Therefore we only call this on other schedulers if we either
	   have protection against segfaults, or we know that the process
	   is not garbage collecting. It *should* always be safe to call
	   on a process owned by us, even if it is currently being garbage
	   collected.
	*/
	erts_print(to, to_arg, "Current Process Limited Stack Trace:\n");
	erts_limited_stack_trace(to, to_arg, p);
    } else
	erts_print(to, to_arg, "\n");

}

/*
 * A nice system halt closing all open port goes as follows:
 * 1) This function schedules the aux work ERTS_SSI_AUX_WORK_REAP_PORTS
 *    on all schedulers, then schedules itself out.
 * 2) All shedulers detect this and set the flag ERTS_RUNQ_FLG_HALTING
 *    on their run queue. The last scheduler sets all non-closed ports
 *    ERTS_PORT_SFLG_HALT. Global atomic erts_halt_progress is used
 *    as refcount to determine which is last.
 * 3) While the run queues has flag ERTS_RUNQ_FLG_HALTING no processes
 *    will be scheduled, only ports.
 * 4) When the last port closes that scheduler calls erlang:halt/1.
 *    The same global atomic is used as refcount.
 *
 * A BIF that calls this should make sure to schedule out to never come back:
 *    erts_halt(code);
 *    ERTS_BIF_YIELD1(bif_export[BIF_erlang_halt_1], BIF_P, NIL);
 */
void erts_halt(int code)
{
    if (-1 == erts_smp_atomic32_cmpxchg_acqb(&erts_halt_progress,
					     erts_no_schedulers,
					     -1)) {
#ifdef ERTS_DIRTY_SCHEDULERS
        ERTS_RUNQ_FLGS_SET(ERTS_DIRTY_CPU_RUNQ, ERTS_RUNQ_FLG_HALTING);
        ERTS_RUNQ_FLGS_SET(ERTS_DIRTY_IO_RUNQ, ERTS_RUNQ_FLG_HALTING);
#endif
	erts_halt_code = code;
	notify_reap_ports_relb();
    }
}

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_dbg_check_halloc_lock(Process *p)
{
    ErtsSchedulerData *esdp;
    if (ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p))
	return 1;
    if ((p->static_flags & ERTS_STC_FLG_SHADOW_PROC)
	&& ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()))
	return 1;
    if (p->common.id == ERTS_INVALID_PID)
	return 1;
    esdp = erts_proc_sched_data(p);
    if (esdp && p == esdp->match_pseudo_process)
	return 1;
    if (erts_thr_progress_is_blocking())
	return 1;
    return 0;
}
#endif
