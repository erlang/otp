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

#define ERL_PROCESS_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_BREAK_HANDLING

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
#include "erl_check_io.h"
#include "erl_poll.h"
#include "erl_proc_sig_queue.h"

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

static ERTS_INLINE int
runq_got_work_to_execute(ErtsRunQueue *rq)
{
    return runq_got_work_to_execute_flags(ERTS_RUNQ_FLGS_GET_NOB(rq));
}

const Process erts_invalid_process = {{ERTS_INVALID_PID}};

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];

int ERTS_WRITE_UNLIKELY(erts_default_spo_flags) = SPO_ON_HEAP_MSGQ;
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
int erts_dcpu_sched_thread_suggested_stack_size = -1;
int erts_dio_sched_thread_suggested_stack_size = -1;
#ifdef ERTS_ENABLE_LOCK_CHECK
ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];
#endif

typedef struct {
    int aux_work;
    int tse;
} ErtsBusyWaitParams;

static ErtsBusyWaitParams sched_busy_wait_params[ERTS_SCHED_TYPE_LAST + 1];

static ERTS_INLINE ErtsBusyWaitParams *
sched_get_busy_wait_params(ErtsSchedulerData *esdp)
{
    return &sched_busy_wait_params[esdp->type];
}

static ErtsAuxWorkData *aux_thread_aux_work_data;
static ErtsAuxWorkData *poll_thread_aux_work_data;

#define ERTS_SCHDLR_SSPND_CHNG_NMSB		(((erts_aint32_t) 1) << 0)
#define ERTS_SCHDLR_SSPND_CHNG_MSB		(((erts_aint32_t) 1) << 1)
#define ERTS_SCHDLR_SSPND_CHNG_ONLN		(((erts_aint32_t) 1) << 2)
#define ERTS_SCHDLR_SSPND_CHNG_DCPU_ONLN	(((erts_aint32_t) 1) << 3)

typedef struct ErtsMultiSchedulingBlock_ {
    int ongoing;
    ErtsProcList *blckrs;
    ErtsProcList *chngq;
} ErtsMultiSchedulingBlock;

typedef struct ErtsSchedTypeCounters_ {
    Uint32 normal;
    Uint32 dirty_cpu;
    Uint32 dirty_io;
} ErtsSchedTypeCounters;

static struct ErtsSchedSuspend_ {
    erts_mtx_t mtx;
    ErtsSchedTypeCounters online;
    ErtsSchedTypeCounters curr_online;
    ErtsSchedTypeCounters active;
    erts_atomic32_t changing;
    ErtsProcList *chngq;
    Eterm changer;
    ErtsMultiSchedulingBlock nmsb; /* Normal multi Scheduling Block */
    ErtsMultiSchedulingBlock msb; /* Multi Scheduling Block */
    ErtsSchedType last_msb_dirty_type;
} schdlr_sspnd;

static void init_scheduler_suspend(void);

static ERTS_INLINE Uint32
schdlr_sspnd_eq_nscheds(ErtsSchedTypeCounters *val1p, ErtsSchedTypeCounters *val2p)
{
    int res = val1p->normal == val2p->normal;
    res &= val1p->dirty_cpu == val2p->dirty_cpu;
    res &= val1p->dirty_io == val2p->dirty_io;
    return res;
}

static ERTS_INLINE Uint32
schdlr_sspnd_get_nscheds(ErtsSchedTypeCounters *valp,
                         ErtsSchedType type)
{
    switch (type) {
    case ERTS_SCHED_NORMAL:
        return valp->normal;
    case ERTS_SCHED_DIRTY_CPU:
        return valp->dirty_cpu;
    case ERTS_SCHED_DIRTY_IO:
        return valp->dirty_io;
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
    res += valp->dirty_cpu;
    res += valp->dirty_io;
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
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu--;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io--;
	break;
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
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu++;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io++;
	break;
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
    case ERTS_SCHED_DIRTY_CPU:
        valp->dirty_cpu = no;
	break;
    case ERTS_SCHED_DIRTY_IO:
        valp->dirty_io = no;
	break;
    default:
	ERTS_INTERNAL_ERROR("Invalid scheduler type");
    }
}

static struct {
    erts_mtx_t update_mtx;
    erts_atomic32_t no_runqs;
    int last_active_runqs;
    int forced_check_balance;
    erts_atomic32_t checking_balance;
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


erts_sched_stat_t erts_sched_stat;

static erts_tsd_key_t ERTS_WRITE_UNLIKELY(sched_data_key);

#if ERTS_POLL_USE_SCHEDULER_POLLING
static erts_atomic32_t function_calls;
static erts_atomic32_t doing_sys_schedule;
#endif
static erts_atomic32_t no_empty_run_queues;
long erts_runq_supervision_interval = 0;
static ethr_event runq_supervision_event;
static erts_tid_t runq_supervisor_tid;
static erts_atomic_t runq_supervisor_sleeping;

ErtsAlignedRunQueue * ERTS_WRITE_UNLIKELY(erts_aligned_run_queues);
Uint ERTS_WRITE_UNLIKELY(erts_no_run_queues);


struct {
    union {
        erts_atomic32_t active;
        char align__[ERTS_CACHE_LINE_SIZE];
    } cpu;
    union {
        erts_atomic32_t active;
        char align__[ERTS_CACHE_LINE_SIZE];
    } io;
} dirty_count erts_align_attribute(ERTS_CACHE_LINE_SIZE);


static ERTS_INLINE void
dirty_active(ErtsSchedulerData *esdp, erts_aint32_t add)
{
    erts_aint32_t val;
    erts_atomic32_t *ap;
    switch (esdp->type) {
    case ERTS_SCHED_DIRTY_CPU:
        ap = &dirty_count.cpu.active;
        break;
    case ERTS_SCHED_DIRTY_IO:
        ap = &dirty_count.io.active;
        break;
    default:
        ap = NULL;
        ERTS_INTERNAL_ERROR("Not a dirty scheduler");
        break;
    }

    /*
     * All updates done under run-queue lock, so
     * no inc or dec needed...
     */
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(esdp->run_queue));

    val = erts_atomic32_read_nob(ap);
    val += add;
    erts_atomic32_set_nob(ap, val);
}

ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_scheduler_data);
ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_cpu_scheduler_data);
ErtsAlignedSchedulerData * ERTS_WRITE_UNLIKELY(erts_aligned_dirty_io_scheduler_data);
typedef union {
    Process dsp;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(Process))];
} ErtsAlignedDirtyShadowProcess;

typedef union {
    ErtsSchedulerSleepInfo ssi;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerSleepInfo))];
} ErtsAlignedSchedulerSleepInfo;

static ErtsAlignedSchedulerSleepInfo *aligned_sched_sleep_info;
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_cpu_sched_sleep_info;
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_io_sched_sleep_info;
static ErtsAlignedSchedulerSleepInfo *aligned_poll_thread_sleep_info;

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
    ERTS_PSTT_ETS_FREE_FIXATION,
    ERTS_PSTT_PRIO_SIG  /* Elevate prio on signal management */
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

#define ERTS_POLL_THREAD_SLEEP_INFO_IX(IX)                              \
        (ASSERT(0 <= ((int) (IX))                                       \
                && ((int) (IX)) < ((int) erts_no_poll_threads)),        \
         &aligned_poll_thread_sleep_info[(IX)].ssi)
#define ERTS_SCHED_SLEEP_INFO_IX(IX)					\
    (ASSERT(((int)-1) <= ((int) (IX))                 \
            && ((int) (IX)) < ((int) erts_no_schedulers)),		\
     &aligned_sched_sleep_info[(IX)].ssi)
#define ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_cpu_schedulers)),	\
     &aligned_dirty_cpu_sched_sleep_info[(IX)].ssi)
#define ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_io_schedulers)),	\
     &aligned_dirty_io_sched_sleep_info[(IX)].ssi)

#define ERTS_FOREACH_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++) {			\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_FOREACH_OP_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    int online__ = (int) schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,	\
						  ERTS_SCHED_NORMAL);	\
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&schdlr_sspnd.mtx));	\
    for (ix__ = 0; ix__ < online__; ix__++) {				\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, NRQS, DO, DOX)		\
do {									\
    ErtsRunQueue *RQVAR;						\
    int nrqs = (NRQS);                                                  \
    int ix__;								\
    for (ix__ = 0; ix__ < nrqs; ix__++) {                               \
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_runq_lock(RQVAR);					\
	{ DO; }								\
    }									\
    { DOX; }								\
    for (ix__ = 0; ix__ < nrqs; ix__++)                                 \
	erts_runq_unlock(ERTS_RUNQ_IX(ix__));			\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ(RQVAR, DO)                             \
  ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, erts_no_run_queues + ERTS_NUM_DIRTY_RUNQS, DO, )

#define ERTS_ATOMIC_FOREACH_NORMAL_RUNQ(RQVAR, DO)                      \
    ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, erts_no_run_queues, DO, )


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
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY;
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
    valid |= ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP;
    valid |= ERTS_SSI_AUX_WORK_MISC_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_DD;
    valid |= ERTS_SSI_AUX_WORK_DD_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_CNCLD_TMRS;
    valid |= ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR;
    valid |= ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP;
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

static void wake_scheduler(ErtsRunQueue *rq);

#if defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_lc_runq_is_locked(ErtsRunQueue *runq)
{
    return erts_lc_mtx_is_locked(&runq->mtx);
}
#endif


static ERTS_INLINE Uint64
ensure_later_proc_interval(Uint64 interval)
{
    return erts_ensure_later_interval_nob(erts_ptab_interval(&erts_proc), interval);
}

Uint64
erts_get_proc_interval(void)
{
    return erts_current_interval_nob(erts_ptab_interval(&erts_proc));
}

Uint64
erts_ensure_later_proc_interval(Uint64 interval)
{
    return ensure_later_proc_interval(interval);
}

Uint64
erts_step_proc_interval(void)
{
    return erts_step_interval_nob(erts_ptab_interval(&erts_proc));
}

void
erts_pre_init_process(void)
{
    erts_tsd_key_create(&sched_data_key, "erts_sched_data_key");

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

    erts_psd_required_locks[ERTS_PSD_DIST_ENTRY].get_locks
        = ERTS_PSD_DIST_ENTRY_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_DIST_ENTRY].set_locks
        = ERTS_PSD_DIST_ENTRY_SET_LOCKS;

    erts_psd_required_locks[ERTS_PSD_PENDING_SUSPEND].get_locks
        = ERTS_PSD_PENDING_SUSPEND_GET_LOCKS;
    erts_psd_required_locks[ERTS_PSD_PENDING_SUSPEND].set_locks
        = ERTS_PSD_PENDING_SUSPEND_SET_LOCKS;
#endif
}

/* initialize the scheduler */
void
erts_init_process(int ncpu, int proc_tab_size, int legacy_proc_tab)
{

    erts_init_proc_lock(ncpu);

    init_proclist_alloc();

    erts_ptab_init_table(&erts_proc,
			 ERTS_ALC_T_PROC_TABLE,
			 NULL,
			 (ErtsPTabElementCommon *) &erts_invalid_process.common,
			 proc_tab_size,
			 sizeof(Process),
			 "process_table",
			 legacy_proc_tab,
			 1
	);

    last_reductions = 0;
    last_exact_reductions = 0;
}

void
erts_late_init_process(void)
{
    int ix;

    erts_spinlock_init(&erts_sched_stat.lock, "sched_stat", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER);

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
    if (esdp->type != ERTS_SCHED_NORMAL) {
        erts_atomic32_init_nob(&esdp->sched_wall_time.u.mod, 0);
        esdp->sched_wall_time.enabled = 1;
        esdp->sched_wall_time.start = time_stamp;
        esdp->sched_wall_time.working.total = 0;
        esdp->sched_wall_time.working.start = ERTS_SCHED_WTIME_IDLE;
    }
    else
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
		erts_runq_lock(rq);
		locked = 1;
	    }
	}
    }

    if (!initially_locked && locked)
	erts_runq_unlock(rq);

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

    if (!working) {
        ERTS_MSACC_SET_STATE_X(ERTS_MSACC_STATE_BUSY_WAIT);
    } else {
        ERTS_MSACC_SET_STATE_X(ERTS_MSACC_STATE_OTHER);
    }
}


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
    erts_atomic32_t refc;
    int want_dirty_cpu;
    int want_dirty_io;
} ErtsSchedWallTimeReq;

typedef struct {
    Process *proc;
    Eterm ref;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Uint req_sched;
    erts_atomic32_t refc;
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

    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));

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
	erts_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    if (erts_atomic32_dec_read_nob(&swtrp->refc) == 0)
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

    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));

    if (!set && !esdp->sched_wall_time.enabled)
	return THE_NON_VALUE;

    swtrp = swtreq_alloc();
    ref = erts_make_ref(c_p);
    hp = &swtrp->ref_heap[0];

    swtrp->set = set;
    swtrp->enable = enable;
    swtrp->proc = c_p;
    swtrp->ref = STORE_NC(&hp, NULL, ref);
    swtrp->req_sched = esdp->no;
    swtrp->want_dirty_cpu = want_dirty_cpu;
    swtrp->want_dirty_io = want_dirty_io;
    erts_atomic32_init_nob(&swtrp->refc,
			       (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint32) erts_no_schedulers);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_sched_wall_time,
					  (void *) swtrp);

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

    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));

    sz = ERTS_REF_THING_SIZE;
    mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);
    hpp = &hp;
    msg = STORE_NC(hpp, ohp, scrp->ref);

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (scrp->req_sched == esdp->no)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;

    if (rp_locks)
	erts_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    if (erts_atomic32_dec_read_nob(&scrp->refc) == 0)
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
    erts_atomic32_init_nob(&scrp->refc, (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint) erts_no_schedulers);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
					  reply_system_check,
					  (void *) scrp);

    reply_system_check((void *) scrp);

    return ref;
}

static ERTS_INLINE ErtsProcList *
proclist_create(Process *p)
{
    ErtsProcList *plp = proclist_alloc();
    ensure_later_proc_interval(p->common.u.alive.started_interval);
    if (erts_atomic32_read_nob(&p->state) & ERTS_PSFLG_FREE)
        plp->u.p = p;
    else
        plp->u.pid = p->common.id;
    plp->started_interval = p->common.u.alive.started_interval;
    return plp;
}

static ERTS_INLINE ErtsProcList *
proclist_copy(ErtsProcList *plp0)
{
    ErtsProcList *plp1 = proclist_alloc();
    plp1->u.pid = plp0->u.pid;
    plp1->started_interval = plp0->started_interval;
    return plp1;
}

static ERTS_INLINE void
proclist_destroy(ErtsProcList *plp)
{
    proclist_free(plp);
}

ErtsProcList *
erts_proclist_create(Process *p)
{
    return proclist_create(p);
}

ErtsProcList *
erts_proclist_copy(ErtsProcList *plp)
{
    return proclist_copy(plp);
}

void
erts_proclist_destroy(ErtsProcList *plp)
{
    proclist_destroy(plp);
}

void
erts_proclist_dump(fmtfn_t to, void *to_arg, ErtsProcList *plp)
{
    ErtsProcList *first = plp;

    while (plp) {
        if (is_pid(plp->u.pid))
            erts_print(to, to_arg, "%T", plp->u.pid);
        else
            erts_print(to, to_arg, "%T", plp->u.p->common.id);
        plp = plp->next;
        if (plp == first)
            break;
    }
    erts_print(to, to_arg, "\n");
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

    psd = (ErtsPSD *) erts_atomic_cmpxchg_mb(&p->psd,
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


void
erts_sched_finish_poke(ErtsSchedulerSleepInfo *ssi,
                       erts_aint32_t flags)
{
    switch (flags & ERTS_SSI_FLGS_SLEEP_TYPE) {
    case ERTS_SSI_FLG_POLL_SLEEPING:
	erts_check_io_interrupt(ssi->psi, 1);
	break;
    case ERTS_SSI_FLG_POLL_SLEEPING|ERTS_SSI_FLG_TSE_SLEEPING:
	/*
	 * Thread progress blocking while poll sleeping; need
	 * to signal on both...
	 */
	erts_check_io_interrupt(ssi->psi, 1);
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
	    erts_sched_poke(ssi);
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
	erts_sched_poke(ssi);
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
	erts_thr_progress_wakeup(erts_thr_prgr_data(awdp->esdp), val);
    }
}

static ERTS_INLINE void
haw_thr_prgr_soft_wakeup(ErtsAuxWorkData *awdp, ErtsThrPrgrVal val)
{
    if (erts_thr_progress_cmp(val, awdp->latest_wakeup) > 0) {
	awdp->latest_wakeup = val;
	haw_chk_later_cleanup_op_wakeup(awdp, val);
	erts_thr_progress_wakeup(erts_thr_prgr_data(awdp->esdp), val);
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
	    erts_thr_progress_wakeup(erts_thr_prgr_data(awdp->esdp), val);
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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ix = 0; /* aux_thread + schedulers */

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
	set_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC_THR_PRGR);
	haw_thr_prgr_soft_wakeup(awdp, erts_thr_q_need_thr_progress(q));
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


static ERTS_INLINE erts_aint32_t
handle_misc_aux_work_thr_prgr(ErtsAuxWorkData *awdp,
			      erts_aint32_t aux_work,
			      int waiting)
{
    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

    if (!erts_thr_progress_has_reached_this(haw_thr_prgr_current(awdp),
					    awdp->misc.thr_prgr))
	return aux_work & ~ERTS_SSI_AUX_WORK_MISC_THR_PRGR;

    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC_THR_PRGR);

    return misc_aux_work_clean(&misc_aux_work_queues[awdp->sched_id].q,
			       awdp,
			       aux_work & ~ERTS_SSI_AUX_WORK_MISC_THR_PRGR);
}


static ERTS_INLINE void
schedule_misc_aux_work(int sched_id,
		       void (*func)(void *),
		       void *arg)
{
    ErtsThrQ_t *q;
    erts_misc_aux_work_t *mawp;

    ASSERT(0 <= sched_id && sched_id <= erts_no_schedulers);

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

        /* ignore_self is meaningless on dirty schedulers since aux work can
         * only run on normal schedulers, and their ids do not translate. */
        if(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp)) {
            self = (int)esdp->no;
        }
    }

    ASSERT(0 < max_sched && max_sched <= erts_no_schedulers);

    for (id = 1; id <= max_sched; id++) {
	if (id == self)
	    continue;
	schedule_misc_aux_work(id, func, arg);
   }
}


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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY);
    if (erts_check_async_ready(awdp->async_ready.queue)) {
	if (set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY)
	    & ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN) {
	    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
	    aux_work &= ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
	}
	return aux_work;
    }
    awdp->async_ready.need_thr_prgr = 0;
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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

    if (awdp->async_ready.need_thr_prgr
	&& !erts_thr_progress_has_reached_this(haw_thr_prgr_current(awdp),
					       awdp->async_ready.thr_prgr)) {
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
    }

    awdp->async_ready.need_thr_prgr = 0;
    thr_prgr_p = (void *) &awdp->async_ready.thr_prgr;

    switch (erts_async_ready_clean(awdp->async_ready.queue, thr_prgr_p)) {
    case ERTS_ASYNC_READY_CLEAN:
	unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN);
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
    case ERTS_ASYNC_READY_NEED_THR_PRGR:
	haw_thr_prgr_soft_wakeup(awdp, awdp->async_ready.thr_prgr);
	awdp->async_ready.need_thr_prgr = 1;
	return aux_work & ~ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
    default:
	return aux_work;
    }
}



static ERTS_INLINE erts_aint32_t
handle_fix_alloc(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    erts_aint32_t res;

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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
    ASSERT(esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp));

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


void
erts_schedule_thr_prgr_later_op(void (*later_func)(void *),
				void *later_data,
				ErtsThrPrgrLaterOp *lop)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsThrPrgrVal later = enqueue_later_op(esdp, later_func, later_data, lop);
    haw_thr_prgr_wakeup(&esdp->aux_work_data, later);
}

void
erts_schedule_thr_prgr_later_cleanup_op(void (*later_func)(void *),
					void *later_data,
					ErtsThrPrgrLaterOp *lop,
					UWord size)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsThrPrgrVal later = enqueue_later_op(esdp, later_func, later_data, lop);
    haw_thr_prgr_later_cleanup_op_wakeup(&esdp->aux_work_data, later, size);
}

static ERTS_INLINE erts_aint32_t
handle_debug_wait_completed(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    erts_aint32_t saved_aux_work, flags;

    ASSERT(!awdp->esdp || !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp));

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
    awdp = esdp ? &esdp->aux_work_data : aux_thread_aux_work_data;

    wait_flags = 0;
    aux_work_flags = ERTS_SSI_AUX_WORK_DEBUG_WAIT_COMPLETED;

    if (debug_wait_completed_flags & ERTS_DEBUG_WAIT_COMPLETED_DEALLOCATIONS) {
	erts_alloc_fix_alloc_shrink(awdp->sched_id, 0);
	wait_flags |= (ERTS_SSI_AUX_WORK_DD
		       | ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	aux_work_flags |= ERTS_SSI_AUX_WORK_DD;
    }

    if (debug_wait_completed_flags & ERTS_DEBUG_WAIT_COMPLETED_TIMER_CANCELLATIONS) {
	wait_flags |= (ERTS_SSI_AUX_WORK_CNCLD_TMRS
		       | ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR);
	if (awdp->esdp && !ERTS_SCHEDULER_IS_DIRTY(awdp->esdp))
	    aux_work_flags |= ERTS_SSI_AUX_WORK_CNCLD_TMRS;
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

    if (erts_atomic32_dec_read_mb(&debug_wait_completed_count) == 1) {
        erts_aint32_t count = (erts_aint32_t) erts_no_schedulers;
        count += 1; /* aux thread */
        erts_atomic32_set_nob(&debug_wait_completed_count, count);

        /* scheduler threads */
        erts_schedule_multi_misc_aux_work(0,
                                          erts_no_schedulers,
                                          setup_thr_debug_wait_completed,
                                          lop->proc);
        /* aux_thread */
        erts_schedule_misc_aux_work(0,
                                    setup_thr_debug_wait_completed,
                                    lop->proc);
    }
    erts_free(ERTS_ALC_T_DEBUG, lop);
}


static void
init_thr_debug_wait_completed(void *vproc)
{
    if (debug_wait_completed_flags == ERTS_DEBUG_WAIT_COMPLETED_AUX_WORK) {
        if (erts_atomic32_dec_read_mb(&debug_wait_completed_count) == 1) {
            erts_atomic32_set_nob(&debug_wait_completed_count, 0);
            erts_resume((Process *) vproc, (ErtsProcLocks) 0);
            erts_proc_dec_refc((Process *) vproc);
        }
    }
    else {
        struct debug_lop* lop = erts_alloc(ERTS_ALC_T_DEBUG,
                                           sizeof(struct debug_lop));
        lop->proc = vproc;
        erts_schedule_thr_prgr_later_op(later_thr_debug_wait_completed, lop, &lop->lop);
    }
}


int
erts_debug_wait_completed(Process *c_p, int flags)
{
    /* Only one process at a time can do this, +1 to mark as busy */
    erts_aint32_t count = (erts_aint32_t) (erts_no_schedulers + 1);

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

erts_atomic32_t erts_halt_progress;
int erts_halt_code;

static ERTS_INLINE erts_aint32_t
handle_reap_ports(ErtsAuxWorkData *awdp, erts_aint32_t aux_work, int waiting)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_REAP_PORTS);
    ERTS_RUNQ_FLGS_SET(awdp->esdp->run_queue, ERTS_RUNQ_FLG_HALTING);

    if (erts_atomic32_dec_read_acqb(&erts_halt_progress) == 0) {
	int i, max = erts_ptab_max(&erts_port);
	erts_atomic32_set_nob(&erts_halt_progress, 1);
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

	    erts_port_lock(prt);

            if (prt->common.u.alive.reg &&
                prt->common.u.alive.reg->name == am_heart_port) {
                /* Leave heart port to not get killed before flushing is done*/
                erts_port_release(prt);
                continue;
            }

	    state = erts_atomic32_read_nob(&prt->state);
	    if (!(state & (ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
			   | ERTS_PORT_SFLG_HALT))) {
		state = erts_atomic32_read_bor_relb(&prt->state,
						    ERTS_PORT_SFLG_HALT);
		erts_atomic32_inc_nob(&erts_halt_progress);
		if (!(state & (ERTS_PORT_SFLG_EXITING|ERTS_PORT_SFLG_CLOSING)))
		    erts_deliver_port_exit(prt, prt->common.id, am_killed, 0, 1);
	    }

	    erts_port_release(prt);
	}
	if (erts_atomic32_dec_read_nob(&erts_halt_progress) == 0) {
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
    yield |= erts_handle_yielded_alcu_blockscan(awdp->esdp,
                                                &awdp->yield.alcu_blockscan);

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
    haw_thr_prgr_current_reset(awdp);

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
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DELAYED_AW_WAKEUP,
		    handle_delayed_aux_work_wakeup);
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DD,
		    handle_delayed_dealloc);
    /* DD must be before DD_THR_PRGR */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_DD_THR_PRGR,
		    handle_delayed_dealloc_thr_prgr);

    HANDLE_AUX_WORK((ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
		     | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC),
		    handle_fix_alloc);

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_THR_PRGR_LATER_OP,
		    handle_thr_prgr_later_op);
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_CNCLD_TMRS,
		    handle_canceled_timers);
    /* CNCLD_TMRS must be before CNCLD_TMRS_THR_PRGR */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_CNCLD_TMRS_THR_PRGR,
		    handle_canceled_timers_thr_prgr);

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_ASYNC_READY,
		    handle_async_ready);
    /* ASYNC_READY must be before ASYNC_READY_CLEAN */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN,
		    handle_async_ready_clean);

    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_MISC_THR_PRGR,
		    handle_misc_aux_work_thr_prgr);
    /* MISC_THR_PRGR must be before MISC */
    HANDLE_AUX_WORK(ERTS_SSI_AUX_WORK_MISC,
		    handle_misc_aux_work);

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

    if (waiting && !aux_work)
	haw_thr_prgr_current_check_progress(awdp);

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

    i = 0;

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

Uint
erts_active_schedulers(void)
{
    Uint as = erts_no_schedulers;

    ERTS_ATOMIC_FOREACH_NORMAL_RUNQ(rq, as -= abs(rq->waiting));

    return as;
}

static ERTS_INLINE void
sched_waiting(Uint no, ErtsRunQueue *rq)
{
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
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
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
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
	erts_aint32_t empty = erts_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 <= empty && empty < 2*erts_no_run_queues);
#endif
	if (!erts_runq_supervision_interval)
	    erts_atomic32_inc_relb(&no_empty_run_queues);
	else {
	    erts_atomic32_inc_mb(&no_empty_run_queues);
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
	erts_aint32_t empty = erts_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 < empty && empty <= 2*erts_no_run_queues);
#endif
	if (!erts_runq_supervision_interval)
	    erts_atomic32_dec_relb(&no_empty_run_queues);
	else {
	    erts_aint32_t no;
	    no = erts_atomic32_dec_read_mb(&no_empty_run_queues);
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
	oflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
	oflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
	flgs = erts_atomic32_read_acqb(&ssi->flags);
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
        erts_check_io_interrupt(ssi->psi, 0);
    }

    while (1) {
	oflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
    erts_atomic32_read_bor_acqb(&ssi->flags,
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
	aflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
    erts_atomic32_read_band_nob(&ssi->flags,
				    ~(ERTS_SSI_FLG_SLEEPING
				      | ERTS_SSI_FLG_TSE_SLEEPING));
}

static void init_aux_work_data(ErtsAuxWorkData *awdp, ErtsSchedulerData *esdp, char *dawwp);

void
erts_aux_thread_poke()
{
    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(-1));
}

static void *
aux_thread(void *unused)
{
    ErtsAuxWorkData *awdp = aux_thread_aux_work_data;
    ErtsSchedulerSleepInfo *ssi = ERTS_SCHED_SLEEP_INFO_IX(-1);
    erts_aint32_t aux_work;
    ErtsThrPrgrCallbacks callbacks;
    ErtsThrPrgrData *tpd;
    int thr_prgr_active = 1;
    ERTS_MSACC_DECLARE_CACHE();

#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[] = "aux_thread";
	erts_lc_set_thread_name(buf);
    }
#endif

    erts_port_task_pre_alloc_init_thread();
    ssi->event = erts_tse_fetch();

    erts_msacc_init_thread("aux", 1, 1);

    callbacks.arg = (void *) ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

    tpd = erts_thr_progress_register_managed_thread(NULL, &callbacks, 1);
    init_aux_work_data(awdp, NULL, NULL);
    awdp->ssi = ssi;

#if ERTS_POLL_USE_FALLBACK
#if ERTS_POLL_USE_SCHEDULER_POLLING
    ssi->psi = erts_create_pollset_thread(-2, tpd);
#else
    ssi->psi = erts_create_pollset_thread(-1, tpd);
#endif
#endif

    sched_prep_spin_wait(ssi);

    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);

    while (1) {
	erts_aint32_t flgs;

	aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	if (aux_work) {
	    if (!thr_prgr_active)
		erts_thr_progress_active(tpd, thr_prgr_active = 1);
	    aux_work = handle_aux_work(awdp, aux_work, 1);
            ERTS_MSACC_UPDATE_CACHE();
	    if (aux_work && erts_thr_progress_update(tpd))
		erts_thr_progress_leader_update(tpd);
	}

	if (!aux_work) {

#ifdef ERTS_BREAK_REQUESTED
            if (ERTS_BREAK_REQUESTED)
                erts_do_break_handling();
#endif

	    if (thr_prgr_active)
		erts_thr_progress_active(tpd, thr_prgr_active = 0);

#if ERTS_POLL_USE_FALLBACK

	    flgs = sched_spin_wait(ssi, 0);

	    if (flgs & ERTS_SSI_FLG_SLEEPING) {
		ASSERT(flgs & ERTS_SSI_FLG_WAITING);
		flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_POLL_SLEEPING);
		if (flgs & ERTS_SSI_FLG_SLEEPING) {
		    ASSERT(flgs & ERTS_SSI_FLG_POLL_SLEEPING);
		    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
                    erts_check_io(ssi->psi, ERTS_POLL_INF_TIMEOUT);
		}
            }
#else
            erts_thr_progress_prepare_wait(tpd);

	    flgs = sched_spin_wait(ssi, 0);

	    if (flgs & ERTS_SSI_FLG_SLEEPING) {
		flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_TSE_SLEEPING);
		if (flgs & ERTS_SSI_FLG_SLEEPING) {
                    int res;
		    ASSERT(flgs & ERTS_SSI_FLG_TSE_SLEEPING);
		    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
                    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_SLEEP);
                    do {
                        res = erts_tse_wait(ssi->event);
                    } while (res == EINTR);
                    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);
		}
            }
            erts_thr_progress_finalize_wait(tpd);
#endif
	}

	flgs = sched_prep_spin_wait(ssi);
    }
    return NULL;
}

static void *
poll_thread(void *arg)
{
    int id = (int)(UWord)arg;
    ErtsAuxWorkData *awdp = poll_thread_aux_work_data+id;
    ErtsSchedulerSleepInfo *ssi = ERTS_POLL_THREAD_SLEEP_INFO_IX(id);
    erts_aint32_t aux_work;
    ErtsThrPrgrCallbacks callbacks;
    int thr_prgr_active = 1;
    struct erts_poll_thread *psi;
    ErtsThrPrgrData *tpd;
    ERTS_MSACC_DECLARE_CACHE();

#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[] = "poll_thread";
	erts_lc_set_thread_name(buf);
    }
#endif

    erts_port_task_pre_alloc_init_thread();
    ssi->event = erts_tse_fetch();

    erts_msacc_init_thread("poll", id, 0);

    callbacks.arg = (void *) ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

    tpd = erts_thr_progress_register_managed_thread(NULL, &callbacks, 0);
    init_aux_work_data(awdp, NULL, NULL);
    awdp->ssi = ssi;

    psi = erts_create_pollset_thread(id, tpd);

    ssi->psi = psi;

    sched_prep_spin_wait(ssi);

    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);

    while (1) {
	erts_aint32_t flgs;

	aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	if (aux_work) {
	    if (!thr_prgr_active)
		erts_thr_progress_active(tpd, thr_prgr_active = 1);
	    aux_work = handle_aux_work(awdp, aux_work, 1);
            ERTS_MSACC_UPDATE_CACHE();
	    if (aux_work && erts_thr_progress_update(tpd))
		erts_thr_progress_leader_update(tpd);
	}

	if (!aux_work) {
	    if (thr_prgr_active)
		erts_thr_progress_active(tpd, thr_prgr_active = 0);

	    flgs = sched_spin_wait(ssi, 0);

	    if (flgs & ERTS_SSI_FLG_SLEEPING) {
		ASSERT(flgs & ERTS_SSI_FLG_WAITING);
		flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_POLL_SLEEPING);
		if (flgs & ERTS_SSI_FLG_SLEEPING) {
		    ASSERT(flgs & ERTS_SSI_FLG_POLL_SLEEPING);
		    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
                    erts_check_io(psi, ERTS_POLL_INF_TIMEOUT);
		}
	    }
	}

	flgs = sched_prep_spin_wait(ssi);
    }
    return NULL;
}

#if ERTS_POLL_USE_SCHEDULER_POLLING
static ERTS_INLINE void
clear_sys_scheduling(void)
{
    erts_atomic32_set_relb(&function_calls, 0);
    erts_atomic32_set_mb(&doing_sys_schedule, 0);
}

static ERTS_INLINE int
try_set_sys_scheduling(void)
{
    return 0 == erts_atomic32_cmpxchg_acqb(&doing_sys_schedule, 1, 0);
}


static ERTS_INLINE int
prepare_for_sys_schedule(void)
{
    while (!erts_port_task_have_outstanding_io_tasks()
           && try_set_sys_scheduling()) {
        if (!erts_port_task_have_outstanding_io_tasks())
            return 1;
        clear_sys_scheduling();
    }
    return 0;
}

#else
#define clear_sys_scheduling()
#define prepare_for_sys_schedule() 0
#endif

#ifdef HARDDEBUG
#define ERTS_HDBG_CHK_SLEEP_LIST(SL, L, F, FN) \
    check_sleepers_list((SL), (L), (F), (FN))
static void check_sleepers_list(ErtsSchedulerSleepList *sl,
                                int lock,
                                ErtsSchedulerSleepInfo *find,
                                ErtsSchedulerSleepInfo *find_not)
{
    ErtsSchedulerSleepInfo *last_out;
    int found = 0;

    if (lock)
        erts_mtx_lock(&sl->lock);

    ERTS_ASSERT(!find_not || (!find_not->next && !find_not->prev));
    
    last_out = sl->list;
    if (last_out) {
        ErtsSchedulerSleepInfo *tmp = last_out;
        do {
            ERTS_ASSERT(tmp->next);
            ERTS_ASSERT(tmp->prev);
            ERTS_ASSERT(tmp->next->prev == tmp);
            ERTS_ASSERT(tmp->prev->next == tmp);
            ERTS_ASSERT(tmp != find_not);
            if (tmp == find)
                found = !0;
            tmp = tmp->next;
            
        } while (tmp != last_out);
    }
    ERTS_ASSERT(!find || found);

    if (lock)
        erts_mtx_unlock(&sl->lock);
}
#else
#define ERTS_HDBG_CHK_SLEEP_LIST(SL, L, F, FN) ((void) 0)
#endif

static void
scheduler_wait(int *fcalls, ErtsSchedulerData *esdp, ErtsRunQueue *rq)
{
    int working = 1;
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    int spincount;
    erts_aint32_t aux_work = 0;
    int thr_prgr_active = 1;
    erts_aint32_t flgs;
    ERTS_MSACC_PUSH_STATE();

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    flgs = sched_prep_spin_wait(ssi);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {
	/* Go suspend instead... */
	return;
    }

    if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix)) {
        erts_mtx_lock(&rq->sleepers.lock);
        ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, NULL, ssi);
        ASSERT(!ssi->next); /* Not in sleepers list */
        ASSERT(!ssi->prev);
        if (!rq->sleepers.list) {
            ssi->next = ssi->prev = ssi;
            rq->sleepers.list = ssi;
        }
        else {
            ssi->prev = rq->sleepers.list;
            ssi->next = rq->sleepers.list->next;
            ssi->prev->next = ssi;
            ssi->next->prev = ssi;
        }
        ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, ssi, NULL);
        erts_mtx_unlock(&rq->sleepers.lock);
        dirty_active(esdp, -1);
    }

    sched_waiting(esdp->no, rq);

    erts_runq_unlock(rq);

    spincount = sched_get_busy_wait_params(esdp)->tse;

    if (ERTS_SCHEDULER_IS_DIRTY(esdp))
        dirty_sched_wall_time_change(esdp, working = 0);
    else if (thr_prgr_active != working)
        sched_wall_time_change(esdp, working = thr_prgr_active);

    while (1) {
        ErtsMonotonicTime current_time = 0;

        aux_work = erts_atomic32_read_acqb(&ssi->aux_work);

        if (aux_work && ERTS_SCHEDULER_IS_DIRTY(esdp)) {
            ERTS_INTERNAL_ERROR("Executing aux work on a dirty scheduler.");
        }

        if (aux_work) {
            if (!thr_prgr_active) {
                erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
                sched_wall_time_change(esdp, 1);
            }
            aux_work = handle_aux_work(&esdp->aux_work_data, aux_work, 1);
            ERTS_MSACC_UPDATE_CACHE();
            if (aux_work && erts_thr_progress_update(erts_thr_prgr_data(esdp)))
                erts_thr_progress_leader_update(erts_thr_prgr_data(esdp));
        }

        if (aux_work) {
            flgs = erts_atomic32_read_acqb(&ssi->flags);
            current_time = erts_get_monotonic_time(esdp);
            if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref)) {
                if (!thr_prgr_active) {
                    erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
                    sched_wall_time_change(esdp, 1);
                }
                erts_bump_timers(esdp->timer_wheel, current_time);
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
                    erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
                    sched_wall_time_change(esdp, 1);
                }
            }
            else if (!ERTS_SCHEDULER_IS_DIRTY(esdp) && prepare_for_sys_schedule()) {
                /* We sleep in check_io, only for normal schedulers */
                if (thr_prgr_active) {
                    erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 0);
                    sched_wall_time_change(esdp, 0);
                }
                flgs = sched_spin_wait(ssi, 0);
                if (flgs & ERTS_SSI_FLG_SLEEPING) {
                    ASSERT(flgs & ERTS_SSI_FLG_WAITING);
                    flgs = sched_set_sleeptype(ssi, ERTS_SSI_FLG_POLL_SLEEPING);
                    if (flgs & ERTS_SSI_FLG_SLEEPING) {
                        ASSERT(flgs & ERTS_SSI_FLG_POLL_SLEEPING);
                        ASSERT(flgs & ERTS_SSI_FLG_WAITING);
                        erts_check_io(ssi->psi, timeout_time);
                        current_time = erts_get_monotonic_time(esdp);
                    }
                }
                *fcalls = 0;
                clear_sys_scheduling();
            } else {
                if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
                    if (thr_prgr_active) {
                        erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 0);
                        sched_wall_time_change(esdp, 0);
                    }
                    erts_thr_progress_prepare_wait(erts_thr_prgr_data(esdp));
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
                            ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_SLEEP);
                            res = erts_tse_twait(ssi->event, timeout);
                            ERTS_MSACC_POP_STATE();
                            current_time = ERTS_SCHEDULER_IS_DIRTY(esdp) ? 0 :
                                erts_get_monotonic_time(esdp);
                        } while (res == EINTR);
                    }
                }
                if (!ERTS_SCHEDULER_IS_DIRTY(esdp))
                    erts_thr_progress_finalize_wait(erts_thr_prgr_data(esdp));
            }
            if (!ERTS_SCHEDULER_IS_DIRTY(esdp) && current_time >= timeout_time)
                erts_bump_timers(esdp->timer_wheel, current_time);
        }

        if (!(flgs & ERTS_SSI_FLG_WAITING)) {
            ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
            break;
        }

        flgs = sched_prep_cont_spin_wait(ssi);
        spincount = sched_get_busy_wait_params(esdp)->aux_work;

        if (!(flgs & ERTS_SSI_FLG_WAITING)) {
            ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
            break;
        }

    }

    if (flgs & ~(ERTS_SSI_FLG_SUSPENDED|ERTS_SSI_FLG_MSB_EXEC))
        erts_atomic32_read_band_nob(&ssi->flags,
                                        (ERTS_SSI_FLG_SUSPENDED
                                         | ERTS_SSI_FLG_MSB_EXEC));

    if (ERTS_SCHEDULER_IS_DIRTY(esdp)) {
        dirty_sched_wall_time_change(esdp, working = 1);
        erts_mtx_lock(&rq->sleepers.lock);
        ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, ssi->next ? ssi : NULL, NULL);
        if (ssi->next) { /* Still in list... */
            if (ssi->next == ssi) {
                ASSERT(rq->sleepers.list == ssi);
                ASSERT(ssi->prev == ssi);
                rq->sleepers.list = NULL;
            }
            else {
                ASSERT(ssi->prev != ssi);
                if (rq->sleepers.list == ssi)
                    rq->sleepers.list = ssi->next;
                ssi->prev->next = ssi->next;
                ssi->next->prev = ssi->prev;
            }
            ssi->next = ssi->prev = NULL;
        }
        ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, NULL, ssi);
        erts_mtx_unlock(&rq->sleepers.lock);
    }
    else if (!thr_prgr_active) {
        erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
        sched_wall_time_change(esdp, 1);
    }

    erts_runq_lock(rq);
    sched_active(esdp->no, rq);

    if (ERTS_SCHEDULER_IS_DIRTY(esdp))
        dirty_active(esdp, 1);

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
}


static ERTS_INLINE erts_aint32_t
ssi_flags_set_wake(ErtsSchedulerSleepInfo *ssi)
{
    /* reset all flags but suspended */
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = 0;
    erts_aint32_t xflgs = ERTS_SSI_FLG_SLEEPING|ERTS_SSI_FLG_WAITING;
    while (1) {
	oflgs = erts_atomic32_cmpxchg_relb(&ssi->flags, nflgs, xflgs);
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


static void
dcpu_sched_ix_suspend_wake(Uint ix)
{
    ErtsSchedulerSleepInfo* ssi = ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(ix);
    erts_atomic32_read_bor_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);
    ssi_wake(ssi);
}

static void
dio_sched_ix_suspend_wake(Uint ix)
{
    ErtsSchedulerSleepInfo* ssi = ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(ix);
    erts_atomic32_read_bor_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);
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
    ERTS_LC_ASSERT(!erts_lc_runq_is_locked(rq)
		       || ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    ssi_wake(rq->scheduler->ssi);
}

static void
wake_dirty_scheduler(ErtsRunQueue *rq)
{
    ErtsSchedulerSleepInfo *lo_ssi, *fo_ssi;
    ErtsSchedulerSleepList *sl;

    ASSERT(ERTS_RUNQ_IX_IS_DIRTY(rq->ix));

    sl = &rq->sleepers;
    erts_mtx_lock(&sl->lock);
    ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, NULL, NULL);
    lo_ssi = sl->list;
    if (!lo_ssi) {
	erts_mtx_unlock(&sl->lock);
        wake_scheduler(rq);
    }
    else {
	erts_aint32_t flgs;
        fo_ssi = lo_ssi->next;
        ASSERT(fo_ssi->prev == lo_ssi);
        if (fo_ssi == lo_ssi) {
	    ASSERT(lo_ssi->prev == lo_ssi);
            sl->list = NULL;
        }
        else {
            ASSERT(lo_ssi->prev != lo_ssi);
	    lo_ssi->next = fo_ssi->next;
	    fo_ssi->next->prev = fo_ssi->prev;
	}
        fo_ssi->next = fo_ssi->prev = NULL;
        ERTS_HDBG_CHK_SLEEP_LIST(&rq->sleepers, 0, NULL, fo_ssi);
	erts_mtx_unlock(&sl->lock);

	ERTS_THR_MEMORY_BARRIER;
	flgs = ssi_flags_set_wake(fo_ssi);
	erts_sched_finish_poke(fo_ssi, flgs);
    }
}

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
    erts_atomic32_init_nob(&balance_info.no_runqs, no_runqs);
}

static ERTS_INLINE void
get_no_runqs(int *active, int *used)
{
    erts_aint32_t no_runqs = erts_atomic32_read_nob(&balance_info.no_runqs);
    if (active)
	*active = (int) (no_runqs & ERTS_NO_RUNQS_MASK);
    if (used)
	*used = (int) ((no_runqs >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK);
}

static ERTS_INLINE void
set_no_used_runqs(int used)
{
    erts_aint32_t exp = erts_atomic32_read_nob(&balance_info.no_runqs);
    while (1) {
	erts_aint32_t act, new;
	new = (used & ERTS_NO_RUNQS_MASK) << ERTS_NO_USED_RUNQS_SHIFT;
	new |= exp & ERTS_NO_RUNQS_MASK;
	act = erts_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
	if (act == exp)
	    break;
	exp = act;
    }
}

static ERTS_INLINE void
set_no_active_runqs(int active)
{
    erts_aint32_t exp = erts_atomic32_read_nob(&balance_info.no_runqs);
    while (1) {
	erts_aint32_t act, new;
	if ((exp & ERTS_NO_RUNQS_MASK) == active)
	    break;
	new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
	new |= active & ERTS_NO_RUNQS_MASK;
	act = erts_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
	if (act == exp)
	    break;
	exp = act;
    }
}

static ERTS_INLINE int
try_inc_no_active_runqs(int active)
{
    erts_aint32_t exp = erts_atomic32_read_nob(&balance_info.no_runqs);
    if (((exp >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK) < active)
	return 0;
    if ((exp & ERTS_NO_RUNQS_MASK) + 1 == active) {
	erts_aint32_t new, act;
	new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
	new |= active & ERTS_NO_RUNQS_MASK;
	act = erts_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
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


static ERTS_INLINE void
smp_notify_inc_runq(ErtsRunQueue *runq)
{
    if (runq) {
	if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
	    wake_dirty_scheduler(runq);
	else
	    wake_scheduler(runq);
    }
}

void
erts_notify_inc_runq(ErtsRunQueue *runq)
{
    smp_notify_inc_runq(runq);
}

void
erts_sched_notify_check_cpu_bind(void)
{
    int ix;
    for (ix = 0; ix < erts_no_run_queues; ix++) {
	ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
	(void) ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_CHK_CPU_BIND);
	wake_scheduler(rq);
    }
}


static ERTS_INLINE void
enqueue_process(ErtsRunQueue *runq, int prio, Process *p)
{
    ErtsRunPrioQueue *rpq;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));

    erts_inc_runq_len(runq, &runq->procs.prio_info[prio], prio);

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
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));

    if (prev_proc)
	prev_proc->next = proc->next;
    else
	rpq->first = proc->next;
    if (!proc->next)
	rpq->last = prev_proc;

    if (!rpq->first)
	rpq->last = NULL;

    erts_dec_runq_len(runq, rqi, prio);
}


static ERTS_INLINE Process *
dequeue_process(ErtsRunQueue *runq, int prio_q, erts_aint32_t *statep)
{
    erts_aint32_t state;
    int prio;
    ErtsRunPrioQueue *rpq;
    ErtsRunQueueInfo *rqi;
    Process *p;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(runq));

    ASSERT(PRIORITY_NORMAL == prio_q
	   || PRIORITY_HIGH == prio_q
	   || PRIORITY_MAX == prio_q);

    rpq = &runq->procs.prio[prio_q];
    p = rpq->first;
    if (!p)
	return NULL;

    ERTS_THR_DATA_DEPENDENCY_READ_MEMORY_BARRIER;

    state = erts_atomic32_read_nob(&p->state);
    ASSERT(state & ERTS_PSFLG_IN_RUNQ);

    if (statep)
	*statep = state;

    prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);

    rqi = &runq->procs.prio_info[prio];

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
    ASSERT(proxy->u.real_proc);
    erts_proc_dec_refc(proxy->u.real_proc);
#ifdef DEBUG
    proxy->u.real_proc = NULL;
#endif
    ASSERT(erts_atomic32_read_nob(&proxy->state) & ERTS_PSFLG_PROXY);
    erts_free(ERTS_ALC_T_PROC, proxy);
}


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
    erts_runq_unlock(c_rq);

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
	    erts_runq_lock(rq);
	    if (prio == ERTS_PORT_PRIO_LEVEL) {
		Port *prt;
		prt = erts_dequeue_port(rq);
		if (prt)
                    erts_set_runq_port(prt, c_rq);
		erts_runq_unlock(rq);
		if (prt) {
		    rq = erts_port_runq(prt);
                    if (rq != c_rq)
                        ERTS_INTERNAL_ERROR("Unexpected run-queue");
                    erts_enqueue_port(c_rq, prt);
                    if (!iflag)
                        return; /* done */
                    erts_runq_unlock(c_rq);
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
		    state = erts_atomic32_read_acqb(&proc->state);
		    if (prio == (int) ERTS_PSFLGS_GET_PRQ_PRIO(state)
                        && erts_try_change_runq_proc(proc, c_rq)) {
			ErtsRunQueueInfo *rqi = &rq->procs.prio_info[prio];
			unqueue_process(rq, rpq, rqi, prio, prev_proc, proc);
			erts_runq_unlock(rq);
			rq_locked = 0;

			erts_runq_lock(c_rq);
			enqueue_process(c_rq, prio, proc);
			if (!iflag)
			    return; /* done */
			erts_runq_unlock(c_rq);
			break;
		    }
		    prev_proc = proc;
		    proc = proc->next;
		}
		if (rq_locked)
		    erts_runq_unlock(rq);
	    }
	}
    }

    erts_runq_lock(c_rq);
}

static ERTS_INLINE void
suspend_run_queue(ErtsRunQueue *rq)
{
    erts_atomic32_read_bor_nob(&rq->scheduler->ssi->flags,
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

    erts_runq_lock(rq);

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
	    len = erts_atomic32_read_dirty(&rq->procs.prio_info[pix].len);
	    rq->procs.prio_info[pix].max_len = len;
	    rq->procs.prio_info[pix].reds = 0;
	}
	len = erts_atomic32_read_dirty(&rq->ports.info.len);
	rq->ports.info.max_len = len;
	rq->ports.info.reds = 0;
	len = erts_atomic32_read_dirty(&rq->len);
	rq->max_len = len;

    }

    erts_runq_unlock(rq);

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
    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

    proc = sbpp->first;
    while (proc) {
	erts_aint32_t state = erts_atomic32_read_acqb(&proc->state);
	next = proc->next;
	enqueue_process(rq, (int) ERTS_PSFLGS_GET_PRQ_PRIO(state), proc);
	proc = next;
    }
}


static ERTS_INLINE void
clear_proc_dirty_queue_bit(Process *p, ErtsRunQueue *rq, int prio_bit)
{
    erts_aint32_t old;
    erts_aint32_t qb = prio_bit;
    if (rq == ERTS_DIRTY_CPU_RUNQ)
	qb <<= ERTS_PDSFLGS_IN_CPU_PRQ_MASK_OFFSET;
    else {
	ASSERT(rq == ERTS_DIRTY_IO_RUNQ);
	qb <<= ERTS_PDSFLGS_IN_IO_PRQ_MASK_OFFSET;
    }
    old = (int) erts_atomic32_read_band_mb(&p->dirty_state, ~qb);
    ASSERT(old & qb); (void)old;
}



static void
evacuate_run_queue(ErtsRunQueue *rq,
		   ErtsStuckBoundProcesses *sbpp)
{
    int prio_q;
    ErtsRunQueue *to_rq;
    ErtsMigrationPaths *mps;
    ErtsMigrationPath *mp;

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

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
	erts_runq_unlock(rq);

	erts_runq_lock(to_rq);
	if (to_rq->misc.end)
	    to_rq->misc.end->next = start;
	else
	    to_rq->misc.start = start;

	to_rq->misc.end = end;

	non_empty_runq(to_rq);

	erts_runq_unlock(to_rq);
	smp_notify_inc_runq(to_rq);
	erts_runq_lock(rq);
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
            erts_set_runq_port(prt, to_rq);
	    erts_runq_unlock(rq);
	    prt_rq = erts_port_runq(prt);
            if (prt_rq != to_rq)
                ERTS_INTERNAL_ERROR("Unexpected run-queue");
            erts_enqueue_port(to_rq, prt);
            erts_runq_unlock(to_rq);
	    erts_runq_lock(rq);
	    prt = rq->ports.start;
	}
	smp_notify_inc_runq(to_rq);
    }

    /* Evacuate scheduled processes */
    for (prio_q = 0; prio_q < ERTS_NO_PROC_PRIO_QUEUES; prio_q++) {
	erts_aint32_t state;
	Process *proc;

        if (!mp->prio[prio_q].runq)
            return;
        if (prio_q == PRIORITY_NORMAL && !mp->prio[PRIORITY_LOW].runq)
            return;

	proc = dequeue_process(rq, prio_q, &state);
	while (proc) {
	    Process *real_proc;
	    int prio;
	    erts_aint32_t max_qbit, qbit;

	    prio = ERTS_PSFLGS_GET_PRQ_PRIO(state);
	    qbit = ((erts_aint32_t) 1) << prio;

            real_proc = ((state & ERTS_PSFLG_PROXY)
                         ? proc->u.real_proc
                         : proc);
            ASSERT(real_proc);

	    max_qbit = (state >> ERTS_PSFLGS_IN_PRQ_MASK_OFFSET);
	    max_qbit &= ERTS_PSFLGS_QMASK;
	    max_qbit |= 1 << ERTS_PSFLGS_QMASK_BITS;
	    max_qbit &= -max_qbit;

	    if (qbit > max_qbit) {
                erts_aint32_t clr_bits;
		/* Process already queued with higher prio; drop it... */
                clr_bits = qbit << ERTS_PSFLGS_IN_PRQ_MASK_OFFSET;
		if (real_proc != proc)
		    free_proxy_proc(proc);
		else
		    clr_bits |= ERTS_PSFLG_IN_RUNQ;

                state = erts_atomic32_read_band_mb(&real_proc->state, ~clr_bits);
                ASSERT((state & clr_bits) == clr_bits);

                if (((state & (ERTS_PSFLG_ACTIVE | ERTS_PSFLG_FREE))
                     | (clr_bits & ERTS_PSFLG_IN_RUNQ))
                    == (ERTS_PSFLG_FREE | ERTS_PSFLG_IN_RUNQ)) {
                    /* 
                     * inactive-free and not queued by proxy
                     *
                     * Corresponds to increment in
                     * erts_continue_exit_process() after
                     * state ERTS_CONTINUE_EXIT_DONE.
                     */
                    erts_proc_dec_refc(real_proc);
                }

		goto handle_next_proc;
	    }

            prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);
            to_rq = mp->prio[prio].runq;

            if (!to_rq)
                goto handle_next_proc;

	    if (!erts_try_change_runq_proc(proc, to_rq)) {
		/* Bound processes get stuck here... */
		proc->next = NULL;
		if (sbpp->last)
		    sbpp->last->next = proc;
		else
		    sbpp->first = proc;
		sbpp->last = proc;
	    }
	    else {
		erts_runq_unlock(rq);

		erts_runq_lock(to_rq);
		enqueue_process(to_rq, prio, proc);
		erts_runq_unlock(to_rq);

                smp_notify_inc_runq(to_rq);

		erts_runq_lock(rq);
	    }

	handle_next_proc:
	    proc = dequeue_process(rq, prio_q, &state);
	}

    }
}

static int
try_steal_task_from_victim(ErtsRunQueue *rq, int *rq_lockedp, ErtsRunQueue *vrq, Uint32 flags)
{
    Uint32 procs_qmask = flags & ERTS_RUNQ_FLGS_PROCS_QMASK;
    int max_prio_bit;
    ErtsRunPrioQueue *rpq;

    if (*rq_lockedp) {
	erts_runq_unlock(rq);
	*rq_lockedp = 0;
    }

    ERTS_LC_ASSERT(!erts_lc_runq_is_locked(rq));

    erts_runq_lock(vrq);

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
	    if (erts_try_change_runq_proc(proc, rq)) {
                erts_aint32_t state = erts_atomic32_read_acqb(&proc->state);
		/* Steal process */
		int prio = (int) ERTS_PSFLGS_GET_PRQ_PRIO(state);
		ErtsRunQueueInfo *rqi = &vrq->procs.prio_info[prio];
		unqueue_process(vrq, rpq, rqi, prio, prev_proc, proc);
		erts_runq_unlock(vrq);

		erts_runq_lock(rq);
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

    ERTS_LC_ASSERT(erts_lc_runq_is_locked(vrq));

    /*
     * Check for a runnable port to steal...
     */

    if (vrq->ports.start) {
	ErtsRunQueue *prt_rq;
	Port *prt = erts_dequeue_port(vrq);
	erts_set_runq_port(prt, rq);
	erts_runq_unlock(vrq);
	prt_rq = erts_port_runq(prt);
        if (prt_rq != rq)
            ERTS_INTERNAL_ERROR("Unexpected run-queue");
        *rq_lockedp = 1;
        erts_enqueue_port(rq, prt);
        return !0;
    }

    erts_runq_unlock(vrq);

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

    ERTS_LC_CHK_RUNQ_LOCK(rq, rq_locked);

    get_no_runqs(&active_rqs, &blnc_rqs);

    if (active_rqs > blnc_rqs)
	active_rqs = blnc_rqs;

    if (rq->ix < active_rqs) {

	/* First try to steal from an inactive run queue... */
	if (active_rqs < blnc_rqs) {
	    int no = blnc_rqs - active_rqs;
	    int stop_ix = vix = active_rqs + rq->ix % no;
	    while (erts_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
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
	while (erts_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
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
	erts_runq_lock(rq);

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
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&balance_info.update_mtx));

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

    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&balance_info.update_mtx));

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

    if (erts_atomic32_xchg_nob(&balance_info.checking_balance, 1)) {
	c_rq->check_balance_reds = INT_MAX;
	return;
    }

    get_no_runqs(NULL, &blnc_no_rqs);
    if (blnc_no_rqs == 1) {
	c_rq->check_balance_reds = INT_MAX;
	erts_atomic32_set_nob(&balance_info.checking_balance, 0);
	return;
    }

    erts_runq_unlock(c_rq);

    if (balance_info.halftime) {	
	balance_info.halftime = 0;
	erts_atomic32_set_nob(&balance_info.checking_balance, 0);
	ERTS_FOREACH_RUNQ(rq,
	{
	    if (rq->waiting)
		(void) ERTS_RUNQ_FLGS_SET(rq, ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
	    else
		(void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
	    rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	});

	erts_runq_lock(c_rq);
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
    erts_mtx_lock(&balance_info.update_mtx);

    forced = balance_info.forced_check_balance;
    balance_info.forced_check_balance = 0;

    get_no_runqs(&current_active, &blnc_no_rqs);

    if (blnc_no_rqs == 1) {
	erts_mtx_unlock(&balance_info.update_mtx);
	erts_runq_lock(c_rq);
	c_rq->check_balance_reds = INT_MAX;
	erts_atomic32_set_nob(&balance_info.checking_balance, 0);
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
	erts_runq_lock(rq);

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

	erts_runq_unlock(rq);
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

	erts_runq_lock(rq);
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
	rq->max_len = erts_atomic32_read_dirty(&rq->len);
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    ErtsRunQueueInfo *rqi;
	    rqi = (pix == ERTS_PORT_PRIO_LEVEL
		   ? &rq->ports.info
		   : &rq->procs.prio_info[pix]);
	    erts_reset_max_len(rq, rqi);
	    rqi->reds = 0;
	}

	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	erts_runq_unlock(rq);
    }

    erts_atomic32_set_nob(&balance_info.checking_balance, 0);

    balance_info.n++;
    retire_mpaths(old_mpaths);
    erts_mtx_unlock(&balance_info.update_mtx);

    erts_runq_lock(c_rq);
}

static void
change_no_used_runqs(int used)
{
    ErtsMigrationPaths *new_mpaths, *old_mpaths;
    int qix;
    erts_mtx_lock(&balance_info.update_mtx);
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

    erts_mtx_unlock(&balance_info.update_mtx);

    erts_runq_lock(ERTS_RUNQ_IX(0));
    ERTS_RUNQ_IX(0)->check_balance_reds = 0;
    erts_runq_unlock(ERTS_RUNQ_IX(0));
}



Uint
erts_debug_nbalance(void)
{
    Uint n;
    erts_mtx_lock(&balance_info.update_mtx);
    n = balance_info.n;
    erts_mtx_unlock(&balance_info.update_mtx);
    return n;
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


typedef struct {
    ErtsSchedWakeupOtherThreshold threshold;
    ErtsSchedWakeupOtherType type;
    int limit;
    int dec_shift;
    int dec_mask;
    void (*check)(ErtsRunQueue *rq, Uint32 flags);
} ErtsWakeupOtherParams;

static ErtsWakeupOtherParams sched_wakeup_other_params[ERTS_SCHED_TYPE_LAST + 1];

static ERTS_INLINE ErtsWakeupOtherParams *
runq_get_wakeup_other_params(ErtsRunQueue *rq)
{
    ErtsSchedulerData *esdp = rq->scheduler;
    return &sched_wakeup_other_params[esdp->type];
}

static void
wakeup_other_check(ErtsRunQueue *rq, Uint32 flags)
{
    ErtsWakeupOtherParams *wo_params = runq_get_wakeup_other_params(rq);
    int wo_reds = rq->wakeup_other_reds;

    if (wo_reds) {
	int left_len = erts_atomic32_read_dirty(&rq->len) - 1;
	if (left_len < 1) {
	    int wo_reduce = wo_reds << wo_params->dec_shift;
	    wo_reduce &= wo_params->dec_mask;
	    rq->wakeup_other -= wo_reduce;
	    if (rq->wakeup_other < 0)
		rq->wakeup_other = 0;
	}
	else {
	    rq->wakeup_other += (left_len*wo_reds
				 + ERTS_WAKEUP_OTHER_FIXED_INC);
	    if (rq->wakeup_other > wo_params->limit) {
		if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix)) {
		    if (rq->waiting) {
			wake_dirty_scheduler(rq);
		    }
		} else
		{
		    int empty_rqs =
			erts_atomic32_read_acqb(&no_empty_run_queues);
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
wakeup_other_set_limit(ErtsWakeupOtherParams *params)
{
    switch (params->threshold) {
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH;
	params->dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_HIGH;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_HIGH;
	params->dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_HIGH;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM;
	params->dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_MEDIUM;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_LOW;
	params->dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_LOW;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW;
	params->dec_shift = ERTS_WAKEUP_OTHER_DEC_SHIFT_VERY_LOW;
	break;
    }

    if (params->dec_shift < 0)
	params->dec_mask = (1 << (sizeof(params->dec_mask)*8
				  + params->dec_shift)) - 1;
    else {
	params->dec_mask = 0;
	params->dec_mask = ~params->dec_mask;
    }
}

static void
wakeup_other_check_legacy(ErtsRunQueue *rq, Uint32 flags)
{
    ErtsWakeupOtherParams *wo_params = runq_get_wakeup_other_params(rq);
    int wo_reds = rq->wakeup_other_reds;
    if (wo_reds) {
	erts_aint32_t len = erts_atomic32_read_dirty(&rq->len);
	if (len < 2) {
	    rq->wakeup_other -= ERTS_WAKEUP_OTHER_DEC_LEGACY*wo_reds;
	    if (rq->wakeup_other < 0)
		rq->wakeup_other = 0;
	}
	else if (rq->wakeup_other < wo_params->limit)
	    rq->wakeup_other += len*wo_reds + ERTS_WAKEUP_OTHER_FIXED_INC_LEGACY;
	else {
	    if (flags & ERTS_RUNQ_FLG_PROTECTED)
		(void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);
	    if (erts_atomic32_read_acqb(&no_empty_run_queues) != 0) {
		wake_scheduler_on_empty_runq(rq);
		rq->wakeup_other = 0;
	    }
	    rq->wakeup_other = 0;
	}
    }
    rq->wakeup_other_reds = 0;
}

static void
wakeup_other_set_limit_legacy(ErtsWakeupOtherParams *params)
{
    switch (params->threshold) {
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_HIGH_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_LOW_LEGACY;
	break;
    case ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW:
	params->limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW_LEGACY;
	break;
    }
}

static void
set_wakeup_other_data(void)
{
    ErtsSchedType type;

    for (type = ERTS_SCHED_TYPE_FIRST; type <= ERTS_SCHED_TYPE_LAST; type++) {
        ErtsWakeupOtherParams *params = &sched_wakeup_other_params[type];

        switch (params->type) {
        case ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT:
            params->check = wakeup_other_check;
            wakeup_other_set_limit(params);
            break;
        case ERTS_SCHED_WAKEUP_OTHER_TYPE_LEGACY:
            params->check = wakeup_other_check_legacy;
            wakeup_other_set_limit_legacy(params);
            break;
        }
    }
}

static int
no_runqs_to_supervise(void)
{
    int used;
    erts_aint32_t nerq = erts_atomic32_read_acqb(&no_empty_run_queues);
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
		erts_runq_lock(rq);
		if (erts_atomic32_read_dirty(&rq->len) != 0)
		    wake_scheduler_on_empty_runq(rq); /* forced wakeup... */
		erts_runq_unlock(rq);
	    }
	}
    }
    return NULL;
}


void
erts_early_init_scheduling(int no_schedulers)
{
    ErtsSchedType type;

    aux_work_timeout_early_init(no_schedulers);

    for (type = ERTS_SCHED_TYPE_FIRST; type <= ERTS_SCHED_TYPE_LAST; type++) {
        erts_sched_set_wakeup_other_threshold(type, "medium");
        erts_sched_set_wakeup_other_type(type, "default");

        erts_sched_set_busy_wait_threshold(type, "medium");
    }

    erts_sched_set_busy_wait_threshold(ERTS_SCHED_DIRTY_CPU, "short");
    erts_sched_set_busy_wait_threshold(ERTS_SCHED_DIRTY_IO, "short");
}

int
erts_sched_set_wakeup_other_threshold(ErtsSchedType sched_type, char *str)
{
    ErtsWakeupOtherParams *params = &sched_wakeup_other_params[sched_type];

    if (sys_strcmp(str, "very_high") == 0) {
        params->threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_HIGH;
    } else if (sys_strcmp(str, "high") == 0) {
        params->threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_HIGH;
    } else if (sys_strcmp(str, "medium") == 0) {
        params->threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_MEDIUM;
    } else if (sys_strcmp(str, "low") == 0) {
        params->threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_LOW;
    } else if (sys_strcmp(str, "very_low") == 0) {
        params->threshold = ERTS_SCHED_WAKEUP_OTHER_THRESHOLD_VERY_LOW;
    } else {
        return EINVAL;
    }

    return 0;
}

int
erts_sched_set_wakeup_other_type(ErtsSchedType sched_type, char *str)
{
    ErtsWakeupOtherParams *params = &sched_wakeup_other_params[sched_type];

    if (sys_strcmp(str, "default") == 0) {
        params->type = ERTS_SCHED_WAKEUP_OTHER_TYPE_DEFAULT;
    } else if (sys_strcmp(str, "legacy") == 0) {
        params->type = ERTS_SCHED_WAKEUP_OTHER_TYPE_LEGACY;
    } else {
        return EINVAL;
    }

    return 0;
}

int
erts_sched_set_busy_wait_threshold(ErtsSchedType sched_type, char *str)
{
    ErtsBusyWaitParams *params = &sched_busy_wait_params[sched_type];
    int aux_work_fact, sys_sched;

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

    params->tse = sys_sched * ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT;
    params->aux_work = sys_sched * aux_work_fact;

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
        case ERTS_SCHED_DIRTY_CPU:
            id = (int) erts_no_schedulers;
            id += (int) esdp->dirty_no;
            break;
        case ERTS_SCHED_DIRTY_IO:
            id = (int) erts_no_schedulers;
            id += (int) erts_no_dirty_cpu_schedulers;
            id += (int) esdp->dirty_no;
            break;
        default:
            ERTS_INTERNAL_ERROR("Invalid scheduler type");
            break;
        }
    }

    awdp->sched_id = id;
    awdp->esdp = esdp;
    awdp->ssi = esdp ? esdp->ssi : NULL;
    awdp->latest_wakeup = ERTS_THR_PRGR_VAL_FIRST;
    awdp->misc.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->dd.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->cncld_tmrs.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->later_op.thr_prgr = ERTS_THR_PRGR_VAL_FIRST;
    awdp->later_op.size = 0;
    awdp->later_op.first = NULL;
    awdp->later_op.last = NULL;
    awdp->async_ready.need_thr_prgr = 0;
    awdp->async_ready.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->async_ready.queue = NULL;
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
    erts_bits_init_state(&esdp->erl_bits_state);
    esdp->match_pseudo_process = NULL;
    esdp->free_process = NULL;
    esdp->x_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   ERTS_X_REGS_ALLOCATED *
					   sizeof(Eterm));
    esdp->f_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   MAX_REG * sizeof(FloatDef));
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
	erts_atomic32_init_nob(&shadow_proc->state,
				   (ERTS_PSFLG_ACTIVE
				    | ERTS_PSFLG_DIRTY_RUNNING
				    | ERTS_PSFLG_PROXY));
	shadow_proc->static_flags = ERTS_STC_FLG_SHADOW_PROC;
    }

    ssi->esdp = esdp;
    esdp->ssi = ssi;
    esdp->current_process = NULL;
    esdp->current_port = NULL;
    esdp->current_nif = NULL;

    esdp->virtual_reds = 0;
    esdp->cpu_id = -1;

    erts_init_atom_cache_map(&esdp->atom_cache_map);

    esdp->last_monotonic_time = 0;
    esdp->check_time_reds = 0;

    esdp->thr_id = (Uint32) num;
    erts_sched_bif_unique_init(esdp);

    esdp->io.out = (Uint64) 0;
    esdp->io.in = (Uint64) 0;

    esdp->pending_signal.sig = NULL;
    esdp->pending_signal.to = THE_NON_VALUE;
#ifdef DEBUG
    esdp->pending_signal.dbg_from = NULL;
#endif

    if (daww_ptr) {
	init_aux_work_data(&esdp->aux_work_data, esdp, *daww_ptr);
	*daww_ptr += daww_sz;
    }

    esdp->reductions = 0;

    init_sched_wall_time(esdp, time_stamp);
    erts_port_task_handle_init(&esdp->nosuspend_port_task_handle);
}

void
erts_init_scheduling(int no_schedulers, int no_schedulers_online, int no_poll_threads,
		     int no_dirty_cpu_schedulers, int no_dirty_cpu_schedulers_online,
		     int no_dirty_io_schedulers
		     )
{
    int ix, n, no_ssi, tot_rqs;
    char *daww_ptr;
    size_t daww_sz;
    size_t size_runqs;
    erts_aint32_t set_schdlr_sspnd_change_flags;

    init_misc_op_list_alloc();
    init_proc_sys_task_queues_alloc();

    set_wakeup_other_data();

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    if (erts_sched_balance_util)
	erts_sched_compact_load = 0;
#endif

    ASSERT(no_schedulers_online <= no_schedulers);
    ASSERT(no_schedulers_online >= 1);
    ASSERT(no_schedulers >= 1);
    ASSERT(no_dirty_cpu_schedulers <= no_schedulers);
    ASSERT(no_dirty_cpu_schedulers >= 1);
    ASSERT(no_dirty_cpu_schedulers_online <= no_schedulers_online);
    ASSERT(no_dirty_cpu_schedulers_online >= 1);
    ASSERT(erts_no_poll_threads == no_poll_threads);

    /* Create and initialize run queues */

    n = no_schedulers;
    tot_rqs = (n + ERTS_NUM_DIRTY_RUNQS);
    size_runqs = sizeof(ErtsAlignedRunQueue) * tot_rqs;
    erts_aligned_run_queues =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_RUNQS, size_runqs);
#if ERTS_POLL_USE_SCHEDULER_POLLING
    erts_atomic32_init_nob(&doing_sys_schedule, 0);
    erts_atomic32_init_nob(&function_calls, 0);
#endif
    erts_atomic32_init_nob(&no_empty_run_queues, 0);

    erts_no_run_queues = n;

    for (ix = 0; ix < tot_rqs; ix++) {
	int pix, rix;
	ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);

	rq->ix = ix;

	/* make sure that the "extra" id correponds to the schedulers
	 * id if the esdp->no <-> ix+1 mapping change.
	 */

	erts_mtx_init(&rq->mtx, "run_queue", make_small(ix + 1),
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER);
	erts_cnd_init(&rq->cnd);

        if (ERTS_RUNQ_IX_IS_DIRTY(ix)) {
            erts_mtx_init(&rq->sleepers.lock, "dirty_run_queue_sleep_list",
                make_small(ix + 1),
                ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER);
        }
	rq->sleepers.list = NULL;

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
	erts_atomic32_set_nob(&rq->len, 0);
	rq->wakeup_other = 0;
	rq->wakeup_other_reds = 0;

	rq->procs.context_switches = 0;
	rq->procs.reductions = 0;

	for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
	    erts_atomic32_init_nob(&rq->procs.prio_info[pix].len, 0);
	    rq->procs.prio_info[pix].max_len = 0;
	    rq->procs.prio_info[pix].reds = 0;
	    if (pix < ERTS_NO_PROC_PRIO_LEVELS - 1) {
		rq->procs.prio[pix].first = NULL;
		rq->procs.prio[pix].last = NULL;
	    }
	}

	rq->misc.start = NULL;
	rq->misc.end = NULL;

	erts_atomic32_init_nob(&rq->ports.info.len, 0);
	rq->ports.info.max_len = 0;
	rq->ports.info.reds = 0;
	rq->ports.start = NULL;
	rq->ports.end = NULL;

#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
	init_runq_sched_util(&rq->sched_util, erts_sched_balance_util);
#endif

    }


    if (erts_no_run_queues != 1) {
	run_queue_info = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				    (sizeof(ErtsRunQueueBalance)
				    * erts_no_run_queues));
	run_queue_compare = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				       (sizeof(ErtsRunQueueCompare)
					* erts_no_run_queues));
    }


    n = (int) no_schedulers;
    erts_no_schedulers = n;
    erts_no_total_schedulers = n;
    erts_no_dirty_cpu_schedulers = no_dirty_cpu_schedulers;
    erts_no_total_schedulers += no_dirty_cpu_schedulers;
    erts_no_dirty_io_schedulers = no_dirty_io_schedulers;
    erts_no_total_schedulers += no_dirty_io_schedulers;

    /* Create and initialize scheduler sleep info */
    no_ssi = n + 1 /* aux thread */;
    aligned_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_ssi*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_ssi; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_sched_sleep_info[ix].ssi;
#if 0 /* no need to initialize these... */
	ssi->next = NULL;
	ssi->prev = NULL;
#endif
        ssi->esdp = NULL;
	erts_atomic32_init_nob(&ssi->flags, 0);
	ssi->event = NULL; /* initialized in sched_thread_func */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }

    aligned_sched_sleep_info += 1 /* aux thread */;

    aligned_dirty_cpu_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_dirty_cpu_schedulers*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_dirty_cpu_schedulers; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_dirty_cpu_sched_sleep_info[ix].ssi;
	erts_atomic32_init_nob(&ssi->flags, 0);
        ssi->next = NULL;
        ssi->prev = NULL;
	ssi->event = NULL; /* initialized in sched_dirty_cpu_thread_func */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }
    aligned_dirty_io_sched_sleep_info =
	erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_dirty_io_schedulers*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_dirty_io_schedulers; ix++) {
	ErtsSchedulerSleepInfo *ssi = &aligned_dirty_io_sched_sleep_info[ix].ssi;
	erts_atomic32_init_nob(&ssi->flags, 0);
        ssi->next = NULL;
        ssi->prev = NULL;
	ssi->event = NULL; /* initialized in sched_dirty_io_thread_func */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }

    aligned_poll_thread_sleep_info =
        erts_alloc_permanent_cache_aligned(
	    ERTS_ALC_T_SCHDLR_SLP_INFO,
	    no_poll_threads*sizeof(ErtsAlignedSchedulerSleepInfo));
    for (ix = 0; ix < no_poll_threads; ix++) {
        ErtsSchedulerSleepInfo *ssi = &aligned_poll_thread_sleep_info[ix].ssi;
        ssi->esdp = NULL;
	erts_atomic32_init_nob(&ssi->flags, 0);
	ssi->event = NULL; /* initialized in poll_thread */
	erts_atomic32_init_nob(&ssi->aux_work, 0);
    }

    /* Create and initialize scheduler specific data */

    daww_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE((sizeof(ErtsDelayedAuxWorkWakeupJob)
					      + sizeof(int))*(n+1));
    daww_ptr = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
						  daww_sz*n);

    erts_aligned_scheduler_data = 
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   n*sizeof(ErtsAlignedSchedulerData));

    for (ix = 0; ix < n; ix++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
	init_scheduler_data(esdp, ix+1, ERTS_SCHED_SLEEP_INFO_IX(ix),
			    ERTS_RUNQ_IX(ix), &daww_ptr, daww_sz,
			    NULL, 0);
    }

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

    init_misc_aux_work();
    init_swtreq_alloc();
    init_screq_alloc();

    erts_atomic32_init_nob(&debug_wait_completed_count, 0); /* debug only */
    debug_wait_completed_flags = 0;

    aux_thread_aux_work_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   sizeof(ErtsAuxWorkData));

    poll_thread_aux_work_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   no_poll_threads * sizeof(ErtsAuxWorkData));

    init_no_runqs(no_schedulers_online, no_schedulers_online);
    balance_info.last_active_runqs = no_schedulers;
    erts_mtx_init(&balance_info.update_mtx, "migration_info_update", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER);
    balance_info.forced_check_balance = 0;
    balance_info.halftime = 1;
    balance_info.full_reds_history_index = 0;
    erts_atomic32_init_nob(&balance_info.checking_balance, 0);
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
	    erts_atomic32_read_bor_nob(&esdp->ssi->flags, ERTS_SSI_FLG_SUSPENDED);
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

    erts_atomic32_init_nob(&dirty_count.cpu.active,
                               (erts_aint32_t) no_dirty_cpu_schedulers);
    erts_atomic32_init_nob(&dirty_count.io.active,
                               (erts_aint32_t) no_dirty_io_schedulers);


    if (set_schdlr_sspnd_change_flags)
	erts_atomic32_set_nob(&schdlr_sspnd.changing,
				  set_schdlr_sspnd_change_flags);

    init_misc_aux_work();


    /* init port tasks */
    erts_port_task_init();


    erts_atomic32_init_relb(&erts_halt_progress, -1);
    erts_halt_code = 0;


}

ErtsRunQueue *
erts_schedid2runq(Uint id)
{
    int ix;
    ix = (int) id - 1;
    ASSERT(0 <= ix && ix < erts_no_run_queues);
    return ERTS_RUNQ_IX(ix);
}


ErtsSchedulerData *
erts_get_scheduler_data(void)
{
    return (ErtsSchedulerData *) erts_tsd_get(sched_data_key);
}


static Process *
make_proxy_proc(Process *prev_proxy, Process *proc, erts_aint32_t prio)
{
    erts_aint32_t state;
    Process *proxy;
    int bound;
    ErtsRunQueue *rq = erts_get_runq_proc(proc, &bound);

    state = (ERTS_PSFLG_PROXY
	     | ERTS_PSFLG_IN_RUNQ
	     | (((erts_aint32_t) 1) << (prio + ERTS_PSFLGS_IN_PRQ_MASK_OFFSET))
	     | (prio << ERTS_PSFLGS_PRQ_PRIO_OFFSET)
	     | (prio << ERTS_PSFLGS_USR_PRIO_OFFSET)
	     | (prio << ERTS_PSFLGS_ACT_PRIO_OFFSET));

    if (prev_proxy) {
	proxy = prev_proxy;
        ASSERT(proxy->u.real_proc);
        erts_proc_dec_refc(proxy->u.real_proc);
#ifdef DEBUG
        proxy->u.real_proc = NULL;
#endif
	ASSERT(erts_atomic32_read_nob(&proxy->state) & ERTS_PSFLG_PROXY);
	erts_atomic32_set_nob(&proxy->state, state);
        (void) erts_set_runq_proc(proxy, rq, &bound);
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
        proxy->u.real_proc = NULL;
#endif
	erts_atomic32_init_nob(&proxy->state, state);
        erts_init_runq_proc(proxy, rq, bound);
    }

    proxy->common.id = proc->common.id;
    ASSERT(proxy->u.real_proc == NULL);
    proxy->u.real_proc = proc;
    erts_proc_inc_refc(proc);

    return proxy;
}

#define ERTS_ENQUEUE_NOT 0
#define ERTS_ENQUEUE_NORMAL_QUEUE 1
#define ERTS_ENQUEUE_DIRTY_CPU_QUEUE 2
#define ERTS_ENQUEUE_DIRTY_IO_QUEUE 3


static int
check_dirty_enqueue_in_prio_queue(Process *c_p,
				  erts_aint32_t *newp,
				  erts_aint32_t actual,
				  erts_aint32_t aprio,
				  erts_aint32_t qbit)
{
    int queue;
    erts_aint32_t dact, max_qbit;

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

    dact = erts_atomic32_read_mb(&c_p->dirty_state);
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

    if (qbit & erts_atomic32_read_bor_mb(&p->dirty_state, qbit)) {
	/* Already enqueue by someone else... */
	if (pstruct_reserved) {
	    /* We reserved process struct for enqueue; clear it... */
	    erts_aint32_t state;

            state = erts_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_IN_RUNQ);
            ASSERT(state & ERTS_PSFLG_IN_RUNQ);

            if ((state & (ERTS_PSFLG_ACTIVE
                          | ERTS_PSFLG_FREE))
                == ERTS_PSFLG_FREE) {
                /*
                 * inactive-free and not queued by proxy
                 *
                 * Corresponds to increment in
                 * erts_continue_exit_process() after
                 * state ERTS_CONTINUE_EXIT_DONE.
                 */
                erts_proc_dec_refc(p);
            }
	}
	return 0;
    }

    return !0;
}


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

    if (actual & ERTS_PSFLGS_DIRTY_WORK) {
	int res = check_dirty_enqueue_in_prio_queue(c_p, newp, actual,
						    aprio, qbit);
	if (res != ERTS_ENQUEUE_NORMAL_QUEUE)
	    return res;
    }

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


    default: {
	ErtsRunQueue* runq;
        int bound;

	ASSERT(enqueue == ERTS_ENQUEUE_NORMAL_QUEUE
	       || enqueue == -ERTS_ENQUEUE_NORMAL_QUEUE);

	runq = erts_get_runq_proc(p, &bound);

	if (!bound) {
	    ErtsRunQueue *new_runq = erts_check_emigration_need(runq, enq_prio);
            if (new_runq) {
                if (erts_try_change_runq_proc(p, new_runq))
                    runq = new_runq;
                else
                    runq = erts_get_runq_proc(p, NULL);
            }
	}

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

    ASSERT(!(state & (ERTS_PSFLG_DIRTY_IO_PROC
                      |ERTS_PSFLG_DIRTY_CPU_PROC))
           || (BeamIsOpCode(*p->i, op_call_nif)
               || BeamIsOpCode(*p->i, op_apply_bif)));

    a = state;

    /* Clear activ-sys if needed... */
    while (1) {
        n = e = a;
        if (a & ERTS_PSFLG_ACTIVE_SYS) {
            if (a & (ERTS_PSFLG_SIG_Q
                     | ERTS_PSFLG_SIG_IN_Q
                     | ERTS_PSFLG_SYS_TASKS))
                break;
            /* Clear active-sys */
            n &= ~ERTS_PSFLG_ACTIVE_SYS;
        }
        a = erts_atomic32_cmpxchg_nob(&p->state, n, e);
        if (a == e) {
            a = n;
            break;
        }
    }

    if (!is_normal_sched)
	running_flgs = ERTS_PSFLG_DIRTY_RUNNING|ERTS_PSFLG_DIRTY_RUNNING_SYS;
    else {
	running_flgs = ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS;
        if ((a & ERTS_PSFLG_DIRTY_ACTIVE_SYS)
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

            a = erts_atomic32_read_band_nob(&p->state,
                                            ~ERTS_PSFLG_DIRTY_ACTIVE_SYS);
            a &= ~ERTS_PSFLG_DIRTY_ACTIVE_SYS;
        }
    }

    while (1) {
	n = e = a;

	ASSERT(a & running_flgs);

	enqueue = ERTS_ENQUEUE_NOT;

        ASSERT(((a & (ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE))
                != ERTS_PSFLG_EXITING)
               || ((a & (ERTS_PSFLG_ACTIVE|ERTS_PSFLG_SUSPENDED))
                   == ERTS_PSFLG_ACTIVE));

	n &= ~running_flgs;
	if ((!!(a & (ERTS_PSFLG_ACTIVE_SYS|ERTS_PSFLG_DIRTY_ACTIVE_SYS))
	    | ((a & (ERTS_PSFLG_ACTIVE|ERTS_PSFLG_SUSPENDED)) == ERTS_PSFLG_ACTIVE))) {
	    enqueue = check_enqueue_in_prio_queue(p, &enq_prio, &n, a);
	}
	a = erts_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
    }

    runq = select_enqueue_run_queue(enqueue, enq_prio, p, n);

    if (!runq) {

	if (erts_system_profile_flags.runnable_procs) {

	    /* Status lock prevents out of order "runnable proc" trace msgs */
	    ERTS_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

	    if (!(a & (ERTS_PSFLG_ACTIVE_SYS|ERTS_PSFLG_DIRTY_ACTIVE_SYS))
		&& (!(a & ERTS_PSFLG_ACTIVE) || (a & ERTS_PSFLG_SUSPENDED))) {
		/* Process inactive */
		profile_runnable_proc(p, am_inactive);
	    }
	}

	if (proxy)
	    free_proxy_proc(proxy);

	erts_runq_lock(c_rq);

        /* Decrement refc if scheduled out from dirty scheduler... */
	return !is_normal_sched;
    }
    else {
	Process* sched_p;

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

        if (runq != c_rq && ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
            erts_proc_inc_refc(p); /* Needs to be done before enqueue_process() */
        
	erts_runq_lock(runq);

	/* Enqueue the process */
	enqueue_process(runq, (int) enq_prio, sched_p);

	if (runq == c_rq)
	    return 0; /* No decrement of refc since enqueued on same dirty scheduler */

	erts_runq_unlock(runq);

	smp_notify_inc_runq(runq);

	erts_runq_lock(c_rq);

        /* Decrement refc if scheduled out from dirty scheduler... */
        return !is_normal_sched;
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
	Process *sched_p = proc;
        
	if (enqueue < 0) { /* use proxy */
	    Process *pxy;

	    if (!proxy)
		pxy = NULL;
	    else {
		pxy = *proxy;
		*proxy = NULL;
	    }
	    sched_p = make_proxy_proc(pxy, proc, prio);
	}

        if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix))
            erts_proc_inc_refc(proc);
        
	erts_runq_lock(runq);

	/* Enqueue the process */
	enqueue_process(runq, (int) prio, sched_p);

	erts_runq_unlock(runq);
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

    ERTS_LC_ASSERT(locks == erts_proc_lc_my_proc_locks(p));

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
	erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);

    while (1) {
	erts_aint32_t e;
	n = e = a;

	enqueue = ERTS_ENQUEUE_NOT;

        if ((a & (ERTS_PSFLG_FREE|ERTS_PSFLG_ACTIVE)) == ERTS_PSFLG_FREE)
            break; /* If free and not active, do not schedule */

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
            /* If exiting and executing dirty, schedule on normal */
	    || (n & (ERTS_PSFLG_RUNNING
		     | ERTS_PSFLG_RUNNING_SYS
		     | ERTS_PSFLG_EXITING)) == ERTS_PSFLG_EXITING
	    ) {
	    /*
	     * Active and seemingly need to be enqueued, but
	     * process may be in a run queue via proxy, need
	     * further inspection...
	     */
	    enqueue = check_enqueue_in_prio_queue(p, enq_prio_p, &n, a);
	}

	a = erts_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
	if (enqueue == ERTS_ENQUEUE_NOT && n == a)
	    break;
    }

    if (prof_runnable_procs) {

	/* Status lock prevents out of order "runnable proc" trace msgs */

	if (((n & (ERTS_PSFLG_SUSPENDED
		   | ERTS_PSFLG_ACTIVE)) == ERTS_PSFLG_ACTIVE)
            & ((a & (ERTS_PSFLG_SUSPENDED
                     | ERTS_PSFLG_ACTIVE)) != ERTS_PSFLG_ACTIVE)
	    & !(a & (ERTS_PSFLG_ACTIVE_SYS
                     | ERTS_PSFLG_RUNNING
                     | ERTS_PSFLG_RUNNING_SYS
                     | ERTS_PSFLG_DIRTY_RUNNING
                     | ERTS_PSFLG_DIRTY_RUNNING_SYS))) {
	    /* We activated a prevously inactive process */
	    profile_runnable_proc(p, am_active);
	}

	if (lock_status)
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }


    *statep = n;

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

/* Enqueues the given sys task on the process and schedules it. The task may be
 * NULL if only scheduling is desired. */
static ERTS_INLINE erts_aint32_t
active_sys_enqueue(Process *p, ErtsProcSysTask *sys_task,
                   erts_aint32_t task_prio, erts_aint32_t enable_flags,
                   erts_aint32_t state, erts_aint32_t *fail_state_p)
{
    int runnable_procs = erts_system_profile_flags.runnable_procs;
    erts_aint32_t n, a, enq_prio, fail_state;
    int already_scheduled;
    int status_locked;
    int enqueue; /* < 0 -> use proxy */

    enable_flags |= ERTS_PSFLG_ACTIVE_SYS;
    fail_state = *fail_state_p;
    already_scheduled = 0;
    status_locked = 0;
    enq_prio = -1;
    a = state;

    ERTS_LC_ASSERT(!(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p)));
    ASSERT(fail_state & (ERTS_PSFLG_EXITING | ERTS_PSFLG_FREE));
    ASSERT(!(fail_state & enable_flags));
    ASSERT(!(state & ERTS_PSFLG_PROXY));

    /* When runnable_procs is enabled, we need to take the status lock to
     * prevent trace messages from being sent in the wrong order. The lock must
     * be held over the call to add2runq.
     *
     * Otherwise, we only need to take it when we're enqueuing a task and can
     * safely release it before add2runq. */
    if (sys_task || runnable_procs) {
        erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
        status_locked = 1;
    }

    while (1) {
	erts_aint32_t e;
	n = e = a;

	if (a & fail_state) {
            *fail_state_p = a & fail_state;
	    goto cleanup;
        }

	enqueue = ERTS_ENQUEUE_NOT;
	n |= enable_flags;

	if (!(a & (ERTS_PSFLG_RUNNING
		   | ERTS_PSFLG_RUNNING_SYS
		   | ERTS_PSFLG_DIRTY_RUNNING
		   | ERTS_PSFLG_DIRTY_RUNNING_SYS))) {
	    enqueue = check_enqueue_in_prio_queue(p, &enq_prio, &n, a);
        }

	a = erts_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e) {
	    break;
	}
        else if (a == n && enqueue == ERTS_ENQUEUE_NOT) {
	    already_scheduled = 1;
            break;
        }
    }

    if (!already_scheduled && runnable_procs) {
	if (!(a & (ERTS_PSFLG_ACTIVE_SYS
		   | ERTS_PSFLG_RUNNING
		   | ERTS_PSFLG_RUNNING_SYS
		   | ERTS_PSFLG_DIRTY_RUNNING
		   | ERTS_PSFLG_DIRTY_RUNNING_SYS))
	    && (!(a & ERTS_PSFLG_ACTIVE) || (a & ERTS_PSFLG_SUSPENDED))) {
	    /* We activated a prevously inactive process */
	    profile_runnable_proc(p, am_active);
	}
    }

    if (sys_task) {
        ErtsProcSysTaskQs *stqs = p->sys_task_qs;

        if (!stqs) {
            sys_task->next = sys_task->prev = sys_task;

            stqs = proc_sys_task_queues_alloc();

            stqs->qmask = 1 << task_prio;
            stqs->ncount = 0;
            stqs->q[PRIORITY_MAX] = NULL;
            stqs->q[PRIORITY_HIGH] = NULL;
            stqs->q[PRIORITY_NORMAL] = NULL;
            stqs->q[PRIORITY_LOW] = NULL;
            stqs->q[task_prio] = sys_task;

            p->sys_task_qs = stqs;
        }
        else {
            if (!stqs->q[task_prio]) {
                sys_task->next = sys_task->prev = sys_task;

                stqs->q[task_prio] = sys_task;
                stqs->qmask |= 1 << task_prio;
            }
            else {
                sys_task->next = stqs->q[task_prio];
                sys_task->prev = stqs->q[task_prio]->prev;
                sys_task->next->prev = sys_task;
                sys_task->prev->next = sys_task;
                ASSERT(stqs->qmask & (1 << task_prio));
            }
        }
    }

    if (status_locked && !runnable_procs) {
        erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
        status_locked = 0;
    }

    if (!already_scheduled) {
        add2runq(enqueue, enq_prio, p, n, NULL);
    }

cleanup:
    if (status_locked) {
        erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    return n;
}

erts_aint32_t
erts_proc_sys_schedule(Process *p, erts_aint32_t state, erts_aint32_t enable_flag)
{
    erts_aint32_t fail_state = ERTS_PSFLG_FREE;

    return active_sys_enqueue(p, NULL, 0, enable_flag, state, &fail_state);
}

static int
schedule_process_sys_task(Process *p, erts_aint32_t prio, ErtsProcSysTask *st,
			  erts_aint32_t *fail_state_p)
{
    erts_aint32_t fail_state, state;

    /* Elevate priority if needed. */
    state = erts_atomic32_read_nob(&p->state);
    if (ERTS_PSFLGS_GET_ACT_PRIO(state) > prio) {
        erts_aint32_t n, a, e;

        a = state;
        do {
            if (ERTS_PSFLGS_GET_ACT_PRIO(a) <= prio) {
                n = a;
                break;
            }
            n = e = a;
            n &= ~ERTS_PSFLGS_ACT_PRIO_MASK;
            n |= (prio << ERTS_PSFLGS_ACT_PRIO_OFFSET);
            a = erts_atomic32_cmpxchg_nob(&p->state, n, e);
        } while (a != e);

        state = n;
    }

    fail_state = *fail_state_p;

    return !(active_sys_enqueue(p, st, prio, ERTS_PSFLG_SYS_TASKS,
                                state, fail_state_p) & fail_state);
}

static ERTS_INLINE int
suspend_process(Process *c_p, Process *p)
{
    erts_aint32_t state;
    int suspended = 0;
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    state = erts_atomic32_read_acqb(&p->state);

    if ((state & ERTS_PSFLG_SUSPENDED))
	suspended = -1;
    else {
	if (c_p == p) {
	    state = erts_atomic32_read_bor_relb(&p->state,
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
		state = erts_atomic32_cmpxchg_relb(&p->state, n, e);
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

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    ASSERT(p->rcount > 0);

    if (--p->rcount > 0)  /* multiple suspend */
	return;

    state = erts_atomic32_read_nob(&p->state);
    enqueue = change_proc_schedule_state(p,
					 ERTS_PSFLG_SUSPENDED,
					 0,
					 &state,
					 &enq_prio,
					 locks);
    add2runq(enqueue, enq_prio, p, state, NULL);
}


static ERTS_INLINE void
sched_resume_wake__(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t xflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_TSE_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);
    erts_aint32_t oflgs;
    do {
	oflgs = erts_atomic32_cmpxchg_relb(&ssi->flags, 0, xflgs);
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


static erts_aint32_t
sched_prep_spin_suspended(ErtsSchedulerSleepInfo *ssi, erts_aint32_t xpct)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);
    erts_aint32_t xflgs = xpct;

    do {
	oflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
	flgs = erts_atomic32_read_acqb(&ssi->flags);
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
sched_set_suspended_sleeptype(ErtsSchedulerSleepInfo *ssi,
                              erts_aint32_t sleep_type)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = ((ERTS_SSI_FLG_SLEEPING
                            | ERTS_SSI_FLG_WAITING
                            | ERTS_SSI_FLG_SUSPENDED)
                           | sleep_type);
    erts_aint32_t xflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING
			   | ERTS_SSI_FLG_SUSPENDED);

    ASSERT(sleep_type == ERTS_SSI_FLG_TSE_SLEEPING);
    erts_tse_reset(ssi->event);

    while (1) {
	oflgs = erts_atomic32_cmpxchg_acqb(&ssi->flags, nflgs, xflgs);
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
    erts_mtx_init(&schdlr_sspnd.mtx, "schdlr_sspnd", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER);
    schdlr_sspnd.online.normal = 1;
    schdlr_sspnd.curr_online.normal = 1;
    schdlr_sspnd.active.normal = 1;
    schdlr_sspnd.online.dirty_cpu = 0;
    schdlr_sspnd.curr_online.dirty_cpu = 0;
    schdlr_sspnd.active.dirty_cpu = 0;
    schdlr_sspnd.online.dirty_io = 0;
    schdlr_sspnd.curr_online.dirty_io = 0;
    schdlr_sspnd.active.dirty_io = 0;
    schdlr_sspnd.last_msb_dirty_type = ERTS_SCHED_DIRTY_IO;
    erts_atomic32_init_nob(&schdlr_sspnd.changing, 0);
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
	erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
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
	schdlr_sspnd_resume_proc(sched_type, plp->u.pid);
	proclist_destroy(plp);
    }
}


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
    erts_aint32_t dbg_val;

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
    dbg_val =
        erts_atomic32_read_bset_mb(&esdp->ssi->flags,
                                       (ERTS_SSI_FLG_SUSPENDED
                                        | ERTS_SSI_FLG_MSB_EXEC),
                                       ERTS_SSI_FLG_SUSPENDED);
    ASSERT(dbg_val & ERTS_SSI_FLG_MSB_EXEC); (void)dbg_val;

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

    dbg_val =
        erts_atomic32_read_bset_mb(&exec_rq->scheduler->ssi->flags,
                                       (ERTS_SSI_FLG_SUSPENDED
                                        | ERTS_SSI_FLG_MSB_EXEC),
                                       ERTS_SSI_FLG_MSB_EXEC);
    ASSERT(dbg_val & ERTS_SSI_FLG_SUSPENDED);

    wake_scheduler(exec_rq);

    return 1; /* suspend this scheduler... */

}

static ERTS_INLINE void
suspend_scheduler_sleep(ErtsSchedulerData *esdp,
                        int normal_sched,
                        ErtsMonotonicTime initial_time,
                        ErtsMonotonicTime timeout_time)
{
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    erts_aint32_t flgs = sched_spin_suspended(ssi,
                                              ERTS_SCHED_SUSPEND_SLEEP_SPINCOUNT);
    ASSERT(!normal_sched || esdp->type == ERTS_SCHED_NORMAL);
    ASSERT(esdp->type != ERTS_SCHED_NORMAL || normal_sched);
    if (flgs == (ERTS_SSI_FLG_SLEEPING
                 | ERTS_SSI_FLG_WAITING
                 | ERTS_SSI_FLG_SUSPENDED)) {
        flgs = sched_set_suspended_sleeptype(ssi, ERTS_SSI_FLG_TSE_SLEEPING);
        if (flgs == (ERTS_SSI_FLG_SLEEPING
                     | ERTS_SSI_FLG_TSE_SLEEPING
                     | ERTS_SSI_FLG_WAITING
                     | ERTS_SSI_FLG_SUSPENDED)) {
            if (!normal_sched) {
                while (1) {
                    int res = erts_tse_wait(ssi->event);
                    if (res != EINTR)
                        break;
                }
            }
            else {
                ErtsMonotonicTime current_time = initial_time;
                while (1) {
                    int res;
                    Sint64 timeout;

                    timeout = ERTS_MONOTONIC_TO_NSEC(timeout_time
                                                     - current_time
                                                     - 1) + 1;
                    res = erts_tse_twait(ssi->event, timeout);
                    if (res != EINTR)
                        break;
                    current_time = erts_get_monotonic_time(esdp);
                    if (current_time >= timeout_time)
                        break;
                }
            }
        }
    }
}

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

#ifdef HARDDEBUG
    if (sched_type != ERTS_SCHED_NORMAL)
        ERTS_HDBG_CHK_SLEEP_LIST(&esdp->run_queue->sleepers, !0, NULL, ssi);
#endif

    if (erts_atomic32_read_nob(&ssi->flags) & ERTS_SSI_FLG_MSB_EXEC) {

        ASSERT(no == 1);
        if (!msb_scheduler_type_switch(sched_type, esdp, no))
            return;
        /* Suspend and let scheduler 1 of another type execute... */
    }


    if (sched_type != ERTS_SCHED_NORMAL) {
        dirty_active(esdp, -1);
	erts_runq_unlock(esdp->run_queue);
        dirty_sched_wall_time_change(esdp, 0);
    }
    else {
        if (no != 1)
            evacuate_run_queue(esdp->run_queue, &sbp);

	erts_runq_unlock(esdp->run_queue);

	erts_sched_check_cpu_bind_prep_suspend(esdp);

	if (erts_system_profile_flags.scheduler)
	    profile_scheduler(make_small(esdp->no), am_inactive);
    }

    erts_mtx_lock(&schdlr_sspnd.mtx);

    flgs = sched_prep_spin_suspended(ssi, ERTS_SSI_FLG_SUSPENDED);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {

	schdlr_sspnd_dec_nscheds(&schdlr_sspnd.active, sched_type);

	changing = erts_atomic32_read_nob(&schdlr_sspnd.changing);

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

                    if (!msb[i]->ongoing)
                        continue;

		    if (msb[i] == &schdlr_sspnd.nmsb) {
			if (schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                     ERTS_SCHED_NORMAL) == 1) {
                            clr_flg = ERTS_SCHDLR_SSPND_CHNG_NMSB;
                        }
		    }
		    else {
                        ASSERT(msb[i] == &schdlr_sspnd.msb);
                        if (schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                     ERTS_SCHED_NORMAL) == 1
                            && schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                        ERTS_SCHED_DIRTY_CPU) == 0
                            && schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                        ERTS_SCHED_DIRTY_IO) == 0) {

                            clr_flg = ERTS_SCHDLR_SSPND_CHNG_MSB;
                            
                            /* Begin switching between scheduler types executing... */
                            ERTS_RUNQ_FLGS_SET_NOB(ERTS_RUNQ_IX(0), ERTS_RUNQ_FLG_MSB_EXEC);
                            erts_atomic32_read_bor_nob(&ERTS_RUNQ_IX(0)->scheduler->ssi->flags,
                                                       ERTS_SSI_FLG_MSB_EXEC);
                        }
		    }

		    if (clr_flg) {
			ErtsProcList *plp, *end_plp;
			changing = erts_atomic32_read_band_nob(&schdlr_sspnd.changing,
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
		    changing = erts_atomic32_read_band_nob(&schdlr_sspnd.changing,
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
			    resume.onln.nxt = plp->u.pid;
			    ASSERT(is_internal_pid(resume.onln.nxt));
			}
		    }
		}
	    }

	    if (curr_online) {
		flgs = erts_atomic32_read_acqb(&ssi->flags);
		if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		    break;
	    }
	    erts_mtx_unlock(&schdlr_sspnd.mtx);

	    schdlr_sspnd_resume_procs(sched_type, &resume);

	    while (1) {
		if (sched_type != ERTS_SCHED_NORMAL)
                    suspend_scheduler_sleep(esdp, 0, 0, 0);
		else
                {
                    ErtsMonotonicTime current_time, timeout_time;
                    int evacuate = no == 1 ? 0 : !ERTS_EMPTY_RUNQ(esdp->run_queue);

		    ASSERT(sched_type == ERTS_SCHED_NORMAL);

		    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);

		    if (aux_work|evacuate) {
			if (!thr_prgr_active) {
			    erts_thr_progress_active(erts_thr_prgr_data(esdp),
                                                     thr_prgr_active = 1);
			    sched_wall_time_change(esdp, 1);
			}
			if (aux_work)
			    aux_work = handle_aux_work(&esdp->aux_work_data,
						       aux_work,
						       1);

			if (aux_work && erts_thr_progress_update(erts_thr_prgr_data(esdp)))
			    erts_thr_progress_leader_update(erts_thr_prgr_data(esdp));
			if (evacuate) {
			    erts_runq_lock(esdp->run_queue);
			    evacuate_run_queue(esdp->run_queue, &sbp);
			    erts_runq_unlock(esdp->run_queue);
			}
		    }


                    if (aux_work)
                        timeout_time = erts_next_timeout_time(esdp->next_tmo_ref);
                    else
                        timeout_time = erts_check_next_timeout_time(esdp);

                    current_time = erts_get_monotonic_time(esdp);

                    if (!aux_work && current_time < timeout_time) {
                        /* go to sleep... */
                        if (thr_prgr_active) {
                            erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 0);
                            sched_wall_time_change(esdp, 0);
                        }
                        erts_thr_progress_prepare_wait(erts_thr_prgr_data(NULL));
                        suspend_scheduler_sleep(esdp, !0, current_time, timeout_time);
                        erts_thr_progress_finalize_wait(erts_thr_prgr_data(NULL));
                        current_time = erts_get_monotonic_time(esdp);
                    }

                    if (current_time >= timeout_time) {
                        if (!thr_prgr_active) {
                            erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
                            sched_wall_time_change(esdp, 1);
                        }
                        erts_bump_timers(esdp->timer_wheel, current_time);
                    }
                }

		flgs = sched_prep_spin_suspended(ssi, (ERTS_SSI_FLG_WAITING
						       | ERTS_SSI_FLG_SUSPENDED));
		if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		    break;
		changing = erts_atomic32_read_nob(&schdlr_sspnd.changing);
		if (changing)
		    break;
	    }

	    erts_mtx_lock(&schdlr_sspnd.mtx);
	    changing = erts_atomic32_read_nob(&schdlr_sspnd.changing);
	}

	schdlr_sspnd_inc_nscheds(&schdlr_sspnd.active, sched_type);
	changing = erts_atomic32_read_nob(&schdlr_sspnd.changing);
        if (changing) {
            if ((changing & ERTS_SCHDLR_SSPND_CHNG_MSB)
                && !schdlr_sspnd.msb.ongoing
                && schdlr_sspnd_eq_nscheds(&schdlr_sspnd.online,
                                           &schdlr_sspnd.active)) {
                erts_atomic32_read_band_nob(&schdlr_sspnd.changing,
                                                ~ERTS_SCHDLR_SSPND_CHNG_MSB);
            }
            if ((changing & ERTS_SCHDLR_SSPND_CHNG_NMSB)
                && !schdlr_sspnd.nmsb.ongoing
                && (schdlr_sspnd_get_nscheds(&schdlr_sspnd.online,
                                             ERTS_SCHED_NORMAL)
                    == schdlr_sspnd_get_nscheds(&schdlr_sspnd.active,
                                                ERTS_SCHED_NORMAL))) {
                erts_atomic32_read_band_nob(&schdlr_sspnd.changing,
                                                ~ERTS_SCHDLR_SSPND_CHNG_NMSB);
            }
        }
	ASSERT(no <= schdlr_sspnd_get_nscheds(&schdlr_sspnd.online, sched_type));
    }

    erts_mtx_unlock(&schdlr_sspnd.mtx);

    schdlr_sspnd_resume_procs(sched_type, &resume);

    ASSERT(curr_online);

    if (sched_type != ERTS_SCHED_NORMAL)
        dirty_sched_wall_time_change(esdp, 1);
    else {
        (void) erts_get_monotonic_time(esdp);
        if (erts_system_profile_flags.scheduler)
            profile_scheduler(make_small(esdp->no), am_active);

        if (!thr_prgr_active) {
            erts_thr_progress_active(erts_thr_prgr_data(esdp), thr_prgr_active = 1);
            sched_wall_time_change(esdp, 1);
        }
    }

    erts_runq_lock(esdp->run_queue);
    non_empty_runq(esdp->run_queue);

    if (sched_type != ERTS_SCHED_NORMAL)
        dirty_active(esdp, 1);
    else {
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
	erts_mtx_lock(&schdlr_sspnd.mtx);
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
	erts_mtx_unlock(&schdlr_sspnd.mtx);
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

    erts_mtx_lock(&schdlr_sspnd.mtx);

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
		    resume = plp->u.pid;
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

    erts_mtx_unlock(&schdlr_sspnd.mtx);

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
    int dirty_no, change_dirty, dirty_online;

    if (new_no < 1)
	return ERTS_SCHDLR_SSPND_EINVAL;
    else if (dirty_only && erts_no_dirty_cpu_schedulers < new_no)
	return ERTS_SCHDLR_SSPND_EINVAL;
    else if (erts_no_schedulers < new_no)
	return ERTS_SCHDLR_SSPND_EINVAL;

    if (dirty_only)
	resume_proc = 0;
    else
    {
	resume_proc = 1;
	/*
	 * If we suspend current process we need to suspend before
	 * requesting the change; otherwise, we got a resume/suspend
	 * race...
	 */
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	suspend_process(p, p);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    erts_mtx_lock(&schdlr_sspnd.mtx);

    change_flags = 0;
    have_unlocked_plocks = 0;
    no = (int) new_no;

    if (!dirty_only)
    {
	changing = erts_atomic32_read_nob(&schdlr_sspnd.changing);
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
    {
	change_flags |= ERTS_SCHDLR_SSPND_CHNG_ONLN;
	schdlr_sspnd_set_nscheds(&schdlr_sspnd.online,
				 ERTS_SCHED_NORMAL,
				 no);
	increase = (no > online);
    }

    erts_atomic32_read_bor_nob(&schdlr_sspnd.changing, change_flags);

    res = ERTS_SCHDLR_SSPND_DONE;
    if (increase) {
	int ix;
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
	{
	    if (schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing) {
		for (ix = online; ix < no; ix++)
		    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
	    }
	    else {
		if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_proc_unlock(p, plocks);
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
	{
	    if (schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing) {
		for (ix = no; ix < online; ix++)
		    erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
	    }
	    else {
		if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_proc_unlock(p, plocks);
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

    erts_mtx_unlock(&schdlr_sspnd.mtx);

    if (have_unlocked_plocks)
	erts_proc_lock(p, plocks);

    if (resume_proc) {
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	resume_process(p, plocks|ERTS_PROC_LOCK_STATUS);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    return res;
}

ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *p, ErtsProcLocks plocks, int on, int normal, int all)
{
    ErtsSchedSuspendResult res;
    int resume_proc, ix, have_unlocked_plocks = 0;
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
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	suspend_process(p, p);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    erts_mtx_lock(&schdlr_sspnd.mtx);
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
		erts_proc_unlock(p, plocks);
	    }
	    ASSERT(!msbp->ongoing);
	    msbp->ongoing = 1;

            erts_atomic32_read_bor_nob(&schdlr_sspnd.changing,
                                           chng_flg);
            change_no_used_runqs(1);
            for (ix = 1; ix < erts_no_run_queues; ix++)
                suspend_run_queue(ERTS_RUNQ_IX(ix));

            for (ix = 1; ix < online; ix++) {
                ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
                wake_scheduler(rq);
            }

            if (!normal) {
                for (ix = 0; ix < erts_no_dirty_cpu_schedulers; ix++)
                    dcpu_sched_ix_suspend_wake(ix);
                for (ix = 0; ix < erts_no_dirty_io_schedulers; ix++)
                    dio_sched_ix_suspend_wake(ix);
            }

        wait_until_msb:

            ASSERT(chng_flg & erts_atomic32_read_nob(&schdlr_sspnd.changing));

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
	    erts_atomic32_read_bor_nob(&schdlr_sspnd.changing,
					   chng_flg);
	    p->flags &= ~have_blckd_flg;
	    msbp->ongoing = 0;
            if (!(schdlr_sspnd.msb.ongoing|schdlr_sspnd.nmsb.ongoing)) {
                if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_proc_unlock(p, plocks);
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
	}

    unblock_res:
	if (schdlr_sspnd.msb.ongoing)
	    res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
	else if (schdlr_sspnd.nmsb.ongoing)
	    res = ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED;
	else
	    res = ERTS_SCHDLR_SSPND_DONE;
    }

    erts_mtx_unlock(&schdlr_sspnd.mtx);

    if (have_unlocked_plocks)
	erts_proc_lock(p, plocks);

    if (resume_proc) {
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	resume_process(p, plocks|ERTS_PROC_LOCK_STATUS);
	if (!(plocks & ERTS_PROC_LOCK_STATUS))
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }

    return res;
}

int
erts_is_multi_scheduling_blocked(void)
{
    int res;
    erts_mtx_lock(&schdlr_sspnd.mtx);
    if (schdlr_sspnd.msb.blckrs)
	res = 1;
    else if (schdlr_sspnd.nmsb.blckrs)
	res = -1;
    else
	res = 0;
    erts_mtx_unlock(&schdlr_sspnd.mtx);
    return res;
}

Eterm
erts_multi_scheduling_blockers(Process *p, int normal)
{
    Eterm res = NIL;
    ErtsMultiSchedulingBlock *msbp;

    msbp = normal ? &schdlr_sspnd.nmsb : &schdlr_sspnd.msb;

    erts_mtx_lock(&schdlr_sspnd.mtx);
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
		 plp2->u.pid != plp1->u.pid;
		 plp2 = erts_proclist_peek_next(msbp->blckrs, plp2));
	    if (plp2 == plp1) {
		res = CONS(hp, plp1->u.pid, res);
		hp += 2;
	    }
	    /* else: already in result list */
	}
	HRelease(p, hp_end, hp);
    }
    erts_mtx_unlock(&schdlr_sspnd.mtx);
    return res;
}

static void *
sched_thread_func(void *vesdp)
{
    ErtsThrPrgrCallbacks callbacks;
    ErtsSchedulerData *esdp = vesdp;
    Uint no = esdp->no;
    erts_tse_t *tse;

    erts_port_task_pre_alloc_init_thread();
    erts_sched_init_time_sup(esdp);

    if (no == 1)
        erts_aux_work_timeout_late_init(esdp);

    (void) ERTS_RUNQ_FLGS_SET_NOB(esdp->run_queue,
				  ERTS_RUNQ_FLG_EXEC);

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

#if ERTS_POLL_USE_SCHEDULER_POLLING
    esdp->ssi->psi = erts_create_pollset_thread(-1, NULL);
#endif

    erts_alloc_register_scheduler(vesdp);
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "scheduler %beu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
    erts_tsd_set(sched_data_key, vesdp);
#if HAVE_ERTS_MSEG
    erts_mseg_late_init();
#endif
    esdp->aux_work_data.async_ready.queue = erts_get_async_ready_queue(no);

    erts_sched_init_check_cpu_bind(esdp);

    erts_proc_lock_prepare_proc_lock_waiter();

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

    erts_alcu_sched_spec_data_init(esdp);
    erts_ets_sched_spec_data_init(esdp);

    process_main(esdp->x_reg_array, esdp->f_reg_array);

    /* No schedulers should *ever* terminate */
    erts_exit(ERTS_ABORT_EXIT,
	     "Scheduler thread number %beu terminated\n",
	     no);
    return NULL;
}

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
    esdp->aux_work_data.async_ready.queue = NULL;

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
    esdp->aux_work_data.async_ready.queue = NULL;

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

void
erts_start_schedulers(void)
{
    ethr_tid tid;
    int res = 0;
    char name[16];
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    int ix;

    opts.detached = 1;

    opts.name = name;

    if (erts_runq_supervision_interval) {
	opts.suggested_stack_size = 16;
        erts_snprintf(opts.name, 16, "runq_supervisor");
	erts_atomic_init_nob(&runq_supervisor_sleeping, 0);
	if (0 != ethr_event_init(&runq_supervision_event))
	    erts_exit(ERTS_ABORT_EXIT, "Failed to create run-queue supervision event\n");
        res = ethr_thr_create(&runq_supervisor_tid,
                              runq_supervisor,
                              NULL,
                              &opts);
	if (0 != res)
	    erts_exit(ERTS_ABORT_EXIT, "Failed to create run-queue supervision thread, "
                      "error = %d\n", res);

    }

    opts.suggested_stack_size = erts_sched_thread_suggested_stack_size;

    ASSERT(erts_no_schedulers > 0 && erts_no_schedulers <= ERTS_MAX_NO_OF_SCHEDULERS);

    for (ix = 0; ix < erts_no_schedulers; ix++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
	ASSERT(ix == esdp->no - 1);
	erts_snprintf(opts.name, 16, "%lu_scheduler", ix + 1);
	res = ethr_thr_create(&esdp->tid, sched_thread_func, (void*)esdp, &opts);
	if (res != 0) {
           erts_exit(ERTS_ABORT_EXIT, "Failed to create scheduler thread %d, error = %d\n", ix, res);
	}
    }

    /* Probably not needed as thread create will imply a memory barrier,
       but we do one just to be safe. */
    ERTS_THR_MEMORY_BARRIER;

    {
	for (ix = 0; ix < erts_no_dirty_cpu_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_CPU_SCHEDULER_IX(ix);
	    erts_snprintf(opts.name, 16, "%d_dirty_cpu_scheduler", ix + 1);
            opts.suggested_stack_size = erts_dcpu_sched_thread_suggested_stack_size;
	    res = ethr_thr_create(&esdp->tid,sched_dirty_cpu_thread_func,(void*)esdp,&opts);
	    if (res != 0)
		erts_exit(ERTS_ABORT_EXIT, "Failed to create dirty cpu scheduler thread %d, error = %d\n", ix, res);
	}
	for (ix = 0; ix < erts_no_dirty_io_schedulers; ix++) {
	    ErtsSchedulerData *esdp = ERTS_DIRTY_IO_SCHEDULER_IX(ix);
	    erts_snprintf(opts.name, 16, "%d_dirty_io_scheduler", ix + 1);
            opts.suggested_stack_size = erts_dio_sched_thread_suggested_stack_size;
	    res = ethr_thr_create(&esdp->tid,sched_dirty_io_thread_func,(void*)esdp,&opts);
	    if (res != 0)
		erts_exit(ERTS_ABORT_EXIT, "Failed to create dirty io scheduler thread %d, error = %d\n", ix, res);
	}
    }

    erts_snprintf(opts.name, 16, "aux");

    res = ethr_thr_create(&tid, aux_thread, NULL, &opts);
    if (res != 0)
	erts_exit(ERTS_ABORT_EXIT, "Failed to create aux thread, error = %d\n", res);

    for (ix = 0; ix < erts_no_poll_threads; ix++) {
        erts_snprintf(opts.name, 16, "%d_poller", ix);

        res = ethr_thr_create(&tid, poll_thread, (void*)(UWord)ix, &opts);
        if (res != 0)
            erts_exit(ERTS_ABORT_EXIT, "Failed to create poll thread\n");
    }
}

BIF_RETTYPE
erts_internal_suspend_process_2(BIF_ALIST_2)
{
    Eterm res;
    Eterm reply_tag = THE_NON_VALUE;
    Eterm reply_res = THE_NON_VALUE;
    int suspend;
    int sync = 0;
    int async = 0;
    int unless_suspending = 0;
    erts_aint_t mstate;
    ErtsMonitorSuspend *msp;
    ErtsMonitorData *mdp;

    if (BIF_P->common.id == BIF_ARG_1)
	BIF_RET(am_badarg); /* We are not allowed to suspend ourselves */

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
		async = 1;
		break;
	    default: {
                if (is_tuple_arity(arg, 2)) {
                    Eterm *tp = tuple_val(arg);
                    if (tp[1] == am_asynchronous) {
                        async = 1;
                        reply_tag = tp[2];
                        break;
                    }
                }
                BIF_RET(am_badarg);
	    }
            }
	    arg = CDR(lp);
        }
	if (is_not_nil(arg))
            BIF_RET(am_badarg);
    }

    if (!unless_suspending) {
        ErtsMonitor *mon;
        mon = erts_monitor_tree_lookup_create(&ERTS_P_MONITORS(BIF_P),
                                              &suspend,
                                              ERTS_MON_TYPE_SUSPEND,
                                              BIF_P->common.id,
                                              BIF_ARG_1);
        ASSERT(mon->other.item == BIF_ARG_1);

        mdp = erts_monitor_to_data(mon);
        msp = (ErtsMonitorSuspend *) mdp;

        mstate = erts_atomic_inc_read_relb(&msp->state);
        ASSERT(suspend || (mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK) > 1);
        sync = !async & !suspend & !(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE);
        suspend = !!suspend; /* ensure 0|1 */
        res = am_true;
    }
    else {
        ErtsMonitor *mon;
        mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P),
                                       BIF_ARG_1);
        if (mon) {
            ASSERT(mon->type == ERTS_MON_TYPE_SUSPEND);
            mdp = erts_monitor_to_data(mon);
            msp = (ErtsMonitorSuspend *) mdp;
            mstate = erts_atomic_read_nob(&msp->state);
            ASSERT((mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK) > 0);
            mdp = NULL;
            sync = !async & !(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE);
            suspend = 0;
            res = am_false;
        }
        else {
            mdp = erts_monitor_create(ERTS_MON_TYPE_SUSPEND, NIL,
                                      BIF_P->common.id,
                                      BIF_ARG_1, NIL);
            mon = &mdp->origin;
            erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P), mon);
            msp = (ErtsMonitorSuspend *) mdp;
            mstate = erts_atomic_inc_read_relb(&msp->state);
            ASSERT(!(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE));
            suspend = !0;
            res = am_true;
        }
    }

    if (suspend) {
        erts_aint32_t state;
        Process *rp;
        int send_sig = 0;

        /* fail state... */
        state = (ERTS_PSFLG_EXITING
                 | ERTS_PSFLG_RUNNING
                 | ERTS_PSFLG_RUNNING_SYS
                 | ERTS_PSFLG_DIRTY_RUNNING
                 | ERTS_PSFLG_DIRTY_RUNNING_SYS);

        rp = erts_try_lock_sig_free_proc(BIF_ARG_1,
                                         ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS,
                                         &state);
        if (!rp)
            goto noproc;
        if (rp == ERTS_PROC_LOCK_BUSY)
            send_sig = !0;
        else {
            send_sig = !suspend_process(BIF_P, rp);
            if (!send_sig) {
                erts_monitor_list_insert(&ERTS_P_LT_MONITORS(rp), &mdp->target);
                erts_atomic_read_bor_relb(&msp->state,
                                          ERTS_MSUSPEND_STATE_FLG_ACTIVE);
            }
            erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
        }
        if (send_sig) {
            if (erts_proc_sig_send_monitor(&mdp->target, BIF_ARG_1))
                sync = !async;
            else {
            noproc:
                erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), &mdp->origin);
                erts_monitor_release_both(mdp);
                if (!async)
                    res = am_badarg;
            }
        }
    }

    if (sync) {
        ASSERT(is_non_value(reply_tag));
        reply_res = res;
        reply_tag = res = erts_make_ref(BIF_P);
        ERTS_RECV_MARK_SAVE(BIF_P);
        ERTS_RECV_MARK_SET(BIF_P);
    }

    if (is_value(reply_tag))
        erts_proc_sig_send_sync_suspend(BIF_P, BIF_ARG_1, reply_tag, reply_res);

    BIF_RET(res);
}

/*
 * The erlang:resume_process/1 BIF
 */

BIF_RETTYPE
resume_process_1(BIF_ALIST_1)
{
    ErtsMonitor *mon;
    ErtsMonitorSuspend *msp;
    erts_aint_t mstate;
 
    if (BIF_P->common.id == BIF_ARG_1)
	BIF_ERROR(BIF_P, BADARG);

    mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P),
                                   BIF_ARG_1);
    if (!mon) {
	/* No previous suspend or dead suspendee */
        BIF_ERROR(BIF_P, BADARG);
    }

    ASSERT(mon->type == ERTS_MON_TYPE_SUSPEND);
    msp = (ErtsMonitorSuspend *) erts_monitor_to_data(mon);

    mstate = erts_atomic_dec_read_relb(&msp->state);

    ASSERT((mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK) >= 0);

    if ((mstate & ERTS_MSUSPEND_STATE_COUNTER_MASK) == 0) {
        erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), mon);
        erts_proc_sig_send_demonitor(mon);
    }

    BIF_RET(am_true);
}

BIF_RETTYPE
erts_internal_is_process_executing_dirty_1(BIF_ALIST_1)
{
    if (is_not_internal_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    else {
	Process *rp = erts_proc_lookup(BIF_ARG_1);
	if (rp) {
	    erts_aint32_t state = erts_atomic32_read_nob(&rp->state);
	    if (state & (ERTS_PSFLG_DIRTY_RUNNING
			 |ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
		BIF_RET(am_true);
	    }
	}
    }
    BIF_RET(am_false);
}

static ERTS_INLINE void
run_queues_len_aux(ErtsRunQueue *rq, Uint *tot_len, Uint *qlen, int *ip, int incl_active_sched, int locked)
{
    Sint rq_len;

    if (locked)
        rq_len = (Sint) erts_atomic32_read_dirty(&rq->len);
    else
        rq_len = (Sint) erts_atomic32_read_nob(&rq->len);
    ASSERT(rq_len >= 0);

    if (incl_active_sched) {
        if (ERTS_RUNQ_IX_IS_DIRTY(rq->ix)) {
            erts_aint32_t dcnt;
            if (ERTS_RUNQ_IS_DIRTY_CPU_RUNQ(rq)) {
                dcnt = erts_atomic32_read_nob(&dirty_count.cpu.active);
                ASSERT(0 <= dcnt && dcnt <= erts_no_dirty_cpu_schedulers);
            }
            else {
                ASSERT(ERTS_RUNQ_IS_DIRTY_IO_RUNQ(rq));
                dcnt = erts_atomic32_read_nob(&dirty_count.io.active);
                ASSERT(0 <= dcnt && dcnt <= erts_no_dirty_io_schedulers);
            }
            rq_len += (Sint) dcnt;
        }
        else
        {
            if (ERTS_RUNQ_FLGS_GET_NOB(rq) & ERTS_RUNQ_FLG_EXEC)
                rq_len++;
        }
    }
    if (qlen)
        qlen[(*ip)++] = rq_len;
    *tot_len += (Uint) rq_len;
}

Uint
erts_run_queues_len(Uint *qlen, int atomic_queues_read, int incl_active_sched,
                    int incl_dirty_io)
{
    int i = 0, j = 0;
    Uint len = 0;
    int no_rqs = erts_no_run_queues;

    if (incl_dirty_io)
        no_rqs += ERTS_NUM_DIRTY_RUNQS;
    else
        no_rqs += ERTS_NUM_DIRTY_CPU_RUNQS;

    if (atomic_queues_read) {
        ERTS_ATOMIC_FOREACH_RUNQ_X(rq, no_rqs,
                                   run_queues_len_aux(rq, &len, qlen, &j,
                                                      incl_active_sched, 1),
                                   /* Nothing... */);
    }
    else {
	for (i = 0; i < no_rqs; i++) {
	    ErtsRunQueue *rq = ERTS_RUNQ_IX(i);
            run_queues_len_aux(rq, &len, qlen, &j, incl_active_sched, 0);
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
	erts_aint32_t state = erts_atomic32_read_acqb(&p->state);
	res = erts_process_state2status(state);
    }
    else {
	int i;
	ErtsSchedulerData *esdp;

	for (i = 0; i < erts_no_schedulers; i++) {
	    esdp = ERTS_SCHEDULER_IX(i);
	    erts_runq_lock(esdp->run_queue);
	    if (esdp->free_process
		&& esdp->free_process->common.id == rpid) {
		res = am_free;
		erts_runq_unlock(esdp->run_queue);
		break;
	    }
	    erts_runq_unlock(esdp->run_queue);
	}
    }
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
    ERTS_LC_ASSERT(c_p_locks == erts_proc_lc_my_proc_locks(c_p));
    if (!(c_p_locks & ERTS_PROC_LOCK_STATUS))
	erts_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    if (busy_port)
	suspend = erts_save_suspend_process_on_port(busy_port, c_p);
    else
	suspend = 1;

    if (suspend) {
	int res = suspend_process(c_p, c_p);
	ASSERT(res); (void)res;
    }

    if (!(c_p_locks & ERTS_PROC_LOCK_STATUS))
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    if (suspend && busy_port && erts_system_monitor_flags.busy_port)
	monitor_generic(c_p, am_busy_port, busy_port->common.id);
}

void
erts_resume(Process* process, ErtsProcLocks process_locks)
{
    ERTS_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_proc_lock(process, ERTS_PROC_LOCK_STATUS);
    resume_process(process, process_locks|ERTS_PROC_LOCK_STATUS);
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_proc_unlock(process, ERTS_PROC_LOCK_STATUS);
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
	ASSERT(is_internal_pid(plp->u.pid) || is_CP((Eterm)plp->u.p));
        if (is_internal_pid(plp->u.pid))
            proc = erts_pid2proc(NULL, 0, plp->u.pid, ERTS_PROC_LOCK_STATUS);
        else {
            proc = plp->u.p;
            erts_proc_lock(proc, ERTS_PROC_LOCK_STATUS);
        }
	if (proc) {
	    if (erts_proclist_same(plp, proc)) {
		resume_process(proc, ERTS_PROC_LOCK_STATUS);
		nresumed++;
	    }
	    erts_proc_unlock(proc, ERTS_PROC_LOCK_STATUS);
	}
	fplp = plp;
	plp = plp->next;
	proclist_destroy(fplp);
    }
    return nresumed;
}

Eterm
erts_get_process_priority(erts_aint32_t state)
{
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

    a = erts_atomic32_read_nob(&p->state);
    if (nprio == ERTS_PSFLGS_GET_USR_PRIO(a))
	oprio = nprio;
    else {
	int slocked = 0;
	erts_aint32_t e, n, aprio;

	if (a & ERTS_PSFLG_ACTIVE_SYS) {
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
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
		    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
		    slocked = 1;
		}

		max_qbit = 0;
                ASSERT((a & ERTS_PSFLG_SYS_TASKS)
                       ? !!p->sys_task_qs
                       : !p->sys_task_qs);
		if (a & ERTS_PSFLG_SYS_TASKS)
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
                    aprio = nprio;
                    break;
		}

		if (aprio > nprio) /* low value -> high prio */
		    aprio = nprio;
	    }

	    n &= ~(ERTS_PSFLGS_USR_PRIO_MASK
		   | ERTS_PSFLGS_ACT_PRIO_MASK);
	    n |= ((nprio << ERTS_PSFLGS_USR_PRIO_OFFSET)
		  | (aprio << ERTS_PSFLGS_ACT_PRIO_OFFSET));

	    a = erts_atomic32_cmpxchg_mb(&p->state, n, e);
	} while (a != e);

        if (slocked)
            erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

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

static void
unlock_lock_rq(int pre_free, void *vrq)
{
    ErtsRunQueue *rq = vrq;
    if (pre_free)
        erts_runq_unlock(rq);
    else
        erts_runq_lock(rq);
}


static void trace_schedule_in(Process *p, erts_aint32_t state);
static void trace_schedule_out(Process *p, erts_aint32_t state);

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
    int fcalls = 0;
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
    }
    else {
	context_reds = CONTEXT_REDS;
    }

    ERTS_LC_ASSERT(ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data())
		       || !erts_thr_progress_is_blocking());

    /*
     * Clean up after the process being scheduled out.
     */
    if (!p) {	/* NULL in the very first schedule() call */
	is_normal_sched = !esdp;
	if (is_normal_sched) {
	    esdp = erts_get_scheduler_data();
	    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
	else {
	    ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
	rq = erts_get_runq_current(esdp);
	ASSERT(esdp);
	actual_reds = reds = 0;
	erts_runq_lock(rq);
    } else {
	is_normal_sched = !esdp;
	if (is_normal_sched) {
            esdp = p->scheduler_data;
	    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(esdp));

            if (esdp->pending_signal.sig) {
                ASSERT(esdp->pending_signal.dbg_from == p);
                erts_proc_sig_send_pending(esdp);
            }
	}
	else {
	    ASSERT(ERTS_SCHEDULER_IS_DIRTY(esdp));
	}
	ASSERT(esdp->current_process == p
	       || esdp->free_process == p);


	reds = actual_reds = calls - esdp->virtual_reds;

    internal_sched_out_proc:

	ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
        ASSERT(p->scheduler_data || ERTS_SCHEDULER_IS_DIRTY(esdp));

	ASSERT(actual_reds >= 0);
	if (reds < ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST)
	    reds = ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST;
	esdp->virtual_reds = 0;

#if ERTS_POLL_USE_SCHEDULER_POLLING
        fcalls = (int) erts_atomic32_add_read_acqb(&function_calls, reds);
#endif

	ASSERT(esdp && esdp == erts_get_scheduler_data());

	rq = erts_get_runq_current(esdp);

	p->reds += actual_reds;

	state = erts_atomic32_read_nob(&p->state);

	if (IS_TRACED(p))
            trace_schedule_out(p, state);

	erts_proc_lock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);

        if (p->trace_msg_q) {
	    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_schedule_flush_trace_messages(p, 1);
	    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
	}

        /* have to re-read state after taking lock */
        state = erts_atomic32_read_nob(&p->state);

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
            if (is_normal_sched)
                p->scheduler_data = NULL;

            erts_proc_unlock(p, (ERTS_PROC_LOCK_MAIN
                                     | ERTS_PROC_LOCK_STATUS
                                     | ERTS_PROC_LOCK_TRACE));

            ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);

            if (state & ERTS_PSFLG_FREE) {
                if (!is_normal_sched) {
                    ASSERT(p->flags & F_DELAYED_DEL_PROC);
                }
                else {
                    ASSERT(esdp->free_process == p);
                    esdp->free_process = NULL;
                }
            }

            if (dec_refc)
                erts_proc_dec_refc_free_func(p,
                                             unlock_lock_rq,
                                             (void *) rq);
        }

	ASSERT(!esdp->free_process);
	ASSERT(!esdp->current_process);

	ERTS_CHK_NO_PROC_LOCKS;

    }

    ERTS_LC_ASSERT(!is_normal_sched || !erts_thr_progress_is_blocking());

 check_activities_to_run: {
	erts_aint32_t psflg_running, psflg_running_sys;
	ErtsMigrationPaths *mps;
	ErtsMigrationPath *mp;

	if (is_normal_sched) {

	    if (esdp->check_time_reds >= ERTS_CHECK_TIME_REDS)
		(void) erts_get_monotonic_time(esdp);

	    if (esdp->last_monotonic_time >= erts_next_timeout_time(esdp->next_tmo_ref)) {
		erts_runq_unlock(rq);
		erts_bump_timers(esdp->timer_wheel, esdp->last_monotonic_time);
		erts_runq_lock(rq);
	    }

	    if (rq->check_balance_reds <= 0)
		check_balance(rq);

	    ERTS_LC_ASSERT(!erts_thr_progress_is_blocking());

	    mps = erts_get_migration_paths_managed();
	    mp = &mps->mpath[rq->ix];

	    if (mp->flags & ERTS_RUNQ_FLGS_IMMIGRATE_QMASK)
		immigrate(rq, mp);
	}

	ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));
    continue_check_activities_to_run:
	flags = ERTS_RUNQ_FLGS_GET_NOB(rq);
    continue_check_activities_to_run_known_flags:
	ASSERT(!is_normal_sched || (flags & ERTS_RUNQ_FLG_NONEMPTY));

	if (!is_normal_sched) {
	    if (erts_atomic32_read_acqb(&esdp->ssi->flags)
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

	    leader_update = erts_thr_progress_update(erts_thr_prgr_data(esdp));
	    aux_work = erts_atomic32_read_acqb(&esdp->ssi->aux_work);
	    if (aux_work | leader_update) {
		erts_runq_unlock(rq);
		if (leader_update)
		    erts_thr_progress_leader_update(erts_thr_prgr_data(esdp));
		if (aux_work)
		    handle_aux_work(&esdp->aux_work_data, aux_work, 0);
		erts_runq_lock(rq);
	    }

	    ERTS_LC_ASSERT(!erts_thr_progress_is_blocking());
	}
	ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));


	flags = ERTS_RUNQ_FLGS_GET_NOB(rq);

	if (!is_normal_sched & !!(flags & ERTS_RUNQ_FLG_HALTING)) {
	    /* Wait for emulator to terminate... */
	    erts_runq_unlock(rq);
	    while (1)
		erts_milli_sleep(1000*1000);
	}
	else if (!runq_got_work_to_execute_flags(flags)) {
	    /* Prepare for scheduler wait */
	    ERTS_LC_ASSERT(erts_lc_runq_is_locked(rq));

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
                    if (erts_atomic32_read_acqb(&esdp->ssi->flags)
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
                        /* If multi scheduling block and we have
                         * dirty work, suspend and let dirty
                         * scheduler handle work... */
                        || ((((flags & (ERTS_RUNQ_FLG_HALTING
                                       | ERTS_RUNQ_FLG_MSB_EXEC))
                              == ERTS_RUNQ_FLG_MSB_EXEC))
                            && have_dirty_work())
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

	    (void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_EXEC);
	    scheduler_wait(&fcalls, esdp, rq);
	    flags = ERTS_RUNQ_FLGS_SET_NOB(rq, ERTS_RUNQ_FLG_EXEC);
	    flags |= ERTS_RUNQ_FLG_EXEC;
            ERTS_MSACC_UPDATE_CACHE();
	    non_empty_runq(rq);

	    goto check_activities_to_run;
	} else if (is_normal_sched &&
                   fcalls > (2 * context_reds) &&
                   prepare_for_sys_schedule()) {
            ErtsMonotonicTime current_time;
	    /*
	     * Schedule system-level activities.
	     */

	    ERTS_MSACC_PUSH_STATE_CACHED_M();

	    erts_runq_unlock(rq);

            ERTS_MSACC_SET_STATE_CACHED_M(ERTS_MSACC_STATE_CHECK_IO);
            LTTNG2(scheduler_poll, esdp->no, 1);

	    erts_check_io(esdp->ssi->psi, ERTS_POLL_NO_TIMEOUT);
	    ERTS_MSACC_POP_STATE_M();

	    current_time = erts_get_monotonic_time(esdp);
	    if (current_time >= erts_next_timeout_time(esdp->next_tmo_ref))
		erts_bump_timers(esdp->timer_wheel, current_time);

	    erts_runq_lock(rq);
	    fcalls = 0;
	    clear_sys_scheduling();
	    goto continue_check_activities_to_run;
        }

        if (flags & ERTS_RUNQ_FLG_MISC_OP)
	    exec_misc_ops(rq);

        runq_get_wakeup_other_params(rq)->check(rq, flags);

	/*
	 * Find a new port to run.
	 */

        flags = ERTS_RUNQ_FLGS_GET_NOB(rq);

	if (flags & PORT_BIT) {
	    erts_port_task_execute(rq, &esdp->current_port);
            if (flags & ERTS_RUNQ_FLG_HALTING)
                goto check_activities_to_run;
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
                ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_OTHER);
		goto check_activities_to_run;
	    }

	    /*
	     * Take the chosen process out of the queue.
	     */
	    p = dequeue_process(rq, prio_q, &state);

	    ASSERT(p); /* Wrong qmask in rq->flags? */

#ifdef DEBUG
            switch (((erts_aint32_t) 1) << ERTS_PSFLGS_GET_PRQ_PRIO(state)) {
	    case MAX_BIT:
                ASSERT(qbit == MAX_BIT);
                break;
            case HIGH_BIT:
                ASSERT(qbit == HIGH_BIT);
                break;
            case NORMAL_BIT:
	    case LOW_BIT:
                ASSERT(qbit == NORMAL_BIT || qbit == LOW_BIT);
                break;
            default:
                ASSERT(0);
                break;
            }
#endif

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
                qbit = ((erts_aint32_t) 1) << ERTS_PSFLGS_GET_PRQ_PRIO(state);
	    }

	    if (!(state & ERTS_PSFLG_PROXY))
		psflg_band_mask &= ~ERTS_PSFLG_IN_RUNQ;
	    else {
		proxy_p = p;
                p = proxy_p->u.real_proc;
                ASSERT(p);
		state = erts_atomic32_read_nob(&p->state);
	    }

	    if (!is_normal_sched)
		clear_proc_dirty_queue_bit(p, rq, qbit);

	    while (1) {
		erts_aint32_t exp, new;
		int run_process, is_active, is_running,
                    is_suspended, need_forced_exit;
		new = exp = state;
		new &= psflg_band_mask;
                
                /* Active? (has work to do) */
                is_active = !!(state & (ERTS_PSFLG_ACTIVE
                                        | ERTS_PSFLG_ACTIVE_SYS
                                        | ERTS_PSFLG_DIRTY_ACTIVE_SYS));

                /* Suspended? (suspend does not effect active-sys) */
                is_suspended = !!((state & (ERTS_PSFLG_SUSPENDED
                                            | ERTS_PSFLG_ACTIVE_SYS
                                            | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
                                  == ERTS_PSFLG_SUSPENDED);

                /* Already running? */
                is_running = !!(state & (ERTS_PSFLG_RUNNING
                                         | ERTS_PSFLG_RUNNING_SYS
                                         | ERTS_PSFLG_DIRTY_RUNNING
                                         | ERTS_PSFLG_DIRTY_RUNNING_SYS));

                /*
                 * If on a normal scheduler, check if we should forcebly
                 * terminate process despite it is executing on a dirty
                 * scheduler (we can not do that if it is executing
                 * dirty system tasks though).
                 */

                need_forced_exit = (!!is_normal_sched
                                    & ((state & (ERTS_PSFLG_RUNNING
                                                 | ERTS_PSFLG_DIRTY_RUNNING
                                                 | ERTS_PSFLG_RUNNING_SYS
                                                 | ERTS_PSFLG_DIRTY_RUNNING_SYS
                                                 | ERTS_PSFLG_EXITING))
                                       == (ERTS_PSFLG_DIRTY_RUNNING
                                           | ERTS_PSFLG_EXITING)));

                run_process = (is_active
                               & (!is_suspended)
                               & ((!is_running) | need_forced_exit));

		if (run_process) {
		    if (state & (ERTS_PSFLG_ACTIVE_SYS
				 | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
			new |= psflg_running_sys;
		    else
			new |= psflg_running;
		}
		state = erts_atomic32_cmpxchg_relb(&p->state, new, exp);
		if (state == exp) {
		    if (!run_process) {
			if (proxy_p) {
			    free_proxy_proc(proxy_p);
			    proxy_p = NULL;
			}
			else if ((state & (ERTS_PSFLG_ACTIVE
                                           | ERTS_PSFLG_FREE))
                                 == ERTS_PSFLG_FREE) {
			    /*
                             * inactive-free and not queued by proxy
                             *
                             * Corresponds to increment in
                             * erts_continue_exit_process() after
                             * state ERTS_CONTINUE_EXIT_DONE.
                             */
                            ASSERT(state & ERTS_PSFLG_IN_RUNQ);
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

	    erts_runq_unlock(rq);

	}

        ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_EMULATOR);


	if (flags & ERTS_RUNQ_FLG_PROTECTED)
	    (void) ERTS_RUNQ_FLGS_UNSET(rq, ERTS_RUNQ_FLG_PROTECTED);

	ERTS_CHK_NO_PROC_LOCKS;

	erts_proc_lock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);

	state = erts_atomic32_read_nob(&p->state);

	if (erts_sched_stat.enabled) {
	    int prio;
	    UWord old = ERTS_PROC_SCHED_ID(p, (UWord) esdp->no);
	    int migrated = old && old != esdp->no;

	    ASSERT(is_normal_sched);

	    prio = (int) ERTS_PSFLGS_GET_USR_PRIO(state);

	    erts_spin_lock(&erts_sched_stat.lock);
	    erts_sched_stat.prio[prio].total_executed++;
	    erts_sched_stat.prio[prio].executed++;
	    if (migrated) {
		erts_sched_stat.prio[prio].total_migrated++;
		erts_sched_stat.prio[prio].migrated++;
	    }
	    erts_spin_unlock(&erts_sched_stat.lock);
	}

	state = erts_atomic32_read_nob(&p->state);

	if (is_normal_sched) {
            ASSERT(!p->scheduler_data);
	    p->scheduler_data = esdp;
	    if ((!!(state & ERTS_PSFLGS_DIRTY_WORK))
		& (!(state & ERTS_PSFLG_RUNNING_SYS))) {
		/* Migrate to dirty scheduler... */
	    sunlock_sched_out_proc:
		erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
                if (IS_TRACED(p))
                    trace_schedule_in(p, state);
		goto sched_out_proc;
	    }
	}
	else {
            /* On dirty scheduler */
	    if (!(state & ERTS_PSFLGS_DIRTY_WORK)
                | !!(state & (ERTS_PSFLG_SYS_TASKS
                              | ERTS_PSFLG_EXITING
                              | ERTS_PSFLG_DIRTY_ACTIVE_SYS))) {
                
                if (!(state & ERTS_PSFLGS_DIRTY_WORK)) {
                    /* Dirty work completed... */
                    goto sunlock_sched_out_proc;
                }
                if (state & (ERTS_PSFLG_SYS_TASKS
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

            }

	    ASSERT(rq == ERTS_DIRTY_CPU_RUNQ
		   ? (state & (ERTS_PSFLG_DIRTY_CPU_PROC
			       | ERTS_PSFLG_DIRTY_ACTIVE_SYS))
		   : (rq == ERTS_DIRTY_IO_RUNQ
		      && (state & ERTS_PSFLG_DIRTY_IO_PROC)));	    
	}

	erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

        if (IS_TRACED(p))
            trace_schedule_in(p, state);

        if (!is_normal_sched) {
            /* On dirty scheduler */
            if (!!(state & ERTS_PSFLG_DIRTY_RUNNING)
                & !!(state & (ERTS_PSFLG_SIG_Q|ERTS_PSFLG_SIG_IN_Q))) {
                /* Ensure signals are handled while executing dirty... */
                int prio = ERTS_PSFLGS_GET_ACT_PRIO(state);
                erts_make_dirty_proc_handled(p->common.id, state, prio);
            }
        }
        else {
            /* On normal scheduler */
            if (state & ERTS_PSFLG_RUNNING_SYS) {
                if (state & (ERTS_PSFLG_SIG_Q|ERTS_PSFLG_SIG_IN_Q)) {
                    int local_only = (!!(p->sig_qs.flags & FS_LOCAL_SIGS_ONLY)
                                      & !(state & (ERTS_PSFLG_SUSPENDED|ERTS_PSFLGS_DIRTY_WORK)));
                    if (!local_only | !!(state & ERTS_PSFLG_SIG_Q)) {
                        int sig_reds;
                        /*
                         * If we have dirty work scheduled we allow
                         * usage of all reductions since we need to
                         * handle all signals before doing dirty
                         * work...
                         */
                        if (state & ERTS_PSFLGS_DIRTY_WORK)
                            sig_reds = reds;
                        else
                            sig_reds = ERTS_SIG_HANDLE_REDS_MAX_PREFERED;
                        (void) erts_proc_sig_handle_incoming(p,
                                                             &state,
                                                             &sig_reds,
                                                             sig_reds,
                                                             local_only);
                        reds -= sig_reds;
                    }
                }
                if ((state & (ERTS_PSFLG_SYS_TASKS
                              | ERTS_PSFLG_EXITING)) == ERTS_PSFLG_SYS_TASKS) {
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
                    }
                }

                if (reds <= 0 || (state & ERTS_PSFLGS_DIRTY_WORK))
                    goto sched_out_proc;

                ASSERT(state & psflg_running_sys);
                ASSERT(!(state & psflg_running));

                while (1) {
                    erts_aint32_t n, e;

                    if (((state & (ERTS_PSFLG_SUSPENDED
                                   | ERTS_PSFLG_ACTIVE)) != ERTS_PSFLG_ACTIVE)
                        & !(state & ERTS_PSFLG_EXITING)) {
                        goto sched_out_proc;
                    }

                    n = e = state;
                    n &= ~psflg_running_sys;
                    n |= psflg_running;

                    state = erts_atomic32_cmpxchg_mb(&p->state, n, e);
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
                    if (p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC))
                        goto sched_out_proc;
                }
            }
        }

	if (proxy_p) {
	    free_proxy_proc(proxy_p);
	    proxy_p = NULL;
	}

	p->fcalls = reds;
        if (reds != context_reds) {
            actual_reds = context_reds - reds - esdp->virtual_reds;
            ASSERT(actual_reds >= 0);
            esdp->virtual_reds = 0;
            p->reds += actual_reds;
            ERTS_PROC_REDUCTIONS_EXECUTED(esdp, rq,
                                          (int) ERTS_PSFLGS_GET_USR_PRIO(state),
                                          reds,
                                          actual_reds);
        }

	ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

	ASSERT(erts_proc_read_refc(p) > 0);

	if (!(state & ERTS_PSFLG_EXITING) && ERTS_PTMR_IS_TIMED_OUT(p)) {
	    BeamInstr** pi;
	    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
	    pi = (BeamInstr **) p->def_arg_reg;
	    p->i = *pi;
	    p->flags &= ~F_INSLPQUEUE;
	    p->flags |= F_TIMO;
	    ERTS_PTMR_CLEAR(p);
	}

        /* if exiting, we *shall* exit... */
        ASSERT(!(state & ERTS_PSFLG_EXITING)
               || p->i == (BeamInstr *) beam_exit
               || p->i == (BeamInstr *) beam_continue_exit);

#ifdef DEBUG
        if (is_normal_sched) {
	    if (state & ERTS_PSFLGS_DIRTY_WORK)
                ERTS_INTERNAL_ERROR("Executing dirty code on normal scheduler");
        }
        else {
	    if (!(state & ERTS_PSFLGS_DIRTY_WORK)) {
                if (esdp->type == ERTS_SCHED_DIRTY_CPU)
                    ERTS_INTERNAL_ERROR("Executing normal code on dirty CPU scheduler");
                else if (esdp->type == ERTS_SCHED_DIRTY_IO)
                    ERTS_INTERNAL_ERROR("Executing normal code on dirty IO scheduler");
                else
                    ERTS_INTERNAL_ERROR("Executing normal code on dirty UNKNOWN scheduler");
            }
        }
        {
            erts_aint32_t dstate = erts_atomic32_read_nob(&p->state);

            /* Never run a suspended process */
            ASSERT(!(ERTS_PSFLG_SUSPENDED & dstate)
                   || (ERTS_PSFLG_DIRTY_RUNNING_SYS & dstate));

            /* Do not execute on the wrong type of scheduler... */
            ASSERT(is_normal_sched
                   ? !(dstate & ERTS_PSFLGS_DIRTY_WORK)
                   : !!(dstate & ERTS_PSFLGS_DIRTY_WORK));
        }
#endif

	return p;

    sched_out_proc:
        actual_reds = context_reds;
        actual_reds -= reds;
        actual_reds -= esdp->virtual_reds;
        reds = actual_reds;
        goto internal_sched_out_proc;

    }
}

static void
trace_schedule_in(Process *p, erts_aint32_t state)
{
    ASSERT(IS_TRACED(p));
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCK_MAIN);

    /* Clear tracer if it has been removed */
    if (erts_is_tracer_proc_enabled(p, ERTS_PROC_LOCK_MAIN, &p->common)) {

        if (state & ERTS_PSFLG_EXITING && p->u.terminate) {
            if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
                trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in_exiting);
        }
        else {
            if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED) ||
                ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
                trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in);
        }
        if (IS_TRACED_FL(p, F_TRACE_CALLS))
            erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_IN);
    }

}

static void
trace_schedule_out(Process *p, erts_aint32_t state)
{
    ASSERT(IS_TRACED(p));
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCK_MAIN);
    
    if (IS_TRACED_FL(p, F_TRACE_CALLS) && !(state & ERTS_PSFLG_FREE))
        erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_OUT);

    if (state & ERTS_PSFLG_EXITING && p->u.terminate) {
        if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
            trace_sched(p, ERTS_PROC_LOCK_MAIN, am_out_exiting);
    }
    else {
        if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED) ||
            ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
            trace_sched(p, ERTS_PROC_LOCK_MAIN, am_out);
    }
}

static int
notify_sys_task_executed(Process *c_p, ErtsProcSysTask *st,
			 Eterm st_result, int normal_sched)
{
    Process *rp;
    if (!normal_sched)
	rp = erts_pid2proc_opt(c_p, ERTS_PROC_LOCK_MAIN,
			       st->requester, 0,
			       ERTS_P2P_FLG_INC_REFC);
    else
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

        ERL_MESSAGE_TOKEN(mp) = am_undefined;
	erts_queue_proc_message(c_p, rp, rp_locks, mp, msg);

	if (c_p == rp)
	    rp_locks &= ~ERTS_PROC_LOCK_MAIN;

	if (rp_locks)
	    erts_proc_unlock(rp, rp_locks);

	if (!normal_sched)
	    erts_proc_dec_refc(rp);
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

    erts_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    if (!c_p->sys_task_qs) {
	qmask = 0;
	st = NULL;
	goto update_state;
    }

    qmask = c_p->sys_task_qs->qmask;

    if ((state & (ERTS_PSFLGS_DIRTY_WORK
                  | ERTS_PSFLG_ACTIVE
		  | ERTS_PSFLG_EXITING
		  | ERTS_PSFLG_SUSPENDED)) == ERTS_PSFLG_ACTIVE) {
	/*
         * No sys tasks if we got exclusively higher prio user work
         * to do; ignoring dirty work...
         */
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
		n &= ~ERTS_PSFLG_SYS_TASKS;

	    if (a == n)
		break;
	    a = erts_atomic32_cmpxchg_nob(&c_p->state, n, e);
	} while (a != e);
    }

done:

    erts_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    if (unused_qs)
	proc_sys_task_queues_free(unused_qs);

    *qmaskp = qmask;

    return st;
}


static void exit_permanent_prio_elevation(Process *c_p, erts_aint32_t state);
static void save_gc_task(Process *c_p, ErtsProcSysTask *st, int prio);
static void save_dirty_task(Process *c_p, ErtsProcSysTask *st);

static int
execute_sys_tasks(Process *c_p, erts_aint32_t *statep, int in_reds)
{
    int minor_gc = 0, major_gc = 0;
    erts_aint32_t state = *statep;
    int reds = in_reds;
    int qmask = 0;

    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(c_p)));
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    do {
	ErtsProcSysTaskType type;
	ErtsProcSysTask *st;
	int st_prio;
	Eterm st_res;

	if (state & ERTS_PSFLG_EXITING)
	    break;

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
		if ((!minor_gc
                     || (!major_gc && type == ERTS_PSTT_GC_MAJOR))
                    && !(c_p->flags & F_HIBERNATED)) {
                    if (type == ERTS_PSTT_GC_MAJOR) {
                        FLAGS(c_p) |= F_NEED_FULLSWEEP;
                    }
		    reds -= scheduler_gc_proc(c_p, reds);
		    if (c_p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)) {
			save_dirty_task(c_p, st);
			st = NULL;
			break;
		    }
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
		if (c_p->flags & F_DIRTY_CLA) {
		    save_dirty_task(c_p, st);
		    st = NULL;
		    break;
		}
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
        case ERTS_PSTT_FTMQ:
	    reds -= erts_flush_trace_messages(c_p, ERTS_PROC_LOCK_MAIN);
	    st_res = am_true;
	    break;
        case ERTS_PSTT_ETS_FREE_FIXATION:
	    reds -= erts_db_execute_free_fixation(c_p, (DbFixation*)st->arg[0]);
	    st_res = am_true;
	    break;
        case ERTS_PSTT_PRIO_SIG: {
            erts_aint32_t fail_state, state;
            int local_only, sig_res, sig_reds = reds;
	    st_res = am_false;

            if (st->arg[0] == am_true)
                local_only = !0;
            else
                local_only = 0;

            sig_reds = reds;
            sig_res = erts_proc_sig_handle_incoming(c_p, &state, &sig_reds,
                                                    reds, local_only);
            reds -= sig_reds;

            if (state & ERTS_PSFLG_EXITING) {
                exit_permanent_prio_elevation(c_p, state);
                break;
            }

            if (sig_res)
                break;

            st->arg[0] = am_true;

            fail_state = ERTS_PSFLG_EXITING;

            if (schedule_process_sys_task(c_p, st_prio, st, &fail_state)) {
                /* Successfully rescheduled task... */
                st = NULL;
            }
            else {
                state = erts_atomic32_read_nob(&c_p->state);                         
                exit_permanent_prio_elevation(c_p, state);
            }
            break;
        }
	default:
	    ERTS_INTERNAL_ERROR("Invalid process sys task type");
	    st_res = am_false;
	}

	if (st)
	    reds += notify_sys_task_executed(c_p, st, st_res, 1);

	state = erts_atomic32_read_acqb(&c_p->state);
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
    int qmask = 1;          /* Set to 1 to force looping as long as there
                             * are dirty tasks.
                             */

    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    do {
	ErtsProcSysTask *st;
	Eterm st_res;
	int st_prio;

	if (c_p->dirty_sys_tasks) {
	    st = c_p->dirty_sys_tasks;
	    c_p->dirty_sys_tasks = st->next;
	}
	else
	{
	    st = fetch_sys_task(c_p, state, &qmask, &st_prio);
	    if (!st)
		break;
	}

	switch (st->type) {
        case ERTS_PSTT_PRIO_SIG:
            state = erts_atomic32_read_nob(&c_p->state);                         
            exit_permanent_prio_elevation(c_p, state);
            /* fall through... */
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
        case ERTS_PSTT_FTMQ:
	    reds -= erts_flush_trace_messages(c_p, ERTS_PROC_LOCK_MAIN);
	    st_res = am_true;
	    break;
	default:
	    ERTS_INTERNAL_ERROR("Invalid process sys task type");
	    st_res = am_false;
	    break;
	}

	reds += notify_sys_task_executed(c_p, st, st_res, 1);

	state = erts_atomic32_read_acqb(&c_p->state);
    } while (qmask && reds < max_reds);

    return reds;
}

static void
exit_permanent_prio_elevation(Process *c_p, erts_aint32_t state)
{
    erts_aint32_t a;
    /*
     * we are about to terminate; permanently elevate
     * prio in order to ensure high prio signal
     * handling...
     */
    a = state;
    while (1) {
        erts_aint32_t aprio, uprio, n, e;
        ASSERT(a & ERTS_PSFLG_EXITING);
        aprio = ERTS_PSFLGS_GET_ACT_PRIO(a);
        uprio = ERTS_PSFLGS_GET_USR_PRIO(a);
        if (aprio >= uprio)
            break; /* user prio >= actual prio */
        /*
         * actual prio is higher than user prio; raise
         * user prio to actual prio...
         */
        n = e = a;
        n &= ~ERTS_PSFLGS_USR_PRIO_MASK;
        n |= aprio << ERTS_PSFLGS_USR_PRIO_OFFSET;
        a = erts_atomic32_cmpxchg_mb(&c_p->state, n, e);
        if (a == e)
            break;
    }
}

void
erts_execute_dirty_system_task(Process *c_p)
{
    Eterm cla_res = THE_NON_VALUE;
    ErtsProcSysTask *stasks;

    ASSERT(erts_atomic32_read_nob(&c_p->state)
           & ERTS_PSFLG_DIRTY_RUNNING_SYS);
    /*
     * Currently all dirty system tasks are handled while holding
     * the main lock. The process is during this in the state
     * ERTS_PSFLG_DIRTY_RUNNING_SYS. The dirty signal handlers
     * (erts/preloaded/src/erts_dirty_process_signal_handler.erl)
     * cannot execute any signal handling on behalf of a process
     * executing dirty unless they will be able to acquire the
     * main lock. If they try to, they will just end up in a
     * busy wait until the lock has been released.
     *
     * We now therefore do not schedule any handling on dirty
     * signal handlers while a process is in the state
     * ERTS_PSFLG_DIRTY_RUNNING_SYS. We instead leave the work
     * scheduled on the process an let it detect it itself
     * when it leaves the ERTS_PSFLG_DIRTY_RUNNING_SYS state.
     * See erts_proc_notify_new_sig() in erl_proc_sig_queue.h,
     * request_system_task() (check_process_code) in
     * erl_process.c, and maybe_elevate_sig_handling_prio()
     * in erl_proc_sig_queue.c for scheduling points.
     *
     * If there are dirty system tasks introduced that execute
     * without the main lock held, we most likely want to trigger
     * handling of signals via dirty signal handlers for these
     * states.
     */

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
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	if (erts_proc_sig_fetch(c_p))
            c_p->flags &= ~F_DIRTY_GC_HIBERNATE; /* operation aborted... */
        else {
	    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	    c_p->fvalue = NIL;
	    erts_garbage_collect_hibernate(c_p);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
	    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	}
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_STATUS);
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

    erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_DIRTY_ACTIVE_SYS);
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
	rp = erts_dirty_process_signal_handler;
	ASSERT(fail_state & ERTS_PSFLG_DIRTY_RUNNING);
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

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (rp_locks)
	erts_proc_unlock(rp, rp_locks);

    return ret;
}


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
	req_id_sz = is_immed(req_id) ? 0 : size_object(req_id);
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
	/*
	 * If the process should start executing dirty
	 * code it is important that this task is
	 * aborted. Therefore this strict fail state...
         *
         * We ignore ERTS_PSFLG_DIRTY_RUNNING_SYS. For
         * more info see erts_execute_dirty_system_task()
         * in erl_process.c.
         */
	fail_state |= ERTS_PSFLG_DIRTY_RUNNING;
	break;

    case am_copy_literals:
	if (st->arg[0] != am_true && st->arg[0] != am_false)
	    goto badarg;
	st->type = ERTS_PSTT_CLA;
	noproc_res = am_ok;
        fail_state = ERTS_PSFLG_FREE;
	if (!rp)
	    goto noproc;
	break;

    default:
	goto badarg;
    }

    if (!schedule_process_sys_task(rp, prio, st, &fail_state)) {
	Eterm failure;
	if (fail_state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE)) {
	noproc:
	    failure = noproc_res;
	}
	else if (fail_state & ERTS_PSFLG_DIRTY_RUNNING) {
	    ret = dispatch_system_task(c_p, fail_state, st,
				       target, priority, operation);
	    goto cleanup_return;
	}
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

cleanup_return:

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

static int
schedule_generic_sys_task(Eterm pid, ErtsProcSysTaskType type,
                          int prio, Eterm arg0, Eterm arg1)
{
    int res = 0;
    Process *rp = erts_proc_lookup_raw(pid);
    if (rp) {
	ErtsProcSysTask *st;
	erts_aint32_t st_prio, fail_state;

	st = erts_alloc(ERTS_ALC_T_PROC_SYS_TSK,
			ERTS_PROC_SYS_TASK_SIZE(0));
	st->type = type;
	st->requester = NIL;
	st->reply_tag = NIL;
	st->req_id = NIL;
	st->req_id_sz = 0;
        st->arg[0] = arg0;
        st->arg[1] = arg1;
	ERTS_INIT_OFF_HEAP(&st->off_heap);

        if (prio >= 0) {
            st_prio = (erts_aint32_t) prio;
            fail_state = ERTS_PSFLG_FREE;
        }
        else {
            erts_aint32_t state = erts_atomic32_read_nob(&rp->state);
            st_prio = ERTS_PSFLGS_GET_USR_PRIO(state);
            fail_state = ERTS_PSFLG_EXITING;
        }
        res = schedule_process_sys_task(rp, st_prio, st, &fail_state);
	if (!res)
	    erts_free(ERTS_ALC_T_PROC_SYS_TSK, st);
    }
    return res;
}

void
erts_schedule_complete_off_heap_message_queue_change(Eterm pid)
{
    schedule_generic_sys_task(pid, ERTS_PSTT_COHMQ,
                              -1, NIL, NIL);
}

void
erts_schedule_ets_free_fixation(Eterm pid, DbFixation* fix)
{
    schedule_generic_sys_task(pid, ERTS_PSTT_ETS_FREE_FIXATION,
                              -1, (Eterm) fix, NIL);
}

int
erts_sig_prio(Eterm pid, int prio)
{
    return schedule_generic_sys_task(pid, ERTS_PSTT_PRIO_SIG,
                                     prio, am_false, NIL);
}

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

    proc = erts_pid2proc_opt(NULL, 0, pid, ERTS_PROC_LOCK_MAIN, 0);
    if (proc) {
	(void) erts_flush_trace_messages(proc, ERTS_PROC_LOCK_MAIN);
        erts_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    }
}


void
erts_schedule_flush_trace_messages(Process *proc, int force_on_proc)
{
    ErtsThrPrgrDelayHandle dhndl;
    Eterm pid = proc->common.id;

    erts_aint32_t state;

    if (!force_on_proc) {
	state = erts_atomic32_read_nob(&proc->state);
	if (state & (ERTS_PSFLG_DIRTY_RUNNING
		     | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
	    goto sched_flush_dirty;
	}
    }

    dhndl = erts_thr_progress_unmanaged_delay();

    schedule_generic_sys_task(pid, ERTS_PSTT_FTMQ, -1, NIL, NIL);

    erts_thr_progress_unmanaged_continue(dhndl);

    if (!force_on_proc) {
	state = erts_atomic32_read_mb(&proc->state);
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
}

static void
save_gc_task(Process *c_p, ErtsProcSysTask *st, int prio)
{
    erts_aint32_t state;
    ErtsProcSysTaskQs *qs;

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

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

    state = erts_atomic32_read_nob(&c_p->state);
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
	state = erts_atomic32_cmpxchg_relb(&c_p->state, n, e);
	if (state == e)
	    break;
    }
}

static void
save_dirty_task(Process *c_p, ErtsProcSysTask *st)
{
    st->next = c_p->dirty_sys_tasks;
    c_p->dirty_sys_tasks = st;
}

int
erts_set_gc_state(Process *c_p, int enable)
{
    ErtsProcSysTaskQs *dgc_tsk_qs;
    ASSERT(c_p == erts_get_current_process());
    ASSERT((ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)
	   & erts_atomic32_read_nob(&c_p->state));
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    if (!enable) {
	c_p->flags |= F_DISABLE_GC;
	return 0;
    }

    c_p->flags &= ~F_DISABLE_GC;

    dgc_tsk_qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(c_p);
    if (!dgc_tsk_qs)
	return 0;

    /* Move delayed gc tasks into sys tasks queues. */

    erts_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

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

	    erts_atomic32_read_bset_nob(&c_p->state,
                                        (ERTS_PSFLG_DELAYED_SYS
                                         | ERTS_PSFLG_ACTIVE_SYS
                                         | ERTS_PSFLG_SYS_TASKS),
                                        (ERTS_PSFLG_ACTIVE_SYS
                                         | ERTS_PSFLG_SYS_TASKS));

#ifdef DEBUG
	ASSERT(state & ERTS_PSFLG_DELAYED_SYS);
	qmask = c_p->sys_task_qs->qmask;
	aprio = ERTS_PSFLGS_GET_ACT_PRIO(state);
	ASSERT(ERTS_PSFLGS_GET_USR_PRIO(state) >= aprio);
	ASSERT((qmask & -qmask) >= (1 << aprio));
    }
#endif

    erts_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

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
	erts_thr_progress_block();
	erts_sched_stat.enabled = 1;
	erts_thr_progress_unblock();
	break;
    case ERTS_SCHED_STAT_MODIFY_DISABLE:
	erts_thr_progress_block();
	erts_sched_stat.enabled = 0;
	erts_thr_progress_unblock();
	break;
    case ERTS_SCHED_STAT_MODIFY_CLEAR:
	erts_spin_lock(&erts_sched_stat.lock);
	for (ix = 0; ix < ERTS_NO_PRIO_LEVELS; ix++) {
	    erts_sched_stat.prio[ix].total_executed = 0;
	    erts_sched_stat.prio[ix].executed = 0;
	    erts_sched_stat.prio[ix].total_migrated = 0;
	    erts_sched_stat.prio[ix].migrated = 0;
	}
	erts_spin_unlock(&erts_sched_stat.lock);
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

    erts_spin_lock(&erts_sched_stat.lock);
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
    erts_spin_unlock(&erts_sched_stat.lock);

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
    ErtsMigrationPaths *mpaths = erts_get_migration_paths();

    if (!mpaths)
	rq = ERTS_RUNQ_IX(0);
    else {
	ErtsRunQueue *erq = mpaths->mpath[rq->ix].misc_evac_runq;
	if (erq)
	    rq = erq;
    }

    erts_runq_lock(rq);

    molp->next = NULL;
    molp->func = func;
    molp->arg = arg;
    if (rq->misc.end)
	rq->misc.end->next = molp;
    else
	rq->misc.start = molp;
    rq->misc.end = molp;

    non_empty_runq(rq);

    ERTS_RUNQ_FLGS_SET_NOB(rq, ERTS_RUNQ_FLG_MISC_OP);

    erts_runq_unlock(rq);

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

    erts_runq_unlock(rq);

    while (molp) {
	tmp_molp = molp;
	(*molp->func)(molp->arg);
	molp = molp->next;
	misc_op_list_free(tmp_molp);
    }

    erts_runq_lock(rq);
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

                               erts_no_run_queues + ERTS_NUM_DIRTY_RUNQS,

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
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    /*
     * Wait for other schedulers to schedule out their processes
     * and update 'reductions'.
     */
    erts_thr_progress_block();
    for (reds = 0, ix = 0; ix < erts_no_run_queues; ix++)
	reds += ERTS_RUNQ_IX(ix)->procs.reductions;
    if (redsp)
	*redsp = reds;
    if (diffp)
	*diffp = reds - last_exact_reductions;
    last_exact_reductions = reds;
    erts_thr_progress_unblock();
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
}

static void delete_process(Process* p);

void
erts_free_proc(Process *p)
{
    erts_proc_lock_fin(p);
    ASSERT(erts_atomic32_read_nob(&p->state) & ERTS_PSFLG_FREE);
    ASSERT(0 == erts_proc_read_refc(p));
    if (p->flags & F_DELAYED_DEL_PROC)
	delete_process(p);
    erts_free(ERTS_ALC_T_PROC, (void *) p);
}

typedef struct {
    Process *proc;
    erts_aint32_t state;
    ErtsRunQueue *run_queue;
    int bound;
} ErtsEarlyProcInit;

static void early_init_process_struct(void *varg, Eterm data)
{
    ErtsEarlyProcInit *arg = (ErtsEarlyProcInit *) varg;
    Process *proc = arg->proc;

    proc->common.id = make_internal_pid(data);
    erts_atomic32_init_nob(&proc->dirty_state, 0);
    proc->dirty_sys_tasks = NULL;
    erts_init_runq_proc(proc, arg->run_queue, arg->bound);
    erts_atomic32_init_relb(&proc->state, arg->state);

    erts_proc_lock_init(proc); /* All locks locked */

}

/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(ErtsRunQueue *rq, int bound, erts_aint32_t state)
{
    ErtsEarlyProcInit init_arg;
    Process *p;

    p = erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;

    ASSERT(rq);

    init_arg.proc = (Process *) p;
    init_arg.state = state;
    init_arg.run_queue = rq;
    init_arg.bound = bound;

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
    int bound = 0;
    Uint flags = 0, qs_flags = 0;
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

    erts_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);

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
            bound = !0;
	}
	prio = (erts_aint32_t) so->priority;
    }

    state |= (((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_ACT_PRIO_OFFSET)
	      | ((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_USR_PRIO_OFFSET));

    if (so->flags & SPO_OFF_HEAP_MSGQ) {
	state |= ERTS_PSFLG_OFF_HEAP_MSGQ;
	qs_flags |= FS_OFF_HEAP_MSGQ;
    }
    else if (so->flags & SPO_ON_HEAP_MSGQ) {
	qs_flags |= FS_ON_HEAP_MSGQ;
    }

    ASSERT((qs_flags & FS_ON_HEAP_MSGQ) || (qs_flags & FS_OFF_HEAP_MSGQ));

    if (!rq)
	rq = erts_get_runq_proc(parent, NULL);

    p = alloc_process(rq, bound, state); /* All proc locks are locked by this thread
                                            on success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

#ifdef SHCOPY_SPAWN
    arg_size = copy_shared_calculate(args, &info);
#else
    arg_size = size_object_litopt(args, &litarea);
#endif
    heap_need = arg_size;

    p->flags = flags;
    p->sig_qs.flags = qs_flags;

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
	p->max_gen_gcs    = (Uint16) erts_atomic32_read_nob(&erts_max_gen_gcs);
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
    ERTS_P_LT_MONITORS(p) = NULL;

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

    p->sig_qs.first = NULL;
    p->sig_qs.last = &p->sig_qs.first;
    p->sig_qs.cont = NULL;
    p->sig_qs.cont_last = &p->sig_qs.cont;
    p->sig_qs.save = &p->sig_qs.first;
    p->sig_qs.saved_last = NULL;
    p->sig_qs.len = 0;
    p->sig_qs.nmsigs.next = NULL;
    p->sig_qs.nmsigs.last = NULL;
    p->sig_inq.first = NULL;
    p->sig_inq.last = &p->sig_inq.first;
    p->sig_inq.len = 0;
    p->sig_inq.nmsigs.next = NULL;
    p->sig_inq.nmsigs.last = NULL;
#ifdef ERTS_PROC_SIG_HARD_DEBUG
    p->sig_inq.may_contain_heap_terms = 0;
#endif
    p->bif_timers = NULL;
    p->mbuf = NULL;
    p->msg_frag = NULL;
    p->mbuf_sz = 0;
    erts_atomic_init_nob(&p->psd, (erts_aint_t) NULL);
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

    p->trace_msg_q = NULL;
    p->scheduler_data = NULL;

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
            erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_proc_unlock(parent, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
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
            erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
            erts_proc_unlock(parent, ERTS_PROC_LOCK_STATUS|ERTS_PROC_LOCK_TRACE);
        }
        trace_proc_spawn(p, am_spawned, parent->common.id, mod, func, args);
        if (so->flags & SPO_LINK)
            trace_proc(p, locks, p, am_getting_linked, parent->common.id);
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
        ErtsLink *lnk;
        ErtsLinkData *ldp = erts_link_create(ERTS_LNK_TYPE_PROC,
                                             parent->common.id,
                                             p->common.id);
        lnk = erts_link_tree_lookup_insert(&ERTS_P_LINKS(parent), &ldp->a);
        if (lnk) {
            /*
             * This should more or less never happen, but could
             * potentially happen if pid:s wrap...
             */
            erts_link_release(lnk);
        }
        erts_link_tree_insert(&ERTS_P_LINKS(p), &ldp->b);
    }

    /*
     * Test whether this process should be initially monitored by its parent.
     */
    if (so->flags & SPO_MONITOR) {
	Eterm mref = erts_make_ref(parent);
        ErtsMonitorData *mdp = erts_monitor_create(ERTS_MON_TYPE_PROC,
                                                   mref,
                                                   parent->common.id,
                                                   p->common.id,
                                                   NIL);
        erts_monitor_tree_insert(&ERTS_P_MONITORS(parent), &mdp->origin);
        erts_monitor_list_insert(&ERTS_P_LT_MONITORS(p), &mdp->target);
	so->mref = mref;
    }

    erts_proc_unlock(p, locks);

    res = p->common.id;

    /*
     * Schedule process for execution.
     */

    erts_proc_unlock(parent, locks & ERTS_PROC_LOCKS_ALL_MINOR);

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

    erts_proc_unlock(parent, locks & ERTS_PROC_LOCKS_ALL_MINOR);

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
    erts_atomic_init_nob(&p->psd, (erts_aint_t) NULL);
    ERTS_P_MONITORS(p) = NULL;
    ERTS_P_LT_MONITORS(p) = NULL;
    ERTS_P_LINKS(p) = NULL;         /* List of links */
    p->sig_qs.first = NULL;
    p->sig_qs.last = &p->sig_qs.first;
    p->sig_qs.cont = NULL;
    p->sig_qs.cont_last = &p->sig_qs.cont;
    p->sig_qs.save = &p->sig_qs.first;
    p->sig_qs.saved_last = NULL;
    p->sig_qs.len = 0;
    p->sig_qs.nmsigs.next = NULL;
    p->sig_qs.nmsigs.last = NULL;
    p->sig_inq.first = NULL;
    p->sig_inq.last = &p->sig_inq.first;
    p->sig_inq.len = 0;
    p->sig_inq.nmsigs.next = NULL;
    p->sig_inq.nmsigs.last = NULL;
#ifdef ERTS_PROC_SIG_HARD_DEBUG
    p->sig_inq.may_contain_heap_terms = 0;
#endif
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
    p->static_flags = 0;

    p->common.u.alive.started_interval = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
#endif

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    erts_atomic32_init_nob(&p->dirty_state, 0);
    p->dirty_sys_tasks = NULL;
    erts_atomic32_init_nob(&p->state, (erts_aint32_t) PRIORITY_NORMAL);

    p->scheduler_data = NULL;
    erts_proc_lock_init(p);
    erts_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    erts_init_runq_proc(p, ERTS_RUNQ_IX(0), 0);

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
    ASSERT(ERTS_P_LT_MONITORS(p) == NULL);
    ASSERT(ERTS_P_LINKS(p) == NULL);
    ASSERT(p->sig_qs.first == NULL);
    ASSERT(p->sig_qs.len == 0);
    ASSERT(p->bif_timers == NULL);
    ASSERT(p->dictionary == NULL);
    ASSERT(p->catches == 0);
    ASSERT(p->cp == NULL);
    ASSERT(p->i == NULL);
    ASSERT(p->current == NULL);

    ASSERT(p->parent == NIL);

    ASSERT(p->sig_inq.first == NULL);
    ASSERT(p->sig_inq.len == 0);

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
    erts_proc_lock_fin(p);
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
    Uint32 block_rla_ref = (Uint32) (Uint) p->u.terminate;

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

    psd = (ErtsPSD *) erts_atomic_read_nob(&p->psd);

    if (psd) {
	erts_atomic_set_nob(&p->psd, (erts_aint_t) NULL); /* Reduction counting depends on this... */
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
    erts_cleanup_messages(p->sig_qs.first);
    p->sig_qs.first = NULL;
    erts_cleanup_messages(p->sig_qs.cont);
    p->sig_qs.cont = NULL;

    p->fvalue = NIL;

    if (block_rla_ref)
        erts_unblock_release_literal_area(block_rla_ref);
}

static ERTS_INLINE void
set_self_exiting(Process *c_p, Eterm reason, int *enqueue,
                 erts_aint32_t *prio, erts_aint32_t *state)
{
    erts_aint32_t st, enq_prio = -1;
    int enq;

    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCKS_ALL);

    c_p->fvalue = reason;

    st = erts_atomic32_read_nob(&c_p->state);
    ASSERT(enqueue || (st & (ERTS_PSFLG_RUNNING
                             |ERTS_PSFLG_RUNNING_SYS
                             | ERTS_PSFLG_DIRTY_RUNNING
                             | ERTS_PSFLG_DIRTY_RUNNING_SYS)));

    enq = change_proc_schedule_state(c_p,
                                     (ERTS_PSFLG_SUSPENDED
                                      | ERTS_PSFLGS_DIRTY_WORK),
                                     ERTS_PSFLG_EXITING|ERTS_PSFLG_ACTIVE,
                                     &st,
                                     &enq_prio,
                                     ERTS_PROC_LOCKS_ALL);

    ASSERT(enqueue || !enq);
    if (enqueue)
        *enqueue = enq;
    if (prio)
        *prio = enq_prio;
    if (state)
        *state = st;
}

void
erts_set_self_exiting(Process *c_p, Eterm reason)
{
    int enqueue;
    erts_aint32_t enq_prio, state;
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    set_self_exiting(c_p, reason, &enqueue, &enq_prio, &state);
    c_p->freason = EXTAG_EXIT;
    KILL_CATCHES(c_p);
    c_p->i = (BeamInstr *) beam_exit;

    /* Always active when exiting... */
    ASSERT(state & ERTS_PSFLG_ACTIVE);

    /*
     * If we are terminating a process that currently
     * is executing on a dirty scheduler. It *should*
     * be scheduled on a normal scheduler...
     */
    ASSERT(!(state & (ERTS_PSFLG_DIRTY_RUNNING
                      | ERTS_PSFLG_DIRTY_RUNNING_SYS))
           || enqueue == ERTS_ENQUEUE_NORMAL_QUEUE
           || enqueue == -ERTS_ENQUEUE_NORMAL_QUEUE);

    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    if (enqueue)
        add2runq(enqueue, enq_prio, c_p, state, NULL);
}

static int
erts_proc_exit_handle_dist_monitor(ErtsMonitor *mon, void *vctxt, Sint reds)
{
    ErtsProcExitContext *ctxt = (ErtsProcExitContext *) vctxt;
    Process *c_p = ctxt->c_p;
    Eterm reason = ctxt->reason;
    int code;
    ErtsDSigSendContext ctx;
    ErtsMonLnkDist *dist;
    DistEntry *dep;
    Eterm watcher, ref, *hp;
    ErtsMonitorData *mdp = NULL;
    Eterm watched;
    Uint watcher_sz, ref_sz;
    ErtsHeapFactory factory;
    Sint reds_consumed = 0;

    ASSERT(c_p->flags & F_DISABLE_GC);
    ASSERT(erts_monitor_is_target(mon) && mon->type == ERTS_MON_TYPE_DIST_PROC);
    ASSERT(ctxt->dist_state == NIL);
    ASSERT(!ctxt->yield);

    mdp = erts_monitor_to_data(mon);

    if (mon->flags & ERTS_ML_FLG_NAME)
        watched = ((ErtsMonitorDataExtended *) mdp)->u.name;
    else
        watched = c_p->common.id;
    ASSERT(is_internal_pid(watched) || is_atom(watched));

    watcher = mon->other.item;
    ASSERT(is_external_pid(watcher));
    dep = external_pid_dist_entry(watcher);
    ASSERT(dep);
    dist = ((ErtsMonitorDataExtended *) mdp)->dist;
    ASSERT(dist);

    code = erts_dsig_prepare(&ctx, dep, c_p, ERTS_PROC_LOCK_MAIN,
                             ERTS_DSP_NO_LOCK, 0, 0, 1);

    ctx.reds = (Sint) (reds * TERM_TO_BINARY_LOOP_FACTOR);

    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
        break;
    case ERTS_DSIG_PREP_PENDING:
    case ERTS_DSIG_PREP_CONNECTED:
        if (dist->connection_id != ctx.connection_id)
            break;
        erts_factory_proc_init(&factory, c_p);
        watcher_sz = size_object(watcher);
        hp = erts_produce_heap(&factory, watcher_sz, 0);
        watcher = copy_struct(watcher, watcher_sz, &hp, factory.off_heap);
        ref_sz = size_object(mdp->ref);
        hp = erts_produce_heap(&factory, ref_sz, 0);
        ref = copy_struct(mdp->ref, ref_sz, &hp, factory.off_heap);
        erts_factory_close(&factory);
        code = erts_dsig_send_m_exit(&ctx,
                                     watcher,
                                     watched,
                                     ref,
                                     reason);
        reds_consumed = reds - (ctx.reds / TERM_TO_BINARY_LOOP_FACTOR);
        switch (code) {
        case ERTS_DSIG_SEND_YIELD:
            reds_consumed = reds; /* force yield */
            ctxt->yield = 1;
            break;
        case ERTS_DSIG_SEND_CONTINUE:
            ctxt->dist_state = erts_dsend_export_trap_context(c_p, &ctx);
            reds_consumed = reds; /* force yield */
            ctxt->yield = 1;
            break;
        case ERTS_DSIG_SEND_OK:
            break;
        case ERTS_DSIG_SEND_TOO_LRG:
            erts_kill_dist_connection(dep, dist->connection_id);
            break;
        default:
            ASSERT(! "Invalid dsig send exit monitor result");
            break;
        }
        break;
    default:
        ASSERT(! "Invalid dsig prep exit monitor result");
        break;
    }
    if (!erts_monitor_dist_delete(&mdp->origin))
        erts_monitor_release(mon);
    else
        erts_monitor_release_both(mdp);
    return reds_consumed;
}

int
erts_proc_exit_handle_monitor(ErtsMonitor *mon, void *vctxt, Sint reds)
{
    ErtsProcExitContext *ctxt = (ErtsProcExitContext *) vctxt;
    Process *c_p = ctxt->c_p;
    Eterm reason = ctxt->reason;
    ErtsMonitorData *mdp = NULL;
    int res = 1;

    if (erts_monitor_is_target(mon)) {
        /* We are being watched... */
        switch (mon->type) {
        case ERTS_MON_TYPE_SUSPEND:
        case ERTS_MON_TYPE_PROC:
            erts_proc_sig_send_monitor_down(mon, reason);
            mon = NULL;
            break;
        case ERTS_MON_TYPE_PORT: {
	    Port *prt;
            ASSERT(is_internal_port(mon->other.item));
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
            prt = erts_id2port(mon->other.item);
	    if (prt) {
                erts_fire_port_monitor(prt, mon);
                erts_port_release(prt);
                mon = NULL;
            }
            erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
            break;
        }
        case ERTS_MON_TYPE_RESOURCE:
            erts_fire_nif_monitor(mon);
            mon = NULL;
            break;
        case ERTS_MON_TYPE_DIST_PROC: {
            ErtsMonLnkDist *dist;
            DistEntry *dep;
            ErtsDSigSendContext ctx;
            int code;
            Eterm watcher;
            Eterm watched;

            if (is_immed(reason)) {
                mdp = erts_monitor_to_data(mon);

                if (mon->flags & ERTS_ML_FLG_NAME)
                    watched = ((ErtsMonitorDataExtended *) mdp)->u.name;
                else
                    watched = c_p->common.id;
                ASSERT(is_internal_pid(watched) || is_atom(watched));

                watcher = mon->other.item;
                ASSERT(is_external_pid(watcher));
                dep = external_pid_dist_entry(watcher);
                ASSERT(dep);
                dist = ((ErtsMonitorDataExtended *) mdp)->dist;
                ASSERT(dist);
                code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                         ERTS_DSP_NO_LOCK, 1, 1, 0);
                switch (code) {
                case ERTS_DSIG_PREP_CONNECTED:
                case ERTS_DSIG_PREP_PENDING:
                    if (dist->connection_id == ctx.connection_id) {
                        code = erts_dsig_send_m_exit(&ctx,
                                                     watcher,
                                                     watched,
                                                     mdp->ref,
                                                     reason);
                        ASSERT(code == ERTS_DSIG_SEND_OK);
                    }
                default:
                    break;
                }
                if (!erts_monitor_dist_delete(&mdp->origin))
                    mdp = NULL;
            } else {
                erts_monitor_tree_insert(&ctxt->dist_monitors, mon);
                return 1;
            }
            break;
        }
        default:
            ERTS_INTERNAL_ERROR("Invalid target monitor type");
            break;
        }
    }
    else { /* Origin monitor */
        /* We are watching someone else... */
        switch (mon->type) {
        case ERTS_MON_TYPE_SUSPEND:
        case ERTS_MON_TYPE_PROC:
            erts_proc_sig_send_demonitor(mon);
            mon = NULL;
            break;
        case ERTS_MON_TYPE_TIME_OFFSET:
            erts_demonitor_time_offset(mon);
            mon = NULL;
            break;
        case ERTS_MON_TYPE_NODE:
            mdp = erts_monitor_to_data(mon);
            if (!erts_monitor_dist_delete(&mdp->target))
                mdp = NULL;
            break;
        case ERTS_MON_TYPE_NODES:
            erts_monitor_nodes_delete(mon);
            mon = NULL;
            break;
        case ERTS_MON_TYPE_PORT: {
            Port *prt;
            ASSERT(is_internal_port(mon->other.item));
            prt = erts_port_lookup_raw(mon->other.item);
            if (prt) {
                if (erts_port_demonitor(c_p, prt, mon) != ERTS_PORT_OP_DROPPED)
                    mon = NULL;
            }
            break;
        }
        case ERTS_MON_TYPE_DIST_PROC: {
            ErtsMonLnkDist *dist;
            DistEntry *dep;
            ErtsDSigSendContext ctx;
            int code;
            Eterm watched;

            mdp = erts_monitor_to_data(mon);
            dist = ((ErtsMonitorDataExtended *) mdp)->dist;
            ASSERT(dist);
            if (mon->flags & ERTS_ML_FLG_NAME) {
                watched = ((ErtsMonitorDataExtended *) mdp)->u.name;
                ASSERT(is_atom(watched));
                dep = erts_sysname_to_connected_dist_entry(dist->nodename);
            }
            else {
                watched = mon->other.item;
                ASSERT(is_external_pid(watched));
		dep = external_pid_dist_entry(watched);
            }
            code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                     ERTS_DSP_NO_LOCK, 1, 1, 0);
            switch (code) {
            case ERTS_DSIG_PREP_CONNECTED:
            case ERTS_DSIG_PREP_PENDING:
                if (dist->connection_id == ctx.connection_id) {
                    code = erts_dsig_send_demonitor(&ctx,
                                                    c_p->common.id,
                                                    watched,
                                                    mdp->ref);
                    ASSERT(code == ERTS_DSIG_SEND_OK);
                }
            default:
                break;
            }
            if (!erts_monitor_dist_delete(&mdp->target))
                mdp = NULL;
            res = 100;
            break;
        }
        default:
            ERTS_INTERNAL_ERROR("Invalid origin monitor type");
            break;
        }
    }

    if (mdp)
        erts_monitor_release_both(mdp);
    else if (mon)
        erts_monitor_release(mon);
    return res;
}

static int
erts_proc_exit_handle_dist_link(ErtsLink *lnk, void *vctxt, Sint reds)
{
    ErtsProcExitContext *ctxt = (ErtsProcExitContext *) vctxt;
    Process *c_p = ctxt->c_p;
    Eterm reason = ctxt->reason, item, *hp;
    Uint item_sz;
    int code;
    ErtsDSigSendContext ctx;
    ErtsMonLnkDist *dist;
    DistEntry *dep;
    ErtsLink *dlnk;
    ErtsLinkData *ldp = NULL;
    ErtsHeapFactory factory;
    Sint reds_consumed = 0;

    ASSERT(c_p->flags & F_DISABLE_GC);
    ASSERT(lnk->type == ERTS_LNK_TYPE_DIST_PROC);
    ASSERT(ctxt->dist_state == NIL);
    ASSERT(!ctxt->yield);

    dlnk = erts_link_to_other(lnk, &ldp);
    dist = ((ErtsLinkDataExtended *) ldp)->dist;

    ASSERT(is_external_pid(lnk->other.item));
    dep = external_pid_dist_entry(lnk->other.item);

    ASSERT(dep != erts_this_dist_entry);

    if (!erts_link_dist_delete(dlnk))
        ldp = NULL;

    code = erts_dsig_prepare(&ctx, dep, c_p, ERTS_PROC_LOCK_MAIN,
                             ERTS_DSP_NO_LOCK, 0, 0, 0);

    ctx.reds = (Sint) (reds * TERM_TO_BINARY_LOOP_FACTOR);

    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
        break;
    case ERTS_DSIG_PREP_PENDING:
    case ERTS_DSIG_PREP_CONNECTED:
        if (dist->connection_id != ctx.connection_id)
            break;
        erts_factory_proc_init(&factory, c_p);
        item_sz = size_object(lnk->other.item);
        hp = erts_produce_heap(&factory, item_sz, 0);
        item = copy_struct(lnk->other.item, item_sz, &hp, factory.off_heap);
        erts_factory_close(&factory);
        code = erts_dsig_send_exit_tt(&ctx,
                                      c_p->common.id,
                                      item,
                                      reason,
                                      SEQ_TRACE_TOKEN(c_p));
        reds_consumed = reds - (ctx.reds / TERM_TO_BINARY_LOOP_FACTOR);
        switch (code) {
        case ERTS_DSIG_SEND_YIELD:
            reds_consumed = reds; /* force yield */
            ctxt->yield = 1;
            break;
        case ERTS_DSIG_SEND_CONTINUE:
            ctxt->dist_state = erts_dsend_export_trap_context(c_p, &ctx);
            reds_consumed = reds; /* force yield */
            ctxt->yield = 1;
            break;
        case ERTS_DSIG_SEND_OK:
            break;
        case ERTS_DSIG_SEND_TOO_LRG:
            erts_kill_dist_connection(dep, dist->connection_id);
            break;
        default:
            ASSERT(! "Invalid dsig send exit monitor result");
            break;
        }
        break;
    default:
        ASSERT(! "Invalid dsig prep exit monitor result");
        break;
    }
    if (ldp)
        erts_link_release_both(ldp);
    else if (lnk)
        erts_link_release(lnk);
    return reds_consumed;
}

int
erts_proc_exit_handle_link(ErtsLink *lnk, void *vctxt, Sint reds)
{
    ErtsProcExitContext *ctxt = (ErtsProcExitContext *) vctxt;
    Process *c_p = ((ErtsProcExitContext *) vctxt)->c_p;
    Eterm reason = ((ErtsProcExitContext *) vctxt)->reason;
    ErtsLinkData *ldp = NULL;

    switch (lnk->type) {
    case ERTS_LNK_TYPE_PROC:
        ASSERT(is_internal_pid(lnk->other.item));
        erts_proc_sig_send_link_exit(c_p, c_p->common.id, lnk,
                                     reason, SEQ_TRACE_TOKEN(c_p));
        lnk = NULL;
        break;
    case ERTS_LNK_TYPE_PORT: {
        Port *prt;
        ASSERT(is_internal_port(lnk->other.item));
        prt = erts_port_lookup(lnk->other.item,
                               ERTS_PORT_SFLGS_INVALID_LOOKUP);
        if (prt)
            erts_port_exit(NULL,
                           (ERTS_PORT_SIG_FLG_FORCE_SCHED
                            | ERTS_PORT_SIG_FLG_BROKEN_LINK),
                           prt,
                           c_p->common.id,
                           reason,
                           NULL);
        break;
    }
    case ERTS_LNK_TYPE_DIST_PROC: {
        DistEntry *dep;
        ErtsMonLnkDist *dist;
        ErtsLink *dlnk;
        ErtsDSigSendContext ctx;
        int code;

        if (is_immed(reason)) {
            dlnk = erts_link_to_other(lnk, &ldp);
            dist = ((ErtsLinkDataExtended *) ldp)->dist;

            ASSERT(is_external_pid(lnk->other.item));
            dep = external_pid_dist_entry(lnk->other.item);

            ASSERT(dep != erts_this_dist_entry);

            if (!erts_link_dist_delete(dlnk))
                ldp = NULL;

            code = erts_dsig_prepare(&ctx, dep, c_p, 0, ERTS_DSP_NO_LOCK, 1, 1, 0);
            switch (code) {
            case ERTS_DSIG_PREP_CONNECTED:
            case ERTS_DSIG_PREP_PENDING:
                if (dist->connection_id == ctx.connection_id) {
                    code = erts_dsig_send_exit_tt(&ctx,
                                                  c_p->common.id,
                                                  lnk->other.item,
                                                  reason,
                                                  SEQ_TRACE_TOKEN(c_p));
                    ASSERT(code == ERTS_DSIG_SEND_OK);
                }
                break;
            default:
                break;
            }
        } else {
            erts_link_tree_insert(&ctxt->dist_links, lnk);
            return 1;
        }
        break;
    }
    default:
        ERTS_INTERNAL_ERROR("Unexpected link type");
        break;
    }

    if (ldp)
        erts_link_release_both(ldp);
    else if (lnk)
        erts_link_release(lnk);
    return 1;
}

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
erts_do_exit_process(Process* p, Eterm reason)
{
    p->arity = 0;		/* No live registers */

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

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    /* By locking all locks (main lock is already locked) when going
       to exiting state (ERTS_PSFLG_EXITING), it is enough to take any lock when
       looking up a process (erts_pid2proc()) to prevent the looked up
       process from exiting until the lock has been released. */
    erts_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);

    set_self_exiting(p, reason, NULL, NULL, NULL);

    if (IS_TRACED_FL(p, F_TRACE_CALLS))
        erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_EXITING);

    erts_trace_check_exiting(p->common.id);

    ASSERT((ERTS_TRACE_FLAGS(p) & F_INITIAL_TRACE_FLAGS)
	   == F_INITIAL_TRACE_FLAGS);

    ASSERT(erts_proc_read_refc(p) > 0);
    if (ERTS_PTMR_IS_SET(p)) {
	erts_cancel_proc_timer(p);
	ASSERT(erts_proc_read_refc(p) > 0);
    }

    erts_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);

    if (IS_TRACED_FL(p, F_TRACE_PROCS))
        trace_proc(p, ERTS_PROC_LOCK_MAIN, p, am_exit, reason);

    /*
     * p->u.initial of this process can *not* be used anymore;
     * will be overwritten by misc termination data.
     */
    p->u.terminate = NULL;

    BUMP_REDS(p, 100);

    erts_continue_exit_process(p);
}

enum continue_exit_phase {
    ERTS_CONTINUE_EXIT_TIMERS,
    ERTS_CONTINUE_EXIT_BLCKD_MSHED,
    ERTS_CONTINUE_EXIT_BLCKD_NMSHED,
    ERTS_CONTINUE_EXIT_USING_DB,
    ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS,
    ERTS_CONTINUE_EXIT_FREE,
    ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS_AFTER,
    ERTS_CONTINUE_EXIT_LINKS,
    ERTS_CONTINUE_EXIT_MONITORS,
    ERTS_CONTINUE_EXIT_LT_MONITORS,
    ERTS_CONTINUE_EXIT_HANDLE_PROC_SIG,
    ERTS_CONTINUE_EXIT_DIST_LINKS,
    ERTS_CONTINUE_EXIT_DIST_MONITORS,
    ERTS_CONTINUE_EXIT_DONE,
};

struct continue_exit_state {
    enum continue_exit_phase phase;
    ErtsLink *links;
    ErtsMonitor *monitors;
    ErtsMonitor *lt_monitors;
    Eterm reason;
    ErtsProcExitContext pectxt;
    DistEntry *dep;
    void *yield_state;
    Uint32 block_rla_ref;
};

void
erts_continue_exit_process(Process *p)
{
    struct continue_exit_state static_state, *trap_state = &static_state;
    ErtsProcLocks curr_locks = ERTS_PROC_LOCK_MAIN;
    erts_aint32_t state;
    int delay_del_proc = 0;
    Sint reds = ERTS_BIF_REDS_LEFT(p);
#ifdef DEBUG
    int yield_allowed = 1;
#endif

    if (p->u.terminate) {
        trap_state = p->u.terminate;
        /* Re-set the reason as it may have been gc:ed */
        trap_state->reason = p->fvalue;
    } else {
        trap_state->phase = ERTS_CONTINUE_EXIT_TIMERS;
        trap_state->reason = p->fvalue;
        trap_state->dep = NULL;
        trap_state->yield_state = NULL;
        trap_state->block_rla_ref = 0;
    }

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

    ASSERT(ERTS_PROC_IS_EXITING(p));

    ASSERT(erts_proc_read_refc(p) > 0);
restart:
    switch (trap_state->phase) {
    case ERTS_CONTINUE_EXIT_TIMERS:
        if (p->bif_timers) {
            reds = erts_cancel_bif_timers(p, &p->bif_timers, &trap_state->yield_state, reds);
            if (reds <= 0) goto yield;
            p->bif_timers = NULL;
        }

        if (p->flags & F_SCHDLR_ONLN_WAITQ) {
            abort_sched_onln_chng_waitq(p);
            reds -= 100;
        }

        trap_state->phase = ERTS_CONTINUE_EXIT_BLCKD_MSHED;
        if (reds <= 0) goto yield;
    case ERTS_CONTINUE_EXIT_BLCKD_MSHED:

        if (p->flags & F_HAVE_BLCKD_MSCHED) {
            ErtsSchedSuspendResult ssr;
            ssr = erts_block_multi_scheduling(p, ERTS_PROC_LOCK_MAIN, 0, 0, 1);
            switch (ssr) {
            case ERTS_SCHDLR_SSPND_DONE:
            case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
            case ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED:
                p->flags &= ~F_HAVE_BLCKD_MSCHED;
                break;
            default:
                erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error: %d\n",
                          __FILE__, __LINE__, (int) ssr);
            }
            reds -= 100;
        }

        trap_state->phase = ERTS_CONTINUE_EXIT_BLCKD_NMSHED;
        if (reds <= 0) goto yield;
    case ERTS_CONTINUE_EXIT_BLCKD_NMSHED:

        if (p->flags & F_HAVE_BLCKD_NMSCHED) {
            ErtsSchedSuspendResult ssr;
            ssr = erts_block_multi_scheduling(p, ERTS_PROC_LOCK_MAIN, 0, 1, 1);
            switch (ssr) {
            case ERTS_SCHDLR_SSPND_DONE:
            case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
            case ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED:
                p->flags &= ~F_HAVE_BLCKD_MSCHED;
                break;
            default:
                erts_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error: %d\n",
                          __FILE__, __LINE__, (int) ssr);
            }
            reds -= 100;
        }

        trap_state->yield_state = NULL;
        trap_state->phase = ERTS_CONTINUE_EXIT_USING_DB;
        if (reds <= 0) goto yield;
    case ERTS_CONTINUE_EXIT_USING_DB:

        if (p->flags & F_USING_DB) {
            if (erts_db_process_exiting(p, ERTS_PROC_LOCK_MAIN, &trap_state->yield_state))
                goto yield;
            p->flags &= ~F_USING_DB;
        }

        trap_state->phase = ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS;
    case ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS:

        state = erts_atomic32_read_acqb(&p->state);
        /*
         * If we might access any literals on the heap after this point,
         * we need to block release of literal areas. After this point,
         * since cleanup of sys-tasks reply to copy-literals requests.
         * Note that we do not only have to prevent release of
         * currently processed literal area, but also future processed
         * literal areas, until we are guaranteed not to access any
         * literal areas at all.
         *
         * - A non-immediate exit reason may refer to literals.
         * - A process executing dirty while terminated, might access
         *   any term on the heap, and therfore literals, until it has
         *   stopped executing dirty.
         */
        if (!trap_state->block_rla_ref
            && (is_not_immed(trap_state->reason)
                || (state & (ERTS_PSFLG_DIRTY_RUNNING
                             | ERTS_PSFLG_DIRTY_RUNNING_SYS)))) {
            Uint32 block_rla_ref = erts_block_release_literal_area();
            ASSERT(block_rla_ref);
            trap_state->block_rla_ref = block_rla_ref;
        }
    
        /* We enable GC again as it can produce more sys-tasks */
        erts_set_gc_state(p, 1);
        state = erts_atomic32_read_acqb(&p->state);
        if ((state & ERTS_PSFLG_SYS_TASKS) || p->dirty_sys_tasks) {
            reds -= cleanup_sys_tasks(p, state, reds);
            if (reds <= 0) goto yield;
        }

        trap_state->phase = ERTS_CONTINUE_EXIT_FREE;
    case ERTS_CONTINUE_EXIT_FREE:

#ifdef DEBUG
        erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
        ASSERT(ERTS_PROC_GET_DELAYED_GC_TASK_QS(p) == NULL);
        ASSERT(p->dirty_sys_tasks == NULL);
        erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
#endif

        if (p->flags & F_USING_DDLL) {
            erts_ddll_proc_dead(p, ERTS_PROC_LOCK_MAIN);
            p->flags &= ~F_USING_DDLL;
        }

        /*
         * The registered name *should* be the last "erlang resource" to
         * cleanup.
         */
        if (p->common.u.alive.reg) {
            (void) erts_unregister_name(p, ERTS_PROC_LOCK_MAIN, NULL, THE_NON_VALUE);
            ASSERT(!p->common.u.alive.reg);
        }

        erts_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
        curr_locks = ERTS_PROC_LOCKS_ALL;

        /*
         * Note! The monitor and link fields will be overwritten 
         * by erts_ptab_delete_element() below.
         */
        trap_state->links = ERTS_P_LINKS(p);
        trap_state->monitors = ERTS_P_MONITORS(p);
        trap_state->lt_monitors = ERTS_P_LT_MONITORS(p);

        {
            /* Do *not* use erts_get_runq_proc() */
            ErtsRunQueue *rq;
            rq = erts_get_runq_current(erts_proc_sched_data(p));

            erts_runq_lock(rq);

            ASSERT(p->scheduler_data);
            ASSERT(p->scheduler_data->current_process == p);
            ASSERT(p->scheduler_data->free_process == NULL);

            /* Time of death! */
            erts_ptab_delete_element(&erts_proc, &p->common);

            erts_runq_unlock(rq);
        }

        /*
         * All "erlang resources" have to be deallocated before this point,
         * e.g. registered name, so monitoring and linked processes can
         * be sure that all interesting resources have been deallocated
         * when the monitors and/or links hit.
         */

        /* notify free */
        erts_atomic32_read_bor_relb(&p->state, ERTS_PSFLG_FREE);

        trap_state->dep = ((p->flags & F_DISTRIBUTION)
                      ? ERTS_PROC_SET_DIST_ENTRY(p, NULL)
                      : NULL);

        reds -= 50;

        erts_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
        curr_locks = ERTS_PROC_LOCK_MAIN;
        trap_state->phase = ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS_AFTER;
    case ERTS_CONTINUE_EXIT_CLEAN_SYS_TASKS_AFTER:
        /*
         * It might show up signal prio elevation tasks until we
         * have entered free state. Cleanup such tasks now.
         */

        state = erts_atomic32_read_acqb(&p->state);
        if ((state & ERTS_PSFLG_SYS_TASKS) || p->dirty_sys_tasks) {
            reds -= cleanup_sys_tasks(p, state, reds);
            if (reds <= 0) goto yield;
        }

        /* Needs to be unlocked for erts_do_net_exits to work?!? */
        // erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);

#ifdef DEBUG
        erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
        ASSERT(p->sys_task_qs == NULL);
        erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
#endif

        if (trap_state->dep) {
            erts_do_net_exits(trap_state->dep,
                              (trap_state->reason == am_kill) ? am_killed : trap_state->reason);
            erts_deref_dist_entry(trap_state->dep);
        }

        /* Disable GC so that reason does not get moved */
        erts_set_gc_state(p, 0);

        trap_state->pectxt.c_p = p;
        trap_state->pectxt.reason = trap_state->reason;
        trap_state->pectxt.dist_links = NULL;
        trap_state->pectxt.dist_monitors = NULL;
        trap_state->pectxt.dist_state = NIL;
        trap_state->pectxt.yield = 0;

        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);

        erts_proc_sig_fetch(p);

        erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);

        trap_state->yield_state = NULL;
        trap_state->phase = ERTS_CONTINUE_EXIT_LINKS;
        if (reds <= 0) goto yield;
    case ERTS_CONTINUE_EXIT_LINKS:

        reds = erts_link_tree_foreach_delete_yielding(
            &trap_state->links,
            erts_proc_exit_handle_link,
            (void *) &trap_state->pectxt,
            &trap_state->yield_state,
            reds);
        if (reds <= 0)
            goto yield;

        ASSERT(!trap_state->links);
        trap_state->yield_state = NULL;
        trap_state->phase = ERTS_CONTINUE_EXIT_MONITORS;
    case ERTS_CONTINUE_EXIT_MONITORS:

    reds = erts_monitor_tree_foreach_delete_yielding(
            &trap_state->monitors,
            erts_proc_exit_handle_monitor,
            (void *) &trap_state->pectxt,
            &trap_state->yield_state,
            reds);
        if (reds <= 0)
            goto yield;

        ASSERT(!trap_state->monitors);
        trap_state->yield_state = NULL;
        trap_state->phase = ERTS_CONTINUE_EXIT_LT_MONITORS;
    case ERTS_CONTINUE_EXIT_LT_MONITORS:

        reds = erts_monitor_list_foreach_delete_yielding(
            &trap_state->lt_monitors,
            erts_proc_exit_handle_monitor,
            (void *) &trap_state->pectxt,
            &trap_state->yield_state,
            reds);
        if (reds <= 0)
            goto yield;

        ASSERT(!trap_state->lt_monitors);
        trap_state->phase = ERTS_CONTINUE_EXIT_HANDLE_PROC_SIG;
    case ERTS_CONTINUE_EXIT_HANDLE_PROC_SIG: {
        Sint r = reds;

        if (!erts_proc_sig_handle_exit(p, &r))
            goto yield;

        reds -= r;

        trap_state->phase = ERTS_CONTINUE_EXIT_DIST_LINKS;
    }
    case ERTS_CONTINUE_EXIT_DIST_LINKS: {

        continue_dist_send:
        if (is_not_nil(trap_state->pectxt.dist_state)) {
            Binary* bin = erts_magic_ref2bin(trap_state->pectxt.dist_state);
            ErtsDSigSendContext* ctx = (ErtsDSigSendContext*) ERTS_MAGIC_BIN_DATA(bin);
            Sint initial_reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
            int result;

            ctx->reds = initial_reds;
            result = erts_dsig_send(ctx);

            /* erts_dsig_send bumps reductions on the process in the ctx */
            reds = ERTS_BIF_REDS_LEFT(p);

            switch (result) {
            case ERTS_DSIG_SEND_OK:
                break;
            case ERTS_DSIG_SEND_TOO_LRG: /*SEND_SYSTEM_LIMIT*/
                erts_kill_dist_connection(ctx->dep, ctx->connection_id);
                break;
            case ERTS_DSIG_SEND_YIELD: /*SEND_YIELD_RETURN*/
                trap_state->pectxt.dist_state = NIL;
                goto yield;
            case ERTS_DSIG_SEND_CONTINUE: { /*SEND_YIELD_CONTINUE*/
                goto yield;
            }
            }

            trap_state->pectxt.dist_state = NIL;
            if (reds <= 0)
                goto yield;
            goto restart;
        }

        reds = erts_link_tree_foreach_delete_yielding(
            &trap_state->pectxt.dist_links,
            erts_proc_exit_handle_dist_link,
            (void *) &trap_state->pectxt,
            &trap_state->yield_state,
            reds);
        if (reds <= 0 || trap_state->pectxt.yield)
            goto yield;
        trap_state->phase = ERTS_CONTINUE_EXIT_DIST_MONITORS;
    }
    case ERTS_CONTINUE_EXIT_DIST_MONITORS: {

        if (is_not_nil(trap_state->pectxt.dist_state))
            goto continue_dist_send;

        reds = erts_monitor_tree_foreach_delete_yielding(
            &trap_state->pectxt.dist_monitors,
            erts_proc_exit_handle_dist_monitor,
            (void *) &trap_state->pectxt,
            &trap_state->yield_state,
            reds);
        if (reds <= 0 || trap_state->pectxt.yield)
            goto yield;

        trap_state->phase = ERTS_CONTINUE_EXIT_DONE;
    }
    case ERTS_CONTINUE_EXIT_DONE: {
        erts_aint_t state;
        /*
         * From this point on we are no longer allowed to yield
         * this process.
         */
#ifdef DEBUG
        yield_allowed = 0;
#endif

        /* Enable GC again, through strictly not needed it puts
           the process in a consistent state. */
        erts_set_gc_state(p, 1);

        /* Set state to not active as we don't want this process
           to be scheduled in again after this. */
        {
            /* Inactivate */
            erts_aint32_t n, e, a = erts_atomic32_read_nob(&p->state);
            int refc_inced = 0;
            while (1) {
                n = e = a;
                ASSERT(a & ERTS_PSFLG_FREE);
                n &= ~(ERTS_PSFLG_ACTIVE
                       | ERTS_PSFLG_ACTIVE_SYS
                       | ERTS_PSFLG_DIRTY_ACTIVE_SYS);
                if ((n & ERTS_PSFLG_IN_RUNQ) && !refc_inced) {
                    /*
                     * Happens when we have been scheduled via
                     * a proxy-proc struct.
                     *
                     * Corresponding decrement in erts_schedule()
                     * right after "if (!run_process)".
                     */
                    erts_proc_inc_refc(p);
                    refc_inced = 1;
                }
                a = erts_atomic32_cmpxchg_mb(&p->state, n, e);
                if (a == e) {
                    state = n;
                    break;
                }
            }

            if (refc_inced && !(n & ERTS_PSFLG_IN_RUNQ))
                erts_proc_dec_refc(p);
        }
        
        ASSERT(p->scheduler_data);
        ASSERT(p->scheduler_data->current_process == p);
        ASSERT(p->scheduler_data->free_process == NULL);

        p->scheduler_data->current_process = NULL;
        p->scheduler_data->free_process = p;

        if (state & (ERTS_PSFLG_DIRTY_RUNNING
                     | ERTS_PSFLG_DIRTY_RUNNING_SYS)) {
            p->flags |= F_DELAYED_DEL_PROC;
            delay_del_proc = 1;
            /*
             * The dirty scheduler decrease refc
             * when done with the process...
             */
        }

        erts_schedule_thr_prgr_later_cleanup_op(
            (void (*)(void*))erts_proc_dec_refc,
            (void *) &p->common,
            &p->common.u.release,
            sizeof(Process));

        break;
    }
    }

    /* block_rla_ref needed by delete_process() */
    p->u.terminate = (void *) (Uint) trap_state->block_rla_ref;
    
    if (trap_state != &static_state)
        erts_free(ERTS_ALC_T_CONT_EXIT_TRAP, trap_state);

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

    if (IS_TRACED_FL(p, F_TRACE_SCHED_EXIT))
        trace_sched(p, curr_locks, am_out_exited);

    erts_flush_trace_messages(p, ERTS_PROC_LOCK_MAIN);

    ERTS_TRACER_CLEAR(&ERTS_TRACER(p));

    if (!delay_del_proc)
	delete_process(p);

    return;

 yield:

    ASSERT(yield_allowed);

    ERTS_LC_ASSERT(curr_locks == erts_proc_lc_my_proc_locks(p));
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & curr_locks);
    ASSERT(erts_proc_read_refc(p) > 0);

    if (trap_state == &static_state) {
        trap_state = erts_alloc(ERTS_ALC_T_CONT_EXIT_TRAP, sizeof(*trap_state));
        sys_memcpy(trap_state, &static_state, sizeof(*trap_state));
        p->u.terminate = trap_state;
    }
    trap_state->pectxt.yield = 0;

    ASSERT(p->scheduler_data);
    ASSERT(p->scheduler_data->current_process == p);
    ASSERT(p->scheduler_data->free_process == NULL);

    if (trap_state->phase >= ERTS_CONTINUE_EXIT_FREE) {
        p->scheduler_data->current_process = NULL;
        p->scheduler_data->free_process = p;
    }

    p->i = (BeamInstr *) beam_continue_exit;

    /* Why is this lock take??? */
    if (!(curr_locks & ERTS_PROC_LOCK_STATUS)) {
	erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	curr_locks |= ERTS_PROC_LOCK_STATUS;
    }

    if (curr_locks != ERTS_PROC_LOCK_MAIN)
	erts_proc_unlock(p, ~ERTS_PROC_LOCK_MAIN & curr_locks);

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

    BUMP_ALL_REDS(p);
}

Process *
erts_try_lock_sig_free_proc(Eterm pid, ErtsProcLocks locks,
                            erts_aint32_t *statep)
{
    Process *rp = erts_proc_lookup_raw(pid);
    erts_aint32_t fail_state = ERTS_PSFLG_SIG_IN_Q|ERTS_PSFLG_SIG_Q;
    erts_aint32_t state;
    ErtsProcLocks tmp_locks = ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_MSGQ;

    tmp_locks |= locks;
    if (statep)
        fail_state |= *statep;

    if (!rp) {
        if (statep)
            *statep = ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE;
        return NULL;
    }

    ERTS_LC_ASSERT(!erts_proc_lc_my_proc_locks(rp));

    state = erts_atomic32_read_nob(&rp->state);
    if (statep)
        *statep = state;

    if (state & ERTS_PSFLG_FREE)
        return NULL;

    if (state & fail_state)
        return ERTS_PROC_LOCK_BUSY;

    if (erts_proc_trylock(rp, tmp_locks) == EBUSY)
        return ERTS_PROC_LOCK_BUSY;

    state = erts_atomic32_read_nob(&rp->state);
    if (statep)
        *statep = state;

    if ((state & fail_state)
        || rp->sig_inq.first
        || rp->sig_qs.cont) {
        erts_proc_unlock(rp, tmp_locks);
        if (state & ERTS_PSFLG_FREE)
            return NULL;
        else
            return ERTS_PROC_LOCK_BUSY;
    }

    if (tmp_locks != locks)
        erts_proc_unlock(rp, tmp_locks & ~locks);

    return rp;
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
    state = erts_atomic32_read_acqb(&p->state);
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

static void print_current_process_info(fmtfn_t, void *to_arg, ErtsSchedulerData*);

/*
 * Print scheduler information
 */
void
erts_print_scheduler_info(fmtfn_t to, void *to_arg, ErtsSchedulerData *esdp)
{
    int i;
    erts_aint32_t flg;

    switch (esdp->type) {
    case ERTS_SCHED_NORMAL:
        erts_print(to, to_arg, "=scheduler:%u\n", esdp->no);
        break;
    case ERTS_SCHED_DIRTY_CPU:
        erts_print(to, to_arg, "=dirty_cpu_scheduler:%u\n",
                   (esdp->dirty_no + erts_no_schedulers));
        break;
    case ERTS_SCHED_DIRTY_IO:
        erts_print(to, to_arg, "=dirty_io_scheduler:%u\n",
                   (esdp->dirty_no + erts_no_schedulers + erts_no_dirty_cpu_schedulers));
        break;
    default:
        erts_print(to, to_arg, "=unknown_scheduler_type:%u\n", esdp->type);
        break;
    }

    flg = erts_atomic32_read_dirty(&esdp->ssi->flags);
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

    if (esdp->type == ERTS_SCHED_NORMAL) {
        erts_print(to, to_arg, "Current Port: ");
        if (esdp->current_port)
            erts_print(to, to_arg, "%T", esdp->current_port->common.id);
        erts_print(to, to_arg, "\n");

        erts_print_run_queue_info(to, to_arg, esdp->run_queue);
    }

    /* This *MUST* to be the last information in scheduler block */
    print_current_process_info(to, to_arg, esdp);
}

void erts_print_run_queue_info(fmtfn_t to, void *to_arg,
                               ErtsRunQueue *run_queue)
{
    erts_aint32_t flg;
    int i;

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
                   erts_atomic32_read_dirty(&run_queue->procs.prio_info[i].len));
    }
    erts_print(to, to_arg, "Run Queue Port Length: %d\n",
               erts_atomic32_read_dirty(&run_queue->ports.info.len));

    flg = erts_atomic32_read_dirty(&run_queue->flags);
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
}


static void print_current_process_info(fmtfn_t to, void *to_arg,
                                       ErtsSchedulerData* esdp)
{
    Process *p = esdp->current_process;
    erts_aint32_t flg;

    erts_print(to, to_arg, "Current Process: ");
    if (esdp->current_process && !(ERTS_TRACE_FLAGS(p) & F_SENSITIVE)) {
	flg = erts_atomic32_read_dirty(&p->state);
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
    if (-1 == erts_atomic32_cmpxchg_acqb(&erts_halt_progress,
					     erts_no_schedulers,
					     -1)) {
        notify_reap_ports_relb();
        ERTS_RUNQ_FLGS_SET(ERTS_DIRTY_CPU_RUNQ, ERTS_RUNQ_FLG_HALTING);
        ERTS_RUNQ_FLGS_SET(ERTS_DIRTY_IO_RUNQ, ERTS_RUNQ_FLG_HALTING);
	erts_halt_code = code;
    }
}

#if defined(ERTS_ENABLE_LOCK_CHECK)
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

void
erts_debug_later_op_foreach(void (*callback)(void*),
                            void (*func)(void *, ErtsThrPrgrVal, void *),
                            void *arg)
{
    int six;
    if (!erts_thr_progress_is_blocking())
	ERTS_INTERNAL_ERROR("Not blocking thread progress");

    for (six = 0; six < erts_no_schedulers; six++) {
        ErtsSchedulerData *esdp = &erts_aligned_scheduler_data[six].esd;
	ErtsThrPrgrLaterOp *lop = esdp->aux_work_data.later_op.first;

        while (lop) {
            if (lop->func == callback)
                func(arg, lop->later, lop->data);
            lop = lop->next;
        }
    }
}

void
erts_debug_free_process_foreach(void (*func)(Process *, void *), void *arg)
{
    ErtsRunQueue *rq;
    int ix, prio;
    for (ix = 0; ix < erts_no_run_queues; ix++) {
        rq = ERTS_RUNQ_IX(ix);
        for (prio = PRIORITY_MAX; prio < PRIORITY_LOW; prio++) {
            Process *p = rq->procs.prio[prio].first;
            for (; p; p = p->next) {
                if (ERTS_PSFLG_FREE & erts_atomic32_read_nob(&p->state))
                    (*func)(p, arg);
            }
        }
    }
}

void
erts_debug_proc_monitor_link_foreach(Process *proc,
                                     int (*monitor_func)(ErtsMonitor *, void *, Sint ),
                                     int (*link_func)(ErtsLink *, void *, Sint ),
                                     void *arg)
{
    if (!(erts_atomic32_read_nob(&proc->state) & ERTS_PSFLG_FREE)) {
        /* For all links */
        erts_link_tree_foreach(ERTS_P_LINKS(proc),
                               link_func,
                               arg);
        /* For all monitors */
        erts_monitor_tree_foreach(ERTS_P_MONITORS(proc),
                                  monitor_func,
                                  arg);
        /* For all local target monitors */
        erts_monitor_list_foreach(ERTS_P_LT_MONITORS(proc),
                                  monitor_func,
                                  arg);
    }
    else {
        struct continue_exit_state *ce_state = proc->u.terminate;

        /* For all links */
        if (ce_state->phase == ERTS_CONTINUE_EXIT_LINKS)
            erts_debug_link_tree_destroying_foreach(ce_state->links,
                                                    link_func,
                                                    arg,
                                                    ce_state->yield_state);
        else
            erts_link_tree_foreach(ce_state->links,
                                   link_func,
                                   arg);

        /* For all monitors */
        if (ce_state->phase == ERTS_CONTINUE_EXIT_MONITORS)
            erts_debug_monitor_tree_destroying_foreach(ce_state->monitors,
                                                       monitor_func,
                                                       arg,
                                                       ce_state->yield_state);
        else
            erts_monitor_tree_foreach(ce_state->monitors,
                                      monitor_func,
                                      arg);

        /* For all local target monitors */
        if (ce_state->phase == ERTS_CONTINUE_EXIT_LT_MONITORS)
            erts_debug_monitor_list_destroying_foreach(ce_state->lt_monitors,
                                                       monitor_func,
                                                       arg,
                                                       ce_state->yield_state);
        else
            erts_monitor_list_foreach(ce_state->lt_monitors,
                                      monitor_func,
                                      arg);

    }
}
