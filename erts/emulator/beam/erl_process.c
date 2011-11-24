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

#define ERL_PROCESS_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_nmgc.h"
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

#define ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED (2000*CONTEXT_REDS)
#define ERTS_RUNQ_CALL_CHECK_BALANCE_REDS \
  (ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED/2)

#define ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST (CONTEXT_REDS/10)

#define ERTS_SCHED_SPIN_UNTIL_YIELD 100

#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT 10
#define ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT 1000
#define ERTS_SCHED_TSE_SLEEP_SPINCOUNT \
  (ERTS_SCHED_SYS_SLEEP_SPINCOUNT*ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT)
#define ERTS_SCHED_SUSPEND_SLEEP_SPINCOUNT 0

#define ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH (200*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_HIGH (50*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_MEDIUM (10*CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_LOW (CONTEXT_REDS)
#define ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW (CONTEXT_REDS/10)

#define ERTS_WAKEUP_OTHER_DEC 10
#define ERTS_WAKEUP_OTHER_FIXED_INC (CONTEXT_REDS/10)

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

#define ERTS_MAYBE_SAVE_TERMINATING_PROCESS(P)			\
do {								\
    ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&proc_tab_mtx));	\
    if (saved_term_procs.end)					\
	save_terminating_process((P));				\
} while (0)

#define ERTS_EMPTY_RUNQ(RQ) \
  ((RQ)->len == 0 && (RQ)->misc.start == NULL)

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];

static Sint p_last;
static Sint p_next;
static Sint p_serial;
static Uint p_serial_mask;
static Uint p_serial_shift;

int erts_sched_compact_load;
Uint erts_no_schedulers;
Uint erts_max_processes = ERTS_DEFAULT_MAX_PROCESSES;
Uint erts_process_tab_index_mask;

static int wakeup_other_limit;

int erts_sched_thread_suggested_stack_size = -1;

#ifdef ERTS_ENABLE_LOCK_CHECK
ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];
#endif

#ifdef ERTS_SMP
int erts_disable_proc_not_running_opt;

static ErtsAuxWorkData *aux_thread_aux_work_data;

#define ERTS_SCHDLR_SSPND_CHNG_WAITER		(((erts_aint32_t) 1) << 0)
#define ERTS_SCHDLR_SSPND_CHNG_MSB		(((erts_aint32_t) 1) << 1)
#define ERTS_SCHDLR_SSPND_CHNG_ONLN		(((erts_aint32_t) 1) << 2)

#ifndef DEBUG

#define ERTS_SCHDLR_SSPND_CHNG_SET(VAL, OLD_VAL) \
  erts_smp_atomic32_set_nob(&schdlr_sspnd.changing, (VAL))

#else

#define ERTS_SCHDLR_SSPND_CHNG_SET(VAL, OLD_VAL)			\
do {									\
    erts_aint32_t old_val__;						\
    old_val__ = erts_smp_atomic32_xchg_nob(&schdlr_sspnd.changing,     	\
					   (VAL));			\
    ASSERT(old_val__ == (OLD_VAL));					\
} while (0)

#endif


static struct {
    erts_smp_mtx_t mtx;
    erts_smp_cnd_t cnd;
    int online;
    int curr_online;
    int wait_curr_online;
    erts_smp_atomic32_t changing;
    erts_smp_atomic32_t active;
    struct {
	int ongoing;
	long wait_active;
	ErtsProcList *procs;
    } msb; /* Multi Scheduling Block */
} schdlr_sspnd;

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
	int max_len;
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

ErtsRunQueue *erts_common_run_queue;

#ifdef USE_THREADS
static erts_tsd_key_t sched_data_key;
#endif

static erts_smp_mtx_t proc_tab_mtx;

static erts_smp_atomic32_t function_calls;

#ifdef ERTS_SMP
static erts_smp_atomic32_t doing_sys_schedule;
static erts_smp_atomic32_t no_empty_run_queues;
#else /* !ERTS_SMP */
ErtsSchedulerData *erts_scheduler_data;
#endif

ErtsAlignedRunQueue *erts_aligned_run_queues;
Uint erts_no_run_queues;

ErtsAlignedSchedulerData *erts_aligned_scheduler_data;

typedef union {
    ErtsSchedulerSleepInfo ssi;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerSleepInfo))];
} ErtsAlignedSchedulerSleepInfo;

static ErtsAlignedSchedulerSleepInfo *aligned_sched_sleep_info;

#ifndef BM_COUNTERS
static int processes_busy;
#endif

Process**  process_tab;
static Uint last_reductions;
static Uint last_exact_reductions;
Uint erts_default_process_flags;
Eterm erts_system_monitor;
Eterm erts_system_monitor_msg_queue_len;
Eterm erts_system_monitor_long_gc;
Eterm erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t erts_system_monitor_flags;

/* system performance monitor */
Eterm erts_system_profile;
struct erts_system_profile_flags_t erts_system_profile_flags;

#ifdef HYBRID
Uint erts_num_active_procs;
Process** erts_active_procs;
#endif

#if ERTS_MAX_PROCESSES > 0x7fffffff
#error "Need to store process_count in another type"
#endif
static erts_smp_atomic32_t process_count;

typedef struct ErtsTermProcElement_ ErtsTermProcElement;
struct ErtsTermProcElement_ {
    ErtsTermProcElement *next;
    ErtsTermProcElement *prev;
    int ix;
    union {
	struct {
	    Eterm pid;
	    SysTimeval spawned;
	    SysTimeval exited;
	} process;
	struct {
	    SysTimeval time;
	} bif_invocation;
    } u;
};

static struct {
    ErtsTermProcElement *start;
    ErtsTermProcElement *end;
} saved_term_procs;

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(misc_op_list,
				 ErtsMiscOpList,
				 10,
				 ERTS_ALC_T_MISC_OP_LIST)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(proclist,
				 ErtsProcList,
				 200,
				 ERTS_ALC_T_PROC_LIST)

#define ERTS_SCHED_SLEEP_INFO_IX(IX)					\
    (ASSERT_EXPR(-1 <= ((int) (IX))					\
		 && ((int) (IX)) < ((int) erts_no_schedulers)),		\
     &aligned_sched_sleep_info[(IX)].ssi)

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
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&schdlr_sspnd.mtx));	\
    for (ix__ = 0; ix__ < schdlr_sspnd.online; ix__++) {		\
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

static void init_processes_bif(void);
static void save_terminating_process(Process *p);
static void exec_misc_ops(ErtsRunQueue *);
static void print_function_from_pc(int to, void *to_arg, BeamInstr* x);
static int stack_element_dump(int to, void *to_arg, Process* p, Eterm* sp,
			      int yreg);

static void aux_work_timeout(void *unused);
static void aux_work_timeout_early_init(int no_schedulers);
static void aux_work_timeout_late_init(void);
static void setup_aux_work_timer(void);

#if defined(DEBUG) || 0
#define ERTS_DBG_CHK_AUX_WORK_VAL(V) dbg_chk_aux_work_val((V))
static void
dbg_chk_aux_work_val(erts_aint32_t value)
{
    erts_aint32_t valid = 0;

#ifdef ERTS_SSI_AUX_WORK_SET_TMO
    valid |= ERTS_SSI_AUX_WORK_SET_TMO;
#endif
#ifdef ERTS_SSI_AUX_WORK_CHECK_CHILDREN
    valid |= ERTS_SSI_AUX_WORK_CHECK_CHILDREN;
#endif
#ifdef ERTS_SSI_AUX_WORK_MISC
    valid |= ERTS_SSI_AUX_WORK_MISC;
#endif
#ifdef ERTS_SSI_AUX_WORK_MISC_THR_PRGR
    valid |= ERTS_SSI_AUX_WORK_MISC_THR_PRGR;
#endif
#ifdef ERTS_SSI_AUX_WORK_ASYNC_READY
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY;
#endif
#ifdef ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN
    valid |= ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN;
#endif

#ifdef ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
    valid |= ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM;
#endif
#ifdef ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC
    valid |= ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC;
#endif
#ifdef ERTS_SSI_AUX_WORK_DD
    valid |= ERTS_SSI_AUX_WORK_DD;
#endif
#ifdef ERTS_SSI_AUX_WORK_DD
    valid |= ERTS_SSI_AUX_WORK_DD_THR_PRGR;
#endif
#ifdef ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK
    valid |= ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK;
#endif

    if (~valid & value)
	erl_exit(ERTS_ABORT_EXIT,
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
static void handle_pending_exiters(ErtsProcList *);

#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_smp_lc_runq_is_locked(ErtsRunQueue *runq)
{
    return erts_smp_lc_mtx_is_locked(&runq->mtx);
}
#endif

void
erts_pre_init_process(void)
{
#ifdef USE_THREADS
    erts_tsd_key_create(&sched_data_key);
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
 {
     int ix;

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

     erts_psd_required_locks[ERTS_PSD_DIST_ENTRY].get_locks
	 = ERTS_PSD_DIST_ENTRY_GET_LOCKS;
     erts_psd_required_locks[ERTS_PSD_DIST_ENTRY].set_locks
	 = ERTS_PSD_DIST_ENTRY_SET_LOCKS;

     erts_psd_required_locks[ERTS_PSD_CALL_TIME_BP].get_locks
	 = ERTS_PSD_CALL_TIME_BP_GET_LOCKS;
     erts_psd_required_locks[ERTS_PSD_CALL_TIME_BP].set_locks
	 = ERTS_PSD_CALL_TIME_BP_SET_LOCKS;

     /* Check that we have locks for all entries */
     for (ix = 0; ix < ERTS_PSD_SIZE; ix++) {
	 ERTS_SMP_LC_ASSERT(erts_psd_required_locks[ix].get_locks);
	 ERTS_SMP_LC_ASSERT(erts_psd_required_locks[ix].set_locks);
     }
 }
#endif
}

/* initialize the scheduler */
void
erts_init_process(int ncpu)
{
    Uint proc_bits = ERTS_PROC_BITS;

#ifdef ERTS_SMP
    erts_disable_proc_not_running_opt = 0;
    erts_init_proc_lock(ncpu);
#endif

    init_proclist_alloc();

    erts_smp_atomic32_init_nob(&process_count, 0);

    if (erts_use_r9_pids_ports) {
	proc_bits = ERTS_R9_PROC_BITS;
	ASSERT(erts_max_processes <= (1 << ERTS_R9_PROC_BITS));
    }

    process_tab = (Process**) erts_alloc(ERTS_ALC_T_PROC_TABLE,
					 erts_max_processes*sizeof(Process*));
    sys_memzero(process_tab, erts_max_processes * sizeof(Process*));
#ifdef HYBRID
    erts_active_procs = (Process**)
        erts_alloc(ERTS_ALC_T_ACTIVE_PROCS,
                   erts_max_processes * sizeof(Process*));
    erts_num_active_procs = 0;
#endif

    erts_smp_mtx_init(&proc_tab_mtx, "proc_tab");
    p_last = -1;
    p_next = 0;
    p_serial = 0;

    p_serial_shift = erts_fit_in_bits(erts_max_processes - 1);
    p_serial_mask = ((~(~((Uint) 0) << proc_bits)) >> p_serial_shift);
    erts_process_tab_index_mask = ~(~((Uint) 0) << p_serial_shift);
#ifndef BM_COUNTERS
    processes_busy = 0;
#endif
    last_reductions = 0;
    last_exact_reductions = 0;
    erts_default_process_flags = 0;
}

void
erts_late_init_process(void)
{
    int ix;
    init_processes_bif();

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

static ERTS_INLINE ErtsProcList *
proclist_create(Process *p)
{
    ErtsProcList *plp = proclist_alloc();
    plp->pid = p->id;
    plp->started = p->started;
    return plp;
}

static ERTS_INLINE void
proclist_destroy(ErtsProcList *plp)
{
    proclist_free(plp);
}

static ERTS_INLINE int
proclist_same(ErtsProcList *plp, Process *p)
{
    return (plp->pid == p->id
	    && erts_cmp_timeval(&plp->started, &p->started) == 0);
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

int
erts_proclist_same(ErtsProcList *plp, Process *p)
{
    return proclist_same(plp, p);
}

void *
erts_psd_set_init(Process *p, ErtsProcLocks plocks, int ix, void *data)
{
    void *old;
    ErtsProcLocks xplocks;
    int refc = 0;
    ErtsPSD *psd = erts_alloc(ERTS_ALC_T_PSD, sizeof(ErtsPSD));
    int i;
    for (i = 0; i < ERTS_PSD_SIZE; i++)
	psd->data[i] = NULL;

    ERTS_SMP_LC_ASSERT(plocks);
    ERTS_SMP_LC_ASSERT(plocks == erts_proc_lc_my_proc_locks(p));

    xplocks = ERTS_PROC_LOCKS_ALL;
    xplocks &= ~plocks;
    if (xplocks && erts_smp_proc_trylock(p, xplocks) == EBUSY) {
	if (xplocks & ERTS_PROC_LOCK_MAIN) {
	    erts_smp_proc_inc_refc(p);
	    erts_smp_proc_unlock(p, plocks);
	    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL);
	    refc = 1;
	}
	else {
	    if (plocks & ERTS_PROC_LOCKS_ALL_MINOR)
		erts_smp_proc_unlock(p, plocks & ERTS_PROC_LOCKS_ALL_MINOR);
	    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }
    if (!p->psd)
	p->psd = psd;
    if (xplocks)
	erts_smp_proc_unlock(p, xplocks);
    if (refc)
	erts_smp_proc_dec_refc(p);
    ASSERT(p->psd);
    if (p->psd != psd)
	erts_free(ERTS_ALC_T_PSD, psd);
    old = p->psd->data[ix];
    p->psd->data[ix] = data;
    ERTS_SMP_LC_ASSERT(plocks == erts_proc_lc_my_proc_locks(p));
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
	erl_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error\n",
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
    if ((old_flgs & flgs) == 0) {

	old_flgs = erts_atomic32_read_bor_nob(&ssi->aux_work, flgs);

	if ((old_flgs & flgs) == 0) {
#ifdef ERTS_SMP
	    erts_sched_poke(ssi);
#else
	    erts_sys_schedule_interrupt(1);
#endif
	}
    }
}

#if 0 /* Currently not used */

static ERTS_INLINE void
set_aux_work_flags_wakeup_relb(ErtsSchedulerSleepInfo *ssi,
			       erts_aint32_t flgs)
{
    erts_aint32_t old_flgs;

    ERTS_DBG_CHK_SSI_AUX_WORK(ssi);

    old_flgs = erts_atomic32_read_bor_relb(&ssi->aux_work, flgs);

    if ((old_flgs & flgs) == 0) {
#ifdef ERTS_SMP
	erts_sched_poke(ssi);
#else
	erts_sys_schedule_interrupt(1);
#endif
    }
}

#endif

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
#ifdef ERTS_SMP
    case ERTS_THR_Q_NEED_THR_PRGR:
	set_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MISC_THR_PRGR);
	erts_thr_progress_wakeup(awdp->esdp,
				 erts_thr_q_need_thr_progress(q));
#endif
    case ERTS_THR_Q_CLEAN:
	break;
    }
    return aux_work;
}

static erts_aint32_t
handle_misc_aux_work(ErtsAuxWorkData *awdp,
		     erts_aint32_t aux_work)
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

static erts_aint32_t
handle_misc_aux_work_thr_prgr(ErtsAuxWorkData *awdp,
			      erts_aint32_t aux_work)
{
    if (!erts_thr_progress_has_reached(awdp->misc.thr_prgr))
	return aux_work;

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

static erts_aint32_t
handle_async_ready(ErtsAuxWorkData *awdp,
		   erts_aint32_t aux_work)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
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

static erts_aint32_t
handle_async_ready_clean(ErtsAuxWorkData *awdp,
			 erts_aint32_t aux_work)
{
    void *thr_prgr_p;

#ifdef ERTS_SMP
    if (awdp->async_ready.need_thr_prgr
	&& !erts_thr_progress_has_reached(awdp->misc.thr_prgr)) {
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
	erts_thr_progress_wakeup(awdp->esdp,
				 awdp->async_ready.thr_prgr);
	awdp->async_ready.need_thr_prgr = 1;
#endif
    default:
	return aux_work;
    }
}

#endif

static erts_aint32_t
handle_fix_alloc(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    erts_aint32_t res;

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
    set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(ix-1),
				  ERTS_SSI_AUX_WORK_DD);
}

static erts_aint32_t
handle_delayed_dealloc(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    ErtsSchedulerSleepInfo *ssi = awdp->ssi;
    int need_thr_progress = 0;
    int more_work = 0;

    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD);
    erts_alloc_scheduler_handle_delayed_dealloc((void *) awdp->esdp,
						&need_thr_progress,
						&more_work);
    if (more_work) {
	if (set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD)
	    & ERTS_SSI_AUX_WORK_DD_THR_PRGR) {
	    unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	    aux_work &= ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;
	}
	return aux_work;
    }

    if (need_thr_progress) {
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	awdp->dd.thr_prgr = erts_thr_progress_later();
	erts_thr_progress_wakeup(awdp->esdp, awdp->dd.thr_prgr);
    }
    else if (awdp->dd.completed_callback) {
	awdp->dd.completed_callback(awdp->dd.completed_arg);
	awdp->dd.completed_callback = NULL;
	awdp->dd.completed_arg = NULL;
    }
    return aux_work & ~ERTS_SSI_AUX_WORK_DD;
}

static erts_aint32_t
handle_delayed_dealloc_thr_prgr(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    ErtsSchedulerSleepInfo *ssi;
    int need_thr_progress;
    int more_work;

    if (!erts_thr_progress_has_reached(awdp->dd.thr_prgr))
	return aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;

    ssi = awdp->ssi;
    need_thr_progress = 0;
    more_work = 0;

    erts_alloc_scheduler_handle_delayed_dealloc((void *) awdp->esdp,
						&need_thr_progress,
						&more_work);
    if (more_work) {
	set_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD);
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	return ((aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR)
		| ERTS_SSI_AUX_WORK_DD);
    }

    if (need_thr_progress) {
	awdp->dd.thr_prgr = erts_thr_progress_later();
	erts_thr_progress_wakeup(awdp->esdp, awdp->dd.thr_prgr);
    }
    else {
	unset_aux_work_flags(ssi, ERTS_SSI_AUX_WORK_DD_THR_PRGR);
	if (awdp->dd.completed_callback) {
	    awdp->dd.completed_callback(awdp->dd.completed_arg);
	    awdp->dd.completed_callback = NULL;
	    awdp->dd.completed_arg = NULL;
	}
    }

    return aux_work & ~ERTS_SSI_AUX_WORK_DD_THR_PRGR;
}

static erts_atomic32_t completed_dealloc_count;

static void
completed_dealloc(void *vproc)
{
    if (erts_atomic32_dec_read_mb(&completed_dealloc_count) == 0) {
	erts_resume((Process *) vproc, (ErtsProcLocks) 0);
	erts_smp_proc_dec_refc((Process *) vproc);
    }
}

static void
setup_completed_dealloc(void *vproc)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsAuxWorkData *awdp = (esdp
			     ? &esdp->aux_work_data
			     : aux_thread_aux_work_data);
    erts_alloc_fix_alloc_shrink(awdp->sched_id, 0);
    set_aux_work_flags_wakeup_nob(awdp->ssi, ERTS_SSI_AUX_WORK_DD);
    awdp->dd.completed_callback = completed_dealloc;
    awdp->dd.completed_arg = vproc;
}

static void
prep_setup_completed_dealloc(void *vproc)
{
    erts_aint32_t count = (erts_aint32_t) (erts_no_schedulers+1);
    if (erts_atomic32_dec_read_mb(&completed_dealloc_count) == count) {
	/* scheduler threads */
	erts_schedule_multi_misc_aux_work(0,
					  erts_no_schedulers,
					  setup_completed_dealloc,
					  vproc);
	/* aux_thread */
	erts_schedule_misc_aux_work(0,
				    setup_completed_dealloc,
				    vproc);
    }
}

#endif /* ERTS_SMP */

int
erts_debug_wait_deallocations(Process *c_p)
{
#ifndef ERTS_SMP
    erts_alloc_fix_alloc_shrink(1, 0);
    return 1;
#else
    /* Only one process at a time can do this */
    erts_aint32_t count = (erts_aint32_t) (2*(erts_no_schedulers+1));
    if (0 == erts_atomic32_cmpxchg_mb(&completed_dealloc_count,
				      count,
				      0)) {
	erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
	erts_smp_proc_inc_refc(c_p);
	/* scheduler threads */
	erts_schedule_multi_misc_aux_work(0,
					  erts_no_schedulers,
					  prep_setup_completed_dealloc,
					  (void *) c_p);
	/* aux_thread */
	erts_schedule_misc_aux_work(0,
				    prep_setup_completed_dealloc,
				    (void *) c_p);
	return 1;
    }
    return 0;
#endif
}


#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
void
erts_smp_notify_check_children_needed(void)
{
    int i;
    for (i = 0; i < erts_no_schedulers; i++)
	set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(i),
				      ERTS_SSI_AUX_WORK_CHECK_CHILDREN);
}

static erts_aint32_t
handle_check_children(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_CHECK_CHILDREN);
    erts_check_children();
    return aux_work & ~ERTS_SSI_AUX_WORK_CHECK_CHILDREN;
}

#endif

#ifdef ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK

static erts_aint32_t
handle_mseg_cache_check(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK);
    erts_mseg_cache_check();
    return aux_work & ~ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK;
}

#endif

static erts_aint32_t
handle_setup_aux_work_timer(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    unset_aux_work_flags(awdp->ssi, ERTS_SSI_AUX_WORK_SET_TMO);
    setup_aux_work_timer();
    return aux_work & ~ERTS_SSI_AUX_WORK_SET_TMO;
}

static ERTS_INLINE erts_aint32_t
handle_aux_work(ErtsAuxWorkData *awdp, erts_aint32_t aux_work)
{
    /*
     * Handlers are *only* allowed to modify flags in return value
     * and ssi flags that are explicity handled by the handler.
     * Handlers are, e.g., not allowed to read the ssi flag field and
     * then unconditionally return that value.
     */
    ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    if (aux_work & ERTS_SSI_AUX_WORK_SET_TMO) {
	aux_work = handle_setup_aux_work_timer(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#ifdef ERTS_SMP
    if (aux_work & ERTS_SSI_AUX_WORK_MISC_THR_PRGR) {
	aux_work = handle_misc_aux_work_thr_prgr(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#endif
    if (aux_work & ERTS_SSI_AUX_WORK_MISC) {
	aux_work = handle_misc_aux_work(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#if ERTS_USE_ASYNC_READY_Q
    if (aux_work & ERTS_SSI_AUX_WORK_ASYNC_READY) {
	aux_work = handle_async_ready(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
    if (aux_work & ERTS_SSI_AUX_WORK_ASYNC_READY_CLEAN) {
	aux_work = handle_async_ready_clean(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#endif
#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
    if (aux_work & ERTS_SSI_AUX_WORK_CHECK_CHILDREN) {
	aux_work = handle_check_children(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#endif
    if (aux_work & (ERTS_SSI_AUX_WORK_FIX_ALLOC_LOWER_LIM
		    | ERTS_SSI_AUX_WORK_FIX_ALLOC_DEALLOC)) {
	aux_work = handle_fix_alloc(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#ifdef ERTS_SMP
    if (aux_work & ERTS_SSI_AUX_WORK_DD) {
	aux_work = handle_delayed_dealloc(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
    if (aux_work & ERTS_SSI_AUX_WORK_DD_THR_PRGR) {
	aux_work = handle_delayed_dealloc_thr_prgr(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#endif
#ifdef ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK
    if (aux_work & ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK) {
	aux_work = handle_mseg_cache_check(awdp, aux_work);
	ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    }
#endif
    ERTS_DBG_CHK_AUX_WORK_VAL(aux_work);
    return aux_work;
}

typedef struct {
    union {
	ErlTimer data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErlTimer))];
    } timer;

    int initialized;
    erts_atomic32_t refc;
    erts_atomic32_t type[1];
} ErtsAuxWorkTmo;

static ErtsAuxWorkTmo *aux_work_tmo;

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
    if (p & ERTS_CACHE_LINE_MASK)
	p = (p & ~ERTS_CACHE_LINE_MASK) + ERTS_CACHE_LINE_SIZE;
    ASSERT((p & ERTS_CACHE_LINE_MASK) == 0);

    aux_work_tmo = (ErtsAuxWorkTmo *) p;
    aux_work_tmo->initialized = 0;
    erts_atomic32_init_nob(&aux_work_tmo->refc, 0);
    for (i = 0; i <= no_schedulers; i++)
	erts_atomic32_init_nob(&aux_work_tmo->type[i], 0);
}

void
aux_work_timeout_late_init(void)
{
    aux_work_tmo->initialized = 1;
    if (erts_atomic32_read_nob(&aux_work_tmo->refc)) {
	aux_work_tmo->timer.data.active = 0;
	erts_set_timer(&aux_work_tmo->timer.data,
		       aux_work_timeout,
		       NULL,
		       NULL,
		       1000);
    }
}

static void
aux_work_timeout(void *unused)
{
    erts_aint32_t refc;
    int i;
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
	aux_work_tmo->timer.data.active = 0;
	erts_set_timer(&aux_work_tmo->timer.data,
		       aux_work_timeout,
		       NULL,
		       NULL,
		       1000);
    }
}

static void
setup_aux_work_timer(void)
{
#ifndef ERTS_SMP
    if (!erts_get_scheduler_data())
	set_aux_work_flags_wakeup_nob(ERTS_SCHED_SLEEP_INFO_IX(0),
				      ERTS_SSI_AUX_WORK_SET_TMO);
    else
#endif
    {
	aux_work_tmo->timer.data.active = 0;
	erts_set_timer(&aux_work_tmo->timer.data,
		       aux_work_timeout,
		       NULL,
		       NULL,
		       1000);
    }
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
//    erts_fprintf(stderr, "t(%d, 0x%x, %d)\n", ix, type, enable);

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
		setup_aux_work_timer();
	}
    }
    return old;
}



static ERTS_INLINE void
sched_waiting_sys(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    ASSERT(rq->waiting >= 0);
    rq->flags |= (ERTS_RUNQ_FLG_OUT_OF_WORK
		  | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
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

    ASSERT(as >= 0);
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
prepare_for_sys_schedule(void)
{
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

#ifdef ERTS_SMP

static ERTS_INLINE void
sched_change_waiting_sys_to_waiting(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    ASSERT(rq->waiting < 0);
    rq->waiting *= -1;
}

static ERTS_INLINE void
sched_waiting(Uint no, ErtsRunQueue *rq)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    rq->flags |= (ERTS_RUNQ_FLG_OUT_OF_WORK
		  | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);
    if (rq->waiting < 0)
	rq->waiting--;
    else
	rq->waiting++;
    rq->woken = 0;
    if (erts_system_profile_flags.scheduler)
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
    if (erts_system_profile_flags.scheduler)
	profile_scheduler(make_small(no), am_active);
}

static int ERTS_INLINE
ongoing_multi_scheduling_block(void)
{
    ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&schdlr_sspnd.mtx));
    return schdlr_sspnd.msb.ongoing;
}

static ERTS_INLINE void
empty_runq(ErtsRunQueue *rq)
{
    erts_aint32_t oifls = erts_smp_atomic32_read_band_nob(&rq->info_flags,
							  ~ERTS_RUNQ_IFLG_NONEMPTY);
    if (oifls & ERTS_RUNQ_IFLG_NONEMPTY) {
#ifdef DEBUG
	erts_aint32_t empty = erts_smp_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 <= empty && empty < 2*erts_no_run_queues);
#endif
	erts_smp_atomic32_inc_relb(&no_empty_run_queues);
    }
}

static ERTS_INLINE void
non_empty_runq(ErtsRunQueue *rq)
{
    erts_aint32_t oifls = erts_smp_atomic32_read_bor_nob(&rq->info_flags,
							 ERTS_RUNQ_IFLG_NONEMPTY);
    if (!(oifls & ERTS_RUNQ_IFLG_NONEMPTY)) {
#ifdef DEBUG
	erts_aint32_t empty = erts_smp_atomic32_read_nob(&no_empty_run_queues);
	/*
	 * For a short period of time no_empty_run_queues may have
	 * been increased twice for a specific run queue.
	 */
	ASSERT(0 < empty && empty <= 2*erts_no_run_queues);
#endif
	erts_smp_atomic32_dec_relb(&no_empty_run_queues);
    }
}

static erts_aint32_t
sched_prep_spin_wait(ErtsSchedulerSleepInfo *ssi)
{
    erts_aint32_t oflgs;
    erts_aint32_t nflgs = (ERTS_SSI_FLG_SLEEPING
			   | ERTS_SSI_FLG_WAITING);
    erts_aint32_t xflgs = 0;

    do {
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
	nflgs |= oflgs & ERTS_SSI_FLG_SUSPENDED;
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
	nflgs |= oflgs & ERTS_SSI_FLG_SUSPENDED;
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

static void init_aux_work_data(ErtsAuxWorkData *awdp, ErtsSchedulerData *esdp);

static void *
aux_thread(void *unused)
{
    ErtsAuxWorkData *awdp = aux_thread_aux_work_data;
    ErtsSchedulerSleepInfo *ssi = ERTS_SCHED_SLEEP_INFO_IX(-1);
    erts_aint32_t aux_work;
    ErtsThrPrgrCallbacks callbacks;
    int thr_prgr_active = 1;

    ssi->event = erts_tse_fetch();

    callbacks.arg = (void *) ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

    erts_thr_progress_register_managed_thread(NULL, &callbacks, 1);
    init_aux_work_data(awdp, NULL);
    awdp->ssi = ssi;

    sched_prep_spin_wait(ssi);

    while (1) {
	erts_aint32_t flgs;

	aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	if (aux_work) {
	    if (!thr_prgr_active)
		erts_thr_progress_active(NULL, thr_prgr_active = 1);
	    aux_work = handle_aux_work(awdp, aux_work);
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

#endif /* ERTS_SMP */

static void
scheduler_wait(int *fcalls, ErtsSchedulerData *esdp, ErtsRunQueue *rq)
{
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    int spincount;
    erts_aint32_t aux_work = 0;
#ifdef ERTS_SMP
    int thr_prgr_active = 1;
    erts_aint32_t flgs;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

    erts_smp_spin_lock(&rq->sleepers.lock);
    flgs = sched_prep_spin_wait(ssi);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {
	/* Go suspend instead... */
	erts_smp_spin_unlock(&rq->sleepers.lock);
	return;
    }

    ssi->prev = NULL;
    ssi->next = rq->sleepers.list;
    if (rq->sleepers.list)
	rq->sleepers.list->prev = ssi;
    rq->sleepers.list = ssi;
    erts_smp_spin_unlock(&rq->sleepers.lock);

    /*
     * If all schedulers are waiting, one of them *should*
     * be waiting in erl_sys_schedule()
     */

    if (!prepare_for_sys_schedule()) {

	sched_waiting(esdp->no, rq);

	erts_smp_runq_unlock(rq);

	spincount = ERTS_SCHED_TSE_SLEEP_SPINCOUNT;

    tse_wait:

	while (1) {

	    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	    if (aux_work) {
		if (!thr_prgr_active)
		    erts_thr_progress_active(esdp, thr_prgr_active = 1);
		aux_work = handle_aux_work(&esdp->aux_work_data, aux_work);
		if (aux_work && erts_thr_progress_update(esdp))
		    erts_thr_progress_leader_update(esdp);
	    }

	    if (aux_work)
		flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
	    else {
		if (thr_prgr_active)
		    erts_thr_progress_active(esdp, thr_prgr_active = 0);
		erts_thr_progress_prepare_wait(esdp);

		flgs = sched_spin_wait(ssi, spincount);
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
		erts_thr_progress_finalize_wait(esdp);
	    }

	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		break;
	    }

	    flgs = sched_prep_cont_spin_wait(ssi);
	    spincount = ERTS_SCHED_TSE_SLEEP_SPINCOUNT;

	    if (!(flgs & ERTS_SSI_FLG_WAITING)) {
		ASSERT(!(flgs & ERTS_SSI_FLG_SLEEPING));
		break;
	    }

	}

	if (flgs & ~ERTS_SSI_FLG_SUSPENDED)
	    erts_smp_atomic32_read_band_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);

	if (!thr_prgr_active)
	    erts_thr_progress_active(esdp, thr_prgr_active = 1);

	erts_smp_runq_lock(rq);
	sched_active(esdp->no, rq);

    }
    else
#endif
    {
	erts_aint_t dt;

	erts_smp_atomic32_set_relb(&function_calls, 0);
	*fcalls = 0;

	sched_waiting_sys(esdp->no, rq);

	erts_smp_runq_unlock(rq);

	spincount = ERTS_SCHED_SYS_SLEEP_SPINCOUNT;

	while (spincount-- > 0) {

	sys_poll_aux_work:

	    ASSERT(!erts_port_task_have_outstanding_io_tasks());

	    erl_sys_schedule(1); /* Might give us something to do */

	    dt = erts_do_time_read_and_reset();
	    if (dt) erts_bump_timer(dt);

	sys_aux_work:
#ifndef ERTS_SMP
	    erts_sys_schedule_interrupt(0);
#endif

	    aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
	    if (aux_work) {
#ifdef ERTS_SMP
		if (!thr_prgr_active)
		    erts_thr_progress_active(esdp, thr_prgr_active = 1);
#endif
		aux_work = handle_aux_work(&esdp->aux_work_data, aux_work);
#ifdef ERTS_SMP
		if (aux_work && erts_thr_progress_update(esdp))
		    erts_thr_progress_leader_update(esdp);
#endif
	    }

#ifndef ERTS_SMP
	    if (rq->len != 0 || rq->misc.start)
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
		if (!prepare_for_sys_schedule()) {
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
	    if (!prepare_for_sys_schedule()) {
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

#ifdef ERTS_SMP
	if (thr_prgr_active)
	    erts_thr_progress_active(esdp, thr_prgr_active = 0);
#endif

	ASSERT(!erts_port_task_have_outstanding_io_tasks());

	erl_sys_schedule(0);

	dt = erts_do_time_read_and_reset();
	if (dt) erts_bump_timer(dt);

#ifndef ERTS_SMP
	if (rq->len == 0 && !rq->misc.start)
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
	if (flgs & ~ERTS_SSI_FLG_SUSPENDED)
	    erts_smp_atomic32_read_band_nob(&ssi->flags, ERTS_SSI_FLG_SUSPENDED);
#endif
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
	nflgs = oflgs & ERTS_SSI_FLG_SUSPENDED;
	xflgs = oflgs;
    }
}

static void
wake_scheduler(ErtsRunQueue *rq, int incq, int one)
{
    ErtsSchedulerSleepInfo *ssi;
    ErtsSchedulerSleepList *sl;

    /*
     * The unlocked run queue is not strictly necessary
     * from a thread safety or deadlock prevention
     * perspective. It will, however, cost us performance
     * if it is locked during wakup of another scheduler,
     * so all code *should* handle this without having
     * the lock on the run queue.
     */
    ERTS_SMP_LC_ASSERT(!erts_smp_lc_runq_is_locked(rq));

    sl = &rq->sleepers;

    erts_smp_spin_lock(&sl->lock);
    ssi = sl->list;
    if (!ssi)
	erts_smp_spin_unlock(&sl->lock);
    else if (one) {
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

	flgs = ssi_flags_set_wake(ssi);
	erts_sched_finish_poke(ssi, flgs);

	if (incq && !erts_common_run_queue && (flgs & ERTS_SSI_FLG_WAITING))
	    non_empty_runq(rq);
    }
    else {
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
wake_all_schedulers(void)
{
    if (erts_common_run_queue)
	wake_scheduler(erts_common_run_queue, 0, 0);
    else {
	int ix;
	for (ix = 0; ix < erts_no_run_queues; ix++) {
	    ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
	    wake_scheduler(rq, 0, 1);
	}
    }
}

#define ERTS_NO_USED_RUNQS_SHIFT 16
#define ERTS_NO_RUNQS_MASK 0xffff

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
    erts_aint32_t iflgs;
    ErtsRunQueue *wrq;
    if (crq->ix == ix)
	return 0;
    wrq = ERTS_RUNQ_IX(ix);
    iflgs = erts_smp_atomic32_read_nob(&wrq->info_flags);
    if (!(iflgs & (ERTS_RUNQ_IFLG_SUSPENDED|ERTS_RUNQ_IFLG_NONEMPTY))) {
	if (activate) {
	    if (try_inc_no_active_runqs(ix+1)) {
		erts_smp_xrunq_lock(crq, wrq);
		wrq->flags &= ~ERTS_RUNQ_FLG_INACTIVE;
		erts_smp_xrunq_unlock(crq, wrq);
	    }
	}
	wake_scheduler(wrq, 0, 1);
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
    if (runq)
	wake_scheduler(runq, 1, 1);
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
    if (erts_common_run_queue) {
	for (ix = 0; ix < erts_no_schedulers; ix++)
	    erts_smp_atomic32_set_relb(&ERTS_SCHEDULER_IX(ix)->chk_cpu_bind, 1);
	wake_all_schedulers();
    }
    else {
	for (ix = 0; ix < erts_no_run_queues; ix++) {
	    ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
	    erts_smp_runq_lock(rq);
	    rq->flags |= ERTS_RUNQ_FLG_CHK_CPU_BIND;
	    erts_smp_runq_unlock(rq);
	    wake_scheduler(rq, 0, 1);
	};
    }
#else
    erts_sched_check_cpu_bind(erts_get_scheduler_data());
#endif
}


#ifdef ERTS_SMP

ErtsRunQueue *
erts_prepare_emigrate(ErtsRunQueue *c_rq, ErtsRunQueueInfo *c_rqi, int prio)
{
    ASSERT(ERTS_CHK_RUNQ_FLG_EMIGRATE(c_rq->flags, prio));
    ASSERT(ERTS_CHK_RUNQ_FLG_EVACUATE(c_rq->flags, prio)
	   || c_rqi->len >= c_rqi->migrate.limit.this);

    while (1) {
	ErtsRunQueue *n_rq = c_rqi->migrate.runq;
	ERTS_DBG_VERIFY_VALID_RUNQP(n_rq);
	erts_smp_xrunq_lock(c_rq, n_rq);
	    
	/*
	 * erts_smp_xrunq_lock() may release lock on c_rq! We have
	 * to check that we still want to emigrate and emigrate
	 * to the same run queue as before.
	 */

	if (ERTS_CHK_RUNQ_FLG_EMIGRATE(c_rq->flags, prio)) {
	    Uint32 force = (ERTS_CHK_RUNQ_FLG_EVACUATE(c_rq->flags, prio)
			    | (c_rq->flags & ERTS_RUNQ_FLG_INACTIVE));
	    if (force || c_rqi->len > c_rqi->migrate.limit.this) {
		ErtsRunQueueInfo *n_rqi;
		/* We still want to emigrate */

		if (n_rq != c_rqi->migrate.runq) {
		    /* Ahh... run queue changed; need to do it all over again... */
		    erts_smp_runq_unlock(n_rq);
		    continue;
		}
		else {

		    if (prio == ERTS_PORT_PRIO_LEVEL)
			n_rqi = &n_rq->ports.info;
		    else
			n_rqi = &n_rq->procs.prio_info[prio];

		    if (force || (n_rqi->len < c_rqi->migrate.limit.other)) {
			/* emigrate ... */
			return n_rq;
		    }
		}
	    }
	}

	ASSERT(n_rq != c_rq);
	erts_smp_runq_unlock(n_rq);
	if (!(c_rq->flags & ERTS_RUNQ_FLG_INACTIVE)) {
	    /* No more emigrations to this runq */
	    ERTS_UNSET_RUNQ_FLG_EMIGRATE(c_rq->flags, prio);
	    ERTS_DBG_SET_INVALID_RUNQP(c_rqi->migrate.runq, 0x3);
	}

	return NULL;
    }
}

static void
immigrate(ErtsRunQueue *rq)
{
    int prio;

    ASSERT(rq->flags & ERTS_RUNQ_FLGS_IMMIGRATE_QMASK);

    for (prio = 0; prio < ERTS_NO_PRIO_LEVELS; prio++) {
	if (ERTS_CHK_RUNQ_FLG_IMMIGRATE(rq->flags, prio)) {
	    ErtsRunQueueInfo *rqi = (prio == ERTS_PORT_PRIO_LEVEL
				     ? &rq->ports.info
				     : &rq->procs.prio_info[prio]);
	    ErtsRunQueue *from_rq = rqi->migrate.runq;
	    int rq_locked, from_rq_locked;

	    ERTS_DBG_VERIFY_VALID_RUNQP(from_rq);

	    rq_locked = 1;
	    from_rq_locked = 1;
	    erts_smp_xrunq_lock(rq, from_rq);
	    /*
	     * erts_smp_xrunq_lock() may release lock on rq! We have
	     * to check that we still want to immigrate from the same 
	     * run queue as before.
	     */
	    if (ERTS_CHK_RUNQ_FLG_IMMIGRATE(rq->flags, prio)
		&& from_rq == rqi->migrate.runq) {
		ErtsRunQueueInfo *from_rqi = (prio == ERTS_PORT_PRIO_LEVEL
					      ? &from_rq->ports.info
					      : &from_rq->procs.prio_info[prio]);
		if ((ERTS_CHK_RUNQ_FLG_EVACUATE(rq->flags, prio)
		     && ERTS_CHK_RUNQ_FLG_EVACUATE(from_rq->flags, prio)
		     && from_rqi->len)
		    || (from_rqi->len > rqi->migrate.limit.other
			&& rqi->len < rqi->migrate.limit.this)) {
		    if (prio == ERTS_PORT_PRIO_LEVEL) {
			Port *prt = from_rq->ports.start;
			if (prt) {
			    int prt_locked = 0;
			    (void) erts_port_migrate(prt, &prt_locked,
						     from_rq, &from_rq_locked,
						     rq, &rq_locked);
			    if (prt_locked)
				erts_smp_port_unlock(prt);
			}
		    }
		    else {
			Process *proc;
			ErtsRunPrioQueue *from_rpq;
			from_rpq = (prio == PRIORITY_LOW
				    ? &from_rq->procs.prio[PRIORITY_NORMAL]
				    : &from_rq->procs.prio[prio]);
			for (proc = from_rpq->first; proc; proc = proc->next)
			    if (proc->prio == prio && !proc->bound_runq)
				break;
			if (proc) {
			    ErtsProcLocks proc_locks = 0;
			    (void) erts_proc_migrate(proc, &proc_locks,
						     from_rq, &from_rq_locked,
						     rq, &rq_locked);
			    if (proc_locks)
				erts_smp_proc_unlock(proc, proc_locks);
			}
		    }
		}
		else {
		    ERTS_UNSET_RUNQ_FLG_IMMIGRATE(rq->flags, prio);
		    ERTS_DBG_SET_INVALID_RUNQP(rqi->migrate.runq, 0x1);
		}
	    }
	    if (from_rq_locked)
		erts_smp_runq_unlock(from_rq);
	    if (!rq_locked)
		erts_smp_runq_lock(rq);
	}
    }
}

static void
evacuate_run_queue(ErtsRunQueue *evac_rq, ErtsRunQueue *rq)
{
    Port *prt;
    int notify_to_rq = 0;
    int prio;
    int prt_locked = 0;
    int rq_locked = 0;
    int evac_rq_locked = 1;
    ErtsMigrateResult mres;

    erts_smp_runq_lock(evac_rq);

    erts_smp_atomic32_read_bor_nob(&evac_rq->scheduler->ssi->flags,
				   ERTS_SSI_FLG_SUSPENDED);

    evac_rq->flags &= ~ERTS_RUNQ_FLGS_IMMIGRATE_QMASK;
    evac_rq->flags |= (ERTS_RUNQ_FLGS_EMIGRATE_QMASK
		       | ERTS_RUNQ_FLGS_EVACUATE_QMASK
		       | ERTS_RUNQ_FLG_SUSPENDED);

    erts_smp_atomic32_read_bor_nob(&evac_rq->info_flags, ERTS_RUNQ_IFLG_SUSPENDED);
    /*
     * Need to set up evacuation paths first since we
     * may release the run queue lock on evac_rq
     * when evacuating.
     */
    evac_rq->misc.evac_runq = rq;
    evac_rq->ports.info.migrate.runq = rq;
    for (prio = 0; prio < ERTS_NO_PROC_PRIO_LEVELS; prio++)
	evac_rq->procs.prio_info[prio].migrate.runq = rq;

    /* Evacuate scheduled misc ops */

    if (evac_rq->misc.start) {
	rq_locked = 1;
	erts_smp_xrunq_lock(evac_rq, rq);
	if (rq->misc.end)
	    rq->misc.end->next = evac_rq->misc.start;
	else
	    rq->misc.start = evac_rq->misc.start;
	rq->misc.end = evac_rq->misc.end;
	evac_rq->misc.start = NULL;
	evac_rq->misc.end = NULL;
    }

    /* Evacuate scheduled ports */
    prt = evac_rq->ports.start;
    while (prt) {
	mres = erts_port_migrate(prt, &prt_locked,
				 evac_rq, &evac_rq_locked,
				 rq, &rq_locked);
	if (mres == ERTS_MIGRATE_SUCCESS)
	    notify_to_rq = 1;
	if (prt_locked)
	    erts_smp_port_unlock(prt);
	if (!evac_rq_locked) {
	    evac_rq_locked = 1;
	    erts_smp_runq_lock(evac_rq);
	}
	prt = evac_rq->ports.start;
    }

    /* Evacuate scheduled processes */
    for (prio = 0; prio < ERTS_NO_PROC_PRIO_LEVELS; prio++) {
	Process *proc;

	switch (prio) {
	case PRIORITY_MAX:
	case PRIORITY_HIGH:
	case PRIORITY_NORMAL:
	    proc = evac_rq->procs.prio[prio].first;
	    while (proc) {
		ErtsProcLocks proc_locks = 0;

		/* Bound processes are stuck... */
		while (proc->bound_runq) {
		    proc = proc->next;
		    if (!proc)
			goto end_of_proc;
		}

		mres = erts_proc_migrate(proc, &proc_locks,
					 evac_rq, &evac_rq_locked,
					 rq, &rq_locked);
		if (mres == ERTS_MIGRATE_SUCCESS)
		    notify_to_rq = 1;
		if (proc_locks)
		    erts_smp_proc_unlock(proc, proc_locks);
		if (!evac_rq_locked) {
		    erts_smp_runq_lock(evac_rq);
		    evac_rq_locked = 1;
		}

		proc = evac_rq->procs.prio[prio].first;
	    }

	end_of_proc:

#ifdef DEBUG
	    for (proc = evac_rq->procs.prio[prio].first;
		 proc;
		 proc = proc->next) {
		ASSERT(proc->bound_runq);
	    }
#endif
	    break;
	case PRIORITY_LOW:
	    break;
	default:
	    ASSERT(!"Invalid process priority");
	    break;
	}
    }

    if (rq_locked)
	erts_smp_runq_unlock(rq);

    if (evac_rq_locked)
	erts_smp_runq_unlock(evac_rq);

    if (notify_to_rq)
	smp_notify_inc_runq(rq);

    wake_scheduler(evac_rq, 0, 1);
}

static int
try_steal_task_from_victim(ErtsRunQueue *rq, int *rq_lockedp, ErtsRunQueue *vrq)
{
    Process *proc;
    int vrq_locked;

    if (*rq_lockedp)
	erts_smp_xrunq_lock(rq, vrq);
    else
	erts_smp_runq_lock(vrq);
    vrq_locked = 1;

    ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

    /*
     * Check for a runnable process to steal...
     */

    switch (vrq->flags & ERTS_RUNQ_FLGS_PROCS_QMASK) {
    case MAX_BIT:
    case MAX_BIT|HIGH_BIT:
    case MAX_BIT|NORMAL_BIT:
    case MAX_BIT|LOW_BIT:
    case MAX_BIT|HIGH_BIT|NORMAL_BIT:
    case MAX_BIT|HIGH_BIT|LOW_BIT:
    case MAX_BIT|NORMAL_BIT|LOW_BIT:
    case MAX_BIT|HIGH_BIT|NORMAL_BIT|LOW_BIT:
	for (proc = vrq->procs.prio[PRIORITY_MAX].last;
	     proc;
	     proc = proc->prev) {
	    if (!proc->bound_runq)
		break;
	}
	if (proc)
	    break;
    case HIGH_BIT:
    case HIGH_BIT|NORMAL_BIT:
    case HIGH_BIT|LOW_BIT:
    case HIGH_BIT|NORMAL_BIT|LOW_BIT:
	for (proc = vrq->procs.prio[PRIORITY_HIGH].last;
	     proc;
	     proc = proc->prev) {
	    if (!proc->bound_runq)
		break;
	}
	if (proc)
	    break;
    case NORMAL_BIT:
    case LOW_BIT:
    case NORMAL_BIT|LOW_BIT:
	for (proc = vrq->procs.prio[PRIORITY_NORMAL].last;
	     proc;
	     proc = proc->prev) {
	    if (!proc->bound_runq)
		break;
	}
	if (proc)
	    break;
    case 0:
	proc = NULL;
	break;
    default:
	ASSERT(!"Invalid queue mask");
	proc = NULL;
	break;
    }

    if (proc) {
	ErtsProcLocks proc_locks = 0;
	int res;
	ErtsMigrateResult mres;
	mres = erts_proc_migrate(proc, &proc_locks,
				 vrq, &vrq_locked,
				 rq, rq_lockedp);
	if (proc_locks)
	    erts_smp_proc_unlock(proc, proc_locks);
	res = !0;
	switch (mres) {
	case ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED:
	    res = 0;
	case ERTS_MIGRATE_SUCCESS:
	    if (vrq_locked)
		erts_smp_runq_unlock(vrq);
	    return res;
	default: /* Other failures */
	    break;			
	}
    }

    ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

    if (!vrq_locked) {
	if (*rq_lockedp)
	    erts_smp_xrunq_lock(rq, vrq);
	else
	    erts_smp_runq_lock(vrq);
	vrq_locked = 1;
    }

    ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

    /*
     * Check for a runnable port to steal...
     */

    if (vrq->ports.info.len) {
	Port *prt = vrq->ports.end;
	int prt_locked = 0;
	int res;
	ErtsMigrateResult mres;

	mres = erts_port_migrate(prt, &prt_locked,
				 vrq, &vrq_locked,
				 rq, rq_lockedp);
	if (prt_locked)
	    erts_smp_port_unlock(prt);
	res = !0;
	switch (mres) {
	case ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED:
	    res = 0;
	case ERTS_MIGRATE_SUCCESS:
	    if (vrq_locked)
		erts_smp_runq_unlock(vrq);
	    return res;
	default: /* Other failures */
	    break;			
	}
    }

    if (vrq_locked)
	erts_smp_runq_unlock(vrq);

    return 0;
}


static ERTS_INLINE int
check_possible_steal_victim(ErtsRunQueue *rq, int *rq_lockedp, int vix)
{
    ErtsRunQueue *vrq = ERTS_RUNQ_IX(vix);
    erts_aint32_t iflgs = erts_smp_atomic32_read_nob(&vrq->info_flags);
    if (iflgs & ERTS_RUNQ_IFLG_NONEMPTY)
	return try_steal_task_from_victim(rq, rq_lockedp, vrq);
    else
	return 0;
}


static int
try_steal_task(ErtsRunQueue *rq)
{
    int res, rq_locked, vix, active_rqs, blnc_rqs;
    
    if (erts_common_run_queue)
	return 0;

    /*
     * We are not allowed to steal jobs to this run queue
     * if it is suspended. Note that it might get suspended
     * at any time when we don't have the lock on the run
     * queue.
     */
    if (rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
	return 0;

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

    if (!res)
	res = !ERTS_EMPTY_RUNQ(rq);

    return res;
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

static void
check_balance(ErtsRunQueue *c_rq)
{
#if ERTS_MAX_PROCESSES >= (1 << 27)
#  error check_balance() assumes ERTS_MAX_PROCESS < (1 << 27)
#endif
    ErtsRunQueueBalance avg = {0};
    Sint64 scheds_reds, full_scheds_reds;
    int forced, active, current_active, oowc, half_full_scheds, full_scheds,
	mmax_len, blnc_no_rqs, qix, pix, freds_hist_ix;

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
		rq->flags |= ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK;
	    else
		rq->flags &= ~ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK;
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

    freds_hist_ix = balance_info.full_reds_history_index;
    balance_info.full_reds_history_index++;
    if (balance_info.full_reds_history_index >= ERTS_FULL_REDS_HISTORY_SIZE)
	balance_info.full_reds_history_index = 0;

    /* Read balance information for all run queues */
    for (qix = 0; qix < blnc_no_rqs; qix++) {
	ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);
	erts_smp_runq_lock(rq);

	run_queue_info[qix].flags = rq->flags;
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

    if (!erts_sched_compact_load)
	goto all_active;

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

	/* Setup migration paths for all priorities */
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    int low = 0, high = 0;
	    for (qix = 0; qix < blnc_no_rqs; qix++) {
		int len_diff = run_queue_info[qix].prio[pix].max_len;
		len_diff -= run_queue_info[qix].prio[pix].migration_limit;
#ifdef DBG_PRINT
if (pix == 2) erts_fprintf(stderr, "%d ", len_diff);
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
    erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);

    /* Write migration paths and reset balance statistics in all queues */
    for (qix = 0; qix < blnc_no_rqs; qix++) {
	int mqix;
	Uint32 flags;
	ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);
	ErtsRunQueueInfo *rqi;
	flags = run_queue_info[qix].flags;
	erts_smp_runq_lock(rq);
	flags |= (rq->flags & ~ERTS_RUNQ_FLGS_MIGRATION_INFO);
	ASSERT(!(flags & ERTS_RUNQ_FLG_OUT_OF_WORK));
	if (rq->waiting)
	    flags |= ERTS_RUNQ_FLG_OUT_OF_WORK;

	rq->full_reds_history_sum
	    = run_queue_info[qix].full_reds_history_sum;
	rq->full_reds_history[freds_hist_ix]
	    = run_queue_info[qix].full_reds_history_change;

	ERTS_DBG_CHK_FULL_REDS_HISTORY(rq);

	rq->out_of_work_count = 0;
	rq->flags = flags;
	rq->max_len = rq->len;
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
	    rqi = (pix == ERTS_PORT_PRIO_LEVEL
		   ? &rq->ports.info
		   : &rq->procs.prio_info[pix]);
	    rqi->max_len = rqi->len;
	    rqi->reds = 0;
	    if (!(ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)
		  | ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix))) {
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0);
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0);
#ifdef DEBUG
		rqi->migrate.limit.this = -1;
		rqi->migrate.limit.other = -1;
		ERTS_DBG_SET_INVALID_RUNQP(rqi->migrate.runq, 0x2);
#endif
		
	    }
	    else if (ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)) {
		ASSERT(!ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix));
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0);
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to >= 0);

		mqix = run_queue_info[qix].prio[pix].emigrate_to;
		rqi->migrate.limit.this
		    = run_queue_info[qix].prio[pix].migration_limit;
		rqi->migrate.limit.other
		    = run_queue_info[mqix].prio[pix].migration_limit;
		rqi->migrate.runq = ERTS_RUNQ_IX(mqix);
	    }
	    else {
		ASSERT(ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix));
		ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0);
		ASSERT(run_queue_info[qix].prio[pix].immigrate_from >= 0);

		mqix = run_queue_info[qix].prio[pix].immigrate_from;
		rqi->migrate.limit.this
		    = run_queue_info[qix].prio[pix].migration_limit;
		rqi->migrate.limit.other
		    = run_queue_info[mqix].prio[pix].migration_limit;
		rqi->migrate.runq = ERTS_RUNQ_IX(mqix);
	    }
	}

	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	erts_smp_runq_unlock(rq);
    }

    balance_info.n++;
    erts_smp_mtx_unlock(&balance_info.update_mtx);

    erts_smp_runq_lock(c_rq);
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

void
erts_early_init_scheduling(int no_schedulers)
{
    aux_work_timeout_early_init(no_schedulers);
    wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM;
}

int
erts_sched_set_wakeup_limit(char *str)
{
    if (sys_strcmp(str, "very_high") == 0)
	wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_HIGH;
    else if (sys_strcmp(str, "high") == 0)
	wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_HIGH;
    else if (sys_strcmp(str, "medium") == 0)
	wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_MEDIUM;
    else if (sys_strcmp(str, "low") == 0)
	wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_LOW;
    else if (sys_strcmp(str, "very_low") == 0)
	wakeup_other_limit = ERTS_WAKEUP_OTHER_LIMIT_VERY_LOW;
    else
	return EINVAL;
    return 0;
}

static void
init_aux_work_data(ErtsAuxWorkData *awdp, ErtsSchedulerData *esdp)
{
    awdp->sched_id = esdp ? (int) esdp->no : 0;
    awdp->esdp = esdp;
    awdp->ssi = esdp ? esdp->ssi : NULL;
#ifdef ERTS_SMP
    awdp->misc.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->dd.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
    awdp->dd.completed_callback = NULL;
    awdp->dd.completed_arg = NULL;
#endif
#ifdef ERTS_USE_ASYNC_READY_Q
#ifdef ERTS_SMP
    awdp->async_ready.need_thr_prgr = 0;
    awdp->async_ready.thr_prgr = ERTS_THR_PRGR_VAL_WAITING;
#endif
    awdp->async_ready.queue = NULL;
#endif
}

void
erts_init_scheduling(int mrq, int no_schedulers, int no_schedulers_online)
{
    int ix, n, no_ssi;

#ifndef ERTS_SMP
    mrq = 0;
#endif

    init_misc_op_list_alloc();

    ASSERT(no_schedulers_online <= no_schedulers);
    ASSERT(no_schedulers_online >= 1);
    ASSERT(no_schedulers >= 1);

    /* Create and initialize run queues */

    n = (int) (mrq ? no_schedulers : 1);

    erts_aligned_run_queues = 
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_RUNQS,
					   sizeof(ErtsAlignedRunQueue) * n);
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&no_empty_run_queues, 0);
#endif

    erts_no_run_queues = n;

    for (ix = 0; ix < n; ix++) {
	int pix, rix;
	ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);

	rq->ix = ix;
	erts_smp_atomic32_init_nob(&rq->info_flags, ERTS_RUNQ_IFLG_NONEMPTY);

	/* make sure that the "extra" id correponds to the schedulers
	 * id if the esdp->no <-> ix+1 mapping change.
	 */

	erts_smp_mtx_init_x(&rq->mtx, "run_queue", make_small(ix + 1));
	erts_smp_cnd_init(&rq->cnd);

#ifdef ERTS_SMP
	erts_smp_spinlock_init(&rq->sleepers.lock, "run_queue_sleep_list");
	rq->sleepers.list = NULL;
#endif

	rq->waiting = 0;
	rq->woken = 0;
	rq->flags = !mrq ? ERTS_RUNQ_FLG_SHARED_RUNQ : 0;
	rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
	rq->full_reds_history_sum = 0;
	for (rix = 0; rix < ERTS_FULL_REDS_HISTORY_SIZE; rix++) {
	    rq->full_reds_history_sum += ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED;
	    rq->full_reds_history[rix] = ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED;
	}
	rq->out_of_work_count = 0;
	rq->max_len = 0;
	rq->len = 0;
	rq->wakeup_other = 0;
	rq->wakeup_other_reds = 0;

	rq->procs.len = 0;
	rq->procs.pending_exiters = NULL;
	rq->procs.context_switches = 0;
	rq->procs.reductions = 0;

	for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
	    rq->procs.prio_info[pix].len = 0;
	    rq->procs.prio_info[pix].max_len = 0;
	    rq->procs.prio_info[pix].reds = 0;
	    rq->procs.prio_info[pix].migrate.limit.this = 0;
	    rq->procs.prio_info[pix].migrate.limit.other = 0;
	    ERTS_DBG_SET_INVALID_RUNQP(rq->procs.prio_info[pix].migrate.runq,
				       0x0);
	    if (pix < ERTS_NO_PROC_PRIO_LEVELS - 1) {
		rq->procs.prio[pix].first = NULL;
		rq->procs.prio[pix].last = NULL;
	    }
	}

	rq->misc.start = NULL;
	rq->misc.end = NULL;
	rq->misc.evac_runq = NULL;

	rq->ports.info.len = 0;
	rq->ports.info.max_len = 0;
	rq->ports.info.reds = 0;
	rq->ports.info.migrate.limit.this = 0;
	rq->ports.info.migrate.limit.other = 0;
	rq->ports.info.migrate.runq = NULL;
	rq->ports.start = NULL;
	rq->ports.end = NULL;
    }

    erts_common_run_queue = !mrq ? ERTS_RUNQ_IX(0) : NULL;

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
#endif

    /* Create and initialize scheduler specific data */

    erts_aligned_scheduler_data = 
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   n*sizeof(ErtsAlignedSchedulerData));					   

    for (ix = 0; ix < n; ix++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
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
#if !HEAP_ON_C_STACK
	esdp->num_tmp_heap_used = 0;
#endif
	esdp->no = (Uint) ix+1;
	esdp->ssi = ERTS_SCHED_SLEEP_INFO_IX(ix);
	esdp->current_process = NULL;
	esdp->current_port = NULL;

	esdp->virtual_reds = 0;
	esdp->cpu_id = -1;

	erts_init_atom_cache_map(&esdp->atom_cache_map);

	if (erts_common_run_queue) {
	    esdp->run_queue = erts_common_run_queue;
	    esdp->run_queue->scheduler = NULL;
	}
	else {
	    esdp->run_queue = ERTS_RUNQ_IX(ix);
	    esdp->run_queue->scheduler = esdp;
	}

#ifdef ERTS_SMP
	erts_smp_atomic32_init_nob(&esdp->chk_cpu_bind, 0);
#endif
	init_aux_work_data(&esdp->aux_work_data, esdp);
    }

    init_misc_aux_work();

#ifdef ERTS_SMP

    erts_atomic32_init_nob(&completed_dealloc_count, 0); /* debug only */

    aux_thread_aux_work_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   sizeof(ErtsAuxWorkData));

    erts_smp_mtx_init(&schdlr_sspnd.mtx, "schdlr_sspnd");
    erts_smp_cnd_init(&schdlr_sspnd.cnd);

    erts_smp_atomic32_init_nob(&schdlr_sspnd.changing, 0);
    schdlr_sspnd.online = no_schedulers_online;
    schdlr_sspnd.curr_online = no_schedulers;
    schdlr_sspnd.msb.ongoing = 0;
    erts_smp_atomic32_init_nob(&schdlr_sspnd.active, no_schedulers);
    schdlr_sspnd.msb.procs = NULL;
    init_no_runqs(no_schedulers,
		  erts_common_run_queue ? 1 : no_schedulers_online);
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

    if (no_schedulers_online < no_schedulers) {
	if (erts_common_run_queue) {
	    for (ix = no_schedulers_online; ix < no_schedulers; ix++)
		erts_smp_atomic32_read_bor_nob(&ERTS_SCHED_SLEEP_INFO_IX(ix)->flags,
					       ERTS_SSI_FLG_SUSPENDED);
	}
	else {
	    for (ix = no_schedulers_online; ix < erts_no_run_queues; ix++)
		evacuate_run_queue(ERTS_RUNQ_IX(ix),
				   ERTS_RUNQ_IX(ix % no_schedulers_online));
	}
    }

    schdlr_sspnd.wait_curr_online = no_schedulers_online;
    schdlr_sspnd.curr_online *= 2; /* Boot strapping... */
    ERTS_SCHDLR_SSPND_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_ONLN
				| ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);

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
    erts_no_schedulers = 1;
#endif

    erts_smp_atomic32_init_nob(&function_calls, 0);

    /* init port tasks */
    erts_port_task_init();

    aux_work_timeout_late_init();

#ifndef ERTS_SMP
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_scheduler_data->verify_unused_temp_alloc
	= erts_alloc_get_verify_unused_temp_alloc(
	    &erts_scheduler_data->verify_unused_temp_alloc_data);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif
#endif
}

ErtsRunQueue *
erts_schedid2runq(Uint id)
{
    int ix;
    if (erts_common_run_queue)
	return erts_common_run_queue;
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

static int remove_proc_from_runq(ErtsRunQueue *rq, Process *p, int to_inactive);

static ERTS_INLINE void
suspend_process(ErtsRunQueue *rq, Process *p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));
    p->rcount++;  /* count number of suspend */
#ifdef ERTS_SMP
    ASSERT(!(p->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING)
	   || p == erts_get_current_process());
    ASSERT(p->status != P_RUNNING
	   || p->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING);
    if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ)
	goto runable;
#endif
    switch(p->status) {
    case P_SUSPENDED:
	break;
    case P_RUNABLE:
#ifdef ERTS_SMP
    runable:
        if (!ERTS_PROC_PENDING_EXIT(p)) 
#endif
	    remove_proc_from_runq(rq, p, 1);
	/* else:
	 * leave process in schedq so it will discover the pending exit
	 */
	p->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_RUNNING:
	p->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_WAITING:
	p->rstatus = P_WAITING; /* wakeup as waiting */
	break;
    case P_EXITING:
	return; /* ignore this */
    case P_GARBING:
    case P_FREE:
	erl_exit(1, "bad state in suspend_process()\n");
    }

    if ((erts_system_profile_flags.runnable_procs) && (p->rcount == 1) && (p->status != P_WAITING)) {
        profile_runnable_proc(p, am_inactive);
    }

    p->status = P_SUSPENDED;
    
}

static ERTS_INLINE void
resume_process(Process *p)
{
    Uint32 *statusp;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    switch (p->status) {
    case P_SUSPENDED:
	statusp = &p->status;
	break;
    case P_GARBING:
	if (p->gcstatus == P_SUSPENDED) {
	    statusp = &p->gcstatus;
	    break;
	}
	/* Fall through */
    default:
	return;
    }

    ASSERT(p->rcount > 0);

    if (--p->rcount > 0)  /* multiple suspend */
	return;
    switch(p->rstatus) {
    case P_RUNABLE:
	erts_add_to_runq(p);
	break;
    case P_WAITING:
	*statusp = P_WAITING;
	break;
    default:
	erl_exit(1, "bad state in resume_process()\n");
    }
    p->rstatus = P_FREE;    
}

int
erts_get_max_no_executing_schedulers(void)
{
#ifdef ERTS_SMP
    if (erts_smp_atomic32_read_nob(&schdlr_sspnd.changing))
	return (int) erts_no_schedulers;
    ERTS_THR_MEMORY_BARRIER;
    return (int) erts_smp_atomic32_read_nob(&schdlr_sspnd.active);
#else
    return 1;
#endif
}

#ifdef ERTS_SMP

static void
scheduler_ix_resume_wake(Uint ix)
{
    ErtsSchedulerSleepInfo *ssi = ERTS_SCHED_SLEEP_INFO_IX(ix);
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
    } while (oflgs & ERTS_SSI_FLG_SUSPENDED);
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
suspend_scheduler(ErtsSchedulerData *esdp)
{
    erts_aint32_t flgs;
    erts_aint32_t changing;
    long no = (long) esdp->no;
    ErtsSchedulerSleepInfo *ssi = esdp->ssi;
    long active_schedulers;
    int curr_online = 1;
    int wake = 0;
    erts_aint32_t aux_work;
    int thr_prgr_active = 1;

    /*
     * Schedulers may be suspended in two different ways:
     * - A scheduler may be suspended since it is not online.
     *   All schedulers with scheduler ids greater than
     *   schdlr_sspnd.online are suspended.
     * - Multi scheduling is blocked. All schedulers except the
     *   scheduler with scheduler id 1 are suspended.
     *
     * Regardless of why a scheduler is suspended, it ends up here.
     */

    ASSERT(no != 1);

    erts_smp_runq_unlock(esdp->run_queue);

    erts_sched_check_cpu_bind_prep_suspend(esdp);

    if (erts_system_profile_flags.scheduler)
    	profile_scheduler(make_small(esdp->no), am_inactive);

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    flgs = sched_prep_spin_suspended(ssi, ERTS_SSI_FLG_SUSPENDED);
    if (flgs & ERTS_SSI_FLG_SUSPENDED) {

	active_schedulers = erts_smp_atomic32_dec_read_nob(&schdlr_sspnd.active);
	ASSERT(active_schedulers >= 1);
	changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
	if (changing & ERTS_SCHDLR_SSPND_CHNG_MSB) {
	    if (active_schedulers == schdlr_sspnd.msb.wait_active)
		wake = 1;
	    if (active_schedulers == 1) {
		changing = erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
							   ~ERTS_SCHDLR_SSPND_CHNG_MSB);
		changing &= ~ERTS_SCHDLR_SSPND_CHNG_MSB;
	    }
	}

	while (1) {
	    if (changing & ERTS_SCHDLR_SSPND_CHNG_ONLN) {
		int changed = 0;
		if (no > schdlr_sspnd.online && curr_online) {
		    schdlr_sspnd.curr_online--;
		    curr_online = 0;
		    changed = 1;
		}
		else if (no <= schdlr_sspnd.online && !curr_online) {
		    schdlr_sspnd.curr_online++;
		    curr_online = 1;
		    changed = 1;
		}
		if (changed
		    && schdlr_sspnd.curr_online == schdlr_sspnd.wait_curr_online)
		    wake = 1;
		if (schdlr_sspnd.online == schdlr_sspnd.curr_online) {
		    changing = erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
							       ~ERTS_SCHDLR_SSPND_CHNG_ONLN);
		    changing &= ~ERTS_SCHDLR_SSPND_CHNG_ONLN;
		}
	    }

	    if (wake) {
		erts_smp_cnd_signal(&schdlr_sspnd.cnd);
		wake = 0;
	    }

	    flgs = erts_smp_atomic32_read_acqb(&ssi->flags);
	    if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		break;
	    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

	    while (1) {
		erts_aint32_t flgs;

		aux_work = erts_atomic32_read_acqb(&ssi->aux_work);
		if (aux_work) {
		    if (!thr_prgr_active)
			erts_thr_progress_active(esdp, thr_prgr_active = 1);
		    aux_work = handle_aux_work(&esdp->aux_work_data, aux_work);
		    if (aux_work && erts_thr_progress_update(esdp))
			erts_thr_progress_leader_update(esdp);
		}

		if (!aux_work) {
		    if (thr_prgr_active)
			erts_thr_progress_active(esdp, thr_prgr_active = 0);
		    erts_thr_progress_prepare_wait(esdp);
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

			    do {
				res = erts_tse_wait(ssi->event);
			    } while (res == EINTR);
			}
		    }
		    erts_thr_progress_finalize_wait(esdp);
		}

		flgs = sched_prep_spin_suspended(ssi, (ERTS_SSI_FLG_WAITING
						       | ERTS_SSI_FLG_SUSPENDED));
		if (!(flgs & ERTS_SSI_FLG_SUSPENDED))
		    break;
		changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
		if (changing & ~ERTS_SCHDLR_SSPND_CHNG_WAITER)
		    break;
	    }

	    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
	    changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
	}

	active_schedulers = erts_smp_atomic32_inc_read_nob(&schdlr_sspnd.active);
	changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
	if ((changing & ERTS_SCHDLR_SSPND_CHNG_MSB)
	    && schdlr_sspnd.online == active_schedulers) {
	    erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
					    ~ERTS_SCHDLR_SSPND_CHNG_MSB);
	}

	ASSERT(no <= schdlr_sspnd.online);
	ASSERT(!ongoing_multi_scheduling_block());

    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    ASSERT(curr_online);

    if (erts_system_profile_flags.scheduler)
    	profile_scheduler(make_small(esdp->no), am_active);

    if (!thr_prgr_active)
	erts_thr_progress_active(esdp, thr_prgr_active = 1);

    erts_smp_runq_lock(esdp->run_queue);
    non_empty_runq(esdp->run_queue);

    erts_sched_check_cpu_bind_post_suspend(esdp);
}

#define ERTS_RUNQ_RESET_SUSPEND_INFO(RQ, DBG_ID)			\
do {									\
    int pix__;								\
    (RQ)->misc.evac_runq = NULL;					\
    (RQ)->ports.info.migrate.runq = NULL;				\
    (RQ)->flags &= ~(ERTS_RUNQ_FLGS_IMMIGRATE_QMASK			\
		     | ERTS_RUNQ_FLGS_EMIGRATE_QMASK			\
		     | ERTS_RUNQ_FLGS_EVACUATE_QMASK			\
		     | ERTS_RUNQ_FLG_SUSPENDED);			\
    (RQ)->flags |= (ERTS_RUNQ_FLG_OUT_OF_WORK				\
                    | ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK);		\
    (RQ)->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;	\
    erts_smp_atomic32_read_band_nob(&(RQ)->info_flags, ~ERTS_RUNQ_IFLG_SUSPENDED);\
    for (pix__ = 0; pix__ < ERTS_NO_PROC_PRIO_LEVELS; pix__++) {	\
	(RQ)->procs.prio_info[pix__].max_len = 0;			\
	(RQ)->procs.prio_info[pix__].reds = 0;				\
	ERTS_DBG_SET_INVALID_RUNQP((RQ)->procs.prio_info[pix__].migrate.runq,\
				   (DBG_ID));				\
    }									\
    (RQ)->ports.info.max_len = 0;					\
    (RQ)->ports.info.reds = 0;						\
} while (0)

#define ERTS_RUNQ_RESET_MIGRATION_PATHS__(RQ)				\
do {									\
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked((RQ)));		\
    (RQ)->misc.evac_runq = NULL;					\
    (RQ)->ports.info.migrate.runq = NULL;				\
    (RQ)->flags &= ~(ERTS_RUNQ_FLGS_IMMIGRATE_QMASK			\
		     | ERTS_RUNQ_FLGS_EMIGRATE_QMASK			\
		     | ERTS_RUNQ_FLGS_EVACUATE_QMASK);			\
} while (0)

#ifdef DEBUG
#define ERTS_RUNQ_RESET_MIGRATION_PATHS(RQ, DBG_ID)			\
do {									\
    int pix__;								\
    ERTS_RUNQ_RESET_MIGRATION_PATHS__((RQ));				\
    for (pix__ = 0; pix__ < ERTS_NO_PROC_PRIO_LEVELS; pix__++)		\
	ERTS_DBG_SET_INVALID_RUNQP((RQ)->procs.prio_info[pix__].migrate.runq,\
				   (DBG_ID));				\
} while (0)
#else
#define ERTS_RUNQ_RESET_MIGRATION_PATHS(RQ, DBG_ID) \
  ERTS_RUNQ_RESET_MIGRATION_PATHS__((RQ))
#endif

ErtsSchedSuspendResult
erts_schedulers_state(Uint *total,
		      Uint *online,
		      Uint *active,
		      int yield_allowed)
{
    int res;
    erts_aint32_t changing;
    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
    if (yield_allowed && (changing & ~ERTS_SCHDLR_SSPND_CHNG_WAITER))
	res = ERTS_SCHDLR_SSPND_YIELD_RESTART;
    else {
	*active = *online = schdlr_sspnd.online;
	if (ongoing_multi_scheduling_block())
	    *active = 1;
	res = ERTS_SCHDLR_SSPND_DONE;
    }
    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    *total = erts_no_schedulers;
    return res;
}

ErtsSchedSuspendResult
erts_set_schedulers_online(Process *p,
			   ErtsProcLocks plocks,
			   Sint new_no,
			   Sint *old_no)
{
    ErtsSchedulerData *esdp;
    int ix, res, no, have_unlocked_plocks, end_wait;
    erts_aint32_t changing;

    if (new_no < 1 || erts_no_schedulers < new_no)
	return ERTS_SCHDLR_SSPND_EINVAL;

    esdp = ERTS_PROC_GET_SCHDATA(p);
    end_wait = 0;

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    have_unlocked_plocks = 0;
    no = (int) new_no;

    changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
    if (changing) {
	res = ERTS_SCHDLR_SSPND_YIELD_RESTART;
    }
    else {
	int online = *old_no = schdlr_sspnd.online;
	if (no == schdlr_sspnd.online) {
	    res = ERTS_SCHDLR_SSPND_DONE;
	}
	else {
	    ERTS_SCHDLR_SSPND_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_ONLN
					| ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);
	    schdlr_sspnd.online = no;
	    if (no > online) {
		int ix;
		schdlr_sspnd.wait_curr_online = no;
		if (ongoing_multi_scheduling_block()) {
		    for (ix = online; ix < no; ix++)
			erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
		}
		else if (erts_common_run_queue) {
		    for (ix = online; ix < no; ix++)
			scheduler_ix_resume_wake(ix);
		}
		else {
		    if (plocks) {
			have_unlocked_plocks = 1;
			erts_smp_proc_unlock(p, plocks);
		    }
		    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		    erts_smp_mtx_lock(&balance_info.update_mtx);
		    for (ix = online; ix < no; ix++) {
			ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
			erts_smp_runq_lock(rq);
			ERTS_RUNQ_RESET_SUSPEND_INFO(rq, 0x5);
			erts_smp_runq_unlock(rq);
			scheduler_ix_resume_wake(ix);
		    }
		    /*
		     * Spread evacuation paths among all online
		     * run queues.
		     */
		    for (ix = no; ix < erts_no_run_queues; ix++) {
			ErtsRunQueue *from_rq = ERTS_RUNQ_IX(ix);
			ErtsRunQueue *to_rq = ERTS_RUNQ_IX(ix % no);
			evacuate_run_queue(from_rq, to_rq);
		    }
		    set_no_used_runqs(no);
		    erts_smp_mtx_unlock(&balance_info.update_mtx);
		    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
		}
		res = ERTS_SCHDLR_SSPND_DONE;
	    }
	    else /* if (no < online) */ {
		if (p->scheduler_data->no <= no) {
		    res = ERTS_SCHDLR_SSPND_DONE;
		    schdlr_sspnd.wait_curr_online = no;
		}
		else {
		    /*
		     * Yield! Current process needs to migrate
		     * before bif returns.
		     */
		    res = ERTS_SCHDLR_SSPND_YIELD_DONE;
		    schdlr_sspnd.wait_curr_online = no+1;
		}

		if (ongoing_multi_scheduling_block()) {
		    for (ix = no; ix < online; ix++)
			erts_sched_poke(ERTS_SCHED_SLEEP_INFO_IX(ix));
		}
		else if (erts_common_run_queue) {
		    for (ix = no; ix < online; ix++) {
			ErtsSchedulerSleepInfo *ssi;
			ssi = ERTS_SCHED_SLEEP_INFO_IX(ix);
			erts_smp_atomic32_read_bor_nob(&ssi->flags,
						       ERTS_SSI_FLG_SUSPENDED);
		    }
		    wake_all_schedulers();
		}
		else {
		    if (plocks) {
			have_unlocked_plocks = 1;
			erts_smp_proc_unlock(p, plocks);
		    }
		    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		    erts_smp_mtx_lock(&balance_info.update_mtx);
		    
		    for (ix = 0; ix < online; ix++) {
			ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
			erts_smp_runq_lock(rq);
			ERTS_RUNQ_RESET_MIGRATION_PATHS(rq, 0x6);
			erts_smp_runq_unlock(rq);
		    }
		    /*
		     * Evacutation order important! Newly suspended run queues
		     * has to be evacuated last.
		     */
		    for (ix = erts_no_run_queues-1; ix >= no; ix--)
			evacuate_run_queue(ERTS_RUNQ_IX(ix),
					   ERTS_RUNQ_IX(ix % no));
		    set_no_used_runqs(no);
		    erts_smp_mtx_unlock(&balance_info.update_mtx);
		    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
		    for (ix = no; ix < online; ix++) {
			ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
			wake_scheduler(rq, 0, 1);
		    }
		}
	    }

	    if (schdlr_sspnd.curr_online != schdlr_sspnd.wait_curr_online) {
		erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		if (plocks && !have_unlocked_plocks) {
		    have_unlocked_plocks = 1;
		    erts_smp_proc_unlock(p, plocks);
		}
		erts_thr_progress_active(esdp, 0);
		erts_thr_progress_prepare_wait(esdp);
		end_wait = 1;
		erts_smp_mtx_lock(&schdlr_sspnd.mtx);
	    }

	    while (schdlr_sspnd.curr_online != schdlr_sspnd.wait_curr_online)
		erts_smp_cnd_wait(&schdlr_sspnd.cnd, &schdlr_sspnd.mtx);

	    ASSERT(res != ERTS_SCHDLR_SSPND_DONE
		   ? (ERTS_SCHDLR_SSPND_CHNG_WAITER
		      & erts_smp_atomic32_read_nob(&schdlr_sspnd.changing))
		   : (ERTS_SCHDLR_SSPND_CHNG_WAITER
		      == erts_smp_atomic32_read_nob(&schdlr_sspnd.changing)));
	    erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
					    ~ERTS_SCHDLR_SSPND_CHNG_WAITER);

	}
    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    if (end_wait) {
	erts_thr_progress_finalize_wait(esdp);
	erts_thr_progress_active(esdp, 1);
    }
    if (have_unlocked_plocks)
	erts_smp_proc_lock(p, plocks);

    return res;
}

ErtsSchedSuspendResult
erts_block_multi_scheduling(Process *p, ErtsProcLocks plocks, int on, int all)
{
    int ix, res, have_unlocked_plocks = 0;
    erts_aint32_t changing;
    ErtsProcList *plp;

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    changing = erts_smp_atomic32_read_nob(&schdlr_sspnd.changing);
    if (changing) {
	res = ERTS_SCHDLR_SSPND_YIELD_RESTART; /* Yield */
    }
    else if (on) { /* ------ BLOCK ------ */
	if (schdlr_sspnd.msb.procs) {
	    plp = proclist_create(p);
	    plp->next = schdlr_sspnd.msb.procs;
	    schdlr_sspnd.msb.procs = plp;
	    p->flags |= F_HAVE_BLCKD_MSCHED;
	    ASSERT(erts_smp_atomic32_read_nob(&schdlr_sspnd.active) == 1);
	    ASSERT(p->scheduler_data->no == 1);
	    res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
	}
	else {
	    int online = schdlr_sspnd.online;
	    p->flags |= F_HAVE_BLCKD_MSCHED;
	    if (plocks) {
		have_unlocked_plocks = 1;
		erts_smp_proc_unlock(p, plocks);
	    }
	    ASSERT(!ongoing_multi_scheduling_block());
	    schdlr_sspnd.msb.ongoing = 1;
	    if (online == 1) {
		res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
		ASSERT(erts_smp_atomic32_read_nob(&schdlr_sspnd.active) == 1);
		ASSERT(p->scheduler_data->no == 1);
	    }
	    else {
		ERTS_SCHDLR_SSPND_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_MSB
					    | ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);
		if (p->scheduler_data->no == 1) {
		    res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
		    schdlr_sspnd.msb.wait_active = 1;
		}
		else {
		    /*
		     * Yield! Current process needs to migrate
		     * before bif returns.
		     */
		    res = ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED;
		    schdlr_sspnd.msb.wait_active = 2;
		}
		if (erts_common_run_queue) {
		    for (ix = 1; ix < online; ix++)
			erts_smp_atomic32_read_bor_nob(&ERTS_SCHED_SLEEP_INFO_IX(ix)->flags,
						       ERTS_SSI_FLG_SUSPENDED);
		    wake_all_schedulers();
		}
		else {
		    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		    erts_smp_mtx_lock(&balance_info.update_mtx);
		    set_no_used_runqs(1);
		    for (ix = 0; ix < online; ix++) {
			ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
			erts_smp_runq_lock(rq);
			ASSERT(!(rq->flags & ERTS_RUNQ_FLG_SUSPENDED));
			ERTS_RUNQ_RESET_MIGRATION_PATHS(rq, 0x7);
			erts_smp_runq_unlock(rq);
		    }
		    /*
		     * Evacuate all activities in all other run queues
		     * into the first run queue. Note order is important,
		     * online run queues has to be evacuated last.
		     */
		    for (ix = erts_no_run_queues-1; ix >= 1; ix--)
			evacuate_run_queue(ERTS_RUNQ_IX(ix), ERTS_RUNQ_IX(0));
		    erts_smp_mtx_unlock(&balance_info.update_mtx);
		    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
		}

		if (erts_smp_atomic32_read_nob(&schdlr_sspnd.active)
		    != schdlr_sspnd.msb.wait_active) {
		    ErtsSchedulerData *esdp;

		    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

		    if (plocks && !have_unlocked_plocks) {
			have_unlocked_plocks = 1;
			erts_smp_proc_unlock(p, plocks);
		    }

		    esdp = ERTS_PROC_GET_SCHDATA(p);

		    erts_thr_progress_active(esdp, 0);
		    erts_thr_progress_prepare_wait(esdp);

		    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

		    while (erts_smp_atomic32_read_nob(&schdlr_sspnd.active)
			   != schdlr_sspnd.msb.wait_active)
			erts_smp_cnd_wait(&schdlr_sspnd.cnd,
					  &schdlr_sspnd.mtx);
		    
		    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		    
		    erts_thr_progress_active(esdp, 1);
		    erts_thr_progress_finalize_wait(esdp);

		    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

		}
		ASSERT(res != ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED
		       ? (ERTS_SCHDLR_SSPND_CHNG_WAITER
			  & erts_smp_atomic32_read_nob(&schdlr_sspnd.changing))
		       : (ERTS_SCHDLR_SSPND_CHNG_WAITER
			  == erts_smp_atomic32_read_nob(&schdlr_sspnd.changing)));
		erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
						~ERTS_SCHDLR_SSPND_CHNG_WAITER);
	    }
	    plp = proclist_create(p);
	    plp->next = schdlr_sspnd.msb.procs;
	    schdlr_sspnd.msb.procs = plp;
#ifdef DEBUG
	    ERTS_FOREACH_RUNQ(srq,
	    {
		if (srq != ERTS_RUNQ_IX(0)) {
		    ASSERT(ERTS_EMPTY_RUNQ(srq));
		    ASSERT(srq->flags & ERTS_RUNQ_FLG_SUSPENDED);
		}
	    });
#endif
	    ASSERT(p->scheduler_data);
	}
    }
    else if (!ongoing_multi_scheduling_block()) {
	/* unblock not ongoing */
	ASSERT(!schdlr_sspnd.msb.procs);
	res = ERTS_SCHDLR_SSPND_DONE;
    }
    else {  /* ------ UNBLOCK ------ */
	if (p->flags & F_HAVE_BLCKD_MSCHED) {
	    ErtsProcList **plpp = &schdlr_sspnd.msb.procs;
	    plp = schdlr_sspnd.msb.procs;

	    while (plp) {
		if (!proclist_same(plp, p)){
		    plpp = &plp->next;
		    plp = plp->next;
		}
		else {
		    *plpp = plp->next;
		    proclist_destroy(plp);
		    if (!all)
			break;
		    plp = *plpp;
		}
	    }
	}
	if (schdlr_sspnd.msb.procs)
	    res = ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED;
	else {
	    ERTS_SCHDLR_SSPND_CHNG_SET(ERTS_SCHDLR_SSPND_CHNG_MSB, 0);
#ifdef DEBUG
	    ERTS_FOREACH_RUNQ(rq,
	    {
		if (rq != p->scheduler_data->run_queue) {
		    if (!ERTS_EMPTY_RUNQ(rq)) {
			Process *rp;
			int pix;
			ASSERT(rq->ports.info.len == 0);
			for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
			    for (rp = rq->procs.prio[pix].first;
				 rp;
				 rp = rp->next) {
				ASSERT(rp->bound_runq);
			    }
			}
		    }

		    ASSERT(rq->flags & ERTS_RUNQ_FLG_SUSPENDED);
		}
	    });
#endif
	    p->flags &= ~F_HAVE_BLCKD_MSCHED;
	    schdlr_sspnd.msb.ongoing = 0;
	    if (schdlr_sspnd.online == 1) {
		/* No schedulers to resume */
		ASSERT(erts_smp_atomic32_read_nob(&schdlr_sspnd.active) == 1);
		ERTS_SCHDLR_SSPND_CHNG_SET(0, ERTS_SCHDLR_SSPND_CHNG_MSB);
	    }
	    else if (erts_common_run_queue) {
		for (ix = 1; ix < schdlr_sspnd.online; ix++)
		    erts_smp_atomic32_read_band_nob(&ERTS_SCHED_SLEEP_INFO_IX(ix)->flags,
						    ~ERTS_SSI_FLG_SUSPENDED);
		wake_all_schedulers();
	    }
	    else {
		int online = schdlr_sspnd.online;
		erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
		if (plocks) {
		    have_unlocked_plocks = 1;
		    erts_smp_proc_unlock(p, plocks);
		}
		erts_smp_mtx_lock(&balance_info.update_mtx);

		/* Resume all online run queues */
		for (ix = 1; ix < online; ix++) {
		    ErtsRunQueue *rq = ERTS_RUNQ_IX(ix);
		    erts_smp_runq_lock(rq);
		    ERTS_RUNQ_RESET_SUSPEND_INFO(rq, 0x4);
		    erts_smp_runq_unlock(rq);
		    scheduler_ix_resume_wake(ix);
		}

		/* Spread evacuation paths among all online run queues */
		for (ix = online; ix < erts_no_run_queues; ix++)
		    evacuate_run_queue(ERTS_RUNQ_IX(ix),
				       ERTS_RUNQ_IX(ix % online));

		set_no_used_runqs(online);
		/* Make sure that we balance soon... */
		balance_info.forced_check_balance = 1;
		erts_smp_runq_lock(ERTS_RUNQ_IX(0));
		ERTS_RUNQ_IX(0)->check_balance_reds = 0;
		erts_smp_runq_unlock(ERTS_RUNQ_IX(0));
		erts_smp_mtx_unlock(&balance_info.update_mtx);
		erts_smp_mtx_lock(&schdlr_sspnd.mtx);
	    }
	    res = ERTS_SCHDLR_SSPND_DONE;
	}
    }

    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    if (have_unlocked_plocks)
	erts_smp_proc_lock(p, plocks);
    return res;
}

#ifdef DEBUG
void
erts_dbg_multi_scheduling_return_trap(Process *p, Eterm return_value)
{
    if (return_value == am_blocked) {
	erts_aint32_t active = erts_smp_atomic32_read_nob(&schdlr_sspnd.active);
	ASSERT(1 <= active && active <= 2);
	ASSERT(ERTS_PROC_GET_SCHDATA(p)->no == 1);
    }
}
#endif

int
erts_is_multi_scheduling_blocked(void)
{
    int res;
    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    res = schdlr_sspnd.msb.procs != NULL;
    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);
    return res;
}

Eterm
erts_multi_scheduling_blockers(Process *p)
{
    Eterm res = NIL;

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);
    if (schdlr_sspnd.msb.procs) {
	Eterm *hp, *hp_end;
	ErtsProcList *plp1, *plp2;
	Uint max_size;
	ASSERT(schdlr_sspnd.msb.procs);
	for (max_size = 0, plp1 = schdlr_sspnd.msb.procs;
	     plp1;
	     plp1 = plp1->next) {
	    max_size += 2;
	}
	ASSERT(max_size);
	hp = HAlloc(p, max_size);
	hp_end = hp + max_size;
	for (plp1 = schdlr_sspnd.msb.procs; plp1; plp1 = plp1->next) {
	    for (plp2 = schdlr_sspnd.msb.procs;
		 plp2->pid != plp1->pid;
		 plp2 = plp2->next);
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
    ERTS_SCHED_SLEEP_INFO_IX(no - 1)->event = erts_tse_fetch();
    callbacks.arg = (void *) esdp->ssi;
    callbacks.wakeup = thr_prgr_wakeup;
    callbacks.prepare_wait = thr_prgr_prep_wait;
    callbacks.wait = thr_prgr_wait;
    callbacks.finalize_wait = thr_prgr_fin_wait;

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

    if (no == 1) {
	erts_thr_progress_active(esdp, 0);
	erts_thr_progress_prepare_wait(esdp);
    }

    erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    ASSERT(erts_smp_atomic32_read_nob(&schdlr_sspnd.changing)
	   & ERTS_SCHDLR_SSPND_CHNG_ONLN);

    if (--schdlr_sspnd.curr_online == schdlr_sspnd.wait_curr_online) {
	erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing,
					~ERTS_SCHDLR_SSPND_CHNG_ONLN);
	if (no != 1)
	    erts_smp_cnd_signal(&schdlr_sspnd.cnd);
    }

    if (no == 1) {
	while (schdlr_sspnd.curr_online != schdlr_sspnd.wait_curr_online)
	    erts_smp_cnd_wait(&schdlr_sspnd.cnd, &schdlr_sspnd.mtx);
	ERTS_SCHDLR_SSPND_CHNG_SET(0, ERTS_SCHDLR_SSPND_CHNG_WAITER);
    }
    erts_smp_mtx_unlock(&schdlr_sspnd.mtx);

    if (no == 1) {
	erts_thr_progress_finalize_wait(esdp);
	erts_thr_progress_active(esdp, 1);
    }

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    esdp->verify_unused_temp_alloc
	= erts_alloc_get_verify_unused_temp_alloc(
	    &esdp->verify_unused_temp_alloc_data);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif

    process_main();
    /* No schedulers should *ever* terminate */
    erl_exit(ERTS_ABORT_EXIT,
	     "Scheduler thread number %beu terminated\n",
	     no);
    return NULL;
}

static ethr_tid aux_tid;

void
erts_start_schedulers(void)
{
    int res = 0;
    Uint actual = 0;
    Uint wanted = erts_no_schedulers;
    Uint wanted_no_schedulers = erts_no_schedulers;
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;

    opts.detached = 1;
    opts.suggested_stack_size = erts_sched_thread_suggested_stack_size;

    if (wanted < 1)
	wanted = 1;
    if (wanted > ERTS_MAX_NO_OF_SCHEDULERS) {
	wanted = ERTS_MAX_NO_OF_SCHEDULERS;
	res = ENOTSUP;
    }

    while (actual < wanted) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(actual);
	actual++;
	ASSERT(actual == esdp->no);
	res = ethr_thr_create(&esdp->tid,sched_thread_func,(void*)esdp,&opts);
	if (res != 0) {
	    actual--;
	    break;
	}
    }
    
    erts_no_schedulers = actual;

    ERTS_THR_MEMORY_BARRIER;

    res = ethr_thr_create(&aux_tid, aux_thread, NULL, &opts);
    if (res != 0)
	erl_exit(1, "Failed to create aux thread\n");

    if (actual < 1)
	erl_exit(1,
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
#if defined(ARCH_64) && !HALFWORD_HEAP
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
	    ErtsRunQueue *rq = erts_get_runq_proc(suspendee);
	    erts_smp_runq_lock(rq);
	    suspend_process(rq, suspendee);
	    erts_smp_runq_unlock(rq);
	    suspender->suspendee = suspendee->id;
	}
	/* suspender is suspended waiting for suspendee to suspend;
	   resume suspender */
	resume_process(suspender);
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

    if (c_p->id == pid)
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
	    resume_process(rp);
    }
    else {
	ErtsRunQueue *cp_rq, *rp_rq;

	rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
			   pid, ERTS_PROC_LOCK_STATUS);

	if (!rp) {
	    c_p->flags &= ~F_P2PNR_RESCHED;
	    goto done;
	}

	ASSERT(!(c_p->flags & F_P2PNR_RESCHED));

	cp_rq = erts_get_runq_proc(c_p);
	rp_rq = erts_get_runq_proc(rp);
	erts_smp_runqs_lock(cp_rq, rp_rq);
	if (rp->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING) {
	running:
	    /* Phiu... */

	    /*
	     * If we got pending suspenders and suspend ourselves waiting
	     * to suspend another process we might deadlock.
	     * In this case we have to yield, be suspended by
	     * someone else and then do it all over again.
	     */
	    if (!c_p->pending_suspenders) {
		/* Mark rp pending for suspend by c_p */
		add_pend_suspend(rp, c_p->id, handle_pend_sync_suspend);
		ASSERT(is_nil(c_p->suspendee));

		/* Suspend c_p; when rp is suspended c_p will be resumed. */
		suspend_process(cp_rq, c_p);
		c_p->flags |= F_P2PNR_RESCHED;
	    }
	    /* Yield (caller is assumed to yield immediately in bif). */
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	    rp = ERTS_PROC_LOCK_BUSY;
	}
	else {
	    ErtsProcLocks need_locks = pid_locks & ~ERTS_PROC_LOCK_STATUS;
	    if (need_locks && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		erts_smp_runqs_unlock(cp_rq, rp_rq);
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
				   pid, pid_locks|ERTS_PROC_LOCK_STATUS);
		if (!rp)
		    goto done;
		/* run-queues may have changed */
		cp_rq = erts_get_runq_proc(c_p);
		rp_rq = erts_get_runq_proc(rp);
		erts_smp_runqs_lock(cp_rq, rp_rq);
		if (rp->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING) {
		    /* Ahh... */
		    erts_smp_proc_unlock(rp,
					 pid_locks & ~ERTS_PROC_LOCK_STATUS);
		    goto running;
		}
	    }

	    /* rp is not running and we got the locks we want... */
	    if (suspend)
		suspend_process(rp_rq, rp);
	}
	erts_smp_runqs_unlock(cp_rq, rp_rq);
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

static ERTS_INLINE void
do_bif_suspend_process(ErtsSuspendMonitor *smon,
		       Process *suspendee,
		       ErtsRunQueue *locked_runq)
{
    ASSERT(suspendee);
    ASSERT(!suspendee->is_exiting);
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
		       & erts_proc_lc_my_proc_locks(suspendee));
    if (smon) {
	if (!smon->active) {
	    ErtsRunQueue *rq;

	    if (locked_runq)
		rq = locked_runq;
	    else {
		rq = erts_get_runq_proc(suspendee);
		erts_smp_runq_lock(rq);
	    }

	    suspend_process(rq, suspendee);

	    if (!locked_runq)
		erts_smp_runq_unlock(rq);
	}
	smon->active += smon->pending;
	ASSERT(smon->active);
	smon->pending = 0;
    }
    
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
					suspendee->id);
	else {
	    ErtsSuspendMonitor *smon;
	    smon = erts_lookup_suspend_monitor(suspender->suspend_monitors,
					       suspendee->id);
	    do_bif_suspend_process(smon, suspendee, NULL);
	    suspender->suspendee = suspendee->id;
	}
	/* suspender is suspended waiting for suspendee to suspend;
	   resume suspender */
	resume_process(suspender);
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
					suspendee->id);
	else {
	    ErtsSuspendMonitor *smon;
	    smon = erts_lookup_suspend_monitor(suspender->suspend_monitors,
					       suspendee->id);
	    do_bif_suspend_process(smon, suspendee, NULL);
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


    if (BIF_P->id == BIF_ARG_1)
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
	suspend_process(erts_common_run_queue, suspendee);
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
		ErtsRunQueue *rq;
		if (smon->pending == INT_MAX)
		    goto system_limit;

		smon->pending++;
		rq = erts_get_runq_proc(suspendee);
		erts_smp_runq_lock(rq);

		if (suspendee->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING)
		    add_pend_suspend(suspendee,
				     BIF_P->id,
				     handle_pend_bif_async_suspend);
		else
		    do_bif_suspend_process(smon, suspendee, rq);
		erts_smp_runq_unlock(rq);

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
	    ErtsRunQueue *cp_rq, *s_rq;
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

	    cp_rq = erts_get_runq_proc(BIF_P);
	    s_rq = erts_get_runq_proc(suspendee);
	    erts_smp_runqs_lock(cp_rq, s_rq);
	    if (!(suspendee->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING)) {
		do_bif_suspend_process(smon, suspendee, s_rq);
		erts_smp_runqs_unlock(cp_rq, s_rq);
		res = (!unless_suspending || smon->active == 1
		       ? am_true
		       : am_false);
		/* done */
	    }
	    else {
		/* Mark suspendee pending for suspend by BIF_P */
		add_pend_suspend(suspendee,
				 BIF_P->id,
				 handle_pend_bif_sync_suspend);

		ASSERT(is_nil(BIF_P->suspendee));

		/*
		 * Suspend BIF_P; when suspendee is suspended, BIF_P
		 * will be resumed and this BIF will be called again.
		 * This time with BIF_P->suspendee == BIF_ARG_1 (see
		 * above).
		 */
		suspend_process(cp_rq, BIF_P);
		erts_smp_runqs_unlock(cp_rq, s_rq);
		goto yield;
	    }
	}
	/* --- Synchronous suspend end ------------------------------------- */
    }

#endif /* ERTS_SMP */

    ASSERT(suspendee->status == P_SUSPENDED || (asynchronous && smon->pending));
    ASSERT(suspendee->status == P_SUSPENDED || !smon->active);

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
 
    if (BIF_P->id == BIF_ARG_1)
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

	ASSERT(suspendee->status == P_SUSPENDED
	       || (suspendee->status == P_GARBING
		   && suspendee->gcstatus == P_SUSPENDED));
	resume_process(suspendee);

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

Uint
erts_run_queues_len(Uint *qlen)
{
    int i = 0;
    Uint len = 0;
    ERTS_ATOMIC_FOREACH_RUNQ(rq,
    {
	if (qlen)
	    qlen[i++] = rq->procs.len;
	len += rq->procs.len;
    }
	);
    return len;
}

#ifdef HARDDEBUG_RUNQS
static void
check_procs_runq(ErtsRunQueue *runq, Process *p_in_q, Process *p_not_in_q)
{
    int len[ERTS_NO_PROC_PRIO_LEVELS] = {0};
    int tot_len;
    int prioq, prio;
    int found_p_in_q;
    Process *p, *prevp;

    found_p_in_q = 0;
    for (prioq = 0; prioq < ERTS_NO_PROC_PRIO_LEVELS - 1; prioq++) {
	prevp = NULL;
	for (p = runq->procs.prio[prioq].first; p; p = p->next) {
	    ASSERT(p != p_not_in_q);
	    if (p == p_in_q)
		found_p_in_q = 1;
	    switch (p->prio) {
	    case PRIORITY_MAX:
	    case PRIORITY_HIGH:
	    case PRIORITY_NORMAL:
		ASSERT(prioq == p->prio);
		break;
	    case PRIORITY_LOW:
		ASSERT(prioq == PRIORITY_NORMAL);
		break;
	    default:
		ASSERT(!"Bad prio on process");
	    }
	    len[p->prio]++;
	    ASSERT(prevp == p->prev);
	    if (p->prev) {
		ASSERT(p->prev->next == p);
	    }
	    else {
		ASSERT(runq->procs.prio[prioq].first == p);
	    }
	    if (p->next) {
		ASSERT(p->next->prev == p);
	    }
	    else {
		ASSERT(runq->procs.prio[prioq].last == p);
	    }
	    ASSERT(p->run_queue == runq);
	    prevp = p;
	}
    }

    ASSERT(!p_in_q || found_p_in_q);

    tot_len = 0;
    for (prio = 0; prio < ERTS_NO_PROC_PRIO_LEVELS; prio++) {
	ASSERT(len[prio] == runq->procs.prio_info[prio].len);
	if (len[prio]) {
	    ASSERT(runq->flags & (1 << prio));
	}
	else {
	    ASSERT(!(runq->flags & (1 << prio)));
	}
	tot_len += len[prio];
    }
    ASSERT(runq->procs.len == tot_len);    
}
#  define ERTS_DBG_CHK_PROCS_RUNQ(RQ) check_procs_runq((RQ), NULL, NULL)
#  define ERTS_DBG_CHK_PROCS_RUNQ_PROC(RQ, P) check_procs_runq((RQ), (P), NULL)
#  define ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(RQ, P) check_procs_runq((RQ), NULL, (P))
#else
#  define ERTS_DBG_CHK_PROCS_RUNQ(RQ)
#  define ERTS_DBG_CHK_PROCS_RUNQ_PROC(RQ, P)
#  define ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(RQ, P)
#endif


static ERTS_INLINE void
enqueue_process(ErtsRunQueue *runq, Process *p)
{
    ErtsRunPrioQueue *rpq;
    ErtsRunQueueInfo *rqi;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    ASSERT(p->bound_runq || !(runq->flags & ERTS_RUNQ_FLG_SUSPENDED));

    rqi = &runq->procs.prio_info[p->prio];
    rqi->len++;
    if (rqi->max_len < rqi->len)
	rqi->max_len = rqi->len;

    runq->procs.len++;
    runq->len++;
    if (runq->max_len < runq->len)
	runq->max_len = runq->len;

    runq->flags |= (1 << p->prio);

    rpq = (p->prio == PRIORITY_LOW
	   ? &runq->procs.prio[PRIORITY_NORMAL]
	   : &runq->procs.prio[p->prio]);

    p->next = NULL;
    p->prev = rpq->last;
    if (rpq->last)
	rpq->last->next = p;
    else
	rpq->first = p;
    rpq->last = p;

    switch (p->status) {
    case P_EXITING:
	break;
    case P_GARBING:
	p->gcstatus = P_RUNABLE;
	break;
    default:
	p->status = P_RUNABLE;
	break;
    }

#ifdef ERTS_SMP
    p->status_flags |= ERTS_PROC_SFLG_INRUNQ;
#endif

    ERTS_DBG_CHK_PROCS_RUNQ_PROC(runq, p);
}


static ERTS_INLINE int
dequeue_process(ErtsRunQueue *runq, Process *p)
{
    ErtsRunPrioQueue *rpq;
    int res = 1;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    ERTS_DBG_CHK_PROCS_RUNQ(runq);

    rpq = &runq->procs.prio[p->prio == PRIORITY_LOW ? PRIORITY_NORMAL : p->prio];
    if (p->prev) {
	p->prev->next = p->next;
    }
    else if (rpq->first == p) {
	rpq->first = p->next;
    }
    else {
	res = 0;
    }
    if (p->next) {
	p->next->prev = p->prev;
    }
    else if (rpq->last == p) {
	rpq->last = p->prev;
    }
    else {
	ASSERT(res == 0);
    }

    if (res) {

	if (--runq->procs.prio_info[p->prio].len == 0)
	    runq->flags &= ~(1 << p->prio);
	runq->procs.len--;
	runq->len--;

#ifdef ERTS_SMP
	p->status_flags &= ~ERTS_PROC_SFLG_INRUNQ;
#endif
    }

    ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(runq, p);
    return res;
}

/* schedule a process */
static ERTS_INLINE ErtsRunQueue *
internal_add_to_runq(ErtsRunQueue *runq, Process *p)
{
    Uint32 prev_status = p->status;
    ErtsRunQueue *add_runq;
#ifdef ERTS_SMP

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(runq));

    if (p->status_flags & ERTS_PROC_SFLG_INRUNQ)
	return NULL;
    else if (p->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING) {
	ASSERT(ERTS_PROC_IS_EXITING(p) || p->rcount == 0);
	ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(runq, p);
	p->status_flags |= ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	return NULL;
    }
    ASSERT(!p->scheduler_data);
#endif

    ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(runq, p);
#ifndef ERTS_SMP
    /* Never schedule a suspended process (ok in smp case) */
    ASSERT(ERTS_PROC_IS_EXITING(p) || p->rcount == 0);
    add_runq = runq;
#else
    ASSERT(!p->bound_runq || p->bound_runq == p->run_queue);
    if (p->bound_runq) {
	if (p->bound_runq == runq)
	    add_runq = runq;
	else {
	    add_runq = p->bound_runq;
	    erts_smp_xrunq_lock(runq, add_runq);
	}
    }
    else {
	add_runq = erts_check_emigration_need(runq, p->prio);
	if (!add_runq)
	    add_runq = runq;
	else /* Process emigrated */
	    p->run_queue = add_runq;
    }
#endif

    /* Enqueue the process */
    enqueue_process(add_runq, p);

    if ((erts_system_profile_flags.runnable_procs)
	&& (prev_status == P_WAITING
	    || prev_status == P_SUSPENDED)) {
    	profile_runnable_proc(p, am_active);
    }

    if (add_runq != runq)
	erts_smp_runq_unlock(add_runq);

    return add_runq;
}


void
erts_add_to_runq(Process *p)
{
    ErtsRunQueue *notify_runq;
    ErtsRunQueue *runq = erts_get_runq_proc(p);
    erts_smp_runq_lock(runq);
    notify_runq = internal_add_to_runq(runq, p);
    erts_smp_runq_unlock(runq);
    smp_notify_inc_runq(notify_runq);

}

/* Possibly remove a scheduled process we need to suspend */

static int
remove_proc_from_runq(ErtsRunQueue *rq, Process *p, int to_inactive)
{
    int res;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

#ifdef ERTS_SMP
    if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ) {
	p->status_flags &= ~ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	ASSERT(!remove_proc_from_runq(rq, p, 0));
	return 1;
    }
#endif

    res = dequeue_process(rq, p);

    if (res && erts_system_profile_flags.runnable_procs && to_inactive)
	profile_runnable_proc(p, am_inactive);

#ifdef ERTS_SMP
    ASSERT(!(p->status_flags & ERTS_PROC_SFLG_INRUNQ));
#endif

    return res;
}

#ifdef ERTS_SMP

ErtsMigrateResult
erts_proc_migrate(Process *p, ErtsProcLocks *plcks,
		  ErtsRunQueue *from_rq, int *from_locked,
		  ErtsRunQueue *to_rq, int *to_locked)
{
    ERTS_SMP_LC_ASSERT(*plcks == erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT((ERTS_PROC_LOCK_STATUS & *plcks)
		       || from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(from_rq, *from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(to_rq, *to_locked);

    ASSERT(!erts_common_run_queue);
    
    /*
     * If we have the lock on the run queue to migrate to,
     * check that it isn't suspended. If it is suspended,
     * we will refuse to migrate to it anyway.
     */
    if (*to_locked && (to_rq->flags & ERTS_RUNQ_FLG_SUSPENDED))
	return ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED;

    /* We need status lock on process and locks on both run queues */

    if (!(ERTS_PROC_LOCK_STATUS & *plcks)) {
	if (erts_smp_proc_trylock(p, ERTS_PROC_LOCK_STATUS) == EBUSY) {
	    ErtsProcLocks lcks = *plcks;
	    Eterm pid = p->id;
	    Process *proc = *plcks ? p : NULL;

	    if (*from_locked) {
		*from_locked = 0;
		erts_smp_runq_unlock(from_rq);
	    }
	    if (*to_locked) {
		*to_locked = 0;
		erts_smp_runq_unlock(to_rq);
	    }

	    proc = erts_pid2proc_opt(proc,
				     lcks,
				     pid,
				     lcks|ERTS_PROC_LOCK_STATUS,
				     ERTS_P2P_FLG_ALLOW_OTHER_X);
	    if (!proc) {
		*plcks = 0;
		return ERTS_MIGRATE_FAILED_NOT_IN_RUNQ;
	    }
	    ASSERT(proc == p);
	}
	*plcks |= ERTS_PROC_LOCK_STATUS;
    }

    ASSERT(!p->bound_runq);

    ERTS_SMP_LC_CHK_RUNQ_LOCK(from_rq, *from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(to_rq, *to_locked);

    if (p->run_queue != from_rq)
	return ERTS_MIGRATE_FAILED_RUNQ_CHANGED;

    if (!*from_locked || !*to_locked) {
	if (from_rq < to_rq) {
	    if (!*to_locked) {
		if (!*from_locked)
		    erts_smp_runq_lock(from_rq);
		erts_smp_runq_lock(to_rq);
	    }
	    else if (erts_smp_runq_trylock(from_rq) == EBUSY) {
		erts_smp_runq_unlock(to_rq);
		erts_smp_runq_lock(from_rq);
		erts_smp_runq_lock(to_rq);
	    }
	}
	else {
	    if (!*from_locked) {
		if (!*to_locked)
		    erts_smp_runq_lock(to_rq);
		erts_smp_runq_lock(from_rq);
	    }
	    else if (erts_smp_runq_trylock(to_rq) == EBUSY) {
		erts_smp_runq_unlock(from_rq);
		erts_smp_runq_lock(to_rq);
		erts_smp_runq_lock(from_rq);
	    }
	}
	*to_locked = *from_locked = 1;
    }

    ERTS_SMP_LC_CHK_RUNQ_LOCK(from_rq, *from_locked);
    ERTS_SMP_LC_CHK_RUNQ_LOCK(to_rq, *to_locked);

    /* Ok we now got all locks we need; do it... */

    /* Refuse to migrate to a suspended run queue */
    if (to_rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
	return ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED;

    if ((p->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING)
	|| !(p->status_flags & ERTS_PROC_SFLG_INRUNQ))
	return ERTS_MIGRATE_FAILED_NOT_IN_RUNQ;

    dequeue_process(from_rq, p);
    p->run_queue = to_rq;
    enqueue_process(to_rq, p);

    return ERTS_MIGRATE_SUCCESS;
}
#endif /* ERTS_SMP */

Eterm
erts_process_status(Process *c_p, ErtsProcLocks c_p_locks,
		    Process *rp, Eterm rpid)
{
    Eterm res = am_undefined;
    Process *p;

    if (rp) {
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
			   & erts_proc_lc_my_proc_locks(rp));
	p = rp;
    }
    else {
	p = erts_pid2proc_opt(c_p, c_p_locks,
			      rpid, ERTS_PROC_LOCK_STATUS,
			      ERTS_P2P_FLG_ALLOW_OTHER_X);
    }

    if (p) {
	switch (p->status) {
	case P_RUNABLE:
	    res = am_runnable;
	    break;
	case P_WAITING:
	    res = am_waiting;
	    break;
	case P_RUNNING:
	    res = am_running;
	    break;
	case P_EXITING:
	    res = am_exiting;
	    break;
	case P_GARBING:
	    res = am_garbage_collecting;
	    break;
	case P_SUSPENDED:
	    res = am_suspended;
	    break;
	case P_FREE:	/* We cannot look up a process in P_FREE... */
	default:	/* Not a valid status... */
	    erl_exit(1, "Bad status (%b32u) found for process %T\n",
		     p->status, p->id);
	    break;
	}

#ifdef ERTS_SMP
	if (!rp && (p != c_p || !(ERTS_PROC_LOCK_STATUS & c_p_locks)))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }
    else {
	int i;
	ErtsSchedulerData *esdp;

	if (erts_common_run_queue)
	    erts_smp_runq_lock(erts_common_run_queue);
	    
	for (i = 0; i < erts_no_schedulers; i++) {
	    esdp = ERTS_SCHEDULER_IX(i);
	    if (!erts_common_run_queue)
		erts_smp_runq_lock(esdp->run_queue);
	    if (esdp->free_process && esdp->free_process->id == rpid) {
		res = am_free;
		if (!erts_common_run_queue)
		    erts_smp_runq_unlock(esdp->run_queue);
		break;
	    }
	    if (!erts_common_run_queue)
		erts_smp_runq_unlock(esdp->run_queue);
	}

	if (erts_common_run_queue)
	    erts_smp_runq_unlock(erts_common_run_queue);
#endif

    }

    return res;
}

/*
** Suspend a process 
** If we are to suspend on a port the busy_port is the thing
** otherwise busy_port is NIL
*/

void
erts_suspend(Process* process, ErtsProcLocks process_locks, Port *busy_port)
{
    ErtsRunQueue *rq;

    ERTS_SMP_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(process, ERTS_PROC_LOCK_STATUS);

    rq = erts_get_runq_proc(process);

    erts_smp_runq_lock(rq);

    suspend_process(rq, process);

    erts_smp_runq_unlock(rq);

    if (busy_port)
	erts_wake_process_later(busy_port, process);

    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(process, ERTS_PROC_LOCK_STATUS);

}

void
erts_resume(Process* process, ErtsProcLocks process_locks)
{
    ERTS_SMP_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(process, ERTS_PROC_LOCK_STATUS);
    resume_process(process);
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(process, ERTS_PROC_LOCK_STATUS);
}

int
erts_resume_processes(ErtsProcList *plp)
{
    int nresumed = 0;
    while (plp) {
	Process *proc;
	ErtsProcList *fplp;
	ASSERT(is_internal_pid(plp->pid));
	proc = erts_pid2proc(NULL, 0, plp->pid, ERTS_PROC_LOCK_STATUS);
	if (proc) {
	    if (proclist_same(plp, proc)) {
		resume_process(proc);
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
    ErtsRunQueue *rq;
    Eterm value;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    rq = erts_get_runq_proc(p);
    erts_smp_runq_lock(rq);
    switch(p->prio) {
    case PRIORITY_MAX:		value = am_max;			break;
    case PRIORITY_HIGH:		value = am_high;		break;
    case PRIORITY_NORMAL:	value = am_normal;		break;
    case PRIORITY_LOW:		value = am_low;			break;
    default: ASSERT(0);		value = am_undefined;		break;
    }
    erts_smp_runq_unlock(rq);
    return value;
}

Eterm
erts_set_process_priority(Process *p, Eterm new_value)
{
    ErtsRunQueue *rq;
    Eterm old_value;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    rq = erts_get_runq_proc(p);
#ifdef ERTS_SMP
    ASSERT(!(p->status_flags & ERTS_PROC_SFLG_INRUNQ));
#endif
    erts_smp_runq_lock(rq);
    switch(p->prio) {
    case PRIORITY_MAX:		old_value = am_max;		break;
    case PRIORITY_HIGH:		old_value = am_high;		break;
    case PRIORITY_NORMAL:	old_value = am_normal;		break;
    case PRIORITY_LOW:		old_value = am_low;		break;
    default: ASSERT(0);		old_value = am_undefined;	break;
    }
    switch (new_value) {
    case am_max:		p->prio = PRIORITY_MAX;		break;
    case am_high:		p->prio = PRIORITY_HIGH;	break;
    case am_normal:		p->prio = PRIORITY_NORMAL;	break;
    case am_low:		p->prio = PRIORITY_LOW;		break;
    default:			old_value = THE_NON_VALUE;	break;
    }
    erts_smp_runq_unlock(rq);
    return old_value;
}

/* note that P_RUNNING is only set so that we don't try to remove
** running processes from the schedule queue if they exit - a running
** process not being in the schedule queue!! 
** Schedule for up to INPUT_REDUCTIONS context switches,
** return 1 if more to do.
*/

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

Process *schedule(Process *p, int calls)
{
    ErtsRunQueue *rq;
    ErtsRunPrioQueue *rpq;
    erts_aint_t dt;
    ErtsSchedulerData *esdp;
    int context_reds;
    int fcalls;
    int input_reductions;
    int actual_reds;
    int reds;

    if (ERTS_USE_MODIFIED_TIMING()) {
	context_reds = ERTS_MODIFIED_TIMING_CONTEXT_REDS;
	input_reductions = ERTS_MODIFIED_TIMING_INPUT_REDS;
    }
    else {
	context_reds = CONTEXT_REDS;
	input_reductions = INPUT_REDUCTIONS;
    }

    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());

    /*
     * Clean up after the process being scheduled out.
     */
    if (!p) {	/* NULL in the very first schedule() call */
	esdp = erts_get_scheduler_data();
	rq = erts_get_runq_current(esdp);
	ASSERT(esdp);
	fcalls = (int) erts_smp_atomic32_read_acqb(&function_calls);
	actual_reds = reds = 0;
	erts_smp_runq_lock(rq);
    } else {
#ifdef ERTS_SMP
	ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
	esdp = p->scheduler_data;
	ASSERT(esdp->current_process == p
	       || esdp->free_process == p);
#else
	esdp = erts_scheduler_data;
	ASSERT(esdp->current_process == p);
#endif
	reds = actual_reds = calls - esdp->virtual_reds;
	if (reds < ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST)
	    reds = ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST;
	esdp->virtual_reds = 0;

	fcalls = (int) erts_smp_atomic32_add_read_acqb(&function_calls, reds);
	ASSERT(esdp && esdp == erts_get_scheduler_data());

	rq = erts_get_runq_current(esdp);

	p->reds += actual_reds;

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);

	if ((erts_system_profile_flags.runnable_procs)
	    && (p->status == P_WAITING)) {
	    profile_runnable_proc(p, am_inactive);
	}

	if (IS_TRACED(p)) {
	    if (IS_TRACED_FL(p, F_TRACE_CALLS) &&  p->status != P_FREE) {
		erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_OUT);
	    }
	    switch (p->status) {
	    case P_EXITING:
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
		    trace_sched(p, am_out_exiting);
		break;
	    case P_FREE:
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
		    trace_sched(p, am_out_exited);
		break;
	    default:
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED))
		    trace_sched(p, am_out);
		else if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
		    trace_virtual_sched(p, am_out);
		break;
	    }
	}	

#ifdef ERTS_SMP
	if (ERTS_PROC_PENDING_EXIT(p)) {
	    erts_handle_pending_exit(p,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    p->status_flags |= ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	}

	if (p->pending_suspenders) {
	    handle_pending_suspend(p,
				   ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    ASSERT(!(p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ)
		   || p->rcount == 0);
	}
#endif
	erts_smp_runq_lock(rq);

	ERTS_PROC_REDUCTIONS_EXECUTED(rq, p->prio, reds, actual_reds);

	esdp->current_process = NULL;
#ifdef ERTS_SMP
	p->scheduler_data = NULL;
	p->runq_flags &= ~ERTS_PROC_RUNQ_FLG_RUNNING;
	p->status_flags &= ~ERTS_PROC_SFLG_RUNNING;

	if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ) {
	    ErtsRunQueue *notify_runq;
	    p->status_flags &= ~ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	    notify_runq = internal_add_to_runq(rq, p);
	    if (notify_runq != rq)
		smp_notify_inc_runq(notify_runq);
	}
#endif


	if (p->status == P_FREE) {
#ifdef ERTS_SMP
	    ASSERT(esdp->free_process == p);
	    esdp->free_process = NULL;
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    erts_smp_proc_dec_refc(p);
#else	    
	    erts_free_proc(p);
#endif
	} else {
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	}

#ifdef ERTS_SMP
	{
	    ErtsProcList *pnd_xtrs = rq->procs.pending_exiters;
	    rq->procs.pending_exiters = NULL;

	    if (pnd_xtrs) {
		erts_smp_runq_unlock(rq);
		handle_pending_exiters(pnd_xtrs);
		erts_smp_runq_lock(rq);
	    }
		
	}
	ASSERT(!esdp->free_process);
#endif
	ASSERT(!esdp->current_process);

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	dt = erts_do_time_read_and_reset();
	if (dt) {
	    erts_smp_runq_unlock(rq);
	    erts_bump_timer(dt);
	    erts_smp_runq_lock(rq);
	}
	BM_STOP_TIMER(system);

    }

    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());

 check_activities_to_run: {

#ifdef ERTS_SMP

	if (!(rq->flags & ERTS_RUNQ_FLG_SHARED_RUNQ)
	    && rq->check_balance_reds <= 0) {
	    check_balance(rq);
	}

	ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
	ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

	if (rq->flags & ERTS_RUNQ_FLGS_IMMIGRATE_QMASK)
	    immigrate(rq);

 continue_check_activities_to_run:

	if (rq->flags & (ERTS_RUNQ_FLG_SHARED_RUNQ
			 | ERTS_RUNQ_FLG_CHK_CPU_BIND
			 | ERTS_RUNQ_FLG_SUSPENDED)) {
	    if ((rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
		|| (erts_smp_atomic32_read_acqb(&esdp->ssi->flags)
		    & ERTS_SSI_FLG_SUSPENDED)) {
		ASSERT(erts_smp_atomic32_read_nob(&esdp->ssi->flags)
		       & ERTS_SSI_FLG_SUSPENDED);
		suspend_scheduler(esdp);
	    }
	    if ((rq->flags & ERTS_RUNQ_FLG_CHK_CPU_BIND)
		|| erts_smp_atomic32_read_acqb(&esdp->chk_cpu_bind)) {
		erts_sched_check_cpu_bind(esdp);
	    }
	}

	{
	    erts_aint32_t aux_work;
	    int leader_update = erts_thr_progress_update(esdp);
	    aux_work = erts_atomic32_read_acqb(&esdp->ssi->aux_work);
	    if (aux_work | leader_update) {
		erts_smp_runq_unlock(rq);
		if (leader_update)
		    erts_thr_progress_leader_update(esdp);
		if (aux_work)
		    handle_aux_work(&esdp->aux_work_data, aux_work);
		erts_smp_runq_lock(rq);
	    }
	}

	ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
	ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

#else /* ERTS_SMP */
	{
	    erts_aint32_t aux_work;
	    aux_work = erts_atomic32_read_acqb(&esdp->ssi->aux_work);
	    if (aux_work)
		handle_aux_work(&esdp->aux_work_data, aux_work);
	}
#endif /* ERTS_SMP */

	ASSERT(rq->len == rq->procs.len + rq->ports.info.len);

	if (rq->len == 0 && !rq->misc.start) {

#ifdef ERTS_SMP

	    ERTS_SMP_LC_ASSERT(erts_smp_lc_runq_is_locked(rq));

	    rq->wakeup_other = 0;
	    rq->wakeup_other_reds = 0;

	    empty_runq(rq);

	    if (rq->flags & (ERTS_RUNQ_FLG_SHARED_RUNQ
			     | ERTS_RUNQ_FLG_SUSPENDED)) {
		if ((rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
		    || (erts_smp_atomic32_read_acqb(&esdp->ssi->flags)
			& ERTS_SSI_FLG_SUSPENDED)) {
		    ASSERT(erts_smp_atomic32_read_nob(&esdp->ssi->flags)
			   & ERTS_SSI_FLG_SUSPENDED);
		    non_empty_runq(rq);
		    goto continue_check_activities_to_run;
		}
	    }
	    else if (!(rq->flags & ERTS_RUNQ_FLG_INACTIVE)) {
		/*
		 * Check for ERTS_RUNQ_FLG_SUSPENDED has to be done
		 * after trying to steal a task.
		 */
		if (try_steal_task(rq)
		    || (rq->flags & ERTS_RUNQ_FLG_SUSPENDED)) {
		    non_empty_runq(rq);
		    goto continue_check_activities_to_run;
		}
	    }

#endif

	    scheduler_wait(&fcalls, esdp, rq);

#ifdef ERTS_SMP
	    non_empty_runq(rq);
#endif

	    goto check_activities_to_run;
	}
	else if (fcalls > input_reductions && prepare_for_sys_schedule()) {
	    /*
	     * Schedule system-level activities.
	     */

	    erts_smp_atomic32_set_relb(&function_calls, 0);
	    fcalls = 0;

	    ASSERT(!erts_port_task_have_outstanding_io_tasks());

#if 0 /* Not needed since we wont wait in sys schedule */
	    erts_sys_schedule_interrupt(0);
#endif
	    erts_smp_runq_unlock(rq);
	    erl_sys_schedule(1);
	    dt = erts_do_time_read_and_reset();
	    if (dt) erts_bump_timer(dt);
#ifdef ERTS_SMP
	    erts_smp_runq_lock(rq);
	    clear_sys_scheduling();
	    goto continue_check_activities_to_run;
#else
	    goto check_activities_to_run;
#endif
	}

	if (rq->misc.start)
	    exec_misc_ops(rq);

#ifdef ERTS_SMP
	{
	    int wo_reds = rq->wakeup_other_reds;
	    if (wo_reds) {
		if (rq->len < 2) {
		    rq->wakeup_other -= ERTS_WAKEUP_OTHER_DEC*wo_reds;
		    if (rq->wakeup_other < 0)
			rq->wakeup_other = 0;
		}
		else if (rq->wakeup_other < wakeup_other_limit)
		    rq->wakeup_other += rq->len*wo_reds + ERTS_WAKEUP_OTHER_FIXED_INC;
		else {
		    if (erts_common_run_queue) {
			if (erts_common_run_queue->waiting)
			    wake_scheduler(erts_common_run_queue, 0, 1);
		    }
		    else if (erts_smp_atomic32_read_acqb(&no_empty_run_queues) != 0) {
			wake_scheduler_on_empty_runq(rq);
			rq->wakeup_other = 0;
		    }
		    rq->wakeup_other = 0;
		}
	    }
	    rq->wakeup_other_reds = 0;
	}
#endif

	/*
	 * Find a new port to run.
	 */

	if (rq->ports.info.len) {
	    int have_outstanding_io;
	    have_outstanding_io = erts_port_task_execute(rq, &esdp->current_port);
	    if (have_outstanding_io && fcalls > 2*input_reductions) {
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
 pick_next_process:

    ERTS_DBG_CHK_PROCS_RUNQ(rq);

      switch (rq->flags & ERTS_RUNQ_FLGS_PROCS_QMASK) {
	case MAX_BIT:
	case MAX_BIT|HIGH_BIT:
	case MAX_BIT|NORMAL_BIT:
	case MAX_BIT|LOW_BIT:
	case MAX_BIT|HIGH_BIT|NORMAL_BIT:
	case MAX_BIT|HIGH_BIT|LOW_BIT:
	case MAX_BIT|NORMAL_BIT|LOW_BIT:
	case MAX_BIT|HIGH_BIT|NORMAL_BIT|LOW_BIT:
	    rpq = &rq->procs.prio[PRIORITY_MAX];
	    break;
	case HIGH_BIT:
	case HIGH_BIT|NORMAL_BIT:
	case HIGH_BIT|LOW_BIT:
	case HIGH_BIT|NORMAL_BIT|LOW_BIT:
	    rpq = &rq->procs.prio[PRIORITY_HIGH];
	    break;
        case NORMAL_BIT:
	    rpq = &rq->procs.prio[PRIORITY_NORMAL];
	    break;
        case LOW_BIT:
	    rpq = &rq->procs.prio[PRIORITY_NORMAL];
	    break;
	case NORMAL_BIT|LOW_BIT:	  
	    rpq = &rq->procs.prio[PRIORITY_NORMAL];
	    ASSERT(rpq->first != NULL);
	    p = rpq->first;
	    if (p->prio == PRIORITY_LOW) {
		if (p == rpq->last || p->skipped >= RESCHEDULE_LOW-1)
		    p->skipped = 0;
		else {
		    /* skip it */
		    p->skipped++;
		    rpq->first = p->next;
		    rpq->first->prev = NULL;
		    rpq->last->next = p;
		    p->prev = rpq->last;
		    p->next = NULL;
		    rpq->last = p;
		    goto pick_next_process;
		}
	    }
	    break;
        case 0:			/* No process at all */
	default:
	    ASSERT((rq->flags & ERTS_RUNQ_FLGS_PROCS_QMASK) == 0);
	    ASSERT(rq->procs.len == 0);
	    goto check_activities_to_run;
	}

        BM_START_TIMER(system);

	/*
	 * Take the chosen process out of the queue.
	 */
	ASSERT(rpq->first); /* Wrong qmask in rq->flags? */
	p = rpq->first;
#ifdef ERTS_SMP
	ERTS_SMP_LC_ASSERT(rq == p->run_queue);
#endif
	rpq->first = p->next;
	if (!rpq->first)
	    rpq->last = NULL;
	else
	    rpq->first->prev = NULL;

	p->next = p->prev = NULL;

	if (--rq->procs.prio_info[p->prio].len == 0)
	    rq->flags &= ~(1 << p->prio);
	ASSERT(rq->procs.len > 0);
	rq->procs.len--;
	ASSERT(rq->len > 0);
	rq->len--;

	{
	    Uint32 ee_flgs = (ERTS_RUNQ_FLG_EVACUATE(p->prio)
			      | ERTS_RUNQ_FLG_EMIGRATE(p->prio));

	    if ((rq->flags & (ERTS_RUNQ_FLG_SUSPENDED|ee_flgs)) == ee_flgs)
		ERTS_UNSET_RUNQ_FLG_EVACUATE(rq->flags, p->prio);
	}

	ERTS_DBG_CHK_PROCS_RUNQ_NOPROC(rq, p);

	rq->procs.context_switches++;

	esdp->current_process = p;

#ifdef ERTS_SMP
	p->runq_flags |= ERTS_PROC_RUNQ_FLG_RUNNING;
	erts_smp_runq_unlock(rq);

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);

	if (erts_sched_stat.enabled) {
	    UWord old = ERTS_PROC_SCHED_ID(p,
					  (ERTS_PROC_LOCK_MAIN
					   | ERTS_PROC_LOCK_STATUS),
					  (UWord) esdp->no);
	    int migrated = old && old != esdp->no;

	    erts_smp_spin_lock(&erts_sched_stat.lock);
	    erts_sched_stat.prio[p->prio].total_executed++;
	    erts_sched_stat.prio[p->prio].executed++;
	    if (migrated) {
		erts_sched_stat.prio[p->prio].total_migrated++;
		erts_sched_stat.prio[p->prio].migrated++;
	    }
	    erts_smp_spin_unlock(&erts_sched_stat.lock);
	}

	p->status_flags |= ERTS_PROC_SFLG_RUNNING;
	p->status_flags &= ~ERTS_PROC_SFLG_INRUNQ;
	if (ERTS_PROC_PENDING_EXIT(p)) {
	    erts_handle_pending_exit(p,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	}
	ASSERT(!p->scheduler_data);
	p->scheduler_data = esdp;

#endif
	ASSERT(p->status != P_SUSPENDED); /* Never run a suspended process */

        ACTIVATE(p);
	reds = context_reds;

	if (IS_TRACED(p)) {
	    switch (p->status) {
	    case P_EXITING:
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_EXIT))
		    trace_sched(p, am_in_exiting);
		break;
	    default:
		if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED))
		    trace_sched(p, am_in);
		else if (ARE_TRACE_FLAGS_ON(p, F_TRACE_SCHED_PROCS))
		    trace_virtual_sched(p, am_in);
		break;
	    }
	    if (IS_TRACED_FL(p, F_TRACE_CALLS)) {
		erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_IN);
	    }
	}

	if (p->status != P_EXITING)
	    p->status = P_RUNNING;

	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

#ifdef ERTS_SMP
	if (is_not_nil(p->tracer_proc))
	    erts_check_my_tracer_proc(p);
#endif

	if (!ERTS_PROC_IS_EXITING(p)
	    && ((FLAGS(p) & F_FORCE_GC)
		|| (MSO(p).overhead > BIN_VHEAP_SZ(p)))) {
	    reds -= erts_garbage_collect(p, 0, p->arg_reg, p->arity);
	    if (reds < 0) {
		reds = 1;
	    }
	}
	    
	p->fcalls = reds;
	ASSERT(IS_ACTIVE(p));
	ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
	return p;
    }
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
	erts_sched_stat.enabled = 1;
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

    erts_smp_runq_lock(rq);

    while (rq->misc.evac_runq) {
	ErtsRunQueue *tmp_rq = rq->misc.evac_runq;
	erts_smp_runq_unlock(rq);
	rq = tmp_rq;
	erts_smp_runq_lock(rq);
    }

    ASSERT(!(rq->flags & ERTS_RUNQ_FLG_SUSPENDED));

    molp->next = NULL;
    molp->func = func;
    molp->arg = arg;
    if (rq->misc.end)
	rq->misc.end->next = molp;
    else
	rq->misc.start = molp;
    rq->misc.end = molp;
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

/*
 * erts_test_next_pid() is only used for testing.
 */
Sint
erts_test_next_pid(int set, Uint next)
{
    Sint res;
    Sint p_prev;


    erts_smp_mtx_lock(&proc_tab_mtx);

    if (!set) {
	res = p_next < 0 ? -1 : (p_serial << p_serial_shift | p_next);
    }
    else {

	p_serial = (Sint) ((next >> p_serial_shift) & p_serial_mask);
	p_next = (Sint) (erts_process_tab_index_mask & next);

	if (p_next >= erts_max_processes) {
	    p_next = 0;
	    p_serial++;
	    p_serial &= p_serial_mask;
	}

	p_prev = p_next;

	do {
	    if (!process_tab[p_next])
		break;
	    p_next++;
	    if(p_next >= erts_max_processes) {
		p_next = 0;
		p_serial++;
		p_serial &= p_serial_mask;
	    }
	} while (p_prev != p_next);

	res = process_tab[p_next] ? -1 : (p_serial << p_serial_shift | p_next);

    }

    erts_smp_mtx_unlock(&proc_tab_mtx);

    return res;

}

Uint erts_process_count(void)
{
    erts_aint32_t res = erts_smp_atomic32_read_nob(&process_count);
    ASSERT(res >= 0);
    return (Uint) res;
}

void
erts_free_proc(Process *p)
{
#if defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP)
    erts_lcnt_proc_lock_destroy(p);
#endif
    erts_free(ERTS_ALC_T_PROC, (void *) p);
}


/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(void)
{
#ifdef ERTS_SMP
    erts_pix_lock_t *pix_lock;
#endif
    Process* p;
    int p_prev;

    erts_smp_mtx_lock(&proc_tab_mtx);

    if (p_next == -1) {
	p = NULL;
	goto error; /* Process table full! */
    }

    p = (Process*) erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	goto error; /* ENOMEM */ 

    p_last = p_next;

    erts_get_emu_time(&p->started);

#ifdef ERTS_SMP
    pix_lock = ERTS_PIX2PIXLOCK(p_next);
    erts_pix_lock(pix_lock);
#endif
    ASSERT(!process_tab[p_next]);

    process_tab[p_next] = p;
    erts_smp_atomic32_inc_nob(&process_count);
    p->id = make_internal_pid(p_serial << p_serial_shift | p_next);
    if (p->id == ERTS_INVALID_PID) {
	/* Do not use the invalid pid; change serial */
	p_serial++;
	p_serial &= p_serial_mask;
	p->id = make_internal_pid(p_serial << p_serial_shift | p_next);
	ASSERT(p->id != ERTS_INVALID_PID);
    }
    ASSERT(internal_pid_serial(p->id) <= (erts_use_r9_pids_ports
					  ? ERTS_MAX_PID_R9_SERIAL
					  : ERTS_MAX_PID_SERIAL));

#ifdef ERTS_SMP
    erts_proc_lock_init(p); /* All locks locked */
    erts_pix_unlock(pix_lock);
#endif

    p->rstatus = P_FREE;
    p->rcount = 0;

    /*
     * set p_next to the next available slot
     */

    p_prev = p_next;

    while (1) {
	p_next++;
	if(p_next >= erts_max_processes) {
	    p_serial++;
	    p_serial &= p_serial_mask;
	    p_next = 0;
	}

	if (p_prev == p_next) {
	    p_next = -1;
	    break; /* Table full! */
	}

	if (!process_tab[p_next])
	    break; /* found a free slot */
    }

 error:

    erts_smp_mtx_unlock(&proc_tab_mtx);

    return p;

}

Eterm
erl_create_process(Process* parent, /* Parent of process (default group leader). */
		   Eterm mod,	/* Tagged atom for module. */
		   Eterm func,	/* Tagged atom for function. */
		   Eterm args,	/* Arguments for function (must be well-formed list). */
		   ErlSpawnOpts* so) /* Options for spawn. */
{
    ErtsRunQueue *rq, *notify_runq;
    Process *p;
    Sint arity;			/* Number of arguments. */
#ifndef HYBRID
    Uint arg_size;		/* Size of arguments. */
#endif
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
    Eterm res = THE_NON_VALUE;

#ifdef ERTS_SMP
    erts_smp_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

#ifdef HYBRID
    /*
     * Copy the arguments to the global heap
     * Since global GC might occur we want to do this before adding the
     * new process to the process_tab.
     */
    BM_SWAP_TIMER(system,copy);
    LAZY_COPY(parent,args);
    BM_SWAP_TIMER(copy,system);
    heap_need = 0;
#endif /* HYBRID */
    /*
     * Check for errors.
     */

    if (is_not_atom(mod) || is_not_atom(func) || ((arity = list_length(args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }
    p = alloc_process(); /* All proc locks are locked by this thread
			    on success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

    processes_busy++;
    BM_COUNT(processes_spawned);

#ifndef HYBRID
    BM_SWAP_TIMER(system,size);
    arg_size = size_object(args);
    BM_SWAP_TIMER(size,system);
    heap_need = arg_size;
#endif

    p->flags = erts_default_process_flags;

    /* Scheduler queue mutex should be locked when changeing
     * prio. In this case we don't have to lock it, since
     * noone except us has access to the process.
     */
    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size  = so->min_heap_size;
	p->min_vheap_size = so->min_vheap_size;
	p->prio           = so->priority;
	p->max_gen_gcs    = so->max_gen_gcs;
    } else {
	p->min_heap_size  = H_MIN_SIZE;
	p->min_vheap_size = BIN_VH_MIN_SIZE;
	p->prio           = PRIORITY_NORMAL;
	p->max_gen_gcs    = (Uint16) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
    }
    p->skipped = 0;
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

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
#ifdef INCREMENTAL
    p->scan_top = p->high_water;
#endif
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
    p->catches = 0;

    p->bin_vheap_sz     = p->min_vheap_size;
    p->bin_old_vheap_sz = p->min_vheap_size;
    p->bin_old_vheap    = 0;
    p->bin_vheap_mature = 0;

    /* No need to initialize p->fcalls. */

    p->current = p->initial+INITIAL_MOD;

    p->i = (BeamInstr *) beam_apply;
    p->cp = (BeamInstr *) beam_apply+1;

    p->arg_reg = p->def_arg_reg;
    p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
    p->arg_reg[0] = mod;
    p->arg_reg[1] = func;
    BM_STOP_TIMER(system);
    BM_MESSAGE(args,p,parent);
    BM_START_TIMER(system);
#ifdef HYBRID
    p->arg_reg[2] = args;
#ifdef INCREMENTAL
    p->active = 0;
    if (ptr_val(args) >= inc_fromspc && ptr_val(args) < inc_fromend)
        INC_ACTIVATE(p);
#endif
#else
    BM_SWAP_TIMER(system,copy);
    p->arg_reg[2] = copy_struct(args, arg_size, &p->htop, &p->off_heap);
    BM_MESSAGE_COPIED(arg_size);
    BM_SWAP_TIMER(copy,system);
#endif
    p->arity = 3;

    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->reds = 0;

#ifdef ERTS_SMP
    p->u.ptimer = NULL;
#else
    sys_memset(&p->u.tm, 0, sizeof(ErlTimer));
#endif

    p->reg = NULL;
    p->nlinks = NULL;
    p->monitors = NULL;
    p->nodes_monitors = NULL;
    p->suspend_monitors = NULL;

    ASSERT(is_pid(parent->group_leader));

    if (parent->group_leader == ERTS_INVALID_PID)
	p->group_leader = p->id;
    else {
	/* Needs to be done after the heap has been set up */
	p->group_leader =
	    IS_CONST(parent->group_leader)
	    ? parent->group_leader
	    : STORE_NC(&p->htop, &p->off_heap, parent->group_leader);
    }

    erts_get_default_tracing(&p->trace_flags, &p->tracer_proc);

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifdef ERTS_SMP
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
    p->bound_runq = NULL;
#endif
    p->bif_timers = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->psd = NULL;
    p->dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
    p->parent = parent->id == ERTS_INVALID_PID ? NIL : parent->id;

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    if (IS_TRACED(parent)) {
	if (parent->trace_flags & F_TRACE_SOS) {
	    p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	}
	if (ARE_TRACE_FLAGS_ON(parent, F_TRACE_PROCS)) {
	    trace_proc_spawn(parent, p->id, mod, func, args);
	}
	if (parent->trace_flags & F_TRACE_SOS1) { /* Overrides TRACE_CHILDREN */
	    p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	    p->trace_flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	    parent->trace_flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	}
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
#ifdef DEBUG
	int ret;
#endif
	if (IS_TRACED_FL(parent, F_TRACE_PROCS)) {
	    trace_proc(parent, parent, am_link, p->id);
	}

#ifdef DEBUG
	ret = erts_add_link(&(parent->nlinks),  LINK_PID, p->id);
	ASSERT(ret == 0);
	ret = erts_add_link(&(p->nlinks), LINK_PID, parent->id);
	ASSERT(ret == 0);
#else	
	erts_add_link(&(parent->nlinks), LINK_PID, p->id);
	erts_add_link(&(p->nlinks), LINK_PID, parent->id);
#endif

	if (IS_TRACED(parent)) {
	    if (parent->trace_flags & (F_TRACE_SOL|F_TRACE_SOL1))  {
		p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
		p->tracer_proc = parent->tracer_proc;    /* maybe steal */

		if (parent->trace_flags & F_TRACE_SOL1)  { /* maybe override */
		    p ->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    parent->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
	    }
	}
    }

    /*
     * Test whether this process should be initially monitored by its parent.
     */
    if (so->flags & SPO_MONITOR) {
	Eterm mref;

	mref = erts_make_ref(parent);
	erts_add_monitor(&(parent->monitors), MON_ORIGIN, mref, p->id, NIL);
	erts_add_monitor(&(p->monitors), MON_TARGET, mref, parent->id, NIL);
	so->mref = mref;
    }

#ifdef HYBRID
    /*
     * Add process to the array of active processes.
     */
    ACTIVATE(p);
    p->active_index = erts_num_active_procs++;
    erts_active_procs[p->active_index] = p;
#endif

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->is_exiting = 0;
    p->status_flags = 0;
    p->runq_flags = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

    /*
     * Schedule process for execution.
     */

    if (!((so->flags & SPO_USE_ARGS) && so->scheduler))
	rq = erts_get_runq_proc(parent);
    else {
	int ix = so->scheduler-1;
	ASSERT(0 <= ix && ix < erts_no_run_queues);
	rq = ERTS_RUNQ_IX(ix);
	p->bound_runq = rq;
    }

    erts_smp_runq_lock(rq);

#ifdef ERTS_SMP
    p->run_queue = rq;
#endif

    p->status = P_WAITING;
    notify_runq = internal_add_to_runq(rq, p);

    erts_smp_runq_unlock(rq);

    smp_notify_inc_runq(notify_runq);

    res = p->id;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);

    VERBOSE(DEBUG_PROCESSES, ("Created a new process: %T\n",p->id));

 error:

    erts_smp_proc_unlock(parent, ERTS_PROC_LOCKS_ALL_MINOR);

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
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
    p->min_heap_size = 0;
    p->min_vheap_size = 0;
    p->status = P_RUNABLE;
    p->gcstatus = P_RUNABLE;
    p->rstatus = P_RUNABLE;
    p->rcount = 0;
    p->id = ERTS_INVALID_PID;
    p->prio = PRIORITY_NORMAL;
    p->reds = 0;
    p->tracer_proc = NIL;
    p->trace_flags = F_INITIAL_TRACE_FLAGS;
    p->group_leader = ERTS_INVALID_PID;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->fcalls = 0;

    p->bin_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap = 0;
    p->bin_vheap_mature = 0;
#ifdef ERTS_SMP
    p->u.ptimer = NULL;
    p->bound_runq = NULL;
#else
    memset(&(p->u.tm), 0, sizeof(ErlTimer));
#endif
    p->next = NULL;
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;
    p->reg = NULL;
    p->heap_sz = 0;
    p->high_water = NULL;
#ifdef INCREMENTAL
    p->scan_top = NULL;
#endif
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->psd = NULL;
    p->monitors = NULL;
    p->nlinks = NULL;         /* List of links */
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
    p->initial[0] = 0;
    p->initial[1] = 0;
    p->initial[2] = 0;
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
    p->started.tv_sec = 0;
    p->started.tv_usec = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif

    ACTIVATE(p);

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif
    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif


#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->is_exiting = 0;
    p->status_flags = 0;
    p->runq_flags = 0;
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
    erts_proc_lock_init(p);
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    p->run_queue = ERTS_RUNQ_IX(0);
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
    ASSERT(p->heap == NULL);
    ASSERT(p->id == ERTS_INVALID_PID);
    ASSERT(p->tracer_proc == NIL);
    ASSERT(p->trace_flags == F_INITIAL_TRACE_FLAGS);
    ASSERT(p->group_leader == ERTS_INVALID_PID);
    ASSERT(p->next == NULL);
    ASSERT(p->reg == NULL);
    ASSERT(p->heap_sz == 0);
    ASSERT(p->high_water == NULL);
#ifdef INCREMENTAL
    ASSERT(p->scan_top == NULL);
#endif
    ASSERT(p->old_hend == NULL);
    ASSERT(p->old_htop == NULL);
    ASSERT(p->old_heap == NULL);

    ASSERT(p->monitors == NULL);
    ASSERT(p->nlinks == NULL);
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
#if defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP)
    erts_lcnt_proc_lock_destroy(p);
#endif
#ifdef DEBUG
    erts_debug_verify_clean_empty_process(p);
#endif
}

/*
 * p must be the currently executing process.
 */
static void
delete_process(Process* p)
{
    ErlMessage* mp;

    VERBOSE(DEBUG_PROCESSES, ("Removing process: %T\n",p->id));

    /* Cleanup psd */

    if (p->psd)
	erts_free(ERTS_ALC_T_PSD, p->psd);

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


#ifdef DEBUG
    sys_memset(p->heap, DEBUG_BAD_BYTE, p->heap_sz*sizeof(Eterm));
#endif

#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP, (void*) p->heap, p->heap_sz*sizeof(Eterm));
    if (p->old_heap != NULL) {

#ifdef DEBUG
	sys_memset(p->old_heap, DEBUG_BAD_BYTE,
                   (p->old_hend-p->old_heap)*sizeof(Eterm));
#endif
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       p->old_heap,
		       (p->old_hend-p->old_heap)*sizeof(Eterm));
    }

    /*
     * Free all pending message buffers.
     */
    if (p->mbuf != NULL) {	
	free_message_buffer(p->mbuf);
    }

    erts_erase_dicts(p);

    /* free all pending messages */
    mp = p->msg.first;
    while(mp != NULL) {
	ErlMessage* next_mp = mp->next;
	if (mp->data.attached) {
	    if (is_value(mp->m[0]))
		free_message_buffer(mp->data.heap_frag);
	    else {
		if (is_not_nil(mp->m[1])) {
		    ErlHeapFragment *heap_frag;
		    heap_frag = (ErlHeapFragment *) mp->data.dist_ext->ext_endp;
		    erts_cleanup_offheap(&heap_frag->off_heap);
		}
		erts_free_dist_ext_copy(mp->data.dist_ext);
	    }
	}
	free_message(mp);
	mp = next_mp;
    }

    ASSERT(!p->monitors);
    ASSERT(!p->nlinks);
    ASSERT(!p->nodes_monitors);
    ASSERT(!p->suspend_monitors);

    p->fvalue = NIL;

#ifdef HYBRID
    erts_active_procs[p->active_index] =
        erts_active_procs[--erts_num_active_procs];
    erts_active_procs[p->active_index]->active_index = p->active_index;
#ifdef INCREMENTAL
    if (INC_IS_ACTIVE(p))
         INC_DEACTIVATE(p);
#endif

    if (p->rrma != NULL) {
        erts_free(ERTS_ALC_T_ROOTSET,p->rrma);
        erts_free(ERTS_ALC_T_ROOTSET,p->rrsrc);
    }
#endif

}

static ERTS_INLINE void
set_proc_exiting(Process *p, Eterm reason, ErlHeapFragment *bp)
{
#ifdef ERTS_SMP
    erts_pix_lock_t *pix_lock = ERTS_PID2PIXLOCK(p->id);
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCKS_ALL);
    /*
     * You are required to have all proc locks and the pix lock when going
     * to status P_EXITING. This makes it is enough to take any lock when
     * looking up a process (pid2proc()) to prevent the looked up process
     * from exiting until the lock has been released.
     */

    erts_pix_lock(pix_lock);
    p->is_exiting = 1;
#endif
    p->status = P_EXITING;
#ifdef ERTS_SMP
    erts_pix_unlock(pix_lock);
#endif
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
    cancel_timer(p);
    p->i = (BeamInstr *) beam_exit;
}


#ifdef ERTS_SMP

void
erts_handle_pending_exit(Process *c_p, ErtsProcLocks locks)
{
    ErtsProcLocks xlocks;
    ASSERT(is_value(c_p->pending_exit.reason));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == locks);
    ERTS_SMP_LC_ASSERT(locks & ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_LC_ASSERT(c_p->status != P_EXITING);
    ERTS_SMP_LC_ASSERT(c_p->status != P_FREE);

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

    set_proc_exiting(c_p, c_p->pending_exit.reason, c_p->pending_exit.bp);
    c_p->pending_exit.reason = THE_NON_VALUE;
    c_p->pending_exit.bp = NULL;

    if (xlocks)
	erts_smp_proc_unlock(c_p, xlocks);
}

static void
handle_pending_exiters(ErtsProcList *pnd_xtrs)
{
    ErtsProcList *plp = pnd_xtrs;
    ErtsProcList *free_plp;
    while (plp) {
	Process *p = erts_pid2proc(NULL, 0, plp->pid, ERTS_PROC_LOCKS_ALL);
	if (p) {
	    if (proclist_same(plp, p)
		&& !(p->status_flags & ERTS_PROC_SFLG_RUNNING)) {
		ASSERT(p->status_flags & ERTS_PROC_SFLG_INRUNQ);
		ASSERT(ERTS_PROC_PENDING_EXIT(p));
		erts_handle_pending_exit(p, ERTS_PROC_LOCKS_ALL);
	    }
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
	}
	free_plp = plp;
	plp = plp->next;
	proclist_destroy(free_plp);
    }
}

static void
save_pending_exiter(Process *p)
{
    ErtsProcList *plp;
    ErtsRunQueue *rq;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

    rq = erts_get_runq_current(NULL);

    plp = proclist_create(p);

    erts_smp_runq_lock(rq);

    plp->next = rq->procs.pending_exiters;
    rq->procs.pending_exiters = plp;

    erts_smp_runq_unlock(rq);

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
    if (token == NIL) {
	Eterm* hp;
	Eterm mess;
	ErlHeapFragment* bp;
	ErlOffHeap *ohp;

	hp = erts_alloc_message_heap(term_size, &bp, &ohp, to, to_locksp);
	mess = copy_struct(exit_term, term_size, &hp, ohp);
	erts_queue_message(to, to_locksp, bp, mess, NIL);
    } else {
	ErlHeapFragment* bp;
	Eterm* hp;
	Eterm mess;
	Eterm temp_token;
	Uint sz_token;

	ASSERT(is_tuple(token));
	sz_token = size_object(token);
	bp = new_message_buffer(term_size+sz_token);
	hp = bp->mem;
	mess = copy_struct(exit_term, term_size, &hp, &bp->off_heap);
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, mess, SEQ_TRACE_SEND, to->id, NULL);
	temp_token = copy_struct(token, sz_token, &hp, &bp->off_heap);
	erts_queue_message(to, to_locksp, bp, mess, temp_token);
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
 * exiting state (status P_EXITING). When a process has gone into the
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
    Eterm rsn = reason == am_kill ? am_killed : reason;

    ERTS_SMP_LC_ASSERT(*rp_locks == erts_proc_lc_my_proc_locks(rp));
    ERTS_SMP_LC_ASSERT((*rp_locks & ERTS_PROC_LOCKS_XSIG_SEND)
		       == ERTS_PROC_LOCKS_XSIG_SEND);

    ASSERT(reason != THE_NON_VALUE);

    if (ERTS_PROC_IS_TRAPPING_EXITS(rp)
	&& (reason != am_kill || (flags & ERTS_XSIG_FLG_IGN_KILL))) {
	if (is_not_nil(token) && token_update)
	    seq_trace_update_send(token_update);
	if (is_value(exit_tuple))
	    send_exit_message(rp, rp_locks, exit_tuple, exit_tuple_sz, token);
	else
	    erts_deliver_exit_message(from, rp, rp_locks, rsn, token);
	return 1; /* Receiver will get a message */
    }
    else if (reason != am_normal || (flags & ERTS_XSIG_FLG_NO_IGN_NORMAL)) {
#ifdef ERTS_SMP
	if (!ERTS_PROC_PENDING_EXIT(rp) && !rp->is_exiting) {
	    ASSERT(rp->status != P_EXITING);
	    ASSERT(rp->status != P_FREE);
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
		set_proc_exiting(c_p, rsn, NULL);
	    }
	    else if (!(rp->status_flags & ERTS_PROC_SFLG_RUNNING)) {
		/* Process not running ... */
		ErtsProcLocks need_locks = ~(*rp_locks) & ERTS_PROC_LOCKS_ALL;
		if (need_locks
		    && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		    /* ... but we havn't got all locks on it ... */
		    save_pending_exiter(rp);
		    /*
		     * The pending exit will be discovered when next
		     * process is scheduled in
		     */
		    goto set_pending_exit;
		}
		else {
		    /* ...and we have all locks on it... */
		    *rp_locks = ERTS_PROC_LOCKS_ALL;
		    set_proc_exiting(rp,
				     (is_immed(rsn)
				      ? rsn
				      : copy_object(rsn, rp)),
				     NULL);
		}
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
		ASSERT(ERTS_PROC_PENDING_EXIT(rp));
	    }
	    if (!(rp->status_flags
		  & (ERTS_PROC_SFLG_INRUNQ|ERTS_PROC_SFLG_RUNNING)))
		erts_add_to_runq(rp);
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
	if (c_p == rp) {
	    rp->status = P_EXITING;
	    c_p->fvalue = rsn;
	}
	else if (rp->status != P_EXITING) { /* No recursive process exits /PaN */
	    Eterm old_status = rp->status;
	    set_proc_exiting(rp,
			     is_immed(rsn) ? rsn : copy_object(rsn, rp),
			     NULL);
	    ACTIVATE(rp);
	    if (old_status != P_RUNABLE && old_status != P_RUNNING)
		erts_add_to_runq(rp);
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
    Process *rp;

    if (mon->type == MON_ORIGIN) {
	/* We are monitoring someone else, we need to demonitor that one.. */
	if (is_atom(mon->pid)) { /* remote by name */
	    ASSERT(is_node_name_atom(mon->pid));
	    dep = erts_sysname_to_connected_dist_entry(mon->pid);
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
							rmon->pid,
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
	    ASSERT(is_pid(mon->pid));
	    if (is_internal_pid(mon->pid)) { /* local by pid or name */
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
	    } else { /* remote by pid */
		ASSERT(is_external_pid(mon->pid));
		dep = external_pid_dist_entry(mon->pid);
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
							    rmon->pid,
							    mon->pid,
							    mon->ref,
							    1);
			    ASSERT(code == ERTS_DSIG_SEND_OK);
			}
			erts_destroy_monitor(rmon);
		    }
		}
	    }
	}
    } else { /* type == MON_TARGET */
	ASSERT(mon->type == MON_TARGET);
	ASSERT(is_pid(mon->pid) || is_internal_port(mon->pid));
	if (is_internal_port(mon->pid)) {
	    Port *prt = erts_id2port(mon->pid, NULL, 0);
	    if (prt == NULL) {
		goto done;
	    }
	    erts_fire_port_monitor(prt, mon->ref);
	    erts_port_release(prt); 
	} else if (is_internal_pid(mon->pid)) {/* local by name or pid */
	    Eterm watched;
	    DeclareTmpHeapNoproc(lhp,3);
	    ErtsProcLocks rp_locks = (ERTS_PROC_LOCK_LINK
				      | ERTS_PROC_LOCKS_MSG_SEND);
	    rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);
	    if (rp == NULL) {
		goto done;
	    }
	    UseTmpHeapNoproc(3);
	    rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	    if (rmon) {
		erts_destroy_monitor(rmon);
		watched = (is_atom(mon->name)
			   ? TUPLE2(lhp, mon->name, 
				    erts_this_dist_entry->sysname)
			   : pcontext->p->id);
		erts_queue_monitor_message(rp, &rp_locks, mon->ref, am_process, 
					   watched, pcontext->reason);
	    }
	    UnUseTmpHeapNoproc(3);
	    /* else: demonitor while we exited, i.e. do nothing... */
	    erts_smp_proc_unlock(rp, rp_locks);
	} else { /* external by pid or name */
	    ASSERT(is_external_pid(mon->pid));    
	    dep = external_pid_dist_entry(mon->pid);
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
						     mon->pid,
						     (rmon->name != NIL
						      ? rmon->name
						      : rmon->pid),
						     mon->ref,
						     pcontext->reason);
			ASSERT(code == ERTS_DSIG_SEND_OK);
		    }
		    erts_destroy_monitor(rmon);
		}
	    }
	}
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
	    Port *prt = erts_id2port(item, NULL, 0);
	    if (prt) {
		rlnk = erts_remove_link(&prt->nlinks, p->id);
		if (rlnk)
		    erts_destroy_link(rlnk);
		erts_do_exit_port(prt, p->id, reason);
		erts_port_release(prt);
	    }
	}
	else if(is_external_port(item)) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Erroneous link between %T and external port %T "
			  "found\n",
			  p->id,
			  item);
	    erts_send_error_to_logger_nogl(dsbufp);
	    ASSERT(0); /* It isn't possible to setup such a link... */
	}
	else if (is_internal_pid(item)) {
	    ErtsProcLocks rp_locks = (ERTS_PROC_LOCK_LINK
				      | ERTS_PROC_LOCKS_XSIG_SEND);
	    rp = erts_pid2proc(NULL, 0, item, rp_locks);
	    if (rp) {
		rlnk = erts_remove_link(&(rp->nlinks), p->id);
		/* If rlnk == NULL, we got unlinked while exiting,
		   i.e., do nothing... */
		if (rlnk) {
		    int xres;
		    erts_destroy_link(rlnk);
		    xres = send_exit_signal(NULL,
					    p->id,
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
			    trace_proc(p, rp, am_getting_unlinked, p->id);
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
		erts_remove_dist_link(&dld, p->id, item, dep);
		erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
		code = erts_dsig_prepare(&dsd, dep, p, ERTS_DSP_NO_LOCK, 0);
		if (code == ERTS_DSIG_PREP_CONNECTED) {
		    code = erts_dsig_send_exit_tt(&dsd, p->id, item, reason,
						  SEQ_TRACE_TOKEN(p));
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
	    rlnk = erts_remove_link(&(dep->node_links), p->id);
	    erts_smp_de_links_unlock(dep);
	    if (rlnk)
		erts_destroy_link(rlnk);
	    erts_deref_dist_entry(dep);
	} else {
#ifndef ERTS_SMP
	    /* XXX Is this possible? Shouldn't this link
	       previously have been removed if the node
	       had previously been disconnected. */
	    ASSERT(0);
#endif
	    /* This is possible when smp support has been enabled,
	       and dist port and process exits simultaneously. */
	}
	break;
	
    default:
	erl_exit(1, "bad type in link list\n");
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
	if (smon->active)
	    resume_process(suspendee);
	erts_smp_proc_unlock(suspendee, ERTS_PROC_LOCK_STATUS);
    }
    erts_destroy_suspend_monitor(smon);
}

static void
continue_exit_process(Process *p
#ifdef ERTS_SMP
		      , erts_pix_lock_t *pix_lock
#endif
    );

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
erts_do_exit_process(Process* p, Eterm reason)
{
#ifdef ERTS_SMP
    erts_pix_lock_t *pix_lock = ERTS_PID2PIXLOCK(p->id);
#endif

    p->arity = 0;		/* No live registers */
    p->fvalue = reason;
    
#ifdef ERTS_SMP
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    /* By locking all locks (main lock is already locked) when going
       to status P_EXITING, it is enough to take any lock when
       looking up a process (erts_pid2proc()) to prevent the looked up
       process from exiting until the lock has been released. */
    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
    
    if (erts_system_profile_flags.runnable_procs && (p->status != P_WAITING)) {
    	profile_runnable_proc(p, am_inactive);
    }

#ifdef ERTS_SMP
    erts_pix_lock(pix_lock);
    p->is_exiting = 1;
#endif
    
    p->status = P_EXITING;

#ifdef ERTS_SMP
    erts_pix_unlock(pix_lock);

    if (ERTS_PROC_PENDING_EXIT(p)) {
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

	if (IS_TRACED_FL(p,F_TRACE_PROCS))
	    trace_proc(p, p, am_exit, reason);
    }

    erts_trace_check_exiting(p->id);

    ASSERT((p->trace_flags & F_INITIAL_TRACE_FLAGS) == F_INITIAL_TRACE_FLAGS);

    cancel_timer(p);		/* Always cancel timer just in case */

    /*
     * The timer of this process can *not* be used anymore. The field used
     * for the timer is now used for misc exiting data.
     */
    p->u.exit_data = NULL;

    if (p->bif_timers)
	erts_cancel_bif_timers(p, ERTS_PROC_LOCKS_ALL);

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);

#ifdef ERTS_SMP
    continue_exit_process(p, pix_lock);
#else
    continue_exit_process(p);
#endif
}

void
erts_continue_exit_process(Process *c_p)
{
#ifdef ERTS_SMP
    continue_exit_process(c_p, ERTS_PID2PIXLOCK(c_p->id));
#else
    continue_exit_process(c_p);
#endif
}

static void
continue_exit_process(Process *p
#ifdef ERTS_SMP
		      , erts_pix_lock_t *pix_lock
#endif
    )
{
    ErtsLink* lnk;
    ErtsMonitor *mon;
    ErtsProcLocks curr_locks = ERTS_PROC_LOCK_MAIN;
    Eterm reason = p->fvalue;
    DistEntry *dep;
    struct saved_calls *scb;
    process_breakpoint_time_t *pbt;

#ifdef DEBUG
    int yield_allowed = 1;
#endif

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

#ifdef DEBUG
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    ASSERT(p->status == P_EXITING);
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
#endif

#ifdef ERTS_SMP
    if (p->flags & F_HAVE_BLCKD_MSCHED) {
	ErtsSchedSuspendResult ssr;
	ssr = erts_block_multi_scheduling(p, ERTS_PROC_LOCK_MAIN, 0, 1);
	switch (ssr) {
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    goto yield;
	case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED:
	case ERTS_SCHDLR_SSPND_DONE:
	case ERTS_SCHDLR_SSPND_YIELD_DONE:
	    p->flags &= ~F_HAVE_BLCKD_MSCHED;
	    break;
	case ERTS_SCHDLR_SSPND_EINVAL:
	default:
	    erl_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error: %d\n",
		     __FILE__, __LINE__, (int) ssr);
	}
    }
#endif

    if (p->flags & F_USING_DB) {
	if (erts_db_process_exiting(p, ERTS_PROC_LOCK_MAIN))
	    goto yield;
	p->flags &= ~F_USING_DB;
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
    if (p->reg) {
	(void) erts_unregister_name(p, ERTS_PROC_LOCK_MAIN, NULL, THE_NON_VALUE);
	ASSERT(!p->reg);
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
    curr_locks = ERTS_PROC_LOCKS_ALL;

    /*
     * From this point on we are no longer allowed to yield
     * this process.
     */
#ifdef DEBUG
    yield_allowed = 0;
#endif

    {
	int pix;
	/* Do *not* use erts_get_runq_proc() */
	ErtsRunQueue *rq;
	rq = erts_get_runq_current(ERTS_GET_SCHEDULER_DATA_FROM_PROC(p));

	ASSERT(internal_pid_index(p->id) < erts_max_processes);
	pix = internal_pid_index(p->id);

	erts_smp_mtx_lock(&proc_tab_mtx);
	erts_smp_runq_lock(rq);

#ifdef ERTS_SMP
	erts_pix_lock(pix_lock);

	ASSERT(p->scheduler_data);
	ASSERT(p->scheduler_data->current_process == p);
	ASSERT(p->scheduler_data->free_process == NULL);

	p->scheduler_data->current_process = NULL;
	p->scheduler_data->free_process = p;
	p->status_flags = 0;
#endif
	process_tab[pix] = NULL; /* Time of death! */
	ASSERT(erts_smp_atomic32_read_nob(&process_count) > 0);
	erts_smp_atomic32_dec_nob(&process_count);

#ifdef ERTS_SMP
	erts_pix_unlock(pix_lock);
#endif
	erts_smp_runq_unlock(rq);

	if (p_next < 0) {
	    if (p_last >= p_next) {
		p_serial++;
		p_serial &= p_serial_mask;
	    }
	    p_next = pix;
	}

	ERTS_MAYBE_SAVE_TERMINATING_PROCESS(p);

	erts_smp_mtx_unlock(&proc_tab_mtx);
    }

    /*
     * All "erlang resources" have to be deallocated before this point,
     * e.g. registered name, so monitoring and linked processes can
     * be sure that all interesting resources have been deallocated
     * when the monitors and/or links hit.
     */

    mon = p->monitors;
    p->monitors = NULL; /* to avoid recursive deletion during traversal */

    lnk = p->nlinks;
    p->nlinks = NULL;
    p->status = P_FREE;
    dep = ((p->flags & F_DISTRIBUTION)
	   ? ERTS_PROC_SET_DIST_ENTRY(p, ERTS_PROC_LOCKS_ALL, NULL)
	   : NULL);
    scb = ERTS_PROC_SET_SAVED_CALLS_BUF(p, ERTS_PROC_LOCKS_ALL, NULL);
    pbt = ERTS_PROC_SET_CALL_TIME(p, ERTS_PROC_LOCKS_ALL, NULL);

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    processes_busy--;

    if (dep) {
	erts_do_net_exits(dep, reason);
	if(dep)
	    erts_deref_dist_entry(dep);
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

	exit_tuple = TUPLE3(hp, am_EXIT, p->id, reason);

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

    if (scb)
        erts_free(ERTS_ALC_T_CALLS_BUF, (void *) scb);

    if (pbt)
        erts_free(ERTS_ALC_T_BPD, (void *) pbt);

    delete_process(p);

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

    return;

 yield:

#ifdef DEBUG
    ASSERT(yield_allowed);
#endif

    ERTS_SMP_LC_ASSERT(curr_locks == erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & curr_locks);

    ASSERT(p->status == P_EXITING);

    p->i = (BeamInstr *) beam_continue_exit;

    if (!(curr_locks & ERTS_PROC_LOCK_STATUS)) {
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	curr_locks |= ERTS_PROC_LOCK_STATUS;
    }

    erts_add_to_runq(p);

    if (curr_locks != ERTS_PROC_LOCK_MAIN)
	erts_smp_proc_unlock(p, ~ERTS_PROC_LOCK_MAIN & curr_locks);

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

}

/* Callback for process timeout */
static void
timeout_proc(Process* p)
{
    BeamInstr** pi = (BeamInstr **) p->def_arg_reg;
    p->i = *pi;
    p->flags |= F_TIMO;
    p->flags &= ~F_INSLPQUEUE;

    switch (p->status) {
    case P_GARBING:
	switch (p->gcstatus) {
	case P_SUSPENDED:
	    goto suspended;
	case P_WAITING:
	    goto waiting;
	default:
	    break;
	}
	break;
    case P_WAITING:
    waiting:
	erts_add_to_runq(p);
	break;
    case P_SUSPENDED:
    suspended:
	p->rstatus = P_RUNABLE;   /* MUST set resume status to runnable */
	break;
    default:
	break;
    }
}


void
cancel_timer(Process* p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    p->flags &= ~(F_INSLPQUEUE|F_TIMO);
#ifdef ERTS_SMP
    erts_cancel_smp_ptimer(p->u.ptimer);
#else
    erts_cancel_timer(&p->u.tm);
#endif
}

/*
 * Insert a process into the time queue, with a timeout 'timeout' in ms.
 */
void
set_timer(Process* p, Uint timeout)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));

    /* check for special case timeout=0 DONT ADD TO time queue */
    if (timeout == 0) {
	p->flags |= F_TIMO;
	return;
    }
    p->flags |= F_INSLPQUEUE;
    p->flags &= ~F_TIMO;

#ifdef ERTS_SMP
    erts_create_smp_ptimer(&p->u.ptimer,
			   p->id,
			   (ErlTimeoutProc) timeout_proc,
			   timeout);
#else
    erts_set_timer(&p->u.tm,
		  (ErlTimeoutProc) timeout_proc,
		  NULL,
		  (void*) p,
		  timeout);
#endif
}

/*
 * Stack dump functions follow.
 */

void
erts_stack_dump(int to, void *to_arg, Process *p)
{
    Eterm* sp;
    int yreg = -1;

    if (p->trace_flags & F_SENSITIVE) {
	return;
    }
    erts_program_counter_info(to, to_arg, p);
    for (sp = p->stop; sp < STACK_START(p); sp++) {
        yreg = stack_element_dump(to, to_arg, p, sp, yreg);
    }
}

void
erts_program_counter_info(int to, void *to_arg, Process *p)
{
    int i;

    erts_print(to, to_arg, "Program counter: %p (", p->i);
    print_function_from_pc(to, to_arg, p->i);
    erts_print(to, to_arg, ")\n");
    erts_print(to, to_arg, "CP: %p (", p->cp);
    print_function_from_pc(to, to_arg, p->cp);
    erts_print(to, to_arg, ")\n");
    if (!((p->status == P_RUNNING) || (p->status == P_GARBING))) {
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
print_function_from_pc(int to, void *to_arg, BeamInstr* x)
{
    BeamInstr* addr = find_function_from_pc(x);
    if (addr == NULL) {
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
		   addr[0], addr[1], addr[2], ((x-addr)-2) * sizeof(Eterm));
    }
}

static int
stack_element_dump(int to, void *to_arg, Process* p, Eterm* sp, int yreg)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erts_print(to, to_arg, "\n%p ", sp);
    } else {
        char sbuf[16];
        sprintf(sbuf, "y(%d)", yreg);
        erts_print(to, to_arg, "%-8s ", sbuf);
        yreg++;
    }

    if (is_CP(x)) {
        erts_print(to, to_arg, "Return addr %p (", (Eterm *) EXPAND_POINTER(x));
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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The processes/0 BIF implementation.                                       *
\*                                                                           */


#define ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED 25
#define ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE 1000
#define ERTS_PROCESSES_BIF_MIN_START_REDS		\
 (ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE			\
  / ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED)

#define ERTS_PROCESSES_BIF_TAB_FREE_TERM_PROC_REDS 1

#define ERTS_PROCESSES_BIF_INSPECT_TERM_PROC_PER_RED 10

#define ERTS_PROCESSES_INSPECT_TERM_PROC_MAX_REDS \
 (ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE			\
  / ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED)
 

#define ERTS_PROCESSES_BIF_BUILD_RESULT_CONSES_PER_RED 75

#define ERTS_PROCS_DBG_DO_TRACE 0

#ifdef DEBUG
#  define ERTS_PROCESSES_BIF_DEBUGLEVEL 100
#else
#  define ERTS_PROCESSES_BIF_DEBUGLEVEL 0
#endif

#define ERTS_PROCS_DBGLVL_CHK_HALLOC 1
#define ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS 5
#define ERTS_PROCS_DBGLVL_CHK_PIDS 10
#define ERTS_PROCS_DBGLVL_CHK_TERM_PROC_LIST 20
#define ERTS_PROCS_DBGLVL_CHK_RESLIST 20

#if ERTS_PROCESSES_BIF_DEBUGLEVEL == 0
#  define ERTS_PROCS_ASSERT(EXP)
#else
#  define ERTS_PROCS_ASSERT(EXP) \
    ((void) ((EXP) \
	     ? 1 \
	     : (debug_processes_assert_error(#EXP, __FILE__, __LINE__), 0)))
#endif


#if ERTS_PROCESSES_BIF_DEBUGLEVEL >=  ERTS_PROCS_DBGLVL_CHK_HALLOC
#  define ERTS_PROCS_DBG_SAVE_HEAP_ALLOC(PBDP, HP, SZ)			\
do {									\
    ERTS_PROCS_ASSERT(!(PBDP)->debug.heap);				\
    ERTS_PROCS_ASSERT(!(PBDP)->debug.heap_size);			\
    (PBDP)->debug.heap = (HP);						\
    (PBDP)->debug.heap_size = (SZ);					\
} while (0)
#  define ERTS_PROCS_DBG_VERIFY_HEAP_ALLOC_USED(PBDP, HP)		\
do {									\
    ERTS_PROCS_ASSERT((PBDP)->debug.heap);				\
    ERTS_PROCS_ASSERT((PBDP)->debug.heap_size);				\
    ERTS_PROCS_ASSERT((PBDP)->debug.heap + (PBDP)->debug.heap_size == (HP));\
    (PBDP)->debug.heap = NULL;						\
    (PBDP)->debug.heap_size = 0;					\
} while (0)
#  define ERTS_PROCS_DBG_HEAP_ALLOC_INIT(PBDP)				\
do {									\
    (PBDP)->debug.heap = NULL;						\
    (PBDP)->debug.heap_size = 0;					\
} while (0)
#else
#  define ERTS_PROCS_DBG_SAVE_HEAP_ALLOC(PBDP, HP, SZ)
#  define ERTS_PROCS_DBG_VERIFY_HEAP_ALLOC_USED(PBDP, HP)
#  define ERTS_PROCS_DBG_HEAP_ALLOC_INIT(PBDP)
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_RESLIST
#  define ERTS_PROCS_DBG_CHK_RESLIST(R) debug_processes_check_res_list((R))
#else
#  define ERTS_PROCS_DBG_CHK_RESLIST(R)
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_PIDS
#  define ERTS_PROCS_DBG_SAVE_PIDS(PBDP) debug_processes_save_all_pids((PBDP))
#  define ERTS_PROCS_DBG_VERIFY_PIDS(PBDP)		\
do {							\
    if (!(PBDP)->debug.correct_pids_verified)		\
	debug_processes_verify_all_pids((PBDP));	\
} while (0)
#  define ERTS_PROCS_DBG_CLEANUP_CHK_PIDS(PBDP)		\
do {							\
    if ((PBDP)->debug.correct_pids) {			\
	erts_free(ERTS_ALC_T_PROCS_PIDS,		\
		  (PBDP)->debug.correct_pids);		\
	(PBDP)->debug.correct_pids = NULL;		\
    }							\
} while(0)
#  define ERTS_PROCS_DBG_CHK_PIDS_INIT(PBDP)		\
do {							\
    (PBDP)->debug.correct_pids_verified = 0;		\
    (PBDP)->debug.correct_pids = NULL;			\
} while (0)
#else
#  define ERTS_PROCS_DBG_SAVE_PIDS(PBDP)
#  define ERTS_PROCS_DBG_VERIFY_PIDS(PBDP)
#  define ERTS_PROCS_DBG_CLEANUP_CHK_PIDS(PBDP)
#  define ERTS_PROCS_DBG_CHK_PIDS_INIT(PBDP)
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
#  define ERTS_PROCS_DBG_CHK_PID_FOUND(PBDP, PID, TVP) \
  debug_processes_check_found_pid((PBDP), (PID), (TVP), 1)
#  define ERTS_PROCS_DBG_CHK_PID_NOT_FOUND(PBDP, PID, TVP) \
  debug_processes_check_found_pid((PBDP), (PID), (TVP), 0)
#else
#  define ERTS_PROCS_DBG_CHK_PID_FOUND(PBDP, PID, TVP)
#  define ERTS_PROCS_DBG_CHK_PID_NOT_FOUND(PBDP, PID, TVP)
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_TERM_PROC_LIST
#  define ERTS_PROCS_DBG_CHK_TPLIST() \
  debug_processes_check_term_proc_list()
#  define ERTS_PROCS_DBG_CHK_FREELIST(FL) \
  debug_processes_check_term_proc_free_list(FL)
#else
#  define ERTS_PROCS_DBG_CHK_TPLIST()
#  define ERTS_PROCS_DBG_CHK_FREELIST(FL)
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL == 0
#if ERTS_PROCS_DBG_DO_TRACE
#    define ERTS_PROCS_DBG_INIT(P, PBDP) (PBDP)->debug.caller = (P)->id
#  else
#    define ERTS_PROCS_DBG_INIT(P, PBDP)
#  endif
#  define ERTS_PROCS_DBG_CLEANUP(PBDP)
#else
#  define ERTS_PROCS_DBG_INIT(P, PBDP)			\
do {							\
    (PBDP)->debug.caller = (P)->id;			\
    ERTS_PROCS_DBG_HEAP_ALLOC_INIT((PBDP));		\
    ERTS_PROCS_DBG_CHK_PIDS_INIT((PBDP));		\
} while (0)
#  define ERTS_PROCS_DBG_CLEANUP(PBDP)			\
do {							\
    ERTS_PROCS_DBG_CLEANUP_CHK_PIDS((PBDP));		\
} while (0)
#endif

#if ERTS_PROCS_DBG_DO_TRACE
#  define ERTS_PROCS_DBG_TRACE(PID, FUNC, WHAT)			\
     erts_fprintf(stderr, "%T %s:%d:%s(): %s\n",		\
		  (PID), __FILE__, __LINE__, #FUNC, #WHAT)
#else
#  define ERTS_PROCS_DBG_TRACE(PID, FUNC, WHAT)
#endif

static Uint processes_bif_tab_chunks;
static Export processes_trap_export;

typedef struct {
    SysTimeval time;
} ErtsProcessesBifChunkInfo;

typedef enum {
    INITIALIZING,
    INSPECTING_TABLE,
    INSPECTING_TERMINATED_PROCESSES,
    BUILDING_RESULT,
    RETURN_RESULT
} ErtsProcessesBifState;

typedef struct {
    ErtsProcessesBifState state;
    Eterm caller;
    ErtsProcessesBifChunkInfo *chunk;
    int tix;
    int pid_ix;
    int pid_sz;
    Eterm *pid;
    ErtsTermProcElement *bif_invocation; /* Only used when > 1 chunk */

#if ERTS_PROCESSES_BIF_DEBUGLEVEL != 0 || ERTS_PROCS_DBG_DO_TRACE
    struct {
	Eterm caller;
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
	SysTimeval *pid_started;
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_HALLOC
	Eterm *heap;
	Uint heap_size;
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_PIDS
	int correct_pids_verified;
	Eterm *correct_pids;
#endif
    } debug;
#endif

} ErtsProcessesBifData;


#if ERTS_PROCESSES_BIF_DEBUGLEVEL != 0
static void debug_processes_assert_error(char* expr, char* file, int line);
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_RESLIST
static void debug_processes_check_res_list(Eterm list);
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_PIDS
static void debug_processes_save_all_pids(ErtsProcessesBifData *pbdp);
static void debug_processes_verify_all_pids(ErtsProcessesBifData *pbdp);
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
static void debug_processes_check_found_pid(ErtsProcessesBifData *pbdp,
					    Eterm pid,
					    SysTimeval *started,
					    int pid_should_be_found);
#endif
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_TERM_PROC_LIST
static SysTimeval debug_tv_start;
static void debug_processes_check_term_proc_list(void);
static void debug_processes_check_term_proc_free_list(ErtsTermProcElement *tpep);
#endif

static void
save_terminating_process(Process *p)
{
    ErtsTermProcElement *tpep = erts_alloc(ERTS_ALC_T_PROCS_TPROC_EL,
					   sizeof(ErtsTermProcElement));
    ERTS_PROCS_ASSERT(saved_term_procs.start && saved_term_procs.end);
    ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&proc_tab_mtx));

    ERTS_PROCS_DBG_CHK_TPLIST();

    tpep->prev = saved_term_procs.end;
    tpep->next = NULL;
    tpep->ix = internal_pid_index(p->id);
    tpep->u.process.pid = p->id;
    tpep->u.process.spawned = p->started;
    erts_get_emu_time(&tpep->u.process.exited);

    saved_term_procs.end->next = tpep;
    saved_term_procs.end = tpep;

    ERTS_PROCS_DBG_CHK_TPLIST();

    ERTS_PROCS_ASSERT((tpep->prev->ix >= 0
		       ? erts_cmp_timeval(&tpep->u.process.exited,
					  &tpep->prev->u.process.exited)
		       : erts_cmp_timeval(&tpep->u.process.exited,
					  &tpep->prev->u.bif_invocation.time)) > 0);
}

static void
cleanup_processes_bif_data(Binary *bp)
{
    ErtsProcessesBifData *pbdp = ERTS_MAGIC_BIN_DATA(bp);

    ERTS_PROCS_DBG_TRACE(pbdp->debug.caller, cleanup_processes_bif_data, call);

    if (pbdp->state != INITIALIZING) {

	if (pbdp->chunk) {
	    erts_free(ERTS_ALC_T_PROCS_CNKINF, pbdp->chunk);
	    pbdp->chunk = NULL;
	}
	if (pbdp->pid) {
	    erts_free(ERTS_ALC_T_PROCS_PIDS, pbdp->pid);
	    pbdp->pid = NULL;
	}

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
	if (pbdp->debug.pid_started) {
	    erts_free(ERTS_ALC_T_PROCS_PIDS, pbdp->debug.pid_started);
	    pbdp->debug.pid_started = NULL;
	}
#endif

	if (pbdp->bif_invocation) {
	    ErtsTermProcElement *tpep;

	    erts_smp_mtx_lock(&proc_tab_mtx);

	    ERTS_PROCS_DBG_TRACE(pbdp->debug.caller,
				 cleanup_processes_bif_data,
				 term_proc_cleanup);

	    tpep = pbdp->bif_invocation;
	    pbdp->bif_invocation = NULL;

	    ERTS_PROCS_DBG_CHK_TPLIST();

	    if (tpep->prev) {
		/*
		 * Only remove this bif invokation when we
		 * have preceding invokations.
		 */
		tpep->prev->next = tpep->next;
		if (tpep->next)
		    tpep->next->prev = tpep->prev;
		else {
		    /*
		     * At the time of writing this branch cannot be
		     * reached. I don't want to remove this code though
		     * since it may be possible to reach this line
		     * in the future if the cleanup order in
		     * erts_do_exit_process() is changed. The ASSERT(0)
		     * is only here to make us aware that the reorder
		     * has happened. /rickard
		     */
		    ASSERT(0);
		    saved_term_procs.end = tpep->prev;
		}
		erts_free(ERTS_ALC_T_PROCS_TPROC_EL, tpep);
	    }
	    else {
		/*
		 * Free all elements until next bif invokation
		 * is found.
		 */
		ERTS_PROCS_ASSERT(saved_term_procs.start == tpep);
		do {
		    ErtsTermProcElement *ftpep = tpep;
		    tpep = tpep->next;
		    erts_free(ERTS_ALC_T_PROCS_TPROC_EL, ftpep);
		} while (tpep && tpep->ix >= 0);
		saved_term_procs.start = tpep;
		if (tpep)
		    tpep->prev = NULL;
		else
		    saved_term_procs.end = NULL;
	    }

	    ERTS_PROCS_DBG_CHK_TPLIST();

	    erts_smp_mtx_unlock(&proc_tab_mtx);

	}
    }

    ERTS_PROCS_DBG_TRACE(pbdp->debug.caller,
			 cleanup_processes_bif_data,
			 return);
    ERTS_PROCS_DBG_CLEANUP(pbdp);
}

static int
processes_bif_engine(Process *p, Eterm *res_accp, Binary *mbp)
{
    ErtsProcessesBifData *pbdp = ERTS_MAGIC_BIN_DATA(mbp);
    int have_reds;
    int reds;
    int locked = 0;

    do {
	switch (pbdp->state) {
	case INITIALIZING:
	    pbdp->chunk = erts_alloc(ERTS_ALC_T_PROCS_CNKINF,
				     (sizeof(ErtsProcessesBifChunkInfo)
				      * processes_bif_tab_chunks));
	    pbdp->tix = 0;
	    pbdp->pid_ix = 0;

	    erts_smp_mtx_lock(&proc_tab_mtx);
	    locked = 1;

	    ERTS_PROCS_DBG_TRACE(p->id, processes_bif_engine, init);

	    pbdp->pid_sz = erts_process_count();
	    pbdp->pid = erts_alloc(ERTS_ALC_T_PROCS_PIDS,
				   sizeof(Eterm)*pbdp->pid_sz);

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
	    pbdp->debug.pid_started = erts_alloc(ERTS_ALC_T_PROCS_PIDS,
					   sizeof(SysTimeval)*pbdp->pid_sz);
#endif

	    ERTS_PROCS_DBG_SAVE_PIDS(pbdp);

	    if (processes_bif_tab_chunks == 1)
		pbdp->bif_invocation = NULL;
	    else {
		/*
		 * We will have to access the table multiple times
		 * releasing the table lock in between chunks.
		 */
		pbdp->bif_invocation = erts_alloc(ERTS_ALC_T_PROCS_TPROC_EL,
						  sizeof(ErtsTermProcElement));
		pbdp->bif_invocation->ix = -1;
		erts_get_emu_time(&pbdp->bif_invocation->u.bif_invocation.time);
		ERTS_PROCS_DBG_CHK_TPLIST();

		pbdp->bif_invocation->next = NULL;
		if (saved_term_procs.end) {
		    pbdp->bif_invocation->prev = saved_term_procs.end;
		    saved_term_procs.end->next = pbdp->bif_invocation;
		    ERTS_PROCS_ASSERT(saved_term_procs.start);
		}
		else {
		    pbdp->bif_invocation->prev = NULL;
		    saved_term_procs.start = pbdp->bif_invocation;
		}
		saved_term_procs.end = pbdp->bif_invocation;

		ERTS_PROCS_DBG_CHK_TPLIST();

	    }

	    pbdp->state = INSPECTING_TABLE;
	    /* Fall through */

	case INSPECTING_TABLE: {
	    int ix = pbdp->tix;
	    int indices = ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE;
	    int cix = ix / ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE;
	    int end_ix = ix + indices;
	    SysTimeval *invocation_timep;

	    invocation_timep = (pbdp->bif_invocation
				? &pbdp->bif_invocation->u.bif_invocation.time
				: NULL);

	    ERTS_PROCS_ASSERT(is_nil(*res_accp));
	    if (!locked) {
		erts_smp_mtx_lock(&proc_tab_mtx);
		locked = 1;
	    }

	    ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&proc_tab_mtx));
	    ERTS_PROCS_DBG_TRACE(p->id, processes_bif_engine, insp_table);

	    if (cix != 0)
		erts_get_emu_time(&pbdp->chunk[cix].time);
	    else if (pbdp->bif_invocation)
		pbdp->chunk[0].time = *invocation_timep;
	    /* else: Time is irrelevant */

	    if (end_ix >= erts_max_processes) {
		ERTS_PROCS_ASSERT(cix+1 == processes_bif_tab_chunks);
		end_ix = erts_max_processes;
		indices = end_ix - ix;
		/* What to do when done with this chunk */
		pbdp->state = (processes_bif_tab_chunks == 1
			       ? BUILDING_RESULT
			       : INSPECTING_TERMINATED_PROCESSES);
	    }
    
	    for (; ix < end_ix; ix++) {
		Process *rp = process_tab[ix];
		if (rp
		    && (!invocation_timep
			|| erts_cmp_timeval(&rp->started,
					    invocation_timep) < 0)) {
		    ERTS_PROCS_ASSERT(is_internal_pid(rp->id));
		    pbdp->pid[pbdp->pid_ix] = rp->id;

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
		    pbdp->debug.pid_started[pbdp->pid_ix] = rp->started;
#endif

		    pbdp->pid_ix++;
		    ERTS_PROCS_ASSERT(pbdp->pid_ix <= pbdp->pid_sz);
		}
	    }

	    pbdp->tix = end_ix;
	    
	    erts_smp_mtx_unlock(&proc_tab_mtx);
	    locked = 0;

	    reds = indices/ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED;
	    BUMP_REDS(p, reds);

	    have_reds = ERTS_BIF_REDS_LEFT(p);

	    if (have_reds && pbdp->state == INSPECTING_TABLE) {
		ix = pbdp->tix;
		indices = ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE;
		end_ix = ix + indices;
		if (end_ix > erts_max_processes) {
		    end_ix = erts_max_processes;
		    indices = end_ix - ix;
		}
		
		reds = indices/ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED;

		/* Pretend we have no reds left if we haven't got enough
		   reductions to complete next chunk */
		if (reds > have_reds)
		    have_reds = 0;
	    }

	    break;
	}

	case INSPECTING_TERMINATED_PROCESSES: {
	    int i;
	    int max_reds;
	    int free_term_procs = 0;
	    SysTimeval *invocation_timep;
	    ErtsTermProcElement *tpep;
	    ErtsTermProcElement *free_list = NULL;

	    tpep = pbdp->bif_invocation;
	    ERTS_PROCS_ASSERT(tpep);
	    invocation_timep = &tpep->u.bif_invocation.time;

	    max_reds = have_reds = ERTS_BIF_REDS_LEFT(p);
	    if (max_reds > ERTS_PROCESSES_INSPECT_TERM_PROC_MAX_REDS)
		max_reds = ERTS_PROCESSES_INSPECT_TERM_PROC_MAX_REDS;

	    reds = 0;
	    erts_smp_mtx_lock(&proc_tab_mtx);
	    ERTS_PROCS_DBG_TRACE(p->id, processes_bif_engine, insp_term_procs);

	    ERTS_PROCS_DBG_CHK_TPLIST();

	    if (tpep->prev)
		tpep->prev->next = tpep->next;
	    else {
		ERTS_PROCS_ASSERT(saved_term_procs.start == tpep);
		saved_term_procs.start = tpep->next;

		if (saved_term_procs.start && saved_term_procs.start->ix >= 0) {
		    free_list = saved_term_procs.start;
		    free_term_procs = 1;
		}
	    }

	    if (tpep->next)
		tpep->next->prev = tpep->prev;
	    else
		saved_term_procs.end = tpep->prev;

	    tpep = tpep->next;

	    i = 0;
	    while (reds < max_reds && tpep) {
		if (tpep->ix < 0) {
		    if (free_term_procs) {
			ERTS_PROCS_ASSERT(free_list);
			ERTS_PROCS_ASSERT(tpep->prev);

			tpep->prev->next = NULL; /* end of free_list */
			saved_term_procs.start = tpep;
			tpep->prev = NULL;
			free_term_procs = 0;
		    }
		}
		else {
		    int cix = tpep->ix/ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE;
		    SysTimeval *chunk_timep = &pbdp->chunk[cix].time;
		    Eterm pid = tpep->u.process.pid;
		    ERTS_PROCS_ASSERT(is_internal_pid(pid));

		    if (erts_cmp_timeval(&tpep->u.process.spawned,
					 invocation_timep) < 0) {
			if (erts_cmp_timeval(&tpep->u.process.exited,
					     chunk_timep) < 0) {
			    ERTS_PROCS_DBG_CHK_PID_NOT_FOUND(pbdp,
							     pid,
							     &tpep->u.process.spawned);
			    pbdp->pid[pbdp->pid_ix] = pid;
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
			    pbdp->debug.pid_started[pbdp->pid_ix] = tpep->u.process.spawned;
#endif
			    pbdp->pid_ix++;
			    ERTS_PROCS_ASSERT(pbdp->pid_ix <= pbdp->pid_sz);
			}
			else {
			    ERTS_PROCS_DBG_CHK_PID_FOUND(pbdp,
							 pid,
							 &tpep->u.process.spawned);
			}
		    }
		    else {
			ERTS_PROCS_DBG_CHK_PID_NOT_FOUND(pbdp,
							 pid,
							 &tpep->u.process.spawned);
		    }

		    i++;
		    if (i == ERTS_PROCESSES_BIF_INSPECT_TERM_PROC_PER_RED) {
			reds++;
			i = 0;
		    }
		    if (free_term_procs)
			reds += ERTS_PROCESSES_BIF_TAB_FREE_TERM_PROC_REDS;
		}
		tpep = tpep->next;
	    }

	    if (free_term_procs) {
 		ERTS_PROCS_ASSERT(free_list);
		saved_term_procs.start = tpep;
		if (!tpep)
		    saved_term_procs.end = NULL;
		else {
		    ERTS_PROCS_ASSERT(tpep->prev);
		    tpep->prev->next = NULL; /* end of free_list */
		    tpep->prev = NULL;
		}
	    }

	    if (!tpep) {
		/* Done */
		ERTS_PROCS_ASSERT(pbdp->pid_ix == pbdp->pid_sz);
		pbdp->state = BUILDING_RESULT;
		pbdp->bif_invocation->next = free_list;
		free_list = pbdp->bif_invocation;
		pbdp->bif_invocation = NULL;
	    }
	    else {
		/* Link in bif_invocation again where we left off */
		pbdp->bif_invocation->prev = tpep->prev;
		pbdp->bif_invocation->next = tpep;
		tpep->prev = pbdp->bif_invocation;
		if (pbdp->bif_invocation->prev)
		    pbdp->bif_invocation->prev->next = pbdp->bif_invocation;
		else {
		    ERTS_PROCS_ASSERT(saved_term_procs.start == tpep);
		    saved_term_procs.start = pbdp->bif_invocation;
		}
	    }

	    ERTS_PROCS_DBG_CHK_TPLIST();
	    ERTS_PROCS_DBG_CHK_FREELIST(free_list);
	    erts_smp_mtx_unlock(&proc_tab_mtx);

	    /*
	     * We do the actual free of term proc structures now when we
	     * have released the table lock instead of when we encountered
	     * them. This since free() isn't for free and we don't want to
	     * unnecessarily block other schedulers.
	     */
	    while (free_list) {
		tpep = free_list;
		free_list = tpep->next;
		erts_free(ERTS_ALC_T_PROCS_TPROC_EL, tpep);
	    }

	    have_reds -= reds;
	    if (have_reds < 0)	
		have_reds = 0;
	    BUMP_REDS(p, reds);
	    break;
	}

	case BUILDING_RESULT: {
	    int conses, ix, min_ix;
	    Eterm *hp;
	    Eterm res = *res_accp;

	    ERTS_PROCS_DBG_VERIFY_PIDS(pbdp);
	    ERTS_PROCS_DBG_CHK_RESLIST(res);

	    ERTS_PROCS_DBG_TRACE(p->id, processes_bif_engine, begin_build_res);

	    have_reds = ERTS_BIF_REDS_LEFT(p);
	    conses = ERTS_PROCESSES_BIF_BUILD_RESULT_CONSES_PER_RED*have_reds;
	    min_ix = pbdp->pid_ix - conses;
	    if (min_ix < 0) {
		min_ix = 0;
		conses = pbdp->pid_ix;
	    }

	    hp = HAlloc(p, conses*2);
	    ERTS_PROCS_DBG_SAVE_HEAP_ALLOC(pbdp, hp, conses*2);

	    for (ix = pbdp->pid_ix - 1; ix >= min_ix; ix--) {
		ERTS_PROCS_ASSERT(is_internal_pid(pbdp->pid[ix]));
		res = CONS(hp, pbdp->pid[ix], res);
		hp += 2;
	    }

	    ERTS_PROCS_DBG_VERIFY_HEAP_ALLOC_USED(pbdp, hp);

	    pbdp->pid_ix = min_ix;
	    if (min_ix == 0)
		pbdp->state = RETURN_RESULT;
	    else {
		pbdp->pid_sz = min_ix;
		pbdp->pid = erts_realloc(ERTS_ALC_T_PROCS_PIDS,
					 pbdp->pid,
					 sizeof(Eterm)*pbdp->pid_sz);
#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
		pbdp->debug.pid_started = erts_realloc(ERTS_ALC_T_PROCS_PIDS,
						 pbdp->debug.pid_started,
						 sizeof(SysTimeval)*pbdp->pid_sz);
#endif
	    }
	    reds = conses/ERTS_PROCESSES_BIF_BUILD_RESULT_CONSES_PER_RED;
	    BUMP_REDS(p, reds);
	    have_reds -= reds;

	    ERTS_PROCS_DBG_CHK_RESLIST(res);
	    ERTS_PROCS_DBG_TRACE(p->id, processes_bif_engine, end_build_res);
	    *res_accp = res;
	    break;
	}
	case RETURN_RESULT:
	    cleanup_processes_bif_data(mbp);
	    return 1;

	default:
	    erl_exit(ERTS_ABORT_EXIT,
		     "erlang:processes/0: Invalid state: %d\n",
		     (int) pbdp->state);
	}

	
    } while (have_reds || pbdp->state == RETURN_RESULT);

    return 0;
}

/*
 * processes_trap/2 is a hidden BIF that processes/0 traps to.
 */

static BIF_RETTYPE processes_trap(BIF_ALIST_2)
{
    Eterm res_acc;
    Binary *mbp;

    /*
     * This bif cannot be called from erlang code. It can only be
     * trapped to from processes/0; therefore, a bad argument
     * is a processes/0 internal error.
     */

    ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_trap, call);
    ERTS_PROCS_ASSERT(is_nil(BIF_ARG_1) || is_list(BIF_ARG_1));

    res_acc = BIF_ARG_1;

    ERTS_PROCS_ASSERT(ERTS_TERM_IS_MAGIC_BINARY(BIF_ARG_2));

    mbp = ((ProcBin *) binary_val(BIF_ARG_2))->val;

    ERTS_PROCS_ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp)
		      == cleanup_processes_bif_data);
    ERTS_PROCS_ASSERT(
	((ErtsProcessesBifData *) ERTS_MAGIC_BIN_DATA(mbp))->debug.caller
	== BIF_P->id);

    if (processes_bif_engine(BIF_P, &res_acc, mbp)) {
	ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_trap, return);
	BIF_RET(res_acc);
    }
    else {
	ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_trap, trap);
	ERTS_BIF_YIELD2(&processes_trap_export, BIF_P, res_acc, BIF_ARG_2);
    }
}



/*
 * The actual processes/0 BIF.
 */

BIF_RETTYPE processes_0(BIF_ALIST_0)
{
    /*
     * A requirement: The list of pids returned should be a consistent
     *                snapshot of all processes existing at some point
     *                in time during the execution of processes/0. Since
     *                processes might terminate while processes/0 is
     *                executing, we have to keep track of terminated
     *                processes and add them to the result. We also
     *                ignore processes created after processes/0 has
     *                begun executing.
     */
    Eterm res_acc = NIL;
    Binary *mbp = erts_create_magic_binary(sizeof(ErtsProcessesBifData),
					   cleanup_processes_bif_data);
    ErtsProcessesBifData *pbdp = ERTS_MAGIC_BIN_DATA(mbp);

    ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_0, call);
    pbdp->state = INITIALIZING;
    ERTS_PROCS_DBG_INIT(BIF_P, pbdp);

    if (ERTS_BIF_REDS_LEFT(BIF_P) >= ERTS_PROCESSES_BIF_MIN_START_REDS
	&& processes_bif_engine(BIF_P, &res_acc, mbp)) {
	erts_bin_free(mbp);
	ERTS_PROCS_DBG_CHK_RESLIST(res_acc);
	ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_0, return);
	BIF_RET(res_acc);
    }
    else {
	Eterm *hp;
	Eterm magic_bin;
	ERTS_PROCS_DBG_CHK_RESLIST(res_acc);
	hp = HAlloc(BIF_P, PROC_BIN_SIZE);
	ERTS_PROCS_DBG_SAVE_HEAP_ALLOC(pbdp, hp, PROC_BIN_SIZE);
	magic_bin = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), mbp);
	ERTS_PROCS_DBG_VERIFY_HEAP_ALLOC_USED(pbdp, hp);
	ERTS_PROCS_DBG_TRACE(BIF_P->id, processes_0, trap);
	ERTS_BIF_YIELD2(&processes_trap_export, BIF_P, res_acc, magic_bin);
    }
}

static void
init_processes_bif(void)
{
    saved_term_procs.start = NULL;
    saved_term_procs.end = NULL;
    processes_bif_tab_chunks = (((erts_max_processes - 1)
				 / ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE)
				+ 1);

    /* processes_trap/2 is a hidden BIF that the processes/0 BIF traps to. */
    sys_memset((void *) &processes_trap_export, 0, sizeof(Export));
    processes_trap_export.address = &processes_trap_export.code[3];
    processes_trap_export.code[0] = am_erlang;
    processes_trap_export.code[1] = am_processes_trap;
    processes_trap_export.code[2] = 2;
    processes_trap_export.code[3] = (BeamInstr) em_apply_bif;
    processes_trap_export.code[4] = (BeamInstr) &processes_trap;

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_TERM_PROC_LIST
    erts_get_emu_time(&debug_tv_start);
#endif

}

/*
 * Debug stuff
 */

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_dbg_check_halloc_lock(Process *p)
{
    if (ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p))
	return 1;
    if (p->id == ERTS_INVALID_PID)
	return 1;
    if (p->scheduler_data && p == p->scheduler_data->match_pseudo_process)
	return 1;
    if (erts_thr_progress_is_blocking())
	return 1;
    return 0;
}
#endif

Eterm
erts_debug_processes(Process *c_p)
{
    /* This is the old processes/0 BIF. */
    int i;
    Uint need;
    Eterm res;
    Eterm* hp;
    Process *p;
#ifdef DEBUG
    Eterm *hp_end;
#endif

    erts_smp_mtx_lock(&proc_tab_mtx);

    res = NIL;
    need = erts_process_count() * 2;
    hp = HAlloc(c_p, need); /* we need two heap words for each pid */
#ifdef DEBUG
    hp_end = hp + need;
#endif
     
    /* make the list by scanning bakward */


    for (i = erts_max_processes-1; i >= 0; i--) {
	if ((p = process_tab[i]) != NULL) {
	    res = CONS(hp, process_tab[i]->id, res);
	    hp += 2;
	}
    }
    ASSERT(hp == hp_end);

    erts_smp_mtx_unlock(&proc_tab_mtx);

    return res;
}

Eterm
erts_debug_processes_bif_info(Process *c_p)
{
    ERTS_DECL_AM(processes_bif_info);
    Eterm elements[] = {
	AM_processes_bif_info,
	make_small((Uint) ERTS_PROCESSES_BIF_MIN_START_REDS),
	make_small((Uint) processes_bif_tab_chunks),
	make_small((Uint) ERTS_PROCESSES_BIF_TAB_CHUNK_SIZE),
	make_small((Uint) ERTS_PROCESSES_BIF_TAB_INSPECT_INDICES_PER_RED),
	make_small((Uint) ERTS_PROCESSES_BIF_TAB_FREE_TERM_PROC_REDS),
	make_small((Uint) ERTS_PROCESSES_BIF_INSPECT_TERM_PROC_PER_RED),
	make_small((Uint) ERTS_PROCESSES_INSPECT_TERM_PROC_MAX_REDS),
	make_small((Uint) ERTS_PROCESSES_BIF_BUILD_RESULT_CONSES_PER_RED),
	make_small((Uint) ERTS_PROCESSES_BIF_DEBUGLEVEL)
    };
    Uint sz = 0;
    Eterm *hp;
    (void) erts_bld_tuplev(NULL, &sz, sizeof(elements)/sizeof(Eterm), elements);
    hp = HAlloc(c_p, sz);
    return erts_bld_tuplev(&hp, NULL, sizeof(elements)/sizeof(Eterm), elements);
}

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_FOUND_PIDS
static void
debug_processes_check_found_pid(ErtsProcessesBifData *pbdp,
				Eterm pid,
				SysTimeval *tvp,
				int pid_should_be_found)
{
    int i;
    for (i = 0; i < pbdp->pid_ix; i++) {
	if (pbdp->pid[i] == pid
	    && pbdp->debug.pid_started[i].tv_sec == tvp->tv_sec
	    && pbdp->debug.pid_started[i].tv_usec == tvp->tv_usec) {
	    ERTS_PROCS_ASSERT(pid_should_be_found);
	    return;
	}
    }
    ERTS_PROCS_ASSERT(!pid_should_be_found);
}
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_RESLIST
static void
debug_processes_check_res_list(Eterm list)
{
    while (is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);
	ERTS_PROCS_ASSERT(is_internal_pid(hd));
	list = CDR(consp);
    }

    ERTS_PROCS_ASSERT(is_nil(list));
}
#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_PIDS

static void
debug_processes_save_all_pids(ErtsProcessesBifData *pbdp)
{
    int ix, tix, cpix;
    pbdp->debug.correct_pids_verified = 0;
    pbdp->debug.correct_pids = erts_alloc(ERTS_ALC_T_PROCS_PIDS,
					  sizeof(Eterm)*pbdp->pid_sz);

    for (tix = 0, cpix = 0; tix < erts_max_processes; tix++) {
	Process *rp = process_tab[tix];
	if (rp) {
	    ERTS_PROCS_ASSERT(is_internal_pid(rp->id));
	    pbdp->debug.correct_pids[cpix++] = rp->id;
	    ERTS_PROCS_ASSERT(cpix <= pbdp->pid_sz);
	}
    }
    ERTS_PROCS_ASSERT(cpix == pbdp->pid_sz);

    for (ix = 0; ix < pbdp->pid_sz; ix++)
	pbdp->pid[ix] = make_small(ix);
}

static void
debug_processes_verify_all_pids(ErtsProcessesBifData *pbdp)
{
    int ix, cpix;

    ERTS_PROCS_ASSERT(pbdp->pid_ix == pbdp->pid_sz);

    for (ix = 0; ix < pbdp->pid_sz; ix++) {
	int found = 0;
	Eterm pid = pbdp->pid[ix];
	ERTS_PROCS_ASSERT(is_internal_pid(pid));
	for (cpix = ix; cpix < pbdp->pid_sz; cpix++) {
	    if (pbdp->debug.correct_pids[cpix] == pid) {
		pbdp->debug.correct_pids[cpix] = NIL;
		found = 1;
		break;
	    }
	}
	if (!found) {
	    for (cpix = 0; cpix < ix; cpix++) {
		if (pbdp->debug.correct_pids[cpix] == pid) {
		    pbdp->debug.correct_pids[cpix] = NIL;
		    found = 1;
		    break;
		}
	    }
	}
	ERTS_PROCS_ASSERT(found);
    }
    pbdp->debug.correct_pids_verified = 1;

    erts_free(ERTS_ALC_T_PROCS_PIDS, pbdp->debug.correct_pids);
    pbdp->debug.correct_pids = NULL;
}
#endif /* ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_PIDS */

#if ERTS_PROCESSES_BIF_DEBUGLEVEL >= ERTS_PROCS_DBGLVL_CHK_TERM_PROC_LIST
static void
debug_processes_check_term_proc_list(void)
{
    ERTS_SMP_LC_ASSERT(erts_lc_mtx_is_locked(&proc_tab_mtx));
    if (!saved_term_procs.start)
	ERTS_PROCS_ASSERT(!saved_term_procs.end);
    else {
	SysTimeval tv_now;
	SysTimeval *prev_xtvp = NULL;
	ErtsTermProcElement *tpep;
	erts_get_emu_time(&tv_now);

	for (tpep = saved_term_procs.start; tpep; tpep = tpep->next) {
	    if (!tpep->prev)
		ERTS_PROCS_ASSERT(saved_term_procs.start == tpep);
	    else
		ERTS_PROCS_ASSERT(tpep->prev->next == tpep);
	    if (!tpep->next)
		ERTS_PROCS_ASSERT(saved_term_procs.end == tpep);
	    else
		ERTS_PROCS_ASSERT(tpep->next->prev == tpep);
	    if (tpep->ix < 0) {
		SysTimeval *tvp = &tpep->u.bif_invocation.time;
		ERTS_PROCS_ASSERT(erts_cmp_timeval(&debug_tv_start, tvp) < 0
				  && erts_cmp_timeval(tvp, &tv_now) < 0);
	    }
	    else {
		SysTimeval *stvp = &tpep->u.process.spawned;
		SysTimeval *xtvp = &tpep->u.process.exited;
		
		ERTS_PROCS_ASSERT(erts_cmp_timeval(&debug_tv_start,
						   stvp) < 0);
		ERTS_PROCS_ASSERT(erts_cmp_timeval(stvp, xtvp) < 0);
		if (prev_xtvp)
		    ERTS_PROCS_ASSERT(erts_cmp_timeval(prev_xtvp, xtvp) < 0);
		prev_xtvp = xtvp;
		ERTS_PROCS_ASSERT(is_internal_pid(tpep->u.process.pid));
		ERTS_PROCS_ASSERT(tpep->ix
				  == internal_pid_index(tpep->u.process.pid));
	    }
	}
	
    }
}

static void
debug_processes_check_term_proc_free_list(ErtsTermProcElement *free_list)
{
    if (saved_term_procs.start) {
	ErtsTermProcElement *ftpep;
	ErtsTermProcElement *tpep;

	for (ftpep = free_list; ftpep; ftpep = ftpep->next) {
	    for (tpep = saved_term_procs.start; tpep; tpep = tpep->next)
		ERTS_PROCS_ASSERT(ftpep != tpep);
	}
    }
}

#endif

#if ERTS_PROCESSES_BIF_DEBUGLEVEL != 0

static void
debug_processes_assert_error(char* expr, char* file, int line)
{   
    fflush(stdout);
    erts_fprintf(stderr, "%s:%d: Assertion failed: %s\n", file, line, expr);
    fflush(stderr);
    abort();
}

#endif

/*                                                                           *\
 * End of the processes/0 BIF implementation.                                *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
