/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "erl_version.h"
#include "erl_compile_flags.h"
#include "erl_db_util.h"
#include "erl_message.h"
#include "erl_binary.h"
#include "erl_db.h"
#include "erl_instrument.h"
#include "dist.h"
#include "erl_gc.h"
#include "erl_cpu_topology.h"
#include "erl_async.h"
#include "erl_thr_progress.h"
#define ERTS_PTAB_WANT_DEBUG_FUNCS__
#include "erl_ptab.h"
#ifdef HIPE
#include "hipe_arch.h"
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#ifdef VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

static Export* alloc_info_trap = NULL;
static Export* alloc_sizes_trap = NULL;

static Export *gather_sched_wall_time_res_trap;
static Export *gather_gc_info_res_trap;

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

/* Keep erts_system_version as a global variable for easy access from a core */
static char erts_system_version[] = ("Erlang " ERLANG_OTP_RELEASE
				     " (erts-" ERLANG_VERSION ")"
#if !HEAP_ON_C_STACK && !HALFWORD_HEAP
				     " [no-c-stack-objects]"
#endif
#ifndef OTP_RELEASE
#ifdef ERLANG_GIT_VERSION
				     " [source-" ERLANG_GIT_VERSION "]"
#else
				     " [source]"
#endif
#endif	
#ifdef ARCH_64
#if HALFWORD_HEAP
				     " [64-bit halfword]"
#else
				     " [64-bit]"
#endif
#endif
#ifdef ERTS_SMP
				     " [smp:%beu:%beu]"
#endif
#ifdef USE_THREADS
				     " [async-threads:%d]"
#endif
#ifdef HIPE
				     " [hipe]"
#endif	
#ifdef ERTS_ENABLE_KERNEL_POLL
				     " [kernel-poll:%s]"
#endif	
#ifdef ET_DEBUG
#if ET_DEBUG
				     " [type-assertions]"
#endif
#endif	
#ifdef DEBUG
				     " [debug-compiled]"
#endif	
#ifdef ERTS_ENABLE_LOCK_CHECK
				     " [lock-checking]"
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
				     " [lock-counting]"
#endif
#ifdef PURIFY
				     " [purify-compiled]"
#endif	
#ifdef VALGRIND
				     " [valgrind-compiled]"
#endif
#ifdef ERTS_FRMPTR
				     " [frame-pointer]"
#endif
#ifdef USE_DTRACE
				     " [dtrace]"
#endif
#ifdef USE_SYSTEMTAP
				     " [systemtap]"
#endif
				     "\n");

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#if defined(HAVE_SOLARIS_SPARC_PERFMON)
# include <sys/ioccom.h>
# define PERFMON_SETPCR			_IOW('P', 1, unsigned long long)
# define PERFMON_GETPCR			_IOR('P', 2, unsigned long long)
#endif

/* Cached, pre-built {OsType,OsFlavor} and {Major,Minor,Build} tuples */
static Eterm os_type_tuple;
static Eterm os_version_tuple;

static Eterm
current_function(Process* p, Process* rp, Eterm** hpp, int full_info);
static Eterm current_stacktrace(Process* p, Process* rp, Eterm** hpp);

static Eterm
bld_bin_list(Uint **hpp, Uint *szp, ErlOffHeap* oh)
{
    struct erl_off_heap_header* ohh;
    Eterm res = NIL;
    Eterm tuple;

    for (ohh = oh->first; ohh; ohh = ohh->next) {
	if (ohh->thing_word == HEADER_PROC_BIN) {
	    ProcBin* pb = (ProcBin*) ohh;
	    Eterm val = erts_bld_uword(hpp, szp, (UWord) pb->val);
	    Eterm orig_size = erts_bld_uint(hpp, szp, pb->val->orig_size);
    
	    if (szp)
		*szp += 4+2;
	    if (hpp) {
		Uint refc = (Uint) erts_smp_atomic_read_nob(&pb->val->refc);
		tuple = TUPLE3(*hpp, val, orig_size, make_small(refc));
		res = CONS(*hpp + 4, tuple, res);
		*hpp += 4+2;
	    }
	}
    }
    return res;
}


/*
  make_monitor_list:
  returns a list of records..
  -record(erl_monitor, {
            type, % MON_ORIGIN or MON_TARGET (1 or 3)
	    ref,
	    pid, % Process or nodename
	    name % registered name or []
          }).
*/

static void do_calc_mon_size(ErtsMonitor *mon, void *vpsz)
{
    Uint *psz = vpsz;
    *psz += IS_CONST(mon->ref) ? 0 : NC_HEAP_SIZE(mon->ref);
    *psz += IS_CONST(mon->pid) ? 0 : NC_HEAP_SIZE(mon->pid);
    *psz += 8; /* CONS + 5-tuple */ 
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} MonListContext;

static void do_make_one_mon_element(ErtsMonitor *mon, void * vpmlc)
{
    MonListContext *pmlc = vpmlc;
    Eterm tup;
    Eterm r = (IS_CONST(mon->ref)
	       ? mon->ref
	       : STORE_NC(&(pmlc->hp), &MSO(pmlc->p), mon->ref));
    Eterm p = (IS_CONST(mon->pid)
	       ? mon->pid
	       : STORE_NC(&(pmlc->hp), &MSO(pmlc->p), mon->pid));
    tup = TUPLE5(pmlc->hp, pmlc->tag, make_small(mon->type), r, p, mon->name);
    pmlc->hp += 6;
    pmlc->res = CONS(pmlc->hp, tup, pmlc->res);
    pmlc->hp += 2;
}

static Eterm 
make_monitor_list(Process *p, ErtsMonitor *root)
{
    DECL_AM(erl_monitor);
    Uint sz = 0;
    MonListContext mlc;

    erts_doforall_monitors(root, &do_calc_mon_size, &sz);
    if (sz == 0) {
	return NIL;
    }
    mlc.p = p;
    mlc.hp = HAlloc(p,sz);
    mlc.res = NIL;
    mlc.tag = AM_erl_monitor;
    erts_doforall_monitors(root, &do_make_one_mon_element, &mlc);
    return mlc.res;
}

/*
  make_link_list:
  returns a list of records..
  -record(erl_link, {
            type, % LINK_NODE or LINK_PID (1 or 3)
	    pid, % Process or nodename
	    targets % List of erl_link's or nil
          }).
*/

static void do_calc_lnk_size(ErtsLink *lnk, void *vpsz)
{
    Uint *psz = vpsz;
    *psz += IS_CONST(lnk->pid) ? 0 : NC_HEAP_SIZE(lnk->pid);
    if (lnk->type != LINK_NODE && ERTS_LINK_ROOT(lnk) != NULL) { 
	/* Node links use this pointer as ref counter... */
	erts_doforall_links(ERTS_LINK_ROOT(lnk),&do_calc_lnk_size,vpsz);
    }
    *psz += 7; /* CONS + 4-tuple */ 
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} LnkListContext;

static void do_make_one_lnk_element(ErtsLink *lnk, void * vpllc)
{
    LnkListContext *pllc = vpllc;
    Eterm tup;
    Eterm old_res, targets = NIL;
    Eterm p = (IS_CONST(lnk->pid)
	       ? lnk->pid
	       : STORE_NC(&(pllc->hp), &MSO(pllc->p), lnk->pid));
    if (lnk->type == LINK_NODE) {
	targets = make_small(ERTS_LINK_REFC(lnk));
    } else if (ERTS_LINK_ROOT(lnk) != NULL) {
	old_res = pllc->res;
	pllc->res = NIL;
	erts_doforall_links(ERTS_LINK_ROOT(lnk),&do_make_one_lnk_element, vpllc);
	targets = pllc->res;
	pllc->res = old_res;
    }
    tup = TUPLE4(pllc->hp, pllc->tag, make_small(lnk->type), p, targets);
    pllc->hp += 5;
    pllc->res = CONS(pllc->hp, tup, pllc->res);
    pllc->hp += 2;
}

static Eterm 
make_link_list(Process *p, ErtsLink *root, Eterm tail)
{
    DECL_AM(erl_link);
    Uint sz = 0;
    LnkListContext llc;

    erts_doforall_links(root, &do_calc_lnk_size, &sz);
    if (sz == 0) {
	return tail;
    }
    llc.p = p;
    llc.hp = HAlloc(p,sz);
    llc.res = tail;
    llc.tag = AM_erl_link;
    erts_doforall_links(root, &do_make_one_lnk_element, &llc);
    return llc.res;
}

int
erts_print_system_version(int to, void *arg, Process *c_p)
{
#ifdef ERTS_SMP
    Uint total, online, active;
    (void) erts_schedulers_state(&total, &online, &active, 0);
#endif
    return erts_print(to, arg, erts_system_version
#ifdef ERTS_SMP
		      , total, online
#endif
#ifdef USE_THREADS
		      , erts_async_max_threads
#endif
#ifdef ERTS_ENABLE_KERNEL_POLL
		      , erts_use_kernel_poll ? "true" : "false"
#endif
	);
}

typedef struct {
    Eterm entity;
    Eterm node;
} MonitorInfo;

typedef struct {
    MonitorInfo *mi;
    Uint mi_i;
    Uint mi_max;
    int sz;
} MonitorInfoCollection;

#define INIT_MONITOR_INFOS(MIC) do {		\
    (MIC).mi = NULL;				\
    (MIC).mi_i = (MIC).mi_max = 0;		\
    (MIC).sz = 0;                               \
} while(0)

#define MI_INC 50
#define EXTEND_MONITOR_INFOS(MICP)					\
do {									\
    if ((MICP)->mi_i >= (MICP)->mi_max) {				\
	(MICP)->mi = ((MICP)->mi ? erts_realloc(ERTS_ALC_T_TMP,		\
						(MICP)->mi,		\
						((MICP)->mi_max+MI_INC)	\
						* sizeof(MonitorInfo))	\
		      : erts_alloc(ERTS_ALC_T_TMP,			\
				   MI_INC*sizeof(MonitorInfo)));	\
	(MICP)->mi_max += MI_INC;					\
    }									\
 } while (0)
#define DESTROY_MONITOR_INFOS(MIC)			\
do {							\
    if ((MIC).mi != NULL) {				\
	erts_free(ERTS_ALC_T_TMP, (void *) (MIC).mi);	\
    }							\
 } while (0)

static void collect_one_link(ErtsLink *lnk, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
    EXTEND_MONITOR_INFOS(micp);
    if (!(lnk->type == LINK_PID)) {
	return;
    }
    micp->mi[micp->mi_i].entity = lnk->pid;
    micp->sz += 2 + NC_HEAP_SIZE(lnk->pid);
    micp->mi_i++;
} 

static void collect_one_origin_monitor(ErtsMonitor *mon, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
 
    if (mon->type != MON_ORIGIN) {
	return;
    }
    EXTEND_MONITOR_INFOS(micp);
    if (is_atom(mon->pid)) { /* external by name */
	micp->mi[micp->mi_i].entity = mon->name;
	micp->mi[micp->mi_i].node = mon->pid;
	micp->sz += 3; /* need one 2-tuple */
    } else if (is_external_pid(mon->pid)) { /* external by pid */
	micp->mi[micp->mi_i].entity = mon->pid;
	micp->mi[micp->mi_i].node = NIL;
	micp->sz += NC_HEAP_SIZE(mon->pid);
    } else if (!is_nil(mon->name)) { /* internal by name */
	micp->mi[micp->mi_i].entity = mon->name;
	micp->mi[micp->mi_i].node = erts_this_dist_entry->sysname;
	micp->sz += 3; /* need one 2-tuple */
    } else { /* internal by pid */
	micp->mi[micp->mi_i].entity = mon->pid;
	micp->mi[micp->mi_i].node = NIL;
	/* no additional heap space needed */
    }
    micp->mi_i++;
    micp->sz += 2 + 3; /* For a cons cell and a 2-tuple */
}

static void collect_one_target_monitor(ErtsMonitor *mon, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
 
    if (mon->type != MON_TARGET) {
	return;
    }

    EXTEND_MONITOR_INFOS(micp);
  
    micp->mi[micp->mi_i].node = NIL;
    micp->mi[micp->mi_i].entity = mon->pid;
    micp->sz += (NC_HEAP_SIZE(mon->pid) + 2 /* cons */);
    micp->mi_i++;
}

typedef struct {
    Process *c_p;
    ErtsProcLocks c_p_locks;
    ErtsSuspendMonitor **smi;
    Uint smi_i;
    Uint smi_max;
    int sz;
} ErtsSuspendMonitorInfoCollection;

#define ERTS_INIT_SUSPEND_MONITOR_INFOS(SMIC, CP, CPL) do {		\
    (SMIC).c_p = (CP);							\
    (SMIC).c_p_locks = (CPL);						\
    (SMIC).smi = NULL;							\
    (SMIC).smi_i = (SMIC).smi_max = 0;					\
    (SMIC).sz = 0;                               			\
} while(0)

#define ERTS_SMI_INC 50
#define ERTS_EXTEND_SUSPEND_MONITOR_INFOS(SMICP)			\
do {									\
    if ((SMICP)->smi_i >= (SMICP)->smi_max) {				\
	(SMICP)->smi = ((SMICP)->smi					\
			? erts_realloc(ERTS_ALC_T_TMP,			\
				       (SMICP)->smi,			\
				       ((SMICP)->smi_max		\
					+ ERTS_SMI_INC)			\
				       * sizeof(ErtsSuspendMonitor *))	\
			: erts_alloc(ERTS_ALC_T_TMP,			\
				     ERTS_SMI_INC			\
				     * sizeof(ErtsSuspendMonitor *)));	\
	(SMICP)->smi_max += ERTS_SMI_INC;				\
    }									\
 } while (0)

#define ERTS_DESTROY_SUSPEND_MONITOR_INFOS(SMIC)			\
do {									\
    if ((SMIC).smi != NULL) {						\
	erts_free(ERTS_ALC_T_TMP, (void *) (SMIC).smi);			\
    }									\
 } while (0)

static void
collect_one_suspend_monitor(ErtsSuspendMonitor *smon, void *vsmicp)
{
    ErtsSuspendMonitorInfoCollection *smicp = vsmicp;
    Process *suspendee = erts_pid2proc(smicp->c_p,
				       smicp->c_p_locks,
				       smon->pid,
				       0);
    if (suspendee) { /* suspendee is alive */
	Sint a, p;
	if (smon->active) {
	    smon->active += smon->pending;
	    smon->pending = 0;
	}

	ASSERT((smon->active && !smon->pending)
	       || (smon->pending && !smon->active));

	ERTS_EXTEND_SUSPEND_MONITOR_INFOS(smicp);

	smicp->smi[smicp->smi_i] = smon;
	smicp->sz += 2 /* cons */ + 4 /* 3-tuple */;

	a = (Sint) smon->active;	/* quiet compiler warnings */
	p = (Sint) smon->pending;	/* on 64-bit machines      */

	if (!IS_SSMALL(a))
	    smicp->sz += BIG_UINT_HEAP_SIZE;
	if (!IS_SSMALL(p))
	    smicp->sz += BIG_UINT_HEAP_SIZE;
	smicp->smi_i++;
    }
}

/*
 * process_info/[1,2]
 */

#define ERTS_PI_FAIL_TYPE_BADARG		0
#define ERTS_PI_FAIL_TYPE_YIELD			1
#define ERTS_PI_FAIL_TYPE_AWAIT_EXIT		2

static ERTS_INLINE ErtsProcLocks
pi_locks(Eterm info)
{
    switch (info) {
    case am_status:
    case am_priority:
	return ERTS_PROC_LOCK_STATUS;
    case am_links:
    case am_monitors:
    case am_monitored_by:
    case am_suspending:
	return ERTS_PROC_LOCK_LINK;
    case am_messages:
    case am_message_queue_len:
    case am_total_heap_size:
	return ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_MSGQ;
    case am_memory:
	return ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_MSGQ;
    default:
	return ERTS_PROC_LOCK_MAIN;
    }
}

/*
 * All valid process_info arguments.
 */
static Eterm pi_args[] = {
    am_registered_name,
    am_current_function,
    am_initial_call,
    am_status,
    am_messages,
    am_message_queue_len,
    am_links,
    am_monitors,
    am_monitored_by,
    am_dictionary,
    am_trap_exit,
    am_error_handler,
    am_heap_size,
    am_stack_size,
    am_memory,
    am_garbage_collection,
    am_group_leader,
    am_reductions,
    am_priority,
    am_trace,
    am_binary,
    am_sequential_trace_token,
    am_catchlevel,
    am_backtrace,
    am_last_calls,
    am_total_heap_size,
    am_suspending,
    am_min_heap_size,
    am_min_bin_vheap_size,
    am_current_location,
    am_current_stacktrace,
};    

#define ERTS_PI_ARGS ((int) (sizeof(pi_args)/sizeof(Eterm)))

static ERTS_INLINE Eterm
pi_ix2arg(int ix)
{
    if (ix < 0 || ERTS_PI_ARGS <= ix)
	return am_undefined;
    return pi_args[ix];
}

static ERTS_INLINE int
pi_arg2ix(Eterm arg)
{
    switch (arg) {
    case am_registered_name:			return 0;
    case am_current_function:			return 1;
    case am_initial_call:			return 2;
    case am_status:				return 3;
    case am_messages:				return 4;
    case am_message_queue_len:			return 5;
    case am_links:				return 6;
    case am_monitors:				return 7;
    case am_monitored_by:			return 8;
    case am_dictionary:				return 9;
    case am_trap_exit:				return 10;
    case am_error_handler:			return 11;
    case am_heap_size:				return 12;
    case am_stack_size:				return 13;
    case am_memory:				return 14;
    case am_garbage_collection:			return 15;
    case am_group_leader:			return 16;
    case am_reductions:				return 17;
    case am_priority:				return 18;
    case am_trace:				return 19;
    case am_binary:				return 20;
    case am_sequential_trace_token:		return 21;
    case am_catchlevel:				return 22;
    case am_backtrace:				return 23;
    case am_last_calls:				return 24;
    case am_total_heap_size:			return 25;
    case am_suspending:				return 26;
    case am_min_heap_size:			return 27;
    case am_min_bin_vheap_size:			return 28;
    case am_current_location:			return 29;
    case am_current_stacktrace:			return 30;
    default:					return -1;
    }
}

static Eterm pi_1_keys[] = {
    am_registered_name,
    am_current_function,
    am_initial_call,
    am_status,
    am_message_queue_len,
    am_messages,
    am_links,
    am_dictionary,
    am_trap_exit,
    am_error_handler,
    am_priority,
    am_group_leader,
    am_total_heap_size,
    am_heap_size,
    am_stack_size,
    am_reductions,
    am_garbage_collection,
    am_suspending
};

#define ERTS_PI_1_NO_OF_KEYS (sizeof(pi_1_keys)/sizeof(Eterm))

static Eterm pi_1_keys_list;
#if HEAP_ON_C_STACK
static Eterm pi_1_keys_list_heap[2*ERTS_PI_1_NO_OF_KEYS];
#endif

static void
process_info_init(void)
{
#if HEAP_ON_C_STACK
    Eterm *hp = &pi_1_keys_list_heap[0];
#else
    Eterm *hp = erts_alloc(ERTS_ALC_T_LL_TEMP_TERM,sizeof(Eterm)*2*ERTS_PI_1_NO_OF_KEYS);
#endif
    int i;

    pi_1_keys_list = NIL;

    for (i = ERTS_PI_1_NO_OF_KEYS-1; i >= 0; i--) {
	pi_1_keys_list = CONS(hp, pi_1_keys[i], pi_1_keys_list);
	hp += 2;
    }

#ifdef DEBUG
    { /* Make sure the process_info argument mappings are consistent */
	int ix;
	for (ix = 0; ix < ERTS_PI_ARGS; ix++) {
	    ASSERT(pi_arg2ix(pi_ix2arg(ix)) == ix);
	}
    }
#endif

}

static ERTS_INLINE Process *
pi_pid2proc(Process *c_p, Eterm pid, ErtsProcLocks info_locks)
{
#ifdef ERTS_SMP
    /*
     * If the main lock is needed, we use erts_pid2proc_not_running()
     * instead of erts_pid2proc() for two reasons:
     * * Current function of pid and possibly other information will
     *   have been updated so that process_info() is consistent with an
     *   info-request/info-response signal model.
     * * We avoid blocking the whole scheduler executing the
     *   process that is calling process_info() for a long time
     *   which will happen if pid is currently running.
     * The caller of process_info() may have to yield if pid
     * is currently running.
     */

    if (info_locks & ERTS_PROC_LOCK_MAIN)
	return erts_pid2proc_not_running(c_p, ERTS_PROC_LOCK_MAIN,
					 pid, info_locks);
    else
#endif
	return erts_pid2proc(c_p, ERTS_PROC_LOCK_MAIN,
			     pid, info_locks);
}



BIF_RETTYPE
process_info_aux(Process *BIF_P,
		 Process *rp,
		 Eterm rpid,
		 Eterm item,
		 int always_wrap);

#define ERTS_PI_RES_ELEM_IX_BUF_INC 1024
#define ERTS_PI_DEF_RES_ELEM_IX_BUF_SZ ERTS_PI_ARGS

static Eterm
process_info_list(Process *c_p, Eterm pid, Eterm list, int always_wrap,
		  int *fail_type)
{
    int want_messages = 0;
    int def_res_elem_ix_buf[ERTS_PI_DEF_RES_ELEM_IX_BUF_SZ];
    int *res_elem_ix = &def_res_elem_ix_buf[0];
    int res_elem_ix_ix = -1;
    int res_elem_ix_sz = ERTS_PI_DEF_RES_ELEM_IX_BUF_SZ;
    Eterm part_res[ERTS_PI_ARGS];
    Eterm res, arg;
    Uint *hp, *hp_end;
    ErtsProcLocks locks = (ErtsProcLocks) 0;
    int res_len, ix;
    Process *rp = NULL;

    *fail_type = ERTS_PI_FAIL_TYPE_BADARG;

    for (ix = 0; ix < ERTS_PI_ARGS; ix++)
	part_res[ix] = THE_NON_VALUE;

    ASSERT(is_list(list));

    while (is_list(list)) {
	Eterm* consp = list_val(list);

	arg = CAR(consp);
	ix = pi_arg2ix(arg);
	if (ix < 0) {
	    res = THE_NON_VALUE;
	    goto done;
	}
	if (arg == am_messages)
	    want_messages = 1;
	locks |= pi_locks(arg);
	res_elem_ix_ix++;
	if (res_elem_ix_ix >= res_elem_ix_sz) {
	    if (res_elem_ix != &def_res_elem_ix_buf[0])
		res_elem_ix =
		    erts_realloc(ERTS_ALC_T_TMP,
				 res_elem_ix,
				 sizeof(int)*(res_elem_ix_sz
					      += ERTS_PI_RES_ELEM_IX_BUF_INC));
	    else {
		int new_res_elem_ix_sz = ERTS_PI_RES_ELEM_IX_BUF_INC;
		int *new_res_elem_ix = erts_alloc(ERTS_ALC_T_TMP,
						  sizeof(int)*new_res_elem_ix_sz);
		sys_memcpy((void *) new_res_elem_ix,
			   (void *) res_elem_ix,
			   sizeof(int)*res_elem_ix_sz);
		res_elem_ix = new_res_elem_ix;
		res_elem_ix_sz = new_res_elem_ix_sz;
	    }
	}
	res_elem_ix[res_elem_ix_ix] = ix;
	list = CDR(consp);
    }
    if (is_not_nil(list)) {
	res = THE_NON_VALUE;
	goto done;
    }

    res_len = res_elem_ix_ix+1;

    ASSERT(res_len > 0);

    rp = pi_pid2proc(c_p, pid, locks|ERTS_PROC_LOCK_STATUS);
    if (!rp) {
	res = am_undefined;
	goto done;
    }
    else if (rp == ERTS_PROC_LOCK_BUSY) {
	rp = NULL;
	res = THE_NON_VALUE;
	*fail_type = ERTS_PI_FAIL_TYPE_YIELD;
	goto done;
    }
    else if (c_p != rp && ERTS_PROC_PENDING_EXIT(rp)) {
	locks |= ERTS_PROC_LOCK_STATUS;
	res = THE_NON_VALUE;
	*fail_type = ERTS_PI_FAIL_TYPE_AWAIT_EXIT;
	goto done;
    }
    else if (!(locks & ERTS_PROC_LOCK_STATUS)) {
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
    }
	

    /*
     * We always handle 'messages' first if it should be part
     * of the result. This since if both 'messages' and
     * 'message_queue_len' are wanted, 'messages' may
     * change the result of 'message_queue_len' (in case
     * the queue contain bad distribution messages).
     */
    if (want_messages) {
	ix = pi_arg2ix(am_messages);
	ASSERT(part_res[ix] == THE_NON_VALUE);
	part_res[ix] = process_info_aux(c_p, rp, pid, am_messages, always_wrap);
	ASSERT(part_res[ix] != THE_NON_VALUE);
    }

    for (; res_elem_ix_ix >= 0; res_elem_ix_ix--) {
	ix = res_elem_ix[res_elem_ix_ix];
	if (part_res[ix] == THE_NON_VALUE) {
	    arg = pi_ix2arg(ix);
	    part_res[ix] = process_info_aux(c_p, rp, pid, arg, always_wrap);
	    ASSERT(part_res[ix] != THE_NON_VALUE);
	}
    }

    hp = HAlloc(c_p, res_len*2);
    hp_end = hp + res_len*2;
    res = NIL;

    for (res_elem_ix_ix = res_len - 1; res_elem_ix_ix >= 0; res_elem_ix_ix--) {
	ix = res_elem_ix[res_elem_ix_ix];
	ASSERT(part_res[ix] != THE_NON_VALUE);
	/*
	 * If we should ignore the value of registered_name,
	 * its value is nil. For more info, see comment in the
	 * beginning of process_info_aux().
	 */
	if (is_nil(part_res[ix])) {
	    ASSERT(!always_wrap);
	    ASSERT(pi_ix2arg(ix) == am_registered_name);
	}
	else {
	    res = CONS(hp, part_res[ix], res);
	    hp += 2;
	}
    }

    if (!always_wrap) {
	HRelease(c_p, hp_end, hp);
    }

 done:

    if (c_p == rp)
	locks &= ~ERTS_PROC_LOCK_MAIN;
    if (locks && rp)
	erts_smp_proc_unlock(rp, locks);

    if (res_elem_ix != &def_res_elem_ix_buf[0])
	erts_free(ERTS_ALC_T_TMP, res_elem_ix);

    return res;
}

BIF_RETTYPE process_info_1(BIF_ALIST_1)
{
    Eterm res;
    int fail_type;

    if (is_external_pid(BIF_ARG_1)
	&& external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	BIF_RET(am_undefined);
	
    if (is_not_internal_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    res = process_info_list(BIF_P, BIF_ARG_1, pi_1_keys_list, 0, &fail_type);
    if (is_non_value(res)) {
	switch (fail_type) {
	case ERTS_PI_FAIL_TYPE_BADARG:
	    BIF_ERROR(BIF_P, BADARG);
	case ERTS_PI_FAIL_TYPE_YIELD:
	    ERTS_BIF_YIELD1(bif_export[BIF_process_info_1], BIF_P, BIF_ARG_1);
	case ERTS_PI_FAIL_TYPE_AWAIT_EXIT:
	    ERTS_BIF_AWAIT_X_DATA_TRAP(BIF_P, BIF_ARG_1, am_undefined);
	default:
	    erl_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error", __FILE__, __LINE__);
	}
    }

    ASSERT(!(BIF_P->flags & F_P2PNR_RESCHED));
    BIF_RET(res);
}


BIF_RETTYPE process_info_2(BIF_ALIST_2) 
{
    Eterm res;
    Process *rp;
    Eterm pid = BIF_ARG_1;
    ErtsProcLocks info_locks;
    int fail_type;

    if (is_external_pid(pid)
	&& external_pid_dist_entry(pid) == erts_this_dist_entry)
	BIF_RET(am_undefined);
	
    if (is_not_internal_pid(pid)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_nil(BIF_ARG_2))
	BIF_RET(NIL);

    if (is_list(BIF_ARG_2)) {
	res = process_info_list(BIF_P, BIF_ARG_1, BIF_ARG_2, 1, &fail_type);
	if (is_non_value(res)) {
	    switch (fail_type) {
	    case ERTS_PI_FAIL_TYPE_BADARG:
		BIF_ERROR(BIF_P, BADARG);
	    case ERTS_PI_FAIL_TYPE_YIELD:
		ERTS_BIF_YIELD2(bif_export[BIF_process_info_2], BIF_P,
				BIF_ARG_1, BIF_ARG_2);
	    case ERTS_PI_FAIL_TYPE_AWAIT_EXIT:
		ERTS_BIF_AWAIT_X_DATA_TRAP(BIF_P, BIF_ARG_1, am_undefined);
	    default:
		erl_exit(ERTS_ABORT_EXIT, "%s:%d: Internal error",
			 __FILE__, __LINE__);
	    }
	}
	ASSERT(!(BIF_P->flags & F_P2PNR_RESCHED));
	BIF_RET(res);
    }

    if (pi_arg2ix(BIF_ARG_2) < 0)
	BIF_ERROR(BIF_P, BADARG);

    info_locks = pi_locks(BIF_ARG_2); 

    rp = pi_pid2proc(BIF_P, pid, info_locks|ERTS_PROC_LOCK_STATUS);
    if (!rp)
	res = am_undefined;
    else if (rp == ERTS_PROC_LOCK_BUSY)
	ERTS_BIF_YIELD2(bif_export[BIF_process_info_2], BIF_P,
			BIF_ARG_1, BIF_ARG_2);
    else if (rp != BIF_P && ERTS_PROC_PENDING_EXIT(rp)) {
	erts_smp_proc_unlock(rp, info_locks|ERTS_PROC_LOCK_STATUS);
	ERTS_BIF_AWAIT_X_DATA_TRAP(BIF_P, BIF_ARG_1, am_undefined);
    }
    else {
	if (!(info_locks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	res = process_info_aux(BIF_P, rp, pid, BIF_ARG_2, 0);
    }
    ASSERT(is_value(res));

#ifdef ERTS_SMP
    if (BIF_P == rp)
	info_locks &= ~ERTS_PROC_LOCK_MAIN;
    if (rp && info_locks)
	erts_smp_proc_unlock(rp, info_locks);
#endif

    ASSERT(!(BIF_P->flags & F_P2PNR_RESCHED));
    BIF_RET(res);
}

Eterm
process_info_aux(Process *BIF_P,
		 Process *rp,
		 Eterm rpid,
		 Eterm item,
		 int always_wrap)
{
    Eterm *hp;
    Eterm res = NIL;

    ASSERT(rp);

    /*
     * Q: Why this always_wrap argument?
     *
     * A: registered_name is strange. If process has no registered name,
     *    process_info(Pid, registered_name) returns [], and
     *    the result of process_info(Pid) has no {registered_name, Name}
     *    tuple in the resulting list. This is inconsistent with all other
     *    options, but we do not dare to change it.
     *
     *    When process_info/2 is called with a list as second argument,
     *    registered_name behaves as it should, i.e. a
     *    {registered_name, []} will appear in the resulting list.
     *
     *    If always_wrap != 0, process_info_aux() always wrap the result
     *    in a key two tuple. 
     */

    switch (item) {

    case am_registered_name:
	if (rp->common.u.alive.reg) {
	    hp = HAlloc(BIF_P, 3);
	    res = rp->common.u.alive.reg->name;
	} else {
	    if (always_wrap) {
		hp = HAlloc(BIF_P, 3);
		res = NIL;
	    }
	    else {
		return NIL;
	    }
	}
	break;

    case am_current_function:
	res = current_function(BIF_P, rp, &hp, 0);
	break;

    case am_current_location:
	res = current_function(BIF_P, rp, &hp, 1);
	break;

    case am_current_stacktrace:
	res = current_stacktrace(BIF_P, rp, &hp);
	break;

    case am_initial_call:
	hp = HAlloc(BIF_P, 3+4);
	res = TUPLE3(hp,
		     rp->initial[INITIAL_MOD],
		     rp->initial[INITIAL_FUN],
		     make_small(rp->initial[INITIAL_ARI]));
	hp += 4;
	break;

    case am_status:
	res = erts_process_status(BIF_P, ERTS_PROC_LOCK_MAIN, rp, rpid);
	ASSERT(res != am_undefined);
	hp = HAlloc(BIF_P, 3);
	break;

    case am_messages: {
	ErlMessage* mp;
	int n;

	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);
	n = rp->msg.len;

	if (n == 0 || ERTS_TRACE_FLAGS(rp) & F_SENSITIVE) {
	    hp = HAlloc(BIF_P, 3);
	} else {
	    int remove_bad_messages = 0;
	    struct {
		Uint copy_struct_size;
		ErlMessage* msgp;
	    } *mq = erts_alloc(ERTS_ALC_T_TMP, n*sizeof(*mq));
	    Sint i = 0;
	    Uint heap_need = 3;
	    Eterm *hp_end;

	    for (mp = rp->msg.first; mp; mp = mp->next) {
		heap_need += 2;
		mq[i].msgp = mp;
		if (rp != BIF_P) {
		    Eterm msg = ERL_MESSAGE_TERM(mq[i].msgp);
		    if (is_value(msg)) {
			mq[i].copy_struct_size = (is_immed(msg)? 0 :
						  size_object(msg));
		    }
		    else if (mq[i].msgp->data.attached) {
			mq[i].copy_struct_size
			    = erts_msg_attached_data_size(mq[i].msgp);
		    }
		    else {
			/* Bad distribution message; ignore */
			remove_bad_messages = 1;
			mq[i].copy_struct_size = 0;
		    }
		    heap_need += mq[i].copy_struct_size;
		}
		else {
		    mq[i].copy_struct_size = 0;
		    if (mp->data.attached)
			heap_need += erts_msg_attached_data_size(mp);
		}
		i++;
	    }

	    hp = HAlloc(BIF_P, heap_need);
	    hp_end = hp + heap_need;
	    ASSERT(i == n);
	    for (i--; i >= 0; i--) {
		Eterm msg = ERL_MESSAGE_TERM(mq[i].msgp);
		if (rp != BIF_P) {
		    if (is_value(msg)) {
			if (mq[i].copy_struct_size)
			    msg = copy_struct(msg,
					      mq[i].copy_struct_size,
					      &hp,
					      &MSO(BIF_P));
		    }
		    else if (mq[i].msgp->data.attached) {
			ErlHeapFragment *hfp;
			/*
			 * Decode it into a message buffer and attach it
			 * to the message instead of the attached external
			 * term.
			 *
			 * Note that we may not pass a process pointer
			 * to erts_msg_distext2heap(), since it would then
			 * try to alter locks on that process.
			 */
			msg = erts_msg_distext2heap(
			    NULL, NULL, &hfp, &ERL_MESSAGE_TOKEN(mq[i].msgp),
			    mq[i].msgp->data.dist_ext);

			ERL_MESSAGE_TERM(mq[i].msgp) = msg;
			mq[i].msgp->data.heap_frag = hfp;

			if (is_non_value(msg)) {
			    ASSERT(!mq[i].msgp->data.heap_frag);
			    /* Bad distribution message; ignore */
			    remove_bad_messages = 1;
			    continue;
			}
			else {
			    /* Make our copy of the message */
			    ASSERT(size_object(msg) == hfp->used_size);
			    msg = copy_struct(msg,
					      hfp->used_size,
					      &hp,
					      &MSO(BIF_P));
			}
		    }
		    else {
			/* Bad distribution message; ignore */
			remove_bad_messages = 1;
			continue;
		    }
		}
		else {
		    if (mq[i].msgp->data.attached) {
			/* Decode it on the heap */
			erts_move_msg_attached_data_to_heap(&hp,
							    &MSO(BIF_P),
							    mq[i].msgp);
			msg = ERL_MESSAGE_TERM(mq[i].msgp);
			ASSERT(!mq[i].msgp->data.attached);
			if (is_non_value(msg)) {
			    /* Bad distribution message; ignore */
			    remove_bad_messages = 1;
			    continue;
			}
		    }
		}
		    
		res = CONS(hp, msg, res);
		hp += 2;
	    }
	    HRelease(BIF_P, hp_end, hp+3);
	    erts_free(ERTS_ALC_T_TMP, mq);
	    if (remove_bad_messages) {
		ErlMessage **mpp;
		/*
		 * We need to remove bad distribution messages from
		 * the queue, so that the value returned for
		 * 'message_queue_len' is consistent with the value
		 * returned for 'messages'.
		 */
		mpp = &rp->msg.first;
		mp = rp->msg.first;
		while (mp) {
		    if (is_value(ERL_MESSAGE_TERM(mp))) {
			mpp = &mp->next;
			mp = mp->next;
		    }
		    else {
			ErlMessage* bad_mp = mp;
			ASSERT(!mp->data.attached);
			if (rp->msg.save == &mp->next)
			    rp->msg.save = mpp;
			if (rp->msg.last == &mp->next)
			    rp->msg.last = mpp;
			*mpp = mp->next;
			mp = mp->next;
			rp->msg.len--;
			free_message(bad_mp);
		    }
		}
	    }
	}
	break;
    }

    case am_message_queue_len:
	hp = HAlloc(BIF_P, 3);
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);
	res = make_small(rp->msg.len);
	break;

    case am_links: {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_doforall_links(ERTS_P_LINKS(rp),&collect_one_link,&mic);

	hp = HAlloc(BIF_P, 3 + mic.sz);
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    item = STORE_NC(&hp, &MSO(BIF_P), mic.mi[i].entity); 
	    res = CONS(hp, item, res);
	    hp += 2;
	}
	DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case am_monitors: {
	MonitorInfoCollection mic;
	int i;

	INIT_MONITOR_INFOS(mic);
	erts_doforall_monitors(ERTS_P_MONITORS(rp),&collect_one_origin_monitor,&mic);
	hp = HAlloc(BIF_P, 3 + mic.sz);
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    if (is_atom(mic.mi[i].entity)) {
		/* Monitor by name. 
		 * Build {process, {Name, Node}} and cons it. 
		 */
		Eterm t1, t2;

		t1 = TUPLE2(hp, mic.mi[i].entity, mic.mi[i].node);
		hp += 3;
		t2 = TUPLE2(hp, am_process, t1);
		hp += 3;
		res = CONS(hp, t2, res);
		hp += 2;
	    }
	    else {
		/* Monitor by pid. Build {process, Pid} and cons it. */
		Eterm t;
		Eterm pid = STORE_NC(&hp, &MSO(BIF_P), mic.mi[i].entity);
		t = TUPLE2(hp, am_process, pid);
		hp += 3;
		res = CONS(hp, t, res);
		hp += 2;
	    }
	}
	DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case am_monitored_by: {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);
	erts_doforall_monitors(ERTS_P_MONITORS(rp),&collect_one_target_monitor,&mic);
	hp = HAlloc(BIF_P, 3 + mic.sz);

	res = NIL;
	for (i = 0; i < mic.mi_i; ++i) {
	    item = STORE_NC(&hp, &MSO(BIF_P), mic.mi[i].entity); 
	    res = CONS(hp, item, res);
	    hp += 2;
	}
	DESTROY_MONITOR_INFOS(mic);
	break;
    }

    case am_suspending: {
	ErtsSuspendMonitorInfoCollection smic;
	int i;
	Eterm item;
#ifdef DEBUG
	Eterm *hp_end;
#endif

	ERTS_INIT_SUSPEND_MONITOR_INFOS(smic,
					BIF_P,
					(BIF_P == rp
					 ? ERTS_PROC_LOCK_MAIN
					 : 0) | ERTS_PROC_LOCK_LINK);

	erts_doforall_suspend_monitors(rp->suspend_monitors,
				       &collect_one_suspend_monitor,
				       &smic);
	hp = HAlloc(BIF_P, 3 + smic.sz);
#ifdef DEBUG
	hp_end = hp + smic.sz;
#endif
	
	res = NIL;
	for (i = 0; i < smic.smi_i; i++) {
	    Sint a = (Sint) smic.smi[i]->active;  /* quiet compiler warnings */
	    Sint p = (Sint) smic.smi[i]->pending; /* on 64-bit machines...   */
	    Eterm active;
	    Eterm pending;
	    if (IS_SSMALL(a))
		active = make_small(a);
	    else {
		active = small_to_big(a, hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    if (IS_SSMALL(p))
		pending = make_small(p);
	    else {
		pending = small_to_big(p, hp);
		hp += BIG_UINT_HEAP_SIZE;
	    }
	    item = TUPLE3(hp, smic.smi[i]->pid, active, pending);
	    hp += 4;
	    res = CONS(hp, item, res);
	    hp += 2;
	}

	ERTS_DESTROY_SUSPEND_MONITOR_INFOS(smic);
	ASSERT(hp == hp_end);

	break;
    }

    case am_dictionary:
	if (ERTS_TRACE_FLAGS(rp) & F_SENSITIVE) {
	    res = NIL;
	} else {
	    res = erts_dictionary_copy(BIF_P, rp->dictionary);
	}
	hp = HAlloc(BIF_P, 3);
	break;

    case am_trap_exit: {
	erts_aint32_t state = erts_smp_atomic32_read_nob(&rp->state);
	hp = HAlloc(BIF_P, 3);
	if (state & ERTS_PSFLG_TRAP_EXIT)
	    res = am_true;
	else
	    res = am_false;
	break;
    }

    case am_error_handler:
	hp = HAlloc(BIF_P, 3);
	res = erts_proc_get_error_handler(BIF_P);
	break;

    case am_heap_size: {
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, HEAP_SIZE(rp));
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, HEAP_SIZE(rp));
	break;
    }

    case am_fullsweep_after: {
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, MAX_GEN_GCS(rp));
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, MAX_GEN_GCS(rp));
	break;
    }

    case am_min_heap_size: {
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, MIN_HEAP_SIZE(rp));
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, MIN_HEAP_SIZE(rp));
	break;
    }

    case am_min_bin_vheap_size: {
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, MIN_VHEAP_SIZE(rp));
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, MIN_VHEAP_SIZE(rp));
	break;
    }

    case am_total_heap_size: {
	ErlMessage *mp;
	Uint total_heap_size;
	Uint hsz = 3;

	total_heap_size = rp->heap_sz;
	if (rp->old_hend && rp->old_heap)
	    total_heap_size += rp->old_hend - rp->old_heap;

	total_heap_size += rp->mbuf_sz;

	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);

	for (mp = rp->msg.first; mp; mp = mp->next)
	    if (mp->data.attached)
		total_heap_size += erts_msg_attached_data_size(mp);

	(void) erts_bld_uint(NULL, &hsz, total_heap_size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, total_heap_size);
	break;
    }

    case am_stack_size: {
	Uint stack_size = STACK_START(rp) - rp->stop;
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, stack_size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, stack_size);
	break;
    }

    case am_memory: { /* Memory consumed in bytes */
	Uint hsz = 3;
	Uint size = erts_process_memory(rp);
	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
	break;
    }

    case am_garbage_collection: {
        DECL_AM(minor_gcs);
        Eterm t;

	hp = HAlloc(BIF_P, 3+2 + 3+2 + 3+2 + 3+2 + 3); /* last "3" is for outside tuple */

	t = TUPLE2(hp, AM_minor_gcs, make_small(GEN_GCS(rp))); hp += 3;
	res = CONS(hp, t, NIL); hp += 2;
	t = TUPLE2(hp, am_fullsweep_after, make_small(MAX_GEN_GCS(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;

	t = TUPLE2(hp, am_min_heap_size, make_small(MIN_HEAP_SIZE(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;
	t = TUPLE2(hp, am_min_bin_vheap_size, make_small(MIN_VHEAP_SIZE(rp))); hp += 3;
	res = CONS(hp, t, res); hp += 2;
	break;
    }

    case am_group_leader: {
	int sz = NC_HEAP_SIZE(rp->group_leader);
	hp = HAlloc(BIF_P, 3 + sz);
	res = STORE_NC(&hp, &MSO(BIF_P), rp->group_leader);
	break;
    }

    case am_reductions: {
	Uint reds = rp->reds + erts_current_reductions(BIF_P, rp);
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, reds);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, reds);
	break;
    }

    case am_priority:
	hp = HAlloc(BIF_P, 3);
	res = erts_get_process_priority(rp);
	break;

    case am_trace:
	hp = HAlloc(BIF_P, 3);
	res = make_small(ERTS_TRACE_FLAGS(rp) & TRACEE_FLAGS);
	break;

    case am_binary: {
	Uint sz = 3;
	(void) bld_bin_list(NULL, &sz, &MSO(rp));
	hp = HAlloc(BIF_P, sz);
	res = bld_bin_list(&hp, NULL, &MSO(rp));
	break;
    }

    case am_sequential_trace_token:
	res = copy_object(rp->seq_trace_token, BIF_P);
	hp = HAlloc(BIF_P, 3);
	break;

    case am_catchlevel:
	hp = HAlloc(BIF_P, 3);
	res = make_small(catchlevel(BIF_P));
	break;

    case am_backtrace: {
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_stack_dump(ERTS_PRINT_DSBUF, (void *) dsbufp, rp);
	res = new_binary(BIF_P, (byte *) dsbufp->str, dsbufp->str_len);
	erts_destroy_tmp_dsbuf(dsbufp);
	hp = HAlloc(BIF_P, 3);
	break;
    }

    case am_last_calls: {
	struct saved_calls *scb = ERTS_PROC_GET_SAVED_CALLS_BUF(BIF_P);
	if (!scb) {
	    hp = HAlloc(BIF_P, 3);
	    res = am_false;
	} else {
	    /*
	     * One cons cell and a 3-struct, and a 2-tuple.
	     * Might be less than that, if there are sends, receives or timeouts,
	     * so we must do a HRelease() to avoid creating holes.
	     */
	    Uint needed = scb->n*(2+4) + 3;
	    Eterm* limit;
	    Eterm term, list;
	    int i, j;

	    hp = HAlloc(BIF_P, needed);
	    limit = hp + needed;
	    list = NIL;
	    for (i = 0; i < scb->n; i++) {
		j = scb->cur - i - 1;
		if (j < 0)
		    j += scb->len;
		if (scb->ct[j] == &exp_send)
		    term = am_send;
		else if (scb->ct[j] == &exp_receive)
		    term = am_receive;
		else if (scb->ct[j] == &exp_timeout)
		    term = am_timeout;
		else {
		    term = TUPLE3(hp,
				  scb->ct[j]->code[0],
				  scb->ct[j]->code[1],
				  make_small(scb->ct[j]->code[2]));
		    hp += 4;
		}
		list = CONS(hp, term, list);
		hp += 2;
	    }
	    res = list;
	    res = TUPLE2(hp, item, res);
	    hp += 3;
	    HRelease(BIF_P,limit,hp);
	    return res;
	}
	break;
    }

    default:
	return THE_NON_VALUE; /* will produce badarg */

    }

    return TUPLE2(hp, item, res);
}
#undef MI_INC

static Eterm
current_function(Process* BIF_P, Process* rp, Eterm** hpp, int full_info)
{
    Eterm* hp;
    Eterm res;
    FunctionInfo fi;

    if (rp->current == NULL) {
	erts_lookup_function_info(&fi, rp->i, full_info);
	rp->current = fi.current;
    } else if (full_info) {
	erts_lookup_function_info(&fi, rp->i, full_info);
	if (fi.current == NULL) {
	    /* Use the current function without location info */
	    erts_set_current_function(&fi, rp->current);
	}
    }

    if (BIF_P == rp) {
	FunctionInfo fi2;

	/*
	 * The current function is erlang:process_info/{1,2},
	 * which is not the answer that the application want.
	 * We will use the function pointed into by rp->cp
	 * instead if it can be looked up.
	 */
	erts_lookup_function_info(&fi2, rp->cp, full_info);
	if (fi2.current) {
	    fi = fi2;
	    rp->current = fi2.current;
	}
    }

    /*
     * Return the result.
     */
    if (rp->current == NULL) {
	hp = HAlloc(BIF_P, 3);
	res = am_undefined;
    } else if (full_info) {
	hp = HAlloc(BIF_P, 3+fi.needed);
	hp = erts_build_mfa_item(&fi, hp, am_true, &res);
    } else {
	hp = HAlloc(BIF_P, 3+4);
	res = TUPLE3(hp, rp->current[0],
		     rp->current[1], make_small(rp->current[2]));
	hp += 4;
    }
    *hpp = hp;
    return res;
}

static Eterm
current_stacktrace(Process* p, Process* rp, Eterm** hpp)
{
    Uint sz;
    struct StackTrace* s;
    int depth;
    FunctionInfo* stk;
    FunctionInfo* stkp;
    Uint heap_size;
    int i;
    Eterm* hp = *hpp;
    Eterm mfa;
    Eterm res = NIL;

    depth = 8;
    sz = offsetof(struct StackTrace, trace) + sizeof(BeamInstr *)*depth;
    s = (struct StackTrace *) erts_alloc(ERTS_ALC_T_TMP, sz);
    s->depth = 0;
    if (rp->i) {
	s->trace[s->depth++] = rp->i;
	depth--;
    }
    if (depth > 0 && rp->cp != 0) {
	s->trace[s->depth++] = rp->cp - 1;
	depth--;
    }
    erts_save_stacktrace(rp, s, depth);

    depth = s->depth;
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
					     depth*sizeof(FunctionInfo));
    heap_size = 3;
    for (i = 0; i < depth; i++) {
	erts_lookup_function_info(stkp, s->trace[i], 1);
	if (stkp->current) {
	    heap_size += stkp->needed + 2;
	    stkp++;
	}
    }

    hp = HAlloc(p, heap_size);
    while (stkp > stk) {
	stkp--;
	hp = erts_build_mfa_item(stkp, hp, am_true, &mfa);
	res = CONS(hp, mfa, res);
	hp += 2;
    }

    erts_free(ERTS_ALC_T_TMP, stk);
    erts_free(ERTS_ALC_T_TMP, s);
    *hpp = hp;
    return res;
}

#if defined(VALGRIND)
static int check_if_xml(void)
{
    char buf[1];
    size_t bufsz = sizeof(buf);
    return erts_sys_getenv_raw("VALGRIND_LOG_XML", buf, &bufsz) >= 0;
}
#else
#define check_if_xml() 0
#endif

/*
 * This function takes care of calls to erlang:system_info/1 when the argument
 * is a tuple.
 */
static BIF_RETTYPE
info_1_tuple(Process* BIF_P,	/* Pointer to current process. */
	     Eterm* tp,		/* Pointer to first element in tuple */
	     int arity)		/* Arity of tuple (untagged). */
{
    Eterm ret;
    Eterm sel;

    sel = *tp++;

    if (sel == am_memory_internal) {
	switch (arity) {
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 1, 1))
		return am_true;
	default:
	    goto badarg;
	}
    }
    else if (sel == am_allocator_sizes) {
	switch (arity) {
	case 2:
	    ERTS_BIF_PREP_TRAP1(ret, alloc_sizes_trap, BIF_P, *tp);
	    return ret;
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 1, 0))
		return am_true;
	default:
	    goto badarg;
	}
    }
    else if (sel == am_wordsize && arity == 2) {
	if (tp[0] == am_internal) {
	    return make_small(sizeof(Eterm));
	}
	if (tp[0] == am_external) {
	    return make_small(sizeof(UWord));
	}
	goto badarg;
    } else if (sel == am_allocated) {
	if (arity == 2) {
	    Eterm res = THE_NON_VALUE;
	    char *buf;
	    int len = is_string(*tp);
	    if (len <= 0)
		return res;
	    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, len+1);
	    if (intlist_to_buf(*tp, buf, len) != len)
		erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
	    buf[len] = '\0';
	    res = erts_instr_dump_memory_map(buf) ? am_true : am_false;
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    if (is_non_value(res))
		goto badarg;
	    return res;
	}
	else if (arity == 3 && tp[0] == am_status) {
	    if (is_atom(tp[1]))
		return erts_instr_get_stat(BIF_P, tp[1], 1);
	    else {
		Eterm res = THE_NON_VALUE;
		char *buf;
		int len = is_string(tp[1]);
		if (len <= 0)
		    return res;
		buf = (char *) erts_alloc(ERTS_ALC_T_TMP, len+1);
		if (intlist_to_buf(tp[1], buf, len) != len)
		    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
		buf[len] = '\0';
		res = erts_instr_dump_stat(buf, 1) ? am_true : am_false;
		erts_free(ERTS_ALC_T_TMP, (void *) buf);
		if (is_non_value(res))
		    goto badarg;
		return res;
	    }
	}
	else
	    goto badarg;
    } else if (sel == am_allocator) {
	switch (arity) {
	case 2:
	    ERTS_BIF_PREP_TRAP1(ret, alloc_info_trap, BIF_P, *tp);
	    return ret;
	case 3:
	    if (erts_request_alloc_info(BIF_P, tp[0], tp[1], 0, 0))
		return am_true;
	default:
	    goto badarg;
	}
    } else if (ERTS_IS_ATOM_STR("internal_cpu_topology", sel) && arity == 2) {
	return erts_get_cpu_topology_term(BIF_P, *tp);
    } else if (ERTS_IS_ATOM_STR("cpu_topology", sel) && arity == 2) {
	Eterm res = erts_get_cpu_topology_term(BIF_P, *tp);
	if (res == THE_NON_VALUE)
	    goto badarg;
	ERTS_BIF_PREP_TRAP1(ret, erts_format_cpu_topology_trap, BIF_P, res);
	return ret;
#if defined(PURIFY) || defined(VALGRIND)
    } else if (ERTS_IS_ATOM_STR("error_checker", sel)
#if defined(PURIFY)
	       || sel == am_purify
#elif defined(VALGRIND)
	       || ERTS_IS_ATOM_STR("valgrind", sel)
#endif
	) {
	if (*tp == am_memory) {
#if defined(PURIFY)
	    BIF_RET(erts_make_integer(purify_new_leaks(), BIF_P));
#elif defined(VALGRIND)
#  ifdef VALGRIND_DO_ADDED_LEAK_CHECK
	    VALGRIND_DO_ADDED_LEAK_CHECK;
#  else
	    VALGRIND_DO_LEAK_CHECK;
#  endif
	    BIF_RET(make_small(0));
#endif
	} else if (*tp == am_fd) {
#if defined(PURIFY)
	    BIF_RET(erts_make_integer(purify_new_fds_inuse(), BIF_P));
#elif defined(VALGRIND)
	    /* Not present in valgrind... */
	    BIF_RET(make_small(0));
#endif
	} else if (*tp == am_running) {
#if defined(PURIFY)
	    BIF_RET(purify_is_running() ? am_true : am_false);
#elif defined(VALGRIND)
	    BIF_RET(RUNNING_ON_VALGRIND ? am_true : am_false);
#endif
	} else if (is_list(*tp)) {
#if defined(PURIFY)
#define ERTS_ERROR_CHECKER_PRINTF purify_printf
#define ERTS_ERROR_CHECKER_PRINTF_XML purify_printf
#elif defined(VALGRIND)
#define ERTS_ERROR_CHECKER_PRINTF VALGRIND_PRINTF
#  ifndef HAVE_VALGRIND_PRINTF_XML
#    define ERTS_ERROR_CHECKER_PRINTF_XML VALGRIND_PRINTF
#  else
#    define ERTS_ERROR_CHECKER_PRINTF_XML VALGRIND_PRINTF_XML
#  endif
#endif
	    ErlDrvSizeT buf_size = 8*1024; /* Try with 8KB first */
	    char *buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
	    ErlDrvSizeT r = erts_iolist_to_buf(*tp, (char*) buf, buf_size - 1);
	    if (ERTS_IOLIST_TO_BUF_FAILED(r)) {
		erts_free(ERTS_ALC_T_TMP, (void *) buf);
		if (erts_iolist_size(*tp, &buf_size)) {
		    goto badarg;
		}
		buf_size++;
		buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
		r = erts_iolist_to_buf(*tp, (char*) buf, buf_size - 1);
		ASSERT(r == buf_size - 1);
	    }
	    buf[buf_size - 1 - r] = '\0';
	    if (check_if_xml()) {
		ERTS_ERROR_CHECKER_PRINTF_XML("<erlang_info_log>"
					      "%s</erlang_info_log>\n", buf);
	    } else {
		ERTS_ERROR_CHECKER_PRINTF("%s\n", buf);
	    }
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    BIF_RET(am_true);
#undef ERTS_ERROR_CHECKER_PRINTF
	}
#endif
#ifdef QUANTIFY
    } else if (sel == am_quantify) {
	if (*tp == am_clear) {
	    quantify_clear_data();
	    BIF_RET(am_true);
	} else if (*tp == am_start) {
	    quantify_start_recording_data();
	    BIF_RET(am_true);
	} else if (*tp == am_stop) {
	    quantify_stop_recording_data();
	    BIF_RET(am_true);
	} else if (*tp == am_running) {
	    BIF_RET(quantify_is_running() ? am_true : am_false);
	}
#endif
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (ERTS_IS_ATOM_STR("ultrasparc_set_pcr", sel)) {
	unsigned long long tmp;
	int fd;
	int rc;

	if (arity != 2 || !is_small(*tp)) {
	    goto badarg;
	}
	tmp = signed_val(*tp);
	if ((fd = open("/dev/perfmon", O_RDONLY)) == -1) {
	    BIF_RET(am_false);
	}
	rc = ioctl(fd, PERFMON_SETPCR, &tmp);
	close(fd);
	if (rc < 0) {
	    BIF_RET(am_false);
	}
	BIF_RET(am_true);
#endif
    }

 badarg:
    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);

    return ret;
}

#define INFO_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_info_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp);

    if (need <= free_size)
	return dsbufp;
    size = need - free_size + INFO_DSBUF_INC_SZ;
    size = ((size + INFO_DSBUF_INC_SZ - 1)/INFO_DSBUF_INC_SZ)*INFO_DSBUF_INC_SZ;
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_INFO_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

static erts_dsprintf_buf_t *
erts_create_info_dsbuf(Uint size)
{
    Uint init_size = size ? size : INFO_DSBUF_INC_SZ;
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_info_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_INFO_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_INFO_DSBUF, init_size);
    dsbufp->str[0] = '\0';
    dsbufp->size = init_size;
    return dsbufp;
}

static void
erts_destroy_info_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    if (dsbufp->str)
	erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp);
}

static Eterm
c_compiler_used(Eterm **hpp, Uint *szp)
{

#if defined(__GNUC__)
#  if defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#    define ERTS_GNUC_VSN_NUMS 3
#  elif defined(__GNUC_MINOR__)
#    define ERTS_GNUC_VSN_NUMS 2
#  else
#    define ERTS_GNUC_VSN_NUMS 1
#  endif
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  erts_bld_atom(hpp, szp, "gnuc"),
#if ERTS_GNUC_VSN_NUMS > 1
			  erts_bld_tuple(hpp,
					 szp,
					 ERTS_GNUC_VSN_NUMS,
#endif
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC__)
#ifdef __GNUC_MINOR__
					 ,
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC_MINOR__)
#ifdef __GNUC_PATCHLEVEL__
					 ,
					 erts_bld_uint(hpp, szp,
						       (Uint) __GNUC_PATCHLEVEL__)
#endif
#endif
#if ERTS_GNUC_VSN_NUMS > 1
			     )
#endif
	);

#elif defined(_MSC_VER)
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  erts_bld_atom(hpp, szp, "msc"),
			  erts_bld_uint(hpp, szp, (Uint) _MSC_VER));

#else
    return erts_bld_tuple(hpp,
			  szp,
			  2,
			  am_undefined,
			  am_undefined);
#endif

}

static int is_snif_term(Eterm module_atom) {
    int i;
    Atom *a = atom_tab(atom_val(module_atom));
    char *aname = (char *) a->name;

    /* if a->name has a '.' then the bif (snif) is bogus i.e a package */
    for (i = 0; i < a->len; i++) {
	if (aname[i] == '.')
	    return 0;
    }

    return 1;
}

static Eterm build_snif_term(Eterm **hpp, Uint *szp, int ix, Eterm res) {
    Eterm tup;
    tup = erts_bld_tuple(hpp, szp, 3, bif_table[ix].module, bif_table[ix].name, make_small(bif_table[ix].arity));
    res = erts_bld_cons( hpp, szp, tup, res);
    return res;
}

static Eterm build_snifs_term(Eterm **hpp, Uint *szp, Eterm res) {
    int i;
    for (i = 0; i < BIF_SIZE; i++) {
	if (is_snif_term(bif_table[i].module)) {
	    res = build_snif_term(hpp, szp, i, res);
	}
    }
    return res;
}

BIF_RETTYPE system_info_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    Eterm val;
    int i;

    if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	Uint arity = *tp++;
	return info_1_tuple(BIF_P, tp, arityval(arity));
    } else if (BIF_ARG_1 == am_scheduler_id) {
#ifdef ERTS_SMP
	    ASSERT(BIF_P->scheduler_data);
	    BIF_RET(make_small(BIF_P->scheduler_data->no));
#else
	    BIF_RET(make_small(1));
#endif
    } else if (BIF_ARG_1 == am_compat_rel) {
	ASSERT(erts_compat_rel > 0);
	BIF_RET(make_small(erts_compat_rel));
    } else if (BIF_ARG_1 == am_multi_scheduling) {
#ifndef ERTS_SMP
	BIF_RET(am_disabled);
#else
	if (erts_no_schedulers == 1)
	    BIF_RET(am_disabled);
	else {
	    BIF_RET(erts_is_multi_scheduling_blocked()
		    ? am_blocked
		    : am_enabled);
	}
#endif
    } else if (BIF_ARG_1 == am_build_type) {
#if defined(DEBUG)
	ERTS_DECL_AM(debug);
	BIF_RET(AM_debug);
#elif defined(PURIFY)
	ERTS_DECL_AM(purify);
	BIF_RET(AM_purify);
#elif defined(QUANTIFY)
	ERTS_DECL_AM(quantify);
	BIF_RET(AM_quantify);
#elif defined(PURECOV)
	ERTS_DECL_AM(purecov);
	BIF_RET(AM_purecov);
#elif defined(ERTS_GCOV)
	ERTS_DECL_AM(gcov);
	BIF_RET(AM_gcov);
#elif defined(VALGRIND)
	ERTS_DECL_AM(valgrind);
	BIF_RET(AM_valgrind);
#elif defined(GPROF)
	ERTS_DECL_AM(gprof);
	BIF_RET(AM_gprof);
#elif defined(ERTS_ENABLE_LOCK_COUNT)
	ERTS_DECL_AM(lcnt);
	BIF_RET(AM_lcnt);
#elif defined(ERTS_FRMPTR)
	ERTS_DECL_AM(frmptr);
	BIF_RET(AM_frmptr);
#else
	BIF_RET(am_opt);
#endif
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_allocated_areas) {
	res = erts_allocated_areas(NULL, NULL, BIF_P);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_allocated) {
	BIF_RET(erts_instr_get_memory_map(BIF_P));
    } else if (BIF_ARG_1 == am_hipe_architecture) {
#if defined(HIPE)
	BIF_RET(hipe_arch_name);
#else
	BIF_RET(am_undefined);
#endif
    } else if (BIF_ARG_1 == am_trace_control_word) {
	BIF_RET(db_get_trace_control_word(BIF_P));
    } else if (ERTS_IS_ATOM_STR("ets_realloc_moves", BIF_ARG_1)) {
 	BIF_RET((erts_ets_realloc_always_moves) ? am_true : am_false);
    } else if (ERTS_IS_ATOM_STR("ets_always_compress", BIF_ARG_1)) {
	BIF_RET((erts_ets_always_compress) ? am_true : am_false);
    } else if (ERTS_IS_ATOM_STR("snifs", BIF_ARG_1)) {
	Uint size = 0;
	Uint *szp;

	szp = &size;
	build_snifs_term(NULL, szp, NIL);
	hp = HAlloc(BIF_P, size);
	res = build_snifs_term(&hp, NULL, NIL);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_sequential_tracer) {
	val = erts_get_system_seq_tracer();
	ASSERT(is_internal_pid(val) || is_internal_port(val) || val==am_false);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_sequential_tracer, val);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection){
	Uint val = (Uint) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
	Eterm tup;
	hp = HAlloc(BIF_P, 3+2 + 3+2 + 3+2);

	tup = TUPLE2(hp, am_fullsweep_after, make_small(val)); hp += 3;
	res = CONS(hp, tup, NIL); hp += 2;

	tup = TUPLE2(hp, am_min_heap_size, make_small(H_MIN_SIZE)); hp += 3;
	res = CONS(hp, tup, res); hp += 2;

	tup = TUPLE2(hp, am_min_bin_vheap_size, make_small(BIN_VH_MIN_SIZE)); hp += 3;
	res = CONS(hp, tup, res); hp += 2;

	BIF_RET(res);
    } else if (BIF_ARG_1 == am_fullsweep_after){
	Uint val = (Uint) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(val));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_min_heap_size) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_min_heap_size,make_small(H_MIN_SIZE));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_min_bin_vheap_size) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_min_bin_vheap_size,make_small(BIN_VH_MIN_SIZE));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_process_count) {
	BIF_RET(make_small(erts_ptab_count(&erts_proc)));
    } else if (BIF_ARG_1 == am_process_limit) {
	BIF_RET(make_small(erts_ptab_max(&erts_proc)));
    } else if (BIF_ARG_1 == am_port_count) {
	BIF_RET(make_small(erts_ptab_count(&erts_port)));
    } else if (BIF_ARG_1 == am_port_limit) {
	BIF_RET(make_small(erts_ptab_max(&erts_port)));
    } else if (BIF_ARG_1 == am_info
	       || BIF_ARG_1 == am_procs
	       || BIF_ARG_1 == am_loaded
	       || BIF_ARG_1 == am_dist) {
	erts_dsprintf_buf_t *dsbufp = erts_create_info_dsbuf(0);

	/* Need to be the only thread running... */
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	if (BIF_ARG_1 == am_info)
	    info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_procs)
	    process_info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_loaded)
	    loaded(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else
	    distribution_info(ERTS_PRINT_DSBUF, (void *) dsbufp);

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	ASSERT(dsbufp && dsbufp->str);
	res = new_binary(BIF_P, (byte *) dsbufp->str, dsbufp->str_len);
	erts_destroy_info_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("dist_ctrl", BIF_ARG_1)) {
	DistEntry *dep;
	i = 0;
	/* Need to be the only thread running... */
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) 
	    ++i;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next)
	    ++i;
	hp = HAlloc(BIF_P,i*(3+2));
	res = NIL;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_version) {
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_print_system_version(ERTS_PRINT_DSBUF, (void *) dsbufp, BIF_P);
	hp = HAlloc(BIF_P, dsbufp->str_len*2);
	res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
	erts_destroy_tmp_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_architecture) {
	hp = HAlloc(BIF_P, 2*(sizeof(ERLANG_ARCHITECTURE)-1));
	BIF_RET(buf_to_intlist(&hp,
			       ERLANG_ARCHITECTURE,
			       sizeof(ERLANG_ARCHITECTURE)-1,
			       NIL));
    } 
    else if (BIF_ARG_1 == am_memory_types) {
	return erts_instr_get_type_info(BIF_P);
    }
    else if (BIF_ARG_1 == am_os_type) {
	BIF_RET(os_type_tuple);
    }
    else if (BIF_ARG_1 == am_allocator) {
	BIF_RET(erts_allocator_options((void *) BIF_P));
    }
    else if (BIF_ARG_1 == am_thread_pool_size) {
#ifdef USE_THREADS
	extern int erts_async_max_threads;
#endif
	int n;
	
#ifdef USE_THREADS
	n = erts_async_max_threads;
#else
	n = 0;
#endif
	BIF_RET(make_small(n));
    }
    else if (BIF_ARG_1 == am_alloc_util_allocators) {
	BIF_RET(erts_alloc_util_allocators((void *) BIF_P));
    }
    else if (BIF_ARG_1 == am_elib_malloc) {
	/* To be removed in R15 */
        BIF_RET(am_false);
    }
    else if (BIF_ARG_1 == am_os_version) {
	BIF_RET(os_version_tuple);
    }
    else if (BIF_ARG_1 == am_version) {
	int n = strlen(ERLANG_VERSION);
	hp = HAlloc(BIF_P, ((sizeof ERLANG_VERSION)-1) * 2);
	BIF_RET(buf_to_intlist(&hp, ERLANG_VERSION, n, NIL));
    }
    else if (BIF_ARG_1 == am_machine) {
	int n = strlen(EMULATOR);
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, EMULATOR, n, NIL));
    }
    else if (BIF_ARG_1 == am_garbage_collection) {
	BIF_RET(am_generational);
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    } else if (BIF_ARG_1 == am_instruction_counts) {
#ifdef DEBUG
	Eterm *endp;
#endif
	Eterm *hp, **hpp;
	Uint hsz, *hszp;
	int i;

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

    bld_instruction_counts:

	res = NIL;
	for (i = num_instructions-1; i >= 0; i--) {
	    res = erts_bld_cons(hpp, hszp,
				erts_bld_tuple(hpp, hszp, 2,
					       erts_atom_put(opc[i].name,
							     strlen(opc[i].name),
							     ERTS_ATOM_ENC_LATIN1,
							     1),
					       erts_bld_uint(hpp, hszp,
							     opc[i].count)),
				res);
	}

	if (!hpp) {
	    hp = HAlloc(BIF_P, hsz);
	    hpp = &hp;
#ifdef DEBUG
	    endp = hp + hsz;
#endif
	    hszp = NULL;
	    goto bld_instruction_counts;
	}

#ifdef DEBUG
	ASSERT(endp == hp);
#endif

	BIF_RET(res);
#endif /* #ifndef ERTS_SMP */
    } else if (BIF_ARG_1 == am_wordsize) {
	return make_small(sizeof(Eterm));
    } else if (BIF_ARG_1 == am_endian) {
#if defined(WORDS_BIGENDIAN)
	return am_big;
#else
	return am_little;
#endif
    } else if (BIF_ARG_1 == am_heap_sizes) {
	return erts_heap_sizes(BIF_P);
    } else if (BIF_ARG_1 == am_heap_type) {
	return am_private;
    } else if (ERTS_IS_ATOM_STR("cpu_topology", BIF_ARG_1)) {
	res = erts_get_cpu_topology_term(BIF_P, am_used);
	BIF_TRAP1(erts_format_cpu_topology_trap, BIF_P, res);
    } else if (ERTS_IS_ATOM_STR("update_cpu_info", BIF_ARG_1)) {
	if (erts_update_cpu_info()) {
	    ERTS_DECL_AM(changed);
	    BIF_RET(AM_changed);
	}
	else {
	    ERTS_DECL_AM(unchanged);
	    BIF_RET(AM_unchanged);
	}
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_tick1", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_tick2", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_pic1", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ultrasparc_read_pic2", BIF_ARG_1)) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
#endif
    } else if (BIF_ARG_1 == am_threads) {
#ifdef USE_THREADS
	return am_true;
#else
	return am_false;
#endif
    } else if (BIF_ARG_1 == am_creation) {
	return make_small(erts_this_node->creation);
    } else if (BIF_ARG_1 == am_break_ignored) {
      extern int ignore_break;
      if (ignore_break) 
	return am_true; 
      else
	return am_false;
    }
    /* Arguments that are unusual follow ... */
    else if (ERTS_IS_ATOM_STR("logical_processors", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(&no, NULL, NULL);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    }
    else if (ERTS_IS_ATOM_STR("logical_processors_online", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(NULL, &no, NULL);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    }
    else if (ERTS_IS_ATOM_STR("logical_processors_available", BIF_ARG_1)) {
	int no;
	erts_get_logical_processors(NULL, NULL, &no);
	if (no > 0)
	    BIF_RET(make_small((Uint) no));
	else {
	    DECL_AM(unknown);
	    BIF_RET(AM_unknown);
	}
    } else if (ERTS_IS_ATOM_STR("otp_release", BIF_ARG_1)) {
	int n = sizeof(ERLANG_OTP_RELEASE)-1;
	hp = HAlloc(BIF_P, 2*n);
	BIF_RET(buf_to_intlist(&hp, ERLANG_OTP_RELEASE, n, NIL));
    } else if (ERTS_IS_ATOM_STR("driver_version", BIF_ARG_1)) {
	char buf[42];
	int n = erts_snprintf(buf, 42, "%d.%d",
			      ERL_DRV_EXTENDED_MAJOR_VERSION,
			      ERL_DRV_EXTENDED_MINOR_VERSION);
	hp = HAlloc(BIF_P, 2*n);
	BIF_RET(buf_to_intlist(&hp, buf, n, NIL));
    } else if (ERTS_IS_ATOM_STR("smp_support", BIF_ARG_1)) {
#ifdef ERTS_SMP
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("scheduler_bind_type", BIF_ARG_1)) {
	BIF_RET(erts_bound_schedulers_term(BIF_P));
    } else if (ERTS_IS_ATOM_STR("scheduler_bindings", BIF_ARG_1)) {
	BIF_RET(erts_get_schedulers_binds(BIF_P));
    } else if (ERTS_IS_ATOM_STR("constant_pool_support", BIF_ARG_1)) {
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("schedulers", BIF_ARG_1)
	       || ERTS_IS_ATOM_STR("schedulers_total", BIF_ARG_1)) {
	res = make_small(erts_no_schedulers);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("schedulers_state", BIF_ARG_1)) {
#ifndef ERTS_SMP
	Eterm *hp = HAlloc(BIF_P, 4);
	res = TUPLE3(hp, make_small(1), make_small(1), make_small(1));
	BIF_RET(res);
#else
	Uint total, online, active;
	switch (erts_schedulers_state(&total,
				      &online,
				      &active,
				      1)) {
	case ERTS_SCHDLR_SSPND_DONE: {
	    Eterm *hp = HAlloc(BIF_P, 4);
	    res = TUPLE3(hp,
			 make_small(total),
			 make_small(online),
			 make_small(active));
	    BIF_RET(res);
	}
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    ERTS_VBUMP_ALL_REDS(BIF_P);
	    BIF_TRAP1(bif_export[BIF_system_info_1],
		      BIF_P, BIF_ARG_1);
	default:
	    ASSERT(0);
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}
#endif
    } else if (ERTS_IS_ATOM_STR("schedulers_online", BIF_ARG_1)) {
#ifndef ERTS_SMP
	BIF_RET(make_small(1));
#else
	Uint total, online, active;
	switch (erts_schedulers_state(&total, &online, &active, 1)) {
	case ERTS_SCHDLR_SSPND_DONE:
	    BIF_RET(make_small(online));
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    ERTS_VBUMP_ALL_REDS(BIF_P);
	    BIF_TRAP1(bif_export[BIF_system_info_1],
		      BIF_P, BIF_ARG_1);
	default:
	    ASSERT(0);
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}
#endif
    } else if (ERTS_IS_ATOM_STR("schedulers_active", BIF_ARG_1)) {
#ifndef ERTS_SMP
	BIF_RET(make_small(1));
#else
	Uint total, online, active;
	switch (erts_schedulers_state(&total, &online, &active, 1)) {
	case ERTS_SCHDLR_SSPND_DONE:
	    BIF_RET(make_small(active));
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    ERTS_VBUMP_ALL_REDS(BIF_P);
	    BIF_TRAP1(bif_export[BIF_system_info_1],
		      BIF_P, BIF_ARG_1);
	default:
	    ASSERT(0);
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}
#endif
    } else if (ERTS_IS_ATOM_STR("run_queues", BIF_ARG_1)) {
	res = make_small(erts_no_run_queues);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("port_parallelism", BIF_ARG_1)) {
	res = erts_port_parallelism ? am_true : am_false;
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("c_compiler_used", BIF_ARG_1)) {
	Eterm *hp = NULL;
	Uint sz = 0;
	(void) c_compiler_used(NULL, &sz);
	if (sz)
	    hp = HAlloc(BIF_P, sz);
	BIF_RET(c_compiler_used(&hp, NULL));
    } else if (ERTS_IS_ATOM_STR("stop_memory_trace", BIF_ARG_1)) {
	erts_mtrace_stop();
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("context_reductions", BIF_ARG_1)) {
	BIF_RET(make_small(CONTEXT_REDS));
    } else if (ERTS_IS_ATOM_STR("kernel_poll", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_KERNEL_POLL
	BIF_RET(erts_use_kernel_poll ? am_true : am_false);
#else
	BIF_RET(am_false);
#endif    
    } else if (ERTS_IS_ATOM_STR("lock_checking", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("lock_counting", BIF_ARG_1)) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("debug_compiled", BIF_ARG_1)) {
#ifdef DEBUG
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    } else if (ERTS_IS_ATOM_STR("check_io", BIF_ARG_1)) {
	BIF_RET(erts_check_io_info(BIF_P));
    } else if (ERTS_IS_ATOM_STR("multi_scheduling_blockers", BIF_ARG_1)) {
#ifndef ERTS_SMP
	BIF_RET(NIL);
#else
	if (erts_no_schedulers == 1)
	    BIF_RET(NIL);
	else
	    BIF_RET(erts_multi_scheduling_blockers(BIF_P));
#endif
    } else if (ERTS_IS_ATOM_STR("modified_timing_level", BIF_ARG_1)) {
	BIF_RET(ERTS_USE_MODIFIED_TIMING()
		? make_small(erts_modified_timing_level)
		: am_undefined);
    } else if (ERTS_IS_ATOM_STR("port_tasks", BIF_ARG_1)) {
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("io_thread", BIF_ARG_1)) {
	BIF_RET(am_false);
    } else if (ERTS_IS_ATOM_STR("scheduling_statistics", BIF_ARG_1)) {
	BIF_RET(erts_sched_stat_term(BIF_P, 0));
    } else if (ERTS_IS_ATOM_STR("total_scheduling_statistics", BIF_ARG_1)) {
	BIF_RET(erts_sched_stat_term(BIF_P, 1));
    } else if (ERTS_IS_ATOM_STR("taints", BIF_ARG_1)) {
	BIF_RET(erts_nif_taints(BIF_P));
    } else if (ERTS_IS_ATOM_STR("reader_groups_map", BIF_ARG_1)) {
	BIF_RET(erts_get_reader_groups_map(BIF_P));
    } else if (ERTS_IS_ATOM_STR("dist_buf_busy_limit", BIF_ARG_1)) {
	Uint hsz = 0;

 	(void) erts_bld_uint(NULL, &hsz, erts_dist_buf_busy_limit);
	hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
	res = erts_bld_uint(&hp, NULL, erts_dist_buf_busy_limit);
	BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("ethread_info", BIF_ARG_1)) {
	BIF_RET(erts_get_ethread_info(BIF_P));
    }
    else if (ERTS_IS_ATOM_STR("emu_args", BIF_ARG_1)) {
	BIF_RET(erts_get_emu_args(BIF_P));
    }
    else if (ERTS_IS_ATOM_STR("beam_jump_table", BIF_ARG_1)) {
	BIF_RET(erts_beam_jump_table() ? am_true : am_false);
    }
    else if (ERTS_IS_ATOM_STR("dynamic_trace", BIF_ARG_1)) {
#if defined(USE_DTRACE)
	DECL_AM(dtrace);
	BIF_RET(AM_dtrace);
#elif defined(USE_SYSTEMTAP)
	DECL_AM(systemtap);
	BIF_RET(AM_systemtap);
#else
	BIF_RET(am_none);
#endif
    }	    
    else if (ERTS_IS_ATOM_STR("dynamic_trace_probes", BIF_ARG_1)) {
#if defined(USE_VM_PROBES)
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif	
    }
#ifdef ERTS_SMP
    else if (ERTS_IS_ATOM_STR("thread_progress", BIF_ARG_1)) {
	erts_thr_progress_dbg_print_state();
	BIF_RET(am_true);
    }
#endif
    else if (ERTS_IS_ATOM_STR("compile_info",BIF_ARG_1)) {
	Uint  sz;
	Eterm res = NIL, tup, text;
	Eterm *hp = HAlloc(BIF_P, 3*(2 + 3) + /* three 2-tuples and three cons */
		2*(strlen(erts_build_flags_CONFIG_H) +
		   strlen(erts_build_flags_CFLAGS) +
		   strlen(erts_build_flags_LDFLAGS)));

	sz   = strlen(erts_build_flags_CONFIG_H);
	text = buf_to_intlist(&hp, erts_build_flags_CONFIG_H, sz, NIL);
	tup  = TUPLE2(hp, am_config_h, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	sz   = strlen(erts_build_flags_CFLAGS);
	text = buf_to_intlist(&hp, erts_build_flags_CFLAGS, sz, NIL);
	tup  = TUPLE2(hp, am_cflags, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	sz   = strlen(erts_build_flags_LDFLAGS);
	text = buf_to_intlist(&hp, erts_build_flags_LDFLAGS, sz, NIL);
	tup  = TUPLE2(hp, am_ldflags, text); hp += 3;
	res  = CONS(hp, tup, res); hp += 2;

	BIF_RET(res);
    }
    else if (ERTS_IS_ATOM_STR("ets_limit",BIF_ARG_1)) {
        BIF_RET(make_small(erts_db_get_max_tabs()));
    }

    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/ 
/* Return information on ports */
/* Info:
**    id          Port index
**    connected   (Pid)
**    links       List of pids
**    name        String
**    input       Number of bytes input from port program
**    output      Number of bytes output to the port program
**    os_pid      The child's process ID
*/

Eterm
erts_bld_port_info(Eterm **hpp, ErlOffHeap *ohp, Uint *szp, Port *prt, Eterm item)
{
    Eterm res = THE_NON_VALUE;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (item == am_id) {
	if (hpp)
	    res = make_small(internal_port_index(prt->common.id));
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_links) {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_doforall_links(ERTS_P_LINKS(prt), &collect_one_link, &mic);

	if (szp)
	    *szp += mic.sz;

	if (hpp) {
	    res = NIL;
	    for (i = 0; i < mic.mi_i; i++) {
		item = STORE_NC(hpp, ohp, mic.mi[i].entity);
		res = CONS(*hpp, item, res);
		*hpp += 2;
	    }
	}

	DESTROY_MONITOR_INFOS(mic);

	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_monitors) {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_doforall_monitors(ERTS_P_MONITORS(prt), &collect_one_origin_monitor, &mic);

	if (szp)
	    *szp += mic.sz;

	if (hpp) {
	    res = NIL;
	    for (i = 0; i < mic.mi_i; i++) {
		Eterm t;
		item = STORE_NC(hpp, ohp, mic.mi[i].entity);
		t = TUPLE2(*hpp, am_process, item);
		*hpp += 3;
		res = CONS(*hpp, t, res);
		*hpp += 2;
	    }
	}

	DESTROY_MONITOR_INFOS(mic);

	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_name) {
	int count = sys_strlen(prt->name);

	if (hpp)
	    res = buf_to_intlist(hpp, prt->name, count, NIL);

	if (szp) {
	    *szp += 2*count;
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_connected) {
	if (hpp)
	    res = ERTS_PORT_GET_CONNECTED(prt); /* internal pid */
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_input) {
	res = erts_bld_uint(hpp, szp, prt->bytes_in);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_output) {
	res = erts_bld_uint(hpp, szp, prt->bytes_out);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_os_pid) {
	res = (prt->os_pid < 0
	       ? am_undefined
	       : erts_bld_uword(hpp, szp, (UWord) prt->os_pid));
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_registered_name) {
	RegProc *reg = prt->common.u.alive.reg;
	if (reg) {
	    res = reg->name;
	    if (szp) {
		res = am_true;
		goto done;
	    }
	}
	else {
	    if (szp)
		return am_undefined;
	    return NIL;
	}
    }
    else if (item == am_memory) {
	/* All memory consumed in bytes (the Port struct should not be
	   included though).
	 */
	Uint size = 0;

	erts_doforall_links(ERTS_P_LINKS(prt), &erts_one_link_size, &size);

	size += erts_port_data_size(prt);

	if (prt->linebuf)
	    size += sizeof(LineBuf) + prt->linebuf->ovsiz;

	/* ... */


	/* All memory allocated by the driver should be included, but it is
	   hard to retrieve... */
	
	res = erts_bld_uint(hpp, szp, size);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_queue_size) {
	Uint ioq_size = erts_port_ioq_size(prt);
	res = erts_bld_uint(hpp, szp, ioq_size);
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (ERTS_IS_ATOM_STR("locking", item)) {
	if (hpp) {
#ifndef ERTS_SMP
	    res = am_false;
#else
	    if (erts_atomic32_read_nob(&prt->state)
		& ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK) {
		DECL_AM(port_level);
		ASSERT(prt->drv_ptr->flags
		       & ERL_DRV_FLAG_USE_PORT_LOCKING);
		res = AM_port_level;
	    }
	    else {
		DECL_AM(driver_level);
		ASSERT(!(prt->drv_ptr->flags
			 & ERL_DRV_FLAG_USE_PORT_LOCKING));
		res = AM_driver_level;
	    }
#endif
	}
	if (szp) {
	    res = am_true;
	    goto done;
	}
    }
    else if (item == am_parallelism) {
	if (szp) {
	    res = am_true;
	    goto done;
	}
	res = ((ERTS_PTS_FLG_PARALLELISM &
		erts_smp_atomic32_read_nob(&prt->sched.flags))
	       ? am_true
	       : am_false);
    }
    else {
	if (szp)
	    return am_false;
	return THE_NON_VALUE;
    }

done:
    if (szp)
	*szp += 3;
    if (hpp) {
	res = TUPLE2(*hpp, item, res);
	*hpp += 3;
    }

    return res;
}

BIF_RETTYPE
fun_info_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;
    Eterm what = BIF_ARG_2;
    Eterm* hp;
    Eterm val;

    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);

	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_local;
	    break;
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = funp->creator;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = funp->fe->module;
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->index);
	    break;
	case am_new_uniq:
	    val = new_binary(p, funp->fe->uniq, 16);
	    hp = HAlloc(p, 3);
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_index);
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_uniq);
	    break;
	case am_env:
	    {
		Uint num_free = funp->num_free;
		int i;

		hp = HAlloc(p, 3 + 2*num_free);
		val = NIL;
		for (i = num_free-1; i >= 0; i--) {
		    val = CONS(hp, funp->env[i], val);
		    hp += 2;
		}
	    }
	    break;
	case am_refc:
	    val = erts_make_integer(erts_smp_atomic_read_nob(&funp->fe->refc), p);
	    hp = HAlloc(p, 3);
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->arity);
	    break;
	case am_name:
	    hp = HAlloc(p, 3);
	    val = funp->fe->address[-2];
	    break;
	default:
	    goto error;
	}
    } else if (is_export(fun)) {
	Export* exp = (Export *) ((UWord) (export_val(fun))[1]);
	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_external;
	    break;
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = exp->code[0];
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_new_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_env:
	    hp = HAlloc(p, 3);
	    val = NIL;
	    break;
	case am_refc:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(exp->code[2]);
	    break;
	case am_name:
	    hp = HAlloc(p, 3);
	    val = exp->code[1];
	    break;
	default:
	    goto error;
	}
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }
    return TUPLE2(hp, what, val);
}

BIF_RETTYPE is_process_alive_1(BIF_ALIST_1) 
{
   if(is_internal_pid(BIF_ARG_1)) {
       Process *rp;

       if (BIF_ARG_1 == BIF_P->common.id)
	   BIF_RET(am_true);

       rp = erts_proc_lookup_raw(BIF_ARG_1);
       if (!rp) {
	   BIF_RET(am_false);
       }
       else {
	   if (erts_smp_atomic32_read_acqb(&rp->state)
		 & (ERTS_PSFLG_PENDING_EXIT|ERTS_PSFLG_EXITING))
	       ERTS_BIF_AWAIT_X_DATA_TRAP(BIF_P, BIF_ARG_1, am_false);
	   else
	       BIF_RET(am_true);
       }
   }
   else if(is_external_pid(BIF_ARG_1)) {
       if(external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	   BIF_RET(am_false); /* A pid from an old incarnation of this node */
       else
	   BIF_ERROR(BIF_P, BADARG);
   }
   else {
      BIF_ERROR(BIF_P, BADARG);
   }
}

BIF_RETTYPE process_display_2(BIF_ALIST_2)
{
   Process *rp;

   if (BIF_ARG_2 != am_backtrace)
       BIF_ERROR(BIF_P, BADARG);

   rp = erts_pid2proc_nropt(BIF_P, ERTS_PROC_LOCK_MAIN,
			    BIF_ARG_1, ERTS_PROC_LOCKS_ALL);
   if(!rp) {
       BIF_ERROR(BIF_P, BADARG);
   }
   if (rp == ERTS_PROC_LOCK_BUSY)
       ERTS_BIF_YIELD2(bif_export[BIF_process_display_2], BIF_P,
		       BIF_ARG_1, BIF_ARG_2);
   if (rp != BIF_P && ERTS_PROC_PENDING_EXIT(rp)) {
       Eterm args[2] = {BIF_ARG_1, BIF_ARG_2};
       erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_ALL);
       ERTS_BIF_AWAIT_X_APPLY_TRAP(BIF_P,
				   BIF_ARG_1,
				   am_erlang,
				   am_process_display,
				   args,
				   2);
   }
   erts_stack_dump(ERTS_PRINT_STDERR, NULL, rp);
#ifdef ERTS_SMP
   erts_smp_proc_unlock(rp, (BIF_P == rp
			     ? ERTS_PROC_LOCKS_ALL_MINOR
			     : ERTS_PROC_LOCKS_ALL));
#endif
   BIF_RET(am_true);
}


/* this is a general call which return some possibly useful information */

BIF_RETTYPE statistics_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;

    if (BIF_ARG_1 == am_scheduler_wall_time) {
	res = erts_sched_wall_time_request(BIF_P, 0, 0);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_sched_wall_time_res_trap, BIF_P, res);
    } else if (BIF_ARG_1 == am_context_switches) {
	Eterm cs = erts_make_integer(erts_get_total_context_switches(), BIF_P);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, cs, SMALL_ZERO);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection) {
	res = erts_gc_info_request(BIF_P);
	if (is_non_value(res))
	    BIF_RET(am_undefined);
	BIF_TRAP1(gather_gc_info_res_trap, BIF_P, res);
    } else if (BIF_ARG_1 == am_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_total_reductions(&reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_exact_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_exact_total_reductions(BIF_P, &reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_runtime) {
	UWord u1, u2, dummy;
	Eterm b1, b2;
	elapsed_time_both(&u1,&dummy,&u2,&dummy);
	b1 = erts_make_integer(u1,BIF_P);
	b2 = erts_make_integer(u2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 ==  am_run_queue) {
	res = erts_run_queues_len(NULL);
	BIF_RET(make_small(res));
    } else if (BIF_ARG_1 == am_wall_clock) {
	UWord w1, w2;
	Eterm b1, b2;
	wall_clock_elapsed_time_both(&w1, &w2);
	b1 = erts_make_integer((Uint) w1,BIF_P);
	b2 = erts_make_integer((Uint) w2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_io) {
	Eterm r1, r2;
	Eterm in, out;
	Uint hsz = 9;
	Uint bytes_in = (Uint) erts_smp_atomic_read_nob(&erts_bytes_in);
	Uint bytes_out = (Uint) erts_smp_atomic_read_nob(&erts_bytes_out);

	(void) erts_bld_uint(NULL, &hsz, bytes_in);
	(void) erts_bld_uint(NULL, &hsz, bytes_out);
	hp = HAlloc(BIF_P, hsz);
	in = erts_bld_uint(&hp, NULL, bytes_in);
	out = erts_bld_uint(&hp, NULL, bytes_out);

	r1 = TUPLE2(hp,  am_input, in);
	hp += 3;
	r2 = TUPLE2(hp, am_output, out);
	hp += 3;
	BIF_RET(TUPLE2(hp, r1, r2));
    }
    else if (ERTS_IS_ATOM_STR("run_queues", BIF_ARG_1)) {
	Eterm res, *hp, **hpp;
	Uint sz, *szp;
	int no_qs = erts_no_run_queues;
	Uint *qszs = erts_alloc(ERTS_ALC_T_TMP,sizeof(Uint)*no_qs*2);
	(void) erts_run_queues_len(qszs);
	sz = 0;
	szp = &sz;
	hpp = NULL;
	while (1) {
	    int i;
	    for (i = 0; i < no_qs; i++)
		qszs[no_qs+i] = erts_bld_uint(hpp, szp, qszs[i]);
	    res = erts_bld_tuplev(hpp, szp, no_qs, &qszs[no_qs]);
	    if (hpp) {
		erts_free(ERTS_ALC_T_TMP, qszs);
		BIF_RET(res);
	    }
	    hp = HAlloc(BIF_P, sz);
	    szp = NULL;
	    hpp = &hp;
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE error_logger_warning_map_0(BIF_ALIST_0)
{
    BIF_RET(erts_error_logger_warnings);
}

static erts_smp_atomic_t available_internal_state;

BIF_RETTYPE erts_debug_get_internal_state_1(BIF_ALIST_1)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */

    if (!erts_smp_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    /* Used by (emulator) */
	    BIF_RET(make_small((Uint) ERTS_BIF_REDS_LEFT(BIF_P)));
	}
	else if (ERTS_IS_ATOM_STR("node_and_dist_references", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
	    Eterm res = erts_get_node_and_dist_references(BIF_P);
	    BIF_RET(res);
	}
	else if (ERTS_IS_ATOM_STR("monitoring_nodes", BIF_ARG_1)) {
	    BIF_RET(erts_processes_monitoring_nodes(BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("next_port", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
	    Sint res;
	    if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1))
		res = erts_ptab_test_next_id(&erts_proc, 0, 0);
	    else
		res = erts_ptab_test_next_id(&erts_port, 0, 0);
	    if (res < 0)
		BIF_RET(am_false);
	    BIF_RET(erts_make_integer(res, BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("DbTable_words", BIF_ARG_1)) {
	    /* Used by ets_SUITE (stdlib) */
	    size_t words = (sizeof(DbTable) + sizeof(Uint) - 1)/sizeof(Uint);
	    BIF_RET(make_small((Uint) words));
	}
	else if (ERTS_IS_ATOM_STR("check_io_debug", BIF_ARG_1)) {
	    /* Used by (emulator) */
	    int res;
#ifdef HAVE_ERTS_CHECK_IO_DEBUG
	    erts_smp_proc_unlock(BIF_P,ERTS_PROC_LOCK_MAIN);
	    res = erts_check_io_debug();
	    erts_smp_proc_lock(BIF_P,ERTS_PROC_LOCK_MAIN);
#else
	    res = 0;
#endif
	    ASSERT(res >= 0);
	    BIF_RET(erts_make_integer((Uint) res, BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("process_info_args", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    int i;
	    Eterm res = NIL;
	    Uint *hp = HAlloc(BIF_P, 2*ERTS_PI_ARGS);
	    for (i = ERTS_PI_ARGS-1; i >= 0; i--) {
		res = CONS(hp, pi_args[i], res);
		hp += 2;
	    }
	    BIF_RET(res);
	}
	else if (ERTS_IS_ATOM_STR("processes", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    BIF_RET(erts_debug_ptab_list(BIF_P, &erts_proc));
	}
	else if (ERTS_IS_ATOM_STR("processes_bif_info", BIF_ARG_1)) {
	    /* Used by process_SUITE (emulator) */
	    BIF_RET(erts_debug_ptab_list_bif_info(BIF_P, &erts_proc));
	}
	else if (ERTS_IS_ATOM_STR("max_atom_out_cache_index", BIF_ARG_1)) {
	    /* Used by distribution_SUITE (emulator) */
	    BIF_RET(make_small((Uint) erts_debug_max_atom_out_cache_index()));
	}
	else if (ERTS_IS_ATOM_STR("nbalance", BIF_ARG_1)) {
	    Uint n;
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    n = erts_debug_nbalance();
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    BIF_RET(erts_make_integer(n, BIF_P));
	}
	else if (ERTS_IS_ATOM_STR("available_internal_state", BIF_ARG_1)) {
	    BIF_RET(am_true);
	}
	else if (ERTS_IS_ATOM_STR("force_heap_frags", BIF_ARG_1)) {
#ifdef FORCE_HEAP_FRAGS
	    BIF_RET(am_true);
#else
	    BIF_RET(am_false);
#endif
	}
	else if (ERTS_IS_ATOM_STR("memory", BIF_ARG_1)) {
	    Eterm res;
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_smp_thr_progress_block();
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    res = erts_memory(NULL, NULL, BIF_P, THE_NON_VALUE);
	    erts_smp_thr_progress_unblock();
	    BIF_RET(res);
	}
        else if (ERTS_IS_ATOM_STR("mmap", BIF_ARG_1)) {
            BIF_RET(erts_mmap_debug_info(BIF_P));
        }
    }
    else if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	switch (arityval(tp[0])) {
	case 2: {
	    if (ERTS_IS_ATOM_STR("process_status", tp[1])) {
		/* Used by timer process_SUITE, timer_bif_SUITE, and
		   node_container_SUITE (emulator) */
		if (is_internal_pid(tp[2])) {
		    BIF_RET(erts_process_status(BIF_P,
						ERTS_PROC_LOCK_MAIN,
						NULL,
						tp[2]));
		}
	    }
	    else if (ERTS_IS_ATOM_STR("link_list", tp[1])) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
		    Eterm res;
		    Process *p;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_LINK);
		    if (!p) {
			ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
		    res = make_link_list(BIF_P, ERTS_P_LINKS(p), NIL);
		    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
		    BIF_RET(res);
		}
		else if(is_internal_port(tp[2])) {
		    Eterm res;
		    Port *p = erts_id2port_sflgs(tp[2],
						 BIF_P,
						 ERTS_PROC_LOCK_MAIN,
						 ERTS_PORT_SFLGS_INVALID_LOOKUP);
		    if(!p)
			BIF_RET(am_undefined);
		    res = make_link_list(BIF_P, ERTS_P_LINKS(p), NIL);
		    erts_port_release(p);
		    BIF_RET(res);
		}
		else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm subres;
			erts_smp_de_links_lock(dep);
			subres = make_link_list(BIF_P, dep->nlinks, NIL);
			subres = make_link_list(BIF_P, dep->node_links, subres);
			erts_smp_de_links_unlock(dep);
			erts_deref_dist_entry(dep);
			BIF_RET(subres);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (ERTS_IS_ATOM_STR("monitor_list", tp[1])) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
		    Process *p;
		    Eterm res;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_LINK);
		    if (!p) {
			ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
		    res = make_monitor_list(BIF_P, ERTS_P_MONITORS(p));
		    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
		    BIF_RET(res);
		} else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm ml;
			erts_smp_de_links_lock(dep);
			ml = make_monitor_list(BIF_P, dep->monitors);
			erts_smp_de_links_unlock(dep);
			erts_deref_dist_entry(dep);
			BIF_RET(ml);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (ERTS_IS_ATOM_STR("channel_number", tp[1])) {
		Eterm res;
		DistEntry *dep = erts_find_dist_entry(tp[2]);
		if (!dep)
		    res = am_undefined;
		else {
		    Uint cno = dist_entry_channel_no(dep);
		    res = make_small(cno);
		    erts_deref_dist_entry(dep);
		}
		BIF_RET(res);
	    }
	    else if (ERTS_IS_ATOM_STR("have_pending_exit", tp[1])) {
		Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
					    tp[2], ERTS_PROC_LOCK_STATUS);
		if (!rp) {
		    BIF_RET(am_undefined);
		}
		else {
		    Eterm res = ERTS_PROC_PENDING_EXIT(rp) ? am_true : am_false;
		    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		    BIF_RET(res);
		}
	    }
	    else if (ERTS_IS_ATOM_STR("binary_info", tp[1])) {
		Eterm bin = tp[2];
		if (is_binary(bin)) {
		    Eterm real_bin = bin;
		    Eterm res = am_true;
		    ErlSubBin* sb = (ErlSubBin *) binary_val(real_bin);

		    if (sb->thing_word == HEADER_SUB_BIN) {
			real_bin = sb->orig;
		    }
		    if (*binary_val(real_bin) == HEADER_PROC_BIN) {
			ProcBin* pb;
			Binary* val;
			Eterm SzTerm;
			Uint hsz = 3 + 5;
			Eterm* hp;
			DECL_AM(refc_binary);

			pb = (ProcBin *) binary_val(real_bin);
			val = pb->val;
			(void) erts_bld_uint(NULL, &hsz, pb->size);
			(void) erts_bld_uint(NULL, &hsz, val->orig_size);
			hp = HAlloc(BIF_P, hsz);

			/* Info about the Binary* object */
			SzTerm = erts_bld_uint(&hp, NULL, val->orig_size);
			res = TUPLE2(hp, am_binary, SzTerm);
			hp += 3;

			/* Info about the ProcBin* object */
			SzTerm = erts_bld_uint(&hp, NULL, pb->size);
			res = TUPLE4(hp, AM_refc_binary, SzTerm,
				     res, make_small(pb->flags));
		    } else {	/* heap binary */
			DECL_AM(heap_binary);
			res = AM_heap_binary;
		    }
		    BIF_RET(res);
		}
	    }
	    else if (ERTS_IS_ATOM_STR("term_to_binary_no_funs", tp[1])) {
		Uint dflags = (DFLAG_EXTENDED_REFERENCES |
			       DFLAG_EXTENDED_PIDS_PORTS |
			       DFLAG_BIT_BINARIES);
		BIF_RET(erts_term_to_binary(BIF_P, tp[2], 0, dflags));
	    }
	    else if (ERTS_IS_ATOM_STR("dist_port", tp[1])) {
		Eterm res = am_undefined;
		DistEntry *dep = erts_sysname_to_connected_dist_entry(tp[2]);
		if (dep) {
		    erts_smp_de_rlock(dep);
		    if (is_internal_port(dep->cid))
			res = dep->cid;
		    erts_smp_de_runlock(dep);
		    erts_deref_dist_entry(dep);
		}
		BIF_RET(res);
	    }
	    else if (ERTS_IS_ATOM_STR("atom_out_cache_index", tp[1])) {
		/* Used by distribution_SUITE (emulator) */
		if (is_atom(tp[2])) {
		    BIF_RET(make_small(
				(Uint)
				erts_debug_atom_to_out_cache_index(tp[2])));
		}
	    }
	    else if (ERTS_IS_ATOM_STR("fake_scheduler_bindings", tp[1])) {
		return erts_fake_scheduler_bindings(BIF_P, tp[2]);
	    }
	    else if (ERTS_IS_ATOM_STR("reader_groups_map", tp[1])) {
		Sint groups;
		if (is_not_small(tp[2]))
		    BIF_ERROR(BIF_P, BADARG);
		groups = signed_val(tp[2]);
		if (groups < (Sint) 1 || groups > (Sint) INT_MAX)
		    BIF_ERROR(BIF_P, BADARG);

		BIF_RET(erts_debug_reader_groups_map(BIF_P, (int) groups));
	    }
	    break;
	}
	default:
	    break;
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

static erts_smp_atomic_t hipe_test_reschedule_flag;


BIF_RETTYPE erts_debug_set_internal_state_2(BIF_ALIST_2)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */
    if (ERTS_IS_ATOM_STR("available_internal_state", BIF_ARG_1)
	&& (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false)) {
	erts_aint_t on = (erts_aint_t) (BIF_ARG_2 == am_true);
	erts_aint_t prev_on = erts_smp_atomic_xchg_nob(&available_internal_state, on);
	if (on) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Process %T ", BIF_P->common.id);
	    if (erts_is_alive)
		erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname);
	    erts_dsprintf(dsbufp,
			  "enabled access to the emulator internal state.\n");
	    erts_dsprintf(dsbufp,
			  "NOTE: This is an erts internal test feature and "
			  "should *only* be used by OTP test-suites.\n");
	    erts_send_warning_to_logger(BIF_P->group_leader, dsbufp);
	}
	BIF_RET(prev_on ? am_true : am_false);
    }

    if (!erts_smp_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    Sint reds;
	    if (term_to_Sint(BIF_ARG_2, &reds) != 0) {
		if (0 <= reds && reds <= CONTEXT_REDS) {
		    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(BIF_P))
			BIF_P->fcalls = reds;
		    else
			BIF_P->fcalls = reds - CONTEXT_REDS;
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("block", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("sleep", BIF_ARG_1)) {
	    int block = ERTS_IS_ATOM_STR("block", BIF_ARG_1);
	    Sint ms;
	    if (term_to_Sint(BIF_ARG_2, &ms) != 0) {
		if (ms > 0) {
		    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    if (block)
			erts_smp_thr_progress_block();
		    while (erts_milli_sleep((long) ms) != 0);
		    if (block)
			erts_smp_thr_progress_unblock();
		    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("block_scheduler", BIF_ARG_1)) {
	    Sint ms;
	    if (term_to_Sint(BIF_ARG_2, &ms) != 0) {
		if (ms > 0) {
		    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    while (erts_milli_sleep((long) ms) != 0);
		    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		}
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1)
		 || ERTS_IS_ATOM_STR("next_port", BIF_ARG_1)) {
	    /* Used by node_container_SUITE (emulator) */
	    Uint next;

	    if (term_to_Uint(BIF_ARG_2, &next) != 0) {
		Sint res;

		if (ERTS_IS_ATOM_STR("next_pid", BIF_ARG_1))
		    res = erts_ptab_test_next_id(&erts_proc, 1, next);
		else
		    res = erts_ptab_test_next_id(&erts_port, 1, next);
		if (res < 0)
		    BIF_RET(am_false);
		BIF_RET(erts_make_integer(res, BIF_P));
	    }
	}
	else if (ERTS_IS_ATOM_STR("force_gc", BIF_ARG_1)) {
	    /* Used by signal_SUITE (emulator) */
	    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
					BIF_ARG_2, ERTS_PROC_LOCK_MAIN);
	    if (!rp) {
		BIF_RET(am_false);
	    }
	    else {
		FLAGS(rp) |= F_FORCE_GC;
		if (BIF_P != rp)
		    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("send_fake_exit_signal", BIF_ARG_1)) {
	    /* Used by signal_SUITE (emulator) */

	    /* Testcases depend on the exit being received via
	       a pending exit when the receiver is the same as
	       the caller.  */
	    if (is_tuple(BIF_ARG_2)) {
		Eterm* tp = tuple_val(BIF_ARG_2);
		if (arityval(tp[0]) == 3
		    && (is_pid(tp[1]) || is_port(tp[1]))
		    && is_internal_pid(tp[2])) {
		    int xres;
		    ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
		    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
						tp[2], rp_locks);
		    if (!rp) {
			DECL_AM(dead);
			BIF_RET(AM_dead);
		    }

#ifdef ERTS_SMP
		    if (BIF_P == rp)
			rp_locks |= ERTS_PROC_LOCK_MAIN;
#endif
		    xres = erts_send_exit_signal(NULL, /* NULL in order to
							  force a pending exit
							  when we send to our
							  selves. */
						 tp[1],
						 rp,
						 &rp_locks,
						 tp[3],
						 NIL,
						 NULL,
						 0);
#ifdef ERTS_SMP
		    if (BIF_P == rp)
			rp_locks &= ~ERTS_PROC_LOCK_MAIN;
#endif
		    erts_smp_proc_unlock(rp, rp_locks);
		    if (xres > 1) {
			DECL_AM(message);
			BIF_RET(AM_message);
		    }
		    else if (xres == 0) {
			DECL_AM(unaffected);
			BIF_RET(AM_unaffected);
		    }
		    else {
			DECL_AM(exit);
			BIF_RET(AM_exit);
		    }
		}
	    }
	}
        else if (ERTS_IS_ATOM_STR("colliding_names", BIF_ARG_1)) {
	    /* Used by ets_SUITE (stdlib) */
	    if (is_tuple(BIF_ARG_2)) {
                Eterm* tpl = tuple_val(BIF_ARG_2);
                Uint cnt;
                if (arityval(tpl[0]) == 2 && is_atom(tpl[1]) && 
                    term_to_Uint(tpl[2], &cnt)) {
                    BIF_RET(erts_ets_colliding_names(BIF_P,tpl[1],cnt));
                }
	    }
	}
	else if (ERTS_IS_ATOM_STR("binary_loop_limit", BIF_ARG_1)) {
	    /* Used by binary_module_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_binary_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_binary_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("re_loop_limit", BIF_ARG_1)) {
	    /* Used by re_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_re_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_re_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("unicode_loop_limit", BIF_ARG_1)) {
	    /* Used by unicode_SUITE (stdlib) */
	    Uint max_loops;
	    if (is_atom(BIF_ARG_2) && ERTS_IS_ATOM_STR("default", BIF_ARG_2)) {
		max_loops = erts_unicode_set_loop_limit(-1);
		BIF_RET(make_small(max_loops));
	    } else if (term_to_Uint(BIF_ARG_2, &max_loops) != 0) {
		max_loops = erts_unicode_set_loop_limit(max_loops);
		BIF_RET(make_small(max_loops));
	    }
	}
	else if (ERTS_IS_ATOM_STR("hipe_test_reschedule_suspend", BIF_ARG_1)) {
	    /* Used by hipe test suites */
	    erts_aint_t flag = erts_smp_atomic_read_nob(&hipe_test_reschedule_flag);
	    if (!flag && BIF_ARG_2 != am_false) {
		erts_smp_atomic_set_nob(&hipe_test_reschedule_flag, 1);
		erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
		ERTS_BIF_YIELD2(bif_export[BIF_erts_debug_set_internal_state_2],
				BIF_P, BIF_ARG_1, BIF_ARG_2);
	    }
	    erts_smp_atomic_set_nob(&hipe_test_reschedule_flag, !flag);
	    BIF_RET(NIL);
	}
	else if (ERTS_IS_ATOM_STR("hipe_test_reschedule_resume", BIF_ARG_1)) {
	    /* Used by hipe test suites */
	    Eterm res = am_false;
	    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
					BIF_ARG_2, ERTS_PROC_LOCK_STATUS);
	    if (rp) {
		erts_resume(rp, ERTS_PROC_LOCK_STATUS);
		res = am_true;
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	    }
	    BIF_RET(res);
	}
	else if (ERTS_IS_ATOM_STR("test_long_gc_sleep", BIF_ARG_1)) {
	    if (term_to_Uint(BIF_ARG_2, &erts_test_long_gc_sleep) > 0)
		BIF_RET(am_true);
	}
	else if (ERTS_IS_ATOM_STR("abort", BIF_ARG_1)) {
	    erl_exit(ERTS_ABORT_EXIT, "%T\n", BIF_ARG_2);
	}
	else if (ERTS_IS_ATOM_STR("kill_dist_connection", BIF_ARG_1)) {
	    DistEntry *dep = erts_sysname_to_connected_dist_entry(BIF_ARG_2);
	    if (!dep)
		BIF_RET(am_false);
	    else {
		Uint32 con_id;
		erts_smp_de_rlock(dep);
		con_id = dep->connection_id;
		erts_smp_de_runlock(dep);
		erts_kill_dist_connection(dep, con_id);
		erts_deref_dist_entry(dep);
		BIF_RET(am_true);
	    }
	}
	else if (ERTS_IS_ATOM_STR("not_running_optimization", BIF_ARG_1)) {
#ifdef ERTS_SMP
	    int old_use_opt, use_opt;
	    switch (BIF_ARG_2) {
	    case am_true:
		use_opt = 1;
		break;
	    case am_false:
		use_opt = 0;
		break;
	    default:
		BIF_ERROR(BIF_P, BADARG);
	    }

	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_smp_thr_progress_block();
	    old_use_opt = !erts_disable_proc_not_running_opt;
	    erts_disable_proc_not_running_opt = !use_opt;
	    erts_smp_thr_progress_unblock();
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    BIF_RET(old_use_opt ? am_true : am_false);
#else
	    BIF_ERROR(BIF_P,  EXC_NOTSUP);
#endif
	}
	else if (ERTS_IS_ATOM_STR("wait", BIF_ARG_1)) {
	    if (ERTS_IS_ATOM_STR("deallocations", BIF_ARG_2)) {
		if (erts_debug_wait_deallocations(BIF_P)) {
		    ERTS_BIF_YIELD_RETURN(BIF_P, am_ok);
		}
	    }
	}
    }

    BIF_ERROR(BIF_P, BADARG);
}

#ifdef ERTS_ENABLE_LOCK_COUNT
static Eterm lcnt_build_lock_stats_term(Eterm **hpp, Uint *szp, erts_lcnt_lock_stats_t *stats, Eterm res) {
    Uint tries = 0, colls = 0;
    unsigned long timer_s = 0, timer_ns = 0, timer_n = 0;
    unsigned int  line = 0;
    
    Eterm af, uil;
    Eterm uit, uic;
    Eterm uits, uitns, uitn;
    Eterm tt, tstat, tloc, t;
	
    /* term:
     * [{{file, line}, {tries, colls, {seconds, nanoseconds, n_blocks}}}]
     */
    
    tries = (Uint) ethr_atomic_read(&stats->tries);
    colls = (Uint) ethr_atomic_read(&stats->colls);
   
    line     = stats->line; 
    timer_s  = stats->timer.s;
    timer_ns = stats->timer.ns;
    timer_n  = stats->timer_n;
   
    af    = erts_atom_put(stats->file, strlen(stats->file), ERTS_ATOM_ENC_LATIN1, 1); 
    uil   = erts_bld_uint( hpp, szp, line);
    tloc  = erts_bld_tuple(hpp, szp, 2, af, uil);
    
    uit   = erts_bld_uint( hpp, szp, tries);             
    uic   = erts_bld_uint( hpp, szp, colls);             
    
    uits  = erts_bld_uint( hpp, szp, timer_s);
    uitns = erts_bld_uint( hpp, szp, timer_ns);
    uitn  = erts_bld_uint( hpp, szp, timer_n);
    tt    = erts_bld_tuple(hpp, szp, 3, uits, uitns, uitn);

    tstat = erts_bld_tuple(hpp, szp, 3, uit, uic, tt);
    
    t     = erts_bld_tuple(hpp, szp, 2, tloc, tstat);
    
    res   = erts_bld_cons( hpp, szp, t, res);

    return res;
}

static Eterm lcnt_build_lock_term(Eterm **hpp, Uint *szp, erts_lcnt_lock_t *lock, Eterm res) {
    Eterm name, type, id, stats = NIL, t;
    Process *proc = NULL;
    char *ltype;
    int i;
    
    /* term:
     * [{name, id, type, stats()}] 
     */
	
    ASSERT(lock->name);
    
    ltype = erts_lcnt_lock_type(lock->flag);
    
    ASSERT(ltype);
    
    type  = erts_atom_put(ltype, strlen(ltype), ERTS_ATOM_ENC_LATIN1, 1);           
    name  = erts_atom_put(lock->name, strlen(lock->name), ERTS_ATOM_ENC_LATIN1, 1); 

    if (lock->flag & ERTS_LCNT_LT_ALLOC) {
	/* use allocator types names as id's for allocator locks */
	ltype = (char *) ERTS_ALC_A2AD(signed_val(lock->id));
	id    = erts_atom_put(ltype, strlen(ltype), ERTS_ATOM_ENC_LATIN1, 1);
    } else if (lock->flag & ERTS_LCNT_LT_PROCLOCK) {
	/* use registered names as id's for process locks if available */
	proc  = erts_proc_lookup(lock->id);
	if (proc && proc->common.u.alive.reg) {
	    id = proc->common.u.alive.reg->name;
	} else {
	    /* otherwise use process id */
	    id = lock->id;
	}
    } else {
	id    = lock->id;                                    
    }
    
    for (i = 0; i < lock->n_stats; i++) {
	stats = lcnt_build_lock_stats_term(hpp, szp, &(lock->stats[i]), stats);
    }
	
    t     = erts_bld_tuple(hpp, szp, 4, name, id, type, stats);
    
    res   = erts_bld_cons( hpp, szp, t, res);          

    return res;
}

static Eterm lcnt_build_result_term(Eterm **hpp, Uint *szp, erts_lcnt_data_t *data, Eterm res) {
    Eterm dts, dtns, tdt, adur, tdur, aloc, lloc = NIL, tloc;
    erts_lcnt_lock_t *lock = NULL;
    char *str_duration = "duration";
    char *str_locks    = "locks";
    
    /* term:
     * [{'duration', {seconds, nanoseconds}}, {'locks', locks()}]
     */
   
    /* duration tuple */ 
    dts  = erts_bld_uint( hpp, szp, data->duration.s);
    dtns = erts_bld_uint( hpp, szp, data->duration.ns);
    tdt  = erts_bld_tuple(hpp, szp, 2, dts, dtns);
    
    adur = erts_atom_put(str_duration, strlen(str_duration), ERTS_ATOM_ENC_LATIN1, 1);
    tdur = erts_bld_tuple(hpp, szp, 2, adur, tdt);
   
    /* lock tuple */
    
    aloc = erts_atom_put(str_locks, strlen(str_locks), ERTS_ATOM_ENC_LATIN1, 1);
    	
    for (lock = data->current_locks->head; lock != NULL ; lock = lock->next ) {
	lloc = lcnt_build_lock_term(hpp, szp, lock, lloc);
    }
    
    for (lock = data->deleted_locks->head; lock != NULL ; lock = lock->next ) {
	lloc = lcnt_build_lock_term(hpp, szp, lock, lloc);
    }
    
    tloc = erts_bld_tuple(hpp, szp, 2, aloc, lloc);
    
    res  = erts_bld_cons( hpp, szp, tloc, res);          
    res  = erts_bld_cons( hpp, szp, tdur, res);          

    return res;
}    
#endif

BIF_RETTYPE erts_debug_lock_counters_1(BIF_ALIST_1)
{
#ifdef ERTS_ENABLE_LOCK_COUNT
    Eterm res = NIL;
#endif


    if (BIF_ARG_1 == am_enabled) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	BIF_RET(am_true);
#else
	BIF_RET(am_false);
#endif
    }
#ifdef ERTS_ENABLE_LOCK_COUNT

    else if (BIF_ARG_1 == am_info) {
	erts_lcnt_data_t *data; 
	Uint hsize = 0;
	Uint *szp;
    	Eterm* hp;

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	erts_lcnt_set_rt_opt(ERTS_LCNT_OPT_SUSPEND);
	data = erts_lcnt_get_data();

	/* calculate size */

	szp = &hsize;
	lcnt_build_result_term(NULL, szp, data, NIL);

	/* alloc and build */

	hp = HAlloc(BIF_P, hsize);

	res = lcnt_build_result_term(&hp, NULL, data, res);
	
	erts_lcnt_clear_rt_opt(ERTS_LCNT_OPT_SUSPEND);

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_clear) {
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	erts_lcnt_clear_counters();

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(am_ok);
    } else if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);

	switch (arityval(tp[0])) {
	    case 2: {
		int opt = 0;
		int val = 0;
		if (ERTS_IS_ATOM_STR("copy_save", tp[1])) {
		    opt = ERTS_LCNT_OPT_COPYSAVE;
		} else if (ERTS_IS_ATOM_STR("process_locks", tp[1])) {
		    opt = ERTS_LCNT_OPT_PROCLOCK;
		} else if (ERTS_IS_ATOM_STR("port_locks", tp[1])) {
		    opt = ERTS_LCNT_OPT_PORTLOCK;
		} else if (ERTS_IS_ATOM_STR("suspend", tp[1])) {
		    opt = ERTS_LCNT_OPT_SUSPEND;
		} else if (ERTS_IS_ATOM_STR("location", tp[1])) {
		    opt = ERTS_LCNT_OPT_LOCATION;
		} else {
		    BIF_ERROR(BIF_P, BADARG);
		}
		if (tp[2] == am_true) {
		    val = 1;
		} else if (tp[2] == am_false) {
		    val = 0;
		} else {
		    BIF_ERROR(BIF_P, BADARG);
		}

		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		erts_smp_thr_progress_block();
		
		if (val) {
		    res = erts_lcnt_set_rt_opt(opt) ? am_true : am_false;
		} else {
		    res = erts_lcnt_clear_rt_opt(opt) ? am_true : am_false;
		}
#ifdef ERTS_SMP
		if (res != tp[2]) {
		    if (opt == ERTS_LCNT_OPT_PORTLOCK) {
			erts_lcnt_enable_io_lock_count(val);
		    } else if (opt == ERTS_LCNT_OPT_PROCLOCK) {
			erts_lcnt_enable_proc_lock_count(val);
		    }
		}
#endif
		erts_smp_thr_progress_unblock();
		erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		BIF_RET(res);
		break;
	    }
     
	    default:
	    break;
	}
    } 

#endif 
    BIF_ERROR(BIF_P, BADARG);
}

static void os_info_init(void)
{
    Eterm type = erts_atom_put((byte *) os_type, strlen(os_type), ERTS_ATOM_ENC_LATIN1, 1);
    Eterm flav;
    int major, minor, build;
    char* buf = erts_alloc(ERTS_ALC_T_TMP, 1024); /* More than enough */
    Eterm* hp;

    os_flavor(buf, 1024);
    flav = erts_atom_put((byte *) buf, strlen(buf), ERTS_ATOM_ENC_LATIN1, 1);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    hp = erts_alloc(ERTS_ALC_T_LL_TEMP_TERM, (3+4)*sizeof(Eterm));
    os_type_tuple = TUPLE2(hp, type, flav);
    hp += 3;
    os_version(&major, &minor, &build);
    os_version_tuple = TUPLE3(hp,
			      make_small(major),
			      make_small(minor),
			      make_small(build));
}

void
erts_bif_info_init(void)
{
    erts_smp_atomic_init_nob(&available_internal_state, 0);
    erts_smp_atomic_init_nob(&hipe_test_reschedule_flag, 0);

    alloc_info_trap = erts_export_put(am_erlang, am_alloc_info, 1);
    alloc_sizes_trap = erts_export_put(am_erlang, am_alloc_sizes, 1);
    gather_sched_wall_time_res_trap
	= erts_export_put(am_erlang, am_gather_sched_wall_time_result, 1);
    gather_gc_info_res_trap
	= erts_export_put(am_erlang, am_gather_gc_info_result, 1);
    process_info_init();
    os_info_init();
}
