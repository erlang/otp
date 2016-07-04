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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_db_util.h"
#include "register.h"
#include "erl_thr_progress.h"
#define ERTS_PTAB_WANT_BIF_IMPL__
#include "erl_ptab.h"
#include "erl_bits.h"
#include "erl_bif_unique.h"
#include "erl_map.h"
#include "erl_msacc.h"

Export *erts_await_result;
static Export* flush_monitor_messages_trap = NULL;
static Export* set_cpu_topology_trap = NULL;
static Export* await_proc_exit_trap = NULL;
static Export* await_port_send_result_trap = NULL;
Export* erts_format_cpu_topology_trap = NULL;
static Export dsend_continue_trap_export;
Export *erts_convert_time_unit_trap = NULL;

static Export *await_msacc_mod_trap = NULL;
static erts_smp_atomic32_t msacc;

static Export *await_sched_wall_time_mod_trap;
static erts_smp_atomic32_t sched_wall_time;

static erts_smp_mtx_t ports_snapshot_mtx;
erts_smp_atomic_t erts_dead_ports_ptr; /* To store dying ports during snapshot */

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

/*
 * The BIF's now follow, see the Erlang Manual for a description of what
 * each individual BIF does.
 */

BIF_RETTYPE spawn_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = erts_default_spo_flags;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	if (ERTS_USE_MODIFIED_TIMING()) {
	    BIF_TRAP2(erts_delay_trap, BIF_P, pid, ERTS_MODIFIED_TIMING_DELAY);
	}
	BIF_RET(pid);
    }
}

/**********************************************************************/

/* Utility to add a new link between processes p and another internal
 * process (rpid). Process p must be the currently executing process.
 */
static int insert_internal_link(Process* p, Eterm rpid)
{
    Process *rp;
    ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;

    ASSERT(is_internal_pid(rpid));

#ifdef ERTS_SMP
    if (IS_TRACED(p)
	&& (ERTS_TRACE_FLAGS(p) & (F_TRACE_SOL|F_TRACE_SOL1))) {
	rp_locks = ERTS_PROC_LOCKS_ALL;
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
#endif

    /* get a pointer to the process struct of the linked process */
    rp = erts_pid2proc_opt(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
			   rpid, rp_locks,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);

    if (!rp) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	return 0;
    }

    if (p != rp) {
	erts_add_link(&ERTS_P_LINKS(p), LINK_PID, rp->common.id);
	erts_add_link(&ERTS_P_LINKS(rp), LINK_PID, p->common.id);

	ASSERT(IS_TRACER_VALID(ERTS_TRACER(p)));

	if (IS_TRACED(p)) {
	    if (ERTS_TRACE_FLAGS(p) & (F_TRACE_SOL|F_TRACE_SOL1))  {
		ERTS_TRACE_FLAGS(rp) |= (ERTS_TRACE_FLAGS(p) & TRACEE_FLAGS);
                erts_tracer_replace(&rp->common, ERTS_TRACER(p));
		if (ERTS_TRACE_FLAGS(p) & F_TRACE_SOL1) { /* maybe override */
		    ERTS_TRACE_FLAGS(rp) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    ERTS_TRACE_FLAGS(p) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
	    }
	}
    }
    if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	trace_proc(p, p == rp ? rp_locks : ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
                   rp, am_getting_linked, p->common.id);

    if (p == rp)
	erts_smp_proc_unlock(p, rp_locks & ~ERTS_PROC_LOCK_MAIN);
    else {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	erts_smp_proc_unlock(rp, rp_locks);
    }

    return 1;
}


/* create a link to the process */
BIF_RETTYPE link_1(BIF_ALIST_1)
{
    DistEntry *dep;

    if (IS_TRACED_FL(BIF_P, F_TRACE_PROCS)) {
	trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN, BIF_P, am_link, BIF_ARG_1);
    }
    /* check that the pid or port which is our argument is OK */

    if (is_internal_pid(BIF_ARG_1)) {
	if (insert_internal_link(BIF_P, BIF_ARG_1)) {
	    BIF_RET(am_true);
	}
	else {
	    goto res_no_proc;
	}
    }

    if (is_internal_port(BIF_ARG_1)) {
	int send_link_signal = 0;
	Port *prt = erts_port_lookup(BIF_ARG_1,
				     (erts_port_synchronous_ops
				      ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
				      : ERTS_PORT_SFLGS_INVALID_LOOKUP));
	if (!prt) {
	    goto res_no_proc;
	}

	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);
	
	if (erts_add_link(&ERTS_P_LINKS(BIF_P), LINK_PID, BIF_ARG_1) >= 0)
	    send_link_signal = 1;
	/* else: already linked */

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

	if (send_link_signal) {
	    Eterm ref;
	    Eterm *refp = erts_port_synchronous_ops ? &ref : NULL;

	    switch (erts_port_link(BIF_P, prt, BIF_P->common.id, refp)) {
	    case ERTS_PORT_OP_DROPPED:
	    case ERTS_PORT_OP_BADARG:
		goto res_no_proc;
	    case ERTS_PORT_OP_SCHEDULED:
		if (refp) {
		    ASSERT(is_internal_ref(ref));
		    BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
		}
	    default:
		break;
	    }
	}
	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	goto res_no_proc;
    }

    if (is_external_pid(BIF_ARG_1)) {

	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);

	/* We may earn time by checking first that we're not linked already */
	if (erts_lookup_link(ERTS_P_LINKS(BIF_P), BIF_ARG_1) != NULL) {
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
	    BIF_RET(am_true);
	}
	else {
	    ErtsLink *lnk;
	    int code;
	    ErtsDSigData dsd;
	    dep = external_pid_dist_entry(BIF_ARG_1);
	    if (dep == erts_this_dist_entry) {
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
		goto res_no_proc;
	    }

	    code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_DSP_RLOCK, 0);
	    switch (code) {
	    case ERTS_DSIG_PREP_NOT_ALIVE:
		/* Let the dlink trap handle it */
	    case ERTS_DSIG_PREP_NOT_CONNECTED:
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
		BIF_TRAP1(dlink_trap, BIF_P, BIF_ARG_1);

	    case ERTS_DSIG_PREP_CONNECTED:
		/* We are connected. Setup link and send link signal */

		erts_smp_de_links_lock(dep);

		erts_add_link(&ERTS_P_LINKS(BIF_P), LINK_PID, BIF_ARG_1);
		lnk = erts_add_or_lookup_link(&(dep->nlinks),
					      LINK_PID,
					      BIF_P->common.id);
		ASSERT(lnk != NULL);
		erts_add_link(&ERTS_LINK_ROOT(lnk), LINK_PID, BIF_ARG_1);

		erts_smp_de_links_unlock(dep);
		erts_smp_de_runlock(dep);
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

		code = erts_dsig_send_link(&dsd, BIF_P->common.id, BIF_ARG_1);
		if (code == ERTS_DSIG_SEND_YIELD)
		    ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
		BIF_RET(am_true);
	    default:
		ASSERT(! "Invalid dsig prepare result");
		BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	    }
	}
    }

    BIF_ERROR(BIF_P, BADARG);

res_no_proc: {
	erts_aint32_t state = erts_smp_atomic32_read_nob(&BIF_P->state);
	if (state & ERTS_PSFLG_TRAP_EXIT) {
	    ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
	    erts_deliver_exit_message(BIF_ARG_1, BIF_P, &locks, am_noproc, NIL);
	    erts_smp_proc_unlock(BIF_P, ~ERTS_PROC_LOCK_MAIN & locks);
	    BIF_RET(am_true);
	}
	else
	    BIF_ERROR(BIF_P, EXC_NOPROC);
    }
}

/* This function is allowed to return range of values handled by demonitor/1-2
 * Namely: atoms true, false, yield, internal_error, badarg or THE_NON_VALUE
 */
static Eterm
remote_demonitor(Process *c_p, DistEntry *dep, Eterm ref, Eterm to)
{
    ErtsDSigData dsd;
    ErtsMonitor *dmon;
    ErtsMonitor *mon;
    int code;
    Eterm res = am_false;
#ifndef ERTS_SMP
    int stale_mon = 0;
#endif

    ERTS_SMP_LC_ASSERT((ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK)
		       == erts_proc_lc_my_proc_locks(c_p));

    code = erts_dsig_prepare(&dsd, dep, c_p, ERTS_DSP_RLOCK, 0);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
#ifndef ERTS_SMP
	/* XXX Is this possible? Shouldn't this link
	   previously have been removed if the node
	   had previously been disconnected. */
	ASSERT(0);
	stale_mon = 1;
#endif
	/*
	 * In the smp case this is possible if the node goes
	 * down just before the call to demonitor.
	 */
	if (dep) {
	    erts_smp_de_links_lock(dep);
	    dmon = erts_remove_monitor(&dep->monitors, ref);
	    erts_smp_de_links_unlock(dep);
	    if (dmon)
		erts_destroy_monitor(dmon);
	}
	mon = erts_remove_monitor(&ERTS_P_MONITORS(c_p), ref);
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_LINK);

        res = am_true;
	break;

    case ERTS_DSIG_PREP_CONNECTED:

	erts_smp_de_links_lock(dep);
	mon = erts_remove_monitor(&ERTS_P_MONITORS(c_p), ref);
	dmon = erts_remove_monitor(&dep->monitors, ref);
	erts_smp_de_links_unlock(dep);
	erts_smp_de_runlock(dep);
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_LINK);

	if (!dmon) {
#ifndef ERTS_SMP
	    /* XXX How is this possible? Shouldn't this link
	       previously have been removed when the distributed
	       end was removed. */
	    ASSERT(0);
	    stale_mon = 1;
#endif
	    /*
	     * This is possible when smp support is enabled.
	     * 'DOWN' message just arrived.
	     */
            res = am_true;
	}
	else {
	    /*
	     * Soft (no force) send, use ->data in dist slot 
	     * monitor list since in case of monitor name 
	     * the atom is stored there. Yield if necessary.
	     */
	    code = erts_dsig_send_demonitor(&dsd,
					    c_p->common.id, 
					    (mon->name != NIL
					     ? mon->name
					     : mon->pid), 
					    ref,
					    0);
            res = (code == ERTS_DSIG_SEND_YIELD ? am_yield : am_true);
	    erts_destroy_monitor(dmon);
	}
	break;
    default:
	ASSERT(! "Invalid dsig prepare result");
        return am_internal_error;
    }

#ifndef ERTS_SMP
    if (stale_mon) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Stale process monitor %T to ", ref);
	if (is_atom(to))
	    erts_dsprintf(dsbufp, "{%T, %T}", to, dep->sysname);
	else
	    erts_dsprintf(dsbufp, "%T", to);
	erts_dsprintf(dsbufp, " found\n");
	erts_send_error_to_logger(c_p->group_leader, dsbufp);
    }
#endif

    /*
     * We aren't allowed to destroy 'mon' until now, since 'to'
     * may refer into 'mon' (external pid).
     */
    ASSERT(mon); /* Since link lock wasn't released between
		    lookup and remove */
    erts_destroy_monitor(mon);

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
    return res;
}

static ERTS_INLINE void
demonitor_local_process(Process *c_p, Eterm ref, Eterm to, Eterm *res)
{
    Process *rp = erts_pid2proc_opt(c_p,
                           ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
                           to,
                           ERTS_PROC_LOCK_LINK,
                           ERTS_P2P_FLG_ALLOW_OTHER_X);
    ErtsMonitor *mon = erts_remove_monitor(&ERTS_P_MONITORS(c_p), ref);

#ifndef ERTS_SMP
    ASSERT(mon);
#else
    if (!mon)
        *res = am_false;
    else
#endif
    {
        *res = am_true;
        erts_destroy_monitor(mon);
    }
    if (rp) {
        ErtsMonitor *rmon;
        rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), ref);
        if (rp != c_p)
            erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
        if (rmon != NULL)
            erts_destroy_monitor(rmon);
    }
    else {
        ERTS_SMP_ASSERT_IS_NOT_EXITING(c_p);
    }
}

static ERTS_INLINE BIF_RETTYPE
demonitor_local_port(Process *origin, Eterm ref, Eterm target)
{
    BIF_RETTYPE res = am_false;
    Port        *port = erts_port_lookup_raw(target);

    if (!port) {
        BIF_ERROR(origin, BADARG);
    }
    erts_smp_proc_unlock(origin, ERTS_PROC_LOCK_LINK);

    if (port) {
        Eterm trap_ref;
        switch (erts_port_demonitor(origin, ERTS_PORT_DEMONITOR_NORMAL,
                                    port, ref, &trap_ref)) {
        case ERTS_PORT_OP_DROPPED:
        case ERTS_PORT_OP_BADARG:
            break;
        case ERTS_PORT_OP_SCHEDULED:
            BIF_TRAP3(await_port_send_result_trap, origin, trap_ref,
                      am_busy_port, am_true);
            /* the busy_port atom will never be returned, because it cannot be
             * returned from erts_port_(de)monitor, but just in case if in future
             * internal API changes - you may see this atom */
        default:
            break;
        }
    }
    else {
        ERTS_SMP_ASSERT_IS_NOT_EXITING(origin);
    }
    BIF_RET(res);
}

/* Can return atom true, false, yield, internal_error, badarg or
 * THE_NON_VALUE if error occured or trap has been set up
 */
static
BIF_RETTYPE demonitor(Process *c_p, Eterm ref, Eterm *multip)
{
   ErtsMonitor  *mon = NULL;  /* The monitor entry to delete */
   Eterm        to = NIL;     /* Monitor link traget */
   DistEntry    *dep = NULL;  /* Target's distribution entry */
   int          deref_de = 0;
   BIF_RETTYPE  res = am_false;
   int          unlock_link = 1;

   erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_LINK);

   if (is_not_internal_ref(ref)) {
       res = am_badarg;
       goto done; /* Cannot be this monitor's ref */
   }

   mon = erts_lookup_monitor(ERTS_P_MONITORS(c_p), ref);
   if (!mon) {
       goto done;
   }

   switch (mon->type) {
   case MON_TIME_OFFSET:
       *multip = am_true;
       erts_demonitor_time_offset(ref);
       res = am_true;
       break;
   case MON_ORIGIN:
       to = mon->pid;
       *multip = am_false;
       if (is_atom(to)) {
           /* Monitoring a name at node to */
           ASSERT(is_node_name_atom(to));
           dep = erts_sysname_to_connected_dist_entry(to);
           ASSERT(dep != erts_this_dist_entry);
           if (dep)
               deref_de = 1;
       } else if (is_port(to)) {
           if (port_dist_entry(to) != erts_this_dist_entry) {
               goto badarg;
           }
           res = demonitor_local_port(c_p, ref, to);
           unlock_link = 0;
           goto done;
       } else {
           ASSERT(is_pid(to));
           dep = pid_dist_entry(to);
       }
       if (dep != erts_this_dist_entry) {
           res = remote_demonitor(c_p, dep, ref, to);
           /* remote_demonitor() unlocks link lock on c_p */
           unlock_link = 0;
       }
       else { /* Local monitor */
           if (deref_de) {
               deref_de = 0;
               erts_deref_dist_entry(dep);
           }
           dep = NULL;
           demonitor_local_process(c_p, ref, to, &res);
       }
       break;
    default /* case */ :
badarg:
       res = am_badarg; /* will be converted to error by caller */
       *multip = am_false;
       break;
   }
done:

   if (unlock_link)
       erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_LINK);

   if (deref_de) {
       ASSERT(dep);
       erts_deref_dist_entry(dep);
   }

   ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
   BIF_RET(res);
}

BIF_RETTYPE demonitor_1(BIF_ALIST_1)
{
    Eterm multi;
    switch (demonitor(BIF_P, BIF_ARG_1, &multi)) {
    case am_false:
    case am_true:       BIF_RET(am_true);
    case THE_NON_VALUE: BIF_RET(THE_NON_VALUE);
    case am_yield:      ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    case am_badarg:     BIF_ERROR(BIF_P, BADARG);

    case am_internal_error:
    default:
	ASSERT(! "demonitor(): internal error");
	BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
    }
}

BIF_RETTYPE demonitor_2(BIF_ALIST_2)
{
    BIF_RETTYPE res = am_true;
    Eterm       multi = am_false;
    int         info = 0;
    int         flush = 0;
    Eterm       list = BIF_ARG_2;

    while (is_list(list)) {
	Eterm* consp = list_val(list);
	switch (CAR(consp)) {
	case am_flush:
	    flush = 1;
	    break;
	case am_info:
	    info = 1;
	    break;
	default:
	    goto badarg;
	}
	list = CDR(consp);	
    }

    if (is_not_nil(list))
	goto badarg;

    switch (demonitor(BIF_P, BIF_ARG_1, &multi)) {
    case THE_NON_VALUE:
        /* If other error occured or trap has been set up - pass through */
        BIF_RET(THE_NON_VALUE);
    case am_false:
	if (info)
	    res = am_false;
	if (flush) {
flush_messages:
	    BIF_TRAP3(flush_monitor_messages_trap, BIF_P,
		      BIF_ARG_1, multi, res);
	}
    case am_true:
	if (multi == am_true && flush)
	    goto flush_messages;
	BIF_RET(res);
    case am_yield:
	ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    case am_badarg:
badarg:
	BIF_ERROR(BIF_P, BADARG);
    case am_internal_error:
    default:
	ASSERT(! "demonitor(): internal error");
	BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
    }
}

/* Type must be atomic object! */
void
erts_queue_monitor_message(Process *p,
			   ErtsProcLocks *p_locksp,
			   Eterm ref,
			   Eterm type,
			   Eterm item,
			   Eterm reason)
{
    Eterm tup;
    Eterm* hp;
    Eterm reason_copy, ref_copy, item_copy;
    Uint reason_size, ref_size, item_size, heap_size;
    ErlOffHeap *ohp;
    ErtsMessage *msgp;

    reason_size = IS_CONST(reason) ? 0 : size_object(reason);
    item_size   = IS_CONST(item) ? 0 : size_object(item);
    ref_size    = size_object(ref);

    heap_size = 6+reason_size+ref_size+item_size;

    msgp = erts_alloc_message_heap(p, p_locksp, heap_size,
				   &hp, &ohp);

    reason_copy = (IS_CONST(reason)
		   ? reason
		   : copy_struct(reason, reason_size, &hp, ohp));
    item_copy   = (IS_CONST(item)
		   ? item
		   : copy_struct(item, item_size, &hp, ohp));
    ref_copy    = copy_struct(ref, ref_size, &hp, ohp);

    tup = TUPLE5(hp, am_DOWN, ref_copy, type, item_copy, reason_copy);
    erts_queue_message(p, *p_locksp, msgp, tup, am_system);
}

static Eterm
local_pid_monitor(Process *p, Eterm target, Eterm mon_ref, int boolean)
{
    Eterm         ret = mon_ref;
    Process      *rp;
    ErtsProcLocks p_locks = ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK;

    if (target == p->common.id) {
	return ret;
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
    rp = erts_pid2proc_opt(p, p_locks,
			   target, ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    if (!rp) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	p_locks &= ~ERTS_PROC_LOCK_LINK;
	if (boolean)
	    ret = am_false;
	else
	    erts_queue_monitor_message(p, &p_locks,
				       mon_ref, am_process, target, am_noproc);
    }
    else {
	ASSERT(rp != p);

	if (boolean)
	    ret = am_true;

	erts_add_monitor(&ERTS_P_MONITORS(p), MON_ORIGIN, mon_ref, target, NIL);
	erts_add_monitor(&ERTS_P_MONITORS(rp), MON_TARGET, mon_ref, p->common.id, NIL);

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }

    erts_smp_proc_unlock(p, p_locks & ~ERTS_PROC_LOCK_MAIN);

    return ret;
}

static BIF_RETTYPE
local_port_monitor(Process *origin, Eterm target)
{
    BIF_RETTYPE ref = erts_make_ref(origin);
    Port *port = erts_sig_lookup_port(origin, target);
    ErtsProcLocks p_locks = ERTS_PROC_LOCK_MAIN;

    if (!port) {
res_no_proc:
        /* Send the DOWN message immediately. Ref is made on the fly because
         * caller has never seen it yet. */
        erts_queue_monitor_message(origin, &p_locks, ref,
                                   am_port, target, am_noproc);
    }
    else {
        switch (erts_port_monitor(origin, port, target, &ref)) {
        case ERTS_PORT_OP_DROPPED:
        case ERTS_PORT_OP_BADARG:
            goto res_no_proc;
        case ERTS_PORT_OP_SCHEDULED:
            BIF_TRAP3(await_port_send_result_trap, origin, ref,
                      am_busy_port, ref);
            /* the busy_port atom will never be returned, because it cannot be
             * returned from erts_port_monitor, but just in case if in future
             * internal API changes - you may see this atom */
        default:
            break;
        }
    }
    erts_smp_proc_unlock(origin, p_locks & ~ERTS_PROC_LOCK_MAIN);
    BIF_RET(ref);
}

/* Type = process | port :: atom(), 1st argument passed to erlang:monitor/2
 */
static BIF_RETTYPE
local_name_monitor(Process *self, Eterm type, Eterm target_name)
{
    BIF_RETTYPE ret = erts_make_ref(self);

    ErtsProcLocks p_locks = ERTS_PROC_LOCK_MAIN | ERTS_PROC_LOCK_LINK;
    Process     *proc = NULL;
    Port        *port = NULL;

    erts_smp_proc_lock(self, ERTS_PROC_LOCK_LINK);

    erts_whereis_name(self, p_locks, target_name,
                      &proc, ERTS_PROC_LOCK_LINK,
                      ERTS_P2P_FLG_ALLOW_OTHER_X,
                      &port, 0);

    /* If the name is not registered,
     * or if we asked for proc and got a port,
     * or if we asked for port and got a proc,
     * we just send the 'DOWN' message.
     */
    if ((!proc && !port) ||
        (type == am_process && port) ||
        (type == am_port && proc)) {
        DeclareTmpHeap(lhp,3,self);
	Eterm item;
        UseTmpHeap(3,self);

        erts_smp_proc_unlock(self, ERTS_PROC_LOCK_LINK);
	p_locks &= ~ERTS_PROC_LOCK_LINK;

	item = TUPLE2(lhp, target_name, erts_this_dist_entry->sysname);
        erts_queue_monitor_message(self, &p_locks,
                                   ret,
                                   type, /* = process|port :: atom() */
                                   item, am_noproc);
        UnUseTmpHeap(3,self);
    }
    else if (port) {
        erts_smp_proc_unlock(self, p_locks & ~ERTS_PROC_LOCK_MAIN);
        p_locks &= ~ERTS_PROC_LOCK_MAIN;

        switch (erts_port_monitor(self, port, target_name, &ret)) {
        case ERTS_PORT_OP_DONE:
            return ret;
        case ERTS_PORT_OP_SCHEDULED: { /* Scheduled a signal */
            ASSERT(is_internal_ref(ret));
            BIF_TRAP3(await_port_send_result_trap, self,
                      ret, am_true, ret);
            /* bif_trap returns */
        } break;
        default:
            goto badarg;
        }
    }
    else if (proc != self) {
        erts_add_monitor(&ERTS_P_MONITORS(self), MON_ORIGIN, ret,
                         proc->common.id, target_name);
        erts_add_monitor(&ERTS_P_MONITORS(proc), MON_TARGET, ret,
                         self->common.id, target_name);
        erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_LINK);
    }

    if (p_locks) {
        erts_smp_proc_unlock(self, p_locks & ~ERTS_PROC_LOCK_MAIN);
    }
    BIF_RET(ret);
badarg:
    if (p_locks) {
        erts_smp_proc_unlock(self, p_locks & ~ERTS_PROC_LOCK_MAIN);
    }
    BIF_ERROR(self, BADARG);
}

static BIF_RETTYPE
remote_monitor(Process *p, Eterm bifarg1, Eterm bifarg2,
	       DistEntry *dep, Eterm target, int byname)
{
    ErtsDSigData dsd;
    BIF_RETTYPE ret;
    int code;

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
    code = erts_dsig_prepare(&dsd, dep, p, ERTS_DSP_RLOCK, 0);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
	/* Let the dmonitor_p trap handle it */
    case ERTS_DSIG_PREP_NOT_CONNECTED:
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	ERTS_BIF_PREP_TRAP2(ret, dmonitor_p_trap, p, bifarg1, bifarg2);
	break;
    case ERTS_DSIG_PREP_CONNECTED:
	if (!(dep->flags & DFLAG_DIST_MONITOR)
	    || (byname && !(dep->flags & DFLAG_DIST_MONITOR_NAME))) {
	    erts_smp_de_runlock(dep);
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	    ERTS_BIF_PREP_ERROR(ret, p, BADARG);
	}
	else {
	    Eterm p_trgt, p_name, d_name, mon_ref;

	    mon_ref = erts_make_ref(p);

	    if (byname) {
		p_trgt = dep->sysname;
		p_name = target;
		d_name = target;
	    }
	    else {
		p_trgt = target;
		p_name = NIL;
		d_name = NIL;
	    }

	    erts_smp_de_links_lock(dep);

	    erts_add_monitor(&ERTS_P_MONITORS(p), MON_ORIGIN, mon_ref, p_trgt,
			     p_name);
	    erts_add_monitor(&(dep->monitors), MON_TARGET, mon_ref, p->common.id,
			     d_name);

	    erts_smp_de_links_unlock(dep);
	    erts_smp_de_runlock(dep);
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);

	    code = erts_dsig_send_monitor(&dsd, p->common.id, target, mon_ref);
	    if (code == ERTS_DSIG_SEND_YIELD)
		ERTS_BIF_PREP_YIELD_RETURN(ret, p, mon_ref);
	    else
		ERTS_BIF_PREP_RET(ret, mon_ref);
	}
	break;
    default:
	ASSERT(! "Invalid dsig prepare result");
	ERTS_BIF_PREP_ERROR(ret, p, EXC_INTERNAL_ERROR);
	break;
    }

    BIF_RET(ret);
}
	
BIF_RETTYPE monitor_2(BIF_ALIST_2)
{
    Eterm target = BIF_ARG_2;
    BIF_RETTYPE ret;
    DistEntry  *dep = NULL; 
    int deref_de = 0;

    /* Only process monitors are implemented */
    switch (BIF_ARG_1) {
    case am_time_offset: {
	Eterm ref;
        if (BIF_ARG_2 != am_clock_service) {
            goto badarg;
        }
	ref = erts_make_ref(BIF_P);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);
	erts_add_monitor(&ERTS_P_MONITORS(BIF_P), MON_TIME_OFFSET,
			 ref, am_clock_service, NIL);
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
	erts_monitor_time_offset(BIF_P->common.id, ref);
	BIF_RET(ref);
    }
    case am_process:
    case am_port:
	break;
    default:
        goto badarg;
    }

    if (is_internal_pid(target) && BIF_ARG_1 == am_process) {
local_pid:
        ret = local_pid_monitor(BIF_P, target, erts_make_ref(BIF_P), 0);
    } else if (is_external_pid(target) && BIF_ARG_1 == am_process) {
	dep = external_pid_dist_entry(target);
	if (dep == erts_this_dist_entry)
	    goto local_pid;
	ret = remote_monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, dep, target, 0);
    } else if (is_internal_port(target) && BIF_ARG_1 == am_port) {
local_port:
        ret = local_port_monitor(BIF_P, target);
    } else if (is_external_port(target) && BIF_ARG_1 == am_port) {
        dep = external_port_dist_entry(target);
        if (dep == erts_this_dist_entry) {
            goto local_port;
        }
        goto badarg; /* No want remote port */
    } else if (is_atom(target)) {
        ret = local_name_monitor(BIF_P, BIF_ARG_1, target);
    } else if (is_tuple(target)) {
	Eterm *tp = tuple_val(target);
	Eterm remote_node;
	Eterm name;
        if (arityval(*tp) != 2) {
            goto badarg;
        }
	remote_node = tp[2];
	name = tp[1];
	if (!is_atom(remote_node) || !is_atom(name)) {
            goto badarg;
	}
	if (!erts_is_alive && remote_node != am_Noname) {
            goto badarg; /* Remote monitor from (this) undistributed node */
	}
	dep = erts_sysname_to_connected_dist_entry(remote_node);
	if (dep == erts_this_dist_entry) {
	    deref_de = 1;
            ret = local_name_monitor(BIF_P, BIF_ARG_1, name);
	} else {
	    if (dep)
		deref_de = 1;
	    ret = remote_monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, dep, name, 1);
	}
    } else {
badarg:
	ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    }
    if (deref_de) {
	deref_de = 0;
	erts_deref_dist_entry(dep);
    }

    return ret;
}

/**********************************************************************/
/* this is a combination of the spawn and link BIFs */

BIF_RETTYPE spawn_link_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = erts_default_spo_flags|SPO_LINK;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	if (ERTS_USE_MODIFIED_TIMING()) {
	    BIF_TRAP2(erts_delay_trap, BIF_P, pid, ERTS_MODIFIED_TIMING_DELAY);
	}
	BIF_RET(pid);
    }
}

/**********************************************************************/

BIF_RETTYPE spawn_opt_1(BIF_ALIST_1)
{
    ErlSpawnOpts so;
    Eterm pid;
    Eterm* tp;
    Eterm ap;
    Eterm arg;
    Eterm res;

    /*
     * Check that the first argument is a tuple of four elements.
     */
    if (is_not_tuple(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    tp = tuple_val(BIF_ARG_1);
    if (*tp != make_arityval(4))
	goto error;

    /*
     * Store default values for options.
     */
    so.flags          = erts_default_spo_flags|SPO_USE_ARGS;
    so.min_heap_size  = H_MIN_SIZE;
    so.min_vheap_size = BIN_VH_MIN_SIZE;
    so.max_heap_size  = H_MAX_SIZE;
    so.max_heap_flags = H_MAX_FLAGS;
    so.priority       = PRIORITY_NORMAL;
    so.max_gen_gcs    = (Uint16) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
    so.scheduler      = 0;

    /*
     * Walk through the option list.
     */
    ap = tp[4];
    while (is_list(ap)) {
	arg = CAR(list_val(ap));
	if (arg == am_link) {
	    so.flags |= SPO_LINK;
	} else if (arg == am_monitor) {
	    so.flags |= SPO_MONITOR;
	} else if (is_tuple(arg)) {
	    Eterm* tp2 = tuple_val(arg);
	    Eterm val;
	    if (*tp2 != make_arityval(2))
		goto error;
	    arg = tp2[1];
	    val = tp2[2];
	    if (arg == am_priority) {
		if (val == am_max)
		    so.priority = PRIORITY_MAX;
		else if (val == am_high)
		    so.priority = PRIORITY_HIGH;
		else if (val == am_normal)
		    so.priority = PRIORITY_NORMAL;
		else if (val == am_low)
		    so.priority = PRIORITY_LOW;
		else
		    goto error;
	    } else if (arg == am_message_queue_data) {
		switch (val) {
		case am_on_heap:
		    so.flags &= ~SPO_OFF_HEAP_MSGQ;
		    so.flags |= SPO_ON_HEAP_MSGQ;
		    break;
		case am_off_heap:
		    so.flags &= ~SPO_ON_HEAP_MSGQ;
		    so.flags |= SPO_OFF_HEAP_MSGQ;
		    break;
		default:
		    goto error;
		}
	    } else if (arg == am_min_heap_size && is_small(val)) {
		Sint min_heap_size = signed_val(val);
		if (min_heap_size < 0) {
		    goto error;
		} else if (min_heap_size < H_MIN_SIZE) {
		    so.min_heap_size = H_MIN_SIZE;
		} else {
		    so.min_heap_size = erts_next_heap_size(min_heap_size, 0);
		}
            } else if (arg == am_max_heap_size) {
                if (!erts_max_heap_size(val, &so.max_heap_size, &so.max_heap_flags))
                    goto error;
	    } else if (arg == am_min_bin_vheap_size && is_small(val)) {
		Sint min_vheap_size = signed_val(val);
		if (min_vheap_size < 0) {
		    goto error;
		} else if (min_vheap_size < BIN_VH_MIN_SIZE) {
		    so.min_vheap_size = BIN_VH_MIN_SIZE;
		} else {
		    so.min_vheap_size = erts_next_heap_size(min_vheap_size, 0);
		}
	    } else if (arg == am_fullsweep_after && is_small(val)) {
		Sint max_gen_gcs = signed_val(val);
		if (max_gen_gcs < 0) {
		    goto error;
		} else {
		    so.max_gen_gcs = max_gen_gcs;
		}
	    } else if (arg == am_scheduler && is_small(val)) {
		Sint scheduler = signed_val(val);
		if (scheduler < 0 || erts_no_schedulers < scheduler)
		    goto error;
		so.scheduler = (int) scheduler;
	    } else {
		goto error;
	    }
	} else {
	    goto error;
	}
	ap = CDR(list_val(ap));
    }
    if (is_not_nil(ap)) {
	goto error;
    }

    if (so.max_heap_size != 0 && so.max_heap_size < so.min_heap_size) {
        goto error;
    }

    /*
     * Spawn the process.
     */
    pid = erl_create_process(BIF_P, tp[1], tp[2], tp[3], &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else if (so.flags & SPO_MONITOR) {
	Eterm* hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, pid, so.mref);
    } else {
	res = pid;
    }

    if (ERTS_USE_MODIFIED_TIMING()) {
	BIF_TRAP2(erts_delay_trap, BIF_P, res, ERTS_MODIFIED_TIMING_DELAY);
    }
    else {
	BIF_RET(res);
    }
}

  
/**********************************************************************/
/* remove a link from a process */
BIF_RETTYPE unlink_1(BIF_ALIST_1)
{
    Process *rp;
    DistEntry *dep;
    ErtsLink *l = NULL, *rl = NULL;
    ErtsProcLocks cp_locks = ERTS_PROC_LOCK_MAIN;

    /*
     * SMP specific note concerning incoming exit signals:
     *   We have to have at least the status lock during removal of
     *   the link half on current process, and check for and handle
     *   a present pending exit while the status lock is held. This
     *   in order to ensure that we wont be exited by a link after
     *   it has been removed.
     *
     *   (We also have to have the link lock, of course, in order to
     *    be allowed to remove the link...)
     */

    if (IS_TRACED_FL(BIF_P, F_TRACE_PROCS)) {
	trace_proc(BIF_P, cp_locks, BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_internal_port(BIF_ARG_1)) {
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
#ifdef ERTS_SMP
	if (ERTS_PROC_PENDING_EXIT(BIF_P))
	    goto handle_pending_exit;
#endif

	l = erts_remove_link(&ERTS_P_LINKS(BIF_P), BIF_ARG_1);

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

	if (l) {
	    Port *prt;

	    erts_destroy_link(l);

	    /* Send unlink signal */
	    prt = erts_port_lookup(BIF_ARG_1, ERTS_PORT_SFLGS_DEAD);
	    if (prt) {
		ErtsPortOpResult res;
		Eterm ref;
		Eterm *refp = erts_port_synchronous_ops ? &ref : NULL;
#ifdef DEBUG
		ref = NIL;
#endif
		res = erts_port_unlink(BIF_P, prt, BIF_P->common.id, refp);

		if (refp && res == ERTS_PORT_OP_SCHEDULED) {
		    ASSERT(is_internal_ref(ref));
		    BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
		}
	    }
	}

	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	BIF_RET(am_true);
    }

    if (is_not_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    if (is_external_pid(BIF_ARG_1)) {
	ErtsDistLinkData dld;
	int code;
	ErtsDSigData dsd;
	/* Blind removal, we might have trapped or anything, this leaves
	   us in a state where monitors might be inconsistent, but the dist
	   code should take care of it. */
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
#ifdef ERTS_SMP
	if (ERTS_PROC_PENDING_EXIT(BIF_P))
	    goto handle_pending_exit;
#endif
	l = erts_remove_link(&ERTS_P_LINKS(BIF_P), BIF_ARG_1);

	erts_smp_proc_unlock(BIF_P,
			     ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

	if (l)
	    erts_destroy_link(l);

	dep = external_pid_dist_entry(BIF_ARG_1);
	if (dep == erts_this_dist_entry) {
	    BIF_RET(am_true);
	}

	code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_DSP_NO_LOCK, 0);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
#if 1
	    BIF_RET(am_true);
#else
	    /*
	     * This is how we used to do it, but the link is obviously not
	     * active, so I see no point in setting up a connection.
	     * /Rickard
	     */
	    BIF_TRAP1(dunlink_trap, BIF_P, BIF_ARG_1);
#endif

	case ERTS_DSIG_PREP_CONNECTED:
	    erts_remove_dist_link(&dld, BIF_P->common.id, BIF_ARG_1, dep);
	    code = erts_dsig_send_unlink(&dsd, BIF_P->common.id, BIF_ARG_1);
	    erts_destroy_dist_link(&dld);
	    if (code == ERTS_DSIG_SEND_YIELD)
		ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
	    BIF_RET(am_true);

	default:
	    ASSERT(! "Invalid dsig prepare result");
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}
    }

    /* Internal pid... */

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

    cp_locks |= ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS;

    /* get process struct */
    rp = erts_pid2proc_opt(BIF_P, cp_locks,
			   BIF_ARG_1, ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);

#ifdef ERTS_SMP
    if (ERTS_PROC_PENDING_EXIT(BIF_P)) {
	if (rp && rp != BIF_P)
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	goto handle_pending_exit;
    }
#endif

    /* unlink and ignore errors */
    l = erts_remove_link(&ERTS_P_LINKS(BIF_P), BIF_ARG_1);
    if (l != NULL)
	erts_destroy_link(l);

    if (!rp) {
	ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
    }
    else {
	rl = erts_remove_link(&ERTS_P_LINKS(rp), BIF_P->common.id);
	if (rl != NULL)
	    erts_destroy_link(rl);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rl != NULL) {
            erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_STATUS);
            cp_locks &= ~ERTS_PROC_LOCK_STATUS;
	    trace_proc(BIF_P, (ERTS_PROC_LOCK_MAIN | ERTS_PROC_LOCK_LINK),
                       rp, am_getting_unlinked, BIF_P->common.id);
	}

	if (rp != BIF_P)
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }
 
    erts_smp_proc_unlock(BIF_P, cp_locks & ~ERTS_PROC_LOCK_MAIN);

    BIF_RET(am_true);

#ifdef ERTS_SMP
 handle_pending_exit:
    erts_handle_pending_exit(BIF_P, (ERTS_PROC_LOCK_MAIN
				     | ERTS_PROC_LOCK_LINK
				     | ERTS_PROC_LOCK_STATUS));
    ASSERT(ERTS_PROC_IS_EXITING(BIF_P)); 
    erts_smp_proc_unlock(BIF_P,	ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
    ERTS_BIF_EXITED(BIF_P);
#endif
}

BIF_RETTYPE hibernate_3(BIF_ALIST_3)
{
    /*
     * hibernate/3 is usually translated to an instruction; therefore
     * this function is only called from HiPE or when the call could not
     * be translated.
     */
    Eterm reg[3];

    if (erts_hibernate(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, reg)) {
        /*
         * If hibernate succeeded, TRAP. The process will be wait in a
         * hibernated state if its state is inactive (!ERTS_PSFLG_ACTIVE);
         * otherwise, continue executing (if any message was in the queue).
         */
        BIF_TRAP_CODE_PTR_(BIF_P, BIF_P->i);
    }
    return THE_NON_VALUE;
}

/**********************************************************************/

BIF_RETTYPE get_stacktrace_0(BIF_ALIST_0)
{
    Eterm t = build_stacktrace(BIF_P, BIF_P->ftrace);
    BIF_RET(t);
}

/**********************************************************************/
/*
 * This is like exit/1, except that errors are logged if they terminate
 * the process, and the final error value will be {Term,StackTrace}.
 */

BIF_RETTYPE error_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like error/1, except that the given 'args' will be included
 * in the stacktrace.
 */

BIF_RETTYPE error_2(BIF_ALIST_2)
{
    Eterm* hp = HAlloc(BIF_P, 3);

    BIF_P->fvalue = TUPLE2(hp, BIF_ARG_1, BIF_ARG_2);
    BIF_ERROR(BIF_P, EXC_ERROR_2);
}

/**********************************************************************/
/*
 * This is like exactly like error/1. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like exactly like error/2. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_2(BIF_ALIST_2)
{
    Eterm* hp = HAlloc(BIF_P, 3);

    BIF_P->fvalue = TUPLE2(hp, BIF_ARG_1, BIF_ARG_2);
    BIF_ERROR(BIF_P, EXC_ERROR_2);
}

/**********************************************************************/
/* this is like throw/1 except that we set freason to EXC_EXIT */

BIF_RETTYPE exit_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;  /* exit value */
    BIF_ERROR(BIF_P, EXC_EXIT);
}


/**********************************************************************/
/* raise an exception of given class, value and stacktrace.
 *
 * If there is an error in the argument format, 
 * return the atom 'badarg' instead.
 */
BIF_RETTYPE raise_3(BIF_ALIST_3)
{
    Process *c_p = BIF_P;
    Eterm class = BIF_ARG_1;
    Eterm value = BIF_ARG_2;
    Eterm stacktrace = BIF_ARG_3;
    Eterm reason;
    Eterm l, *hp, *hp_end, *tp;
    int depth, cnt;
    size_t sz;
    int must_copy = 0;
    struct StackTrace *s;

    if (class == am_error) {
	c_p->fvalue = value;
	reason = EXC_ERROR;
    } else if (class == am_exit) {
	c_p->fvalue = value;
	reason = EXC_EXIT;
    } else if (class == am_throw) {
	c_p->fvalue = value;
	reason = EXC_THROWN;
    } else goto error;
    reason &= ~EXF_SAVETRACE;
    
    /* Check syntax of stacktrace, and count depth.
     * Accept anything that can be returned from erlang:get_stacktrace/0,
     * as well as a 2-tuple with a fun as first element that the
     * error_handler may need to give us. Also allow old-style
     * MFA three-tuples.
     */
    for (l = stacktrace, depth = 0;  
	 is_list(l);  
	 l = CDR(list_val(l)), depth++) {
	Eterm t = CAR(list_val(l));
	Eterm location = NIL;

	if (is_not_tuple(t)) goto error;
	tp = tuple_val(t);
	switch (arityval(tp[0])) {
	case 2:
	    /* {Fun,Args} */
	    if (is_fun(tp[1])) {
		must_copy = 1;
	    } else {
		goto error;
	    }
	    break;
	case 3:
	    /*
	     * One of:
	     * {Fun,Args,Location}
	     * {M,F,A}
	     */
	    if (is_fun(tp[1])) {
		location = tp[3];
	    } else if (is_atom(tp[1]) && is_atom(tp[2])) {
		must_copy = 1;
	    } else {
		goto error;
	    }
	    break;
	case 4:
	    if (!(is_atom(tp[1]) && is_atom(tp[2]))) {
		goto error;
	    }
	    location = tp[4];
	    break;
	default:
	    goto error;
	}
	if (is_not_list(location) && is_not_nil(location)) {
	    goto error;
	}
    }
    if (is_not_nil(l)) goto error;
    
    /* Create stacktrace and store */
    if (erts_backtrace_depth < depth) {
	depth = erts_backtrace_depth;
	must_copy = 1;
    }
    if (must_copy) {
	cnt = depth;
	c_p->ftrace = NIL;
    } else {
	/* No need to copy the stacktrace */
	cnt = 0;
	c_p->ftrace = stacktrace;
    }

    tp = &c_p->ftrace;
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm) - 1) 
	/ sizeof(Eterm);
    hp = HAlloc(c_p, sz + (2+6)*(cnt + 1));
    hp_end = hp + sz + (2+6)*(cnt + 1);
    s = (struct StackTrace *) hp;
    s->header = make_neg_bignum_header(sz - 1);
    s->freason = reason;
    s->pc = NULL;
    s->current = NULL;
    s->depth = 0;
    hp += sz;
    if (must_copy) {
	int cnt;

	/* Copy list up to depth */
	for (cnt = 0, l = stacktrace;
	     cnt < depth;
	     cnt++, l = CDR(list_val(l))) {
	    Eterm t;
	    Eterm *tpp;
	    int arity;

	    ASSERT(*tp == NIL);
	    t = CAR(list_val(l));
	    tpp = tuple_val(t);
	    arity = arityval(tpp[0]);
	    if (arity == 2) {
		t = TUPLE3(hp, tpp[1], tpp[2], NIL);
		hp += 4;
	    } else if (arity == 3 && is_atom(tpp[1])) {
		t = TUPLE4(hp, tpp[1], tpp[2], tpp[3], NIL);
		hp += 5;
	    }
	    *tp = CONS(hp, t, *tp);
	    tp = &CDR(list_val(*tp));
	    hp += 2;
	}
    }
    c_p->ftrace = CONS(hp, c_p->ftrace, make_big((Eterm *) s));
    hp += 2;
    ASSERT(hp <= hp_end);
    HRelease(c_p, hp_end, hp);
    BIF_ERROR(c_p, reason);
    
 error:
    return am_badarg;
}

/**********************************************************************/
/* send an exit message to another process (if trapping exits) or
   exit the other process */

BIF_RETTYPE exit_2(BIF_ALIST_2)
{
     Process *rp;

     /*
      * If the first argument is not a pid, or a local port it is an error.
      */

     if (is_internal_port(BIF_ARG_1)) {
	 Eterm ref, *refp;
	 Uint32 invalid_flags;
	 Port *prt;

	 if (erts_port_synchronous_ops) {
	     refp = &ref;
	     invalid_flags = ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP;
	 }
	 else {
	     refp = NULL;
	     invalid_flags = ERTS_PORT_SFLGS_INVALID_LOOKUP;
	 }

	 prt = erts_port_lookup(BIF_ARG_1, invalid_flags);

	 if (prt) {
	     ErtsPortOpResult res;

#ifdef DEBUG
	     ref = NIL;
#endif

	     res = erts_port_exit(BIF_P, 0, prt, BIF_P->common.id, BIF_ARG_2, refp);

	     ERTS_BIF_CHK_EXITED(BIF_P);

	     if (refp && res == ERTS_PORT_OP_SCHEDULED) {
		 ASSERT(is_internal_ref(ref));
		 BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
	     }

	 }

	 BIF_RET(am_true);
     }
     else if(is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	 BIF_RET(am_true);
     
     /*
      * If it is a remote pid, send a signal to the remote node.
      */

     if (is_external_pid(BIF_ARG_1)) {
	 int code;
	 ErtsDSigData dsd;
	 DistEntry *dep;

	 dep = external_pid_dist_entry(BIF_ARG_1);
	 if(dep == erts_this_dist_entry)
	     BIF_RET(am_true);

	 code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_DSP_NO_LOCK, 0);
	 switch (code) {
	 case ERTS_DSIG_PREP_NOT_ALIVE:
	 case ERTS_DSIG_PREP_NOT_CONNECTED:
	     BIF_TRAP2(dexit_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	 case ERTS_DSIG_PREP_CONNECTED:
	     code = erts_dsig_send_exit2(&dsd, BIF_P->common.id, BIF_ARG_1, BIF_ARG_2);
	     if (code == ERTS_DSIG_SEND_YIELD)
		 ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
	     BIF_RET(am_true);
	 default:
	     ASSERT(! "Invalid dsig prepare result");
	     BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	 }
     }
     else if (is_not_internal_pid(BIF_ARG_1)) {
       BIF_ERROR(BIF_P, BADARG);
     }
     else {
	 /*
	  * The pid is internal.  Verify that it refers to an existing process.
	  */
	 ErtsProcLocks rp_locks;

	 if (BIF_ARG_1 == BIF_P->common.id) {
	     rp_locks = ERTS_PROC_LOCKS_ALL;
	     rp = BIF_P;
	     erts_smp_proc_lock(rp, ERTS_PROC_LOCKS_ALL_MINOR);
	 }
	 else {
	     rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	     rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				BIF_ARG_1, rp_locks);
	     if (!rp) {
		 BIF_RET(am_true);
	     }
	 }

	 /*
	  * Send an exit signal.
	  */
	 erts_send_exit_signal(BIF_P,
			       BIF_P->common.id,
			       rp,
			       &rp_locks,
			       BIF_ARG_2,
			       NIL,
			       NULL,
			       BIF_P == rp ? ERTS_XSIG_FLG_NO_IGN_NORMAL : 0);
#ifdef ERTS_SMP
	 if (rp == BIF_P)
	     rp_locks &= ~ERTS_PROC_LOCK_MAIN;
	 if (rp_locks)
	     erts_smp_proc_unlock(rp, rp_locks);
#endif
	 /*
	  * We may have exited ourselves and may have to take action.
	  */
	 ERTS_BIF_CHK_EXITED(BIF_P);
	 BIF_RET(am_true);
     }
}

/**********************************************************************/
/* this sets some process info- trapping exits or the error handler */


/* Handle flags common to both process_flag_2 and process_flag_3. */
static BIF_RETTYPE process_flag_aux(Process *BIF_P,
				    Process *rp,
				    Eterm flag,
				    Eterm val)
{
   Eterm old_value = NIL;	/* shut up warning about use before set */
   Sint i;
   if (flag == am_save_calls) {
       struct saved_calls *scb;
       if (!is_small(val))
	   goto error;
       i = signed_val(val);
       if (i < 0 || i > 10000)
	   goto error;

       if (i == 0)
	   scb = NULL;
       else {
	   Uint sz = sizeof(*scb) + (i-1) * sizeof(scb->ct[0]);
	   scb = erts_alloc(ERTS_ALC_T_CALLS_BUF, sz);
	   scb->len = i;
	   scb->cur = 0;
	   scb->n = 0;
       }

#ifdef HIPE
       if (rp->flags & F_HIPE_MODE) {
	   ASSERT(!ERTS_PROC_GET_SAVED_CALLS_BUF(rp));
	   scb = ERTS_PROC_SET_SUSPENDED_SAVED_CALLS_BUF(rp, scb);
       }
       else
#endif
       {
#ifdef HIPE
	   ASSERT(!ERTS_PROC_GET_SUSPENDED_SAVED_CALLS_BUF(rp));
#endif
	   scb = ERTS_PROC_SET_SAVED_CALLS_BUF(rp, scb);
	   if (rp == BIF_P && ((scb && i == 0) || (!scb && i != 0))) {
	       /* Adjust fcalls to match save calls setting... */
	       if (i == 0)
		   BIF_P->fcalls += CONTEXT_REDS; /* disabled it */
	       else
		   BIF_P->fcalls -= CONTEXT_REDS; /* enabled it */

	       /*
		* Make sure we reschedule immediately so the
		* change take effect at once.
		*/
	       ERTS_VBUMP_ALL_REDS(BIF_P);
	   }
       }

       if (!scb)
	   old_value = make_small(0);
       else {
	   old_value = make_small(scb->len);
	   erts_free(ERTS_ALC_T_CALLS_BUF, (void *) scb);
       }

       BIF_RET(old_value);
   }

 error:
   BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE process_flag_2(BIF_ALIST_2)
{
   Eterm old_value;
   if (BIF_ARG_1 == am_error_handler) {
      if (is_not_atom(BIF_ARG_2)) {
	 goto error;
      }
      old_value = erts_proc_set_error_handler(BIF_P, BIF_ARG_2);
      BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_priority) {
       old_value = erts_set_process_priority(BIF_P, BIF_ARG_2);
       if (old_value == THE_NON_VALUE)
	   goto error;
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_trap_exit) {
       erts_aint32_t state;
       Uint trap_exit;
       if (BIF_ARG_2 == am_true) {
	   trap_exit = 1;
       } else if (BIF_ARG_2 == am_false) {
	   trap_exit = 0;
       } else {
	   goto error;
       }
       /*
	* NOTE: It is important that we check for pending exit signals
	*       and handle them before returning if trap_exit is set to
	*       true. For more info, see implementation of
	*       erts_send_exit_signal().
	*/
       erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCKS_XSIG_SEND);
       if (trap_exit)
	   state = erts_smp_atomic32_read_bor_mb(&BIF_P->state,
						 ERTS_PSFLG_TRAP_EXIT);
       else
	   state = erts_smp_atomic32_read_band_mb(&BIF_P->state,
						  ~ERTS_PSFLG_TRAP_EXIT);
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCKS_XSIG_SEND);

#ifdef ERTS_SMP
       if (state & ERTS_PSFLG_PENDING_EXIT) {
	   erts_handle_pending_exit(BIF_P, ERTS_PROC_LOCK_MAIN);
	   ERTS_BIF_EXITED(BIF_P);
       }
#endif

       old_value = (state & ERTS_PSFLG_TRAP_EXIT) ? am_true : am_false;
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_scheduler) {
       ErtsRunQueue *old, *new, *curr;
       Sint sched;
       erts_aint32_t state;

       if (!is_small(BIF_ARG_2))
	   goto error;
       sched = signed_val(BIF_ARG_2);
       if (sched < 0 || erts_no_schedulers < sched)
	   goto error;

       if (sched == 0) {
	   new = NULL;
	   state = erts_smp_atomic32_read_band_mb(&BIF_P->state,
						  ~ERTS_PSFLG_BOUND);
       }
       else {
	   new = erts_schedid2runq(sched);
#ifdef ERTS_SMP
	   erts_atomic_set_nob(&BIF_P->run_queue, (erts_aint_t) new);
#endif
	   state = erts_smp_atomic32_read_bor_mb(&BIF_P->state,
						 ERTS_PSFLG_BOUND);
       }

       curr = erts_proc_sched_data(BIF_P)->run_queue;
       old = (ERTS_PSFLG_BOUND & state) ? curr : NULL;

       ASSERT(!old || old == curr);

       old_value = old ? make_small(old->ix+1) : make_small(0);
       if (new && new != curr)
	   ERTS_BIF_YIELD_RETURN_X(BIF_P, old_value, am_scheduler);
       else
	   BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_min_heap_size) {
       Sint i;
       if (!is_small(BIF_ARG_2)) {
	   goto error;
       }
       i = signed_val(BIF_ARG_2);
       if (i < 0) {
	   goto error;
       }
       old_value = make_small(BIF_P->min_heap_size);
       if (i < H_MIN_SIZE) {
	   BIF_P->min_heap_size = H_MIN_SIZE;
       } else {
	   BIF_P->min_heap_size = erts_next_heap_size(i, 0);
       }
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_min_bin_vheap_size) {
       Sint i;
       if (!is_small(BIF_ARG_2)) {
	   goto error;
       }
       i = signed_val(BIF_ARG_2);
       if (i < 0) {
	   goto error;
       }
       old_value = make_small(BIF_P->min_vheap_size);
       if (i < BIN_VH_MIN_SIZE) {
	   BIF_P->min_vheap_size = BIN_VH_MIN_SIZE;
       } else {
	   BIF_P->min_vheap_size = erts_next_heap_size(i, 0);
       }
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_max_heap_size) {
       Eterm *hp;
       Uint sz = 0, max_heap_size, max_heap_flags;

       if (!erts_max_heap_size(BIF_ARG_2, &max_heap_size, &max_heap_flags))
           goto error;

       if ((max_heap_size < MIN_HEAP_SIZE(BIF_P) && max_heap_size != 0))
	   goto error;

       erts_max_heap_size_map(MAX_HEAP_SIZE_GET(BIF_P), MAX_HEAP_SIZE_FLAGS_GET(BIF_P), NULL, &sz);
       hp = HAlloc(BIF_P, sz);
       old_value = erts_max_heap_size_map(MAX_HEAP_SIZE_GET(BIF_P), MAX_HEAP_SIZE_FLAGS_GET(BIF_P), &hp, NULL);
       MAX_HEAP_SIZE_SET(BIF_P, max_heap_size);
       MAX_HEAP_SIZE_FLAGS_SET(BIF_P, max_heap_flags);
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_message_queue_data) {
       old_value = erts_change_message_queue_management(BIF_P, BIF_ARG_2);
       if (is_non_value(old_value))
	   goto error;
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_sensitive) {
       Uint is_sensitive;
       if (BIF_ARG_2 == am_true) {
	   is_sensitive = 1;
       } else if (BIF_ARG_2 == am_false) {
	   is_sensitive = 0;
       } else {
	   goto error;
       }
       erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
       old_value = (ERTS_TRACE_FLAGS(BIF_P) & F_SENSITIVE
		    ? am_true
		    : am_false);
       if (is_sensitive) {
	   ERTS_TRACE_FLAGS(BIF_P) |= F_SENSITIVE;
       } else {
	   ERTS_TRACE_FLAGS(BIF_P) &= ~F_SENSITIVE;
       }
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
       /* make sure to bump all reds so that we get
          rescheduled immediately so setting takes effect */
       BIF_RET2(old_value, CONTEXT_REDS);
   }
   else if (BIF_ARG_1 == am_monitor_nodes) {
       /*
	* This argument is intentionally *not* documented. It is intended
	* to be used by net_kernel:monitor_nodes/1.
	*/
       old_value = erts_monitor_nodes(BIF_P, BIF_ARG_2, NIL);
       if (old_value == THE_NON_VALUE)
	   goto error;
       BIF_RET(old_value);
   }
   else if (is_tuple(BIF_ARG_1)) {
       /*
	* This argument is intentionally *not* documented. It is intended
	* to be used by net_kernel:monitor_nodes/2.
	*/
       Eterm *tp = tuple_val(BIF_ARG_1);
       if (arityval(tp[0]) == 2) {
	   if (tp[1] == am_monitor_nodes) {
	       old_value = erts_monitor_nodes(BIF_P, BIF_ARG_2, tp[2]);
	       if (old_value == THE_NON_VALUE)
		   goto error;
	       BIF_RET(old_value);
	   }
       }
       /* Fall through and try process_flag_aux() ... */
   }

   BIF_RET(process_flag_aux(BIF_P, BIF_P, BIF_ARG_1, BIF_ARG_2));
 error:
   BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE process_flag_3(BIF_ALIST_3)
{
   Process *rp;
   Eterm res;

#ifdef ERTS_SMP
   rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				  BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
   if (rp == ERTS_PROC_LOCK_BUSY)
       ERTS_BIF_YIELD3(bif_export[BIF_process_flag_3], BIF_P,
		       BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
#else
   rp = erts_proc_lookup(BIF_ARG_1);
#endif

   if (!rp)
       BIF_ERROR(BIF_P, BADARG);

   res = process_flag_aux(BIF_P, rp, BIF_ARG_2, BIF_ARG_3);

   if (rp != BIF_P)
       erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

   return res;
}

/**********************************************************************/

/* register(atom, Process|Port) registers a global process or port
   (for this node) */

BIF_RETTYPE register_2(BIF_ALIST_2)   /* (Atom, Pid|Port)   */
{
    if (erts_register_name(BIF_P, BIF_ARG_1, BIF_ARG_2))
	BIF_RET(am_true);
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
}


/**********************************************************************/

/* removes the registration of a process or port */

BIF_RETTYPE unregister_1(BIF_ALIST_1)
{
    int res;
    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res = erts_unregister_name(BIF_P, ERTS_PROC_LOCK_MAIN, NULL, BIF_ARG_1);
    if (res == 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

/**********************************************************************/

/* find out the pid of a registered process */
/* this is a rather unsafe BIF as it allows users to do nasty things. */

BIF_RETTYPE whereis_1(BIF_ALIST_1)
{
    Eterm res;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    res = erts_whereis_name_to_id(BIF_P, BIF_ARG_1);
    BIF_RET(res);
}

/**********************************************************************/

/*
 * erlang:'!'/2
 */

HIPE_WRAPPER_BIF_DISABLE_GC(ebif_bang, 2)

BIF_RETTYPE
ebif_bang_2(BIF_ALIST_2)
{
    return erl_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
}


/*
 * Send a message to Process, Port or Registered Process.
 * Returns non-negative reduction bump or negative result code.
 */
#define SEND_TRAP		(-1)
#define SEND_YIELD		(-2)
#define SEND_YIELD_RETURN	(-3)
#define SEND_BADARG		(-4)
#define SEND_USER_ERROR		(-5)
#define SEND_INTERNAL_ERROR	(-6)
#define SEND_AWAIT_RESULT	(-7)
#define SEND_YIELD_CONTINUE     (-8)


Sint do_send(Process *p, Eterm to, Eterm msg, Eterm *refp, ErtsSendContext*);

static Sint remote_send(Process *p, DistEntry *dep,
			Eterm to, Eterm full_to, Eterm msg,
			ErtsSendContext* ctx)
{
    Sint res;
    int code;

    ASSERT(is_atom(to) || is_external_pid(to));

    code = erts_dsig_prepare(&ctx->dsd, dep, p, ERTS_DSP_NO_LOCK, !ctx->suspend);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
	res = SEND_TRAP;
	break;
    case ERTS_DSIG_PREP_WOULD_SUSPEND:
	ASSERT(!ctx->suspend);
	res = SEND_YIELD;
	break;
    case ERTS_DSIG_PREP_CONNECTED: {

	if (is_atom(to))
	    code = erts_dsig_send_reg_msg(to, msg, ctx);
	else
	    code = erts_dsig_send_msg(to, msg, ctx);
	/*
	 * Note that reductions have been bumped on calling
	 * process by erts_dsig_send_reg_msg() or
	 * erts_dsig_send_msg().
	 */
	if (code == ERTS_DSIG_SEND_YIELD)
	    res = SEND_YIELD_RETURN;
	else if (code == ERTS_DSIG_SEND_CONTINUE)
	    res = SEND_YIELD_CONTINUE;
	else
	    res = 0;
	break;
    }
    default:
	ASSERT(! "Invalid dsig prepare result");
	res = SEND_INTERNAL_ERROR;
    }

    if (res >= 0) {
	if (IS_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, full_to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
    }

    return res;
}

Sint
do_send(Process *p, Eterm to, Eterm msg, Eterm *refp, ErtsSendContext* ctx)
{
    Eterm portid;
    Port *pt;
    Process* rp;
    DistEntry *dep;
    Eterm* tp;

    if (is_internal_pid(to)) {
	if (IS_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);

	rp = erts_proc_lookup_raw(to);	
	if (!rp)
	    return 0;
    } else if (is_external_pid(to)) {
	dep = external_pid_dist_entry(to);
	if(dep == erts_this_dist_entry) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Discarding message %T from %T to %T in an old "
			  "incarnation (%d) of this node (%d)\n",
			  msg,
			  p->common.id,
			  to,
			  external_pid_creation(to),
			  erts_this_node->creation);
	    erts_send_error_to_logger(p->group_leader, dsbufp);
	    return 0;
	}
	return remote_send(p, dep, to, to, msg, ctx);
    } else if (is_atom(to)) {
	Eterm id = erts_whereis_name_to_id(p, to);

	rp = erts_proc_lookup_raw(id);
	if (rp) {
	    if (IS_TRACED_FL(p, F_TRACE_SEND))
		trace_send(p, to, msg);
	    if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
		save_calls(p, &exp_send);
	    goto send_message;
	}

	pt = erts_port_lookup(id,
			      (erts_port_synchronous_ops
			       ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
			       : ERTS_PORT_SFLGS_INVALID_LOOKUP));
	if (pt) {
	    portid = id;
	    goto port_common;
	}

	if (IS_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
	
	return SEND_BADARG;
    } else if (is_external_port(to)
	       && (external_port_dist_entry(to)
		   == erts_this_dist_entry)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Discarding message %T from %T to %T in an old "
		      "incarnation (%d) of this node (%d)\n",
		      msg,
		      p->common.id,
		      to,
		      external_port_creation(to),
		      erts_this_node->creation);
	erts_send_error_to_logger(p->group_leader, dsbufp);
	return 0;
    } else if (is_internal_port(to)) {
	int ret_val;
	portid = to;

	pt = erts_port_lookup(portid,
			      (erts_port_synchronous_ops
			       ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
			       : ERTS_PORT_SFLGS_INVALID_LOOKUP));

      port_common:
	ret_val = 0;

	if (pt) {
	    int ps_flags = ctx->suspend ? 0 : ERTS_PORT_SIG_FLG_NOSUSPEND;
	    *refp = NIL;

            if (IS_TRACED_FL(p, F_TRACE_SEND)) 	/* trace once only !! */
                trace_send(p, portid, msg);

            if (have_seqtrace(SEQ_TRACE_TOKEN(p))) {
                seq_trace_update_send(p);
                seq_trace_output(SEQ_TRACE_TOKEN(p), msg,
                                 SEQ_TRACE_SEND, portid, p);
            }

	    switch (erts_port_command(p, ps_flags, pt, msg, refp)) {
	    case ERTS_PORT_OP_CALLER_EXIT:
		/* We are exiting... */
		return SEND_USER_ERROR;
	    case ERTS_PORT_OP_BUSY:
		/* Nothing has been sent */
		if (ctx->suspend)
		    erts_suspend(p, ERTS_PROC_LOCK_MAIN, pt);
		return SEND_YIELD;
	    case ERTS_PORT_OP_BUSY_SCHEDULED:
		/* Message was sent */
		if (ctx->suspend) {
		    erts_suspend(p, ERTS_PROC_LOCK_MAIN, pt);
		    ret_val = SEND_YIELD_RETURN;
		    break;
		}
		/* Fall through */
	    case ERTS_PORT_OP_SCHEDULED:
		if (is_not_nil(*refp)) {
		    ASSERT(is_internal_ref(*refp));
		    ret_val = SEND_AWAIT_RESULT;
		}
		break;
	    case ERTS_PORT_OP_DROPPED:
	    case ERTS_PORT_OP_BADARG:
	    case ERTS_PORT_OP_DONE:
		break;
	    default:
		ERTS_INTERNAL_ERROR("Unexpected erts_port_command() result");
		break;
	    }
	}

	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);

	if (ERTS_PROC_IS_EXITING(p)) {
	    KILL_CATCHES(p); /* Must exit */
	    return SEND_USER_ERROR;
	}
	return ret_val;
    } else if (is_tuple(to)) { /* Remote send */
	int ret;
	tp = tuple_val(to);
	if (*tp != make_arityval(2))
	    return SEND_BADARG;
	if (is_not_atom(tp[1]) || is_not_atom(tp[2]))
	    return SEND_BADARG;
	
	/* sysname_to_connected_dist_entry will return NULL if there
	   is no dist_entry or the dist_entry has no port,
	   but remote_send() will handle that. */

	dep = erts_sysname_to_connected_dist_entry(tp[2]);

	if (dep == erts_this_dist_entry) {
	    Eterm id;
	    erts_deref_dist_entry(dep);
	    if (IS_TRACED_FL(p, F_TRACE_SEND))
		trace_send(p, to, msg);
	    if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
		save_calls(p, &exp_send);

	    id = erts_whereis_name_to_id(p, tp[1]);

	    rp = erts_proc_lookup_raw(id);
	    if (rp)
		goto send_message;
	    pt = erts_port_lookup(id,
				  (erts_port_synchronous_ops
				   ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
				   : ERTS_PORT_SFLGS_INVALID_LOOKUP));
	    if (pt) {
		portid = id;
		goto port_common;
	    }
	    return 0;
	}

	ret = remote_send(p, dep, tp[1], to, msg, ctx);
	if (ret != SEND_YIELD_CONTINUE) {
	    if (dep) {
		erts_deref_dist_entry(dep);
	    }
	} else {
	    ctx->dep_to_deref = dep;
	}
	return ret;
    } else {
	if (IS_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
	return SEND_BADARG;
    }
    
 send_message: {
	ErtsProcLocks rp_locks = 0;
	Sint res;
#ifdef ERTS_SMP
	if (p == rp)
	    rp_locks |= ERTS_PROC_LOCK_MAIN;
#endif
	/* send to local process */
	res = erts_send_message(p, rp, &rp_locks, msg, 0);
	if (erts_use_sender_punish)
	    res *= 4;
	else
	    res = 0;
	erts_smp_proc_unlock(rp,
			     p == rp
			     ? (rp_locks & ~ERTS_PROC_LOCK_MAIN)
			     : rp_locks);
	return res;
    }
}

HIPE_WRAPPER_BIF_DISABLE_GC(send, 3)

BIF_RETTYPE send_3(BIF_ALIST_3)
{
    BIF_RETTYPE retval;
    Eterm ref;
    Process *p = BIF_P;
    Eterm to = BIF_ARG_1;
    Eterm msg = BIF_ARG_2;
    Eterm opts = BIF_ARG_3;

    int connect = !0;
    Eterm l = opts;
    Sint result;

    DeclareTypedTmpHeap(ErtsSendContext, ctx, BIF_P);

    ERTS_MSACC_PUSH_STATE_M_X();

    UseTmpHeap(sizeof(ErtsSendContext)/sizeof(Eterm), BIF_P);

    ctx->suspend = !0;
    ctx->dep_to_deref = NULL;
    ctx->return_term = am_ok;
    ctx->dss.reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
    ctx->dss.phase = ERTS_DSIG_SEND_PHASE_INIT;

    while (is_list(l)) {
	if (CAR(list_val(l)) == am_noconnect) {
	    connect = 0;
	} else if (CAR(list_val(l)) == am_nosuspend) {
	    ctx->suspend = 0;
	} else {
	    ERTS_BIF_PREP_ERROR(retval, p, BADARG);
	    goto done;
	}
	l = CDR(list_val(l));
    }
    if(!is_nil(l)) {
	ERTS_BIF_PREP_ERROR(retval, p, BADARG);
	goto done;
    }

#ifdef DEBUG
    ref = NIL;
#endif

    ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_SEND);
    result = do_send(p, to, msg, &ref, ctx);
    ERTS_MSACC_POP_STATE_M_X();

    if (result > 0) {
	ERTS_VBUMP_REDS(p, result);
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, am_ok);
	goto done;
    }

    switch (result) {
    case 0:
	/* May need to yield even though we do not bump reds here... */
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, am_ok);
	break;
    case SEND_TRAP:
	if (connect) {
	    ERTS_BIF_PREP_TRAP3(retval, dsend3_trap, p, to, msg, opts);
	} else {
	    ERTS_BIF_PREP_RET(retval, am_noconnect);
	}
	break;
    case SEND_YIELD:
	if (ctx->suspend) {
	    ERTS_BIF_PREP_YIELD3(retval,
				 bif_export[BIF_send_3], p, to, msg, opts);
	} else {
	    ERTS_BIF_PREP_RET(retval, am_nosuspend);
	}
	break;
    case SEND_YIELD_RETURN:
	if (!ctx->suspend) {
	    ERTS_BIF_PREP_RET(retval, am_nosuspend);
	    break;
	}
    yield_return:
	ERTS_BIF_PREP_YIELD_RETURN(retval, p, am_ok);
        break;
    case SEND_AWAIT_RESULT:
	ASSERT(is_internal_ref(ref));
	ERTS_BIF_PREP_TRAP3(retval, await_port_send_result_trap, p, ref, am_nosuspend, am_ok);
	break;
    case SEND_BADARG:
	ERTS_BIF_PREP_ERROR(retval, p, BADARG);
	break;
    case SEND_USER_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_ERROR);
	break;
    case SEND_INTERNAL_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_INTERNAL_ERROR);
	break;
    case SEND_YIELD_CONTINUE:
	BUMP_ALL_REDS(p);
	erts_set_gc_state(p, 0);
	ERTS_BIF_PREP_TRAP1(retval, &dsend_continue_trap_export, p,
			    erts_dsend_export_trap_context(p, ctx));
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "send_3 invalid result %d\n", (int)result);
	break;
    }

done:
    UnUseTmpHeap(sizeof(ErtsSendContext)/sizeof(Eterm), BIF_P);
    return retval;
}

HIPE_WRAPPER_BIF_DISABLE_GC(send, 2)

BIF_RETTYPE send_2(BIF_ALIST_2)
{
    return erl_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

static BIF_RETTYPE dsend_continue_trap_1(BIF_ALIST_1)
{
    Binary* bin = ((ProcBin*) binary_val(BIF_ARG_1))->val;
    ErtsSendContext* ctx = (ErtsSendContext*) ERTS_MAGIC_BIN_DATA(bin);
    Sint initial_reds = (Sint) (ERTS_BIF_REDS_LEFT(BIF_P) * TERM_TO_BINARY_LOOP_FACTOR);
    int result;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == erts_dsend_context_dtor);

    ctx->dss.reds = initial_reds;
    result = erts_dsig_send(&ctx->dsd, &ctx->dss);

    switch (result) {
    case ERTS_DSIG_SEND_OK:
	erts_set_gc_state(BIF_P, 1);
	BIF_RET(ctx->return_term);
	break;
    case ERTS_DSIG_SEND_YIELD: /*SEND_YIELD_RETURN*/
	erts_set_gc_state(BIF_P, 1);
	if (!ctx->suspend)
	    BIF_RET(am_nosuspend);
	ERTS_BIF_YIELD_RETURN(BIF_P, ctx->return_term);

    case ERTS_DSIG_SEND_CONTINUE: { /*SEND_YIELD_CONTINUE*/
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP1(&dsend_continue_trap_export, BIF_P, BIF_ARG_1);
    }
    default:
	erts_exit(ERTS_ABORT_EXIT, "dsend_continue_trap invalid result %d\n", (int)result);
	break;
    }
    ASSERT(! "Can not arrive here");
    BIF_ERROR(BIF_P, BADARG);
}

Eterm erl_send(Process *p, Eterm to, Eterm msg)
{
    Eterm retval;
    Eterm ref;
    Sint result;
    DeclareTypedTmpHeap(ErtsSendContext, ctx, p);
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_SEND);
    UseTmpHeap(sizeof(ErtsSendContext)/sizeof(Eterm), p);
#ifdef DEBUG
    ref = NIL;
#endif
    ctx->suspend = !0;
    ctx->dep_to_deref = NULL;
    ctx->return_term = msg;
    ctx->dss.reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
    ctx->dss.phase = ERTS_DSIG_SEND_PHASE_INIT;

    result = do_send(p, to, msg, &ref, ctx);

    ERTS_MSACC_POP_STATE_M_X();

    if (result > 0) {
	ERTS_VBUMP_REDS(p, result);
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, msg);
	goto done;
    }

    switch (result) {
    case 0:
	/* May need to yield even though we do not bump reds here... */
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, msg);
	break;
    case SEND_TRAP:
	ERTS_BIF_PREP_TRAP2(retval, dsend2_trap, p, to, msg);
	break;
    case SEND_YIELD:
	ERTS_BIF_PREP_YIELD2(retval, bif_export[BIF_send_2], p, to, msg);
	break;
    case SEND_YIELD_RETURN:
    yield_return:
	ERTS_BIF_PREP_YIELD_RETURN(retval, p, msg);
        break;
    case SEND_AWAIT_RESULT:
	ASSERT(is_internal_ref(ref));
	ERTS_BIF_PREP_TRAP3(retval,
			    await_port_send_result_trap, p, ref, msg, msg);
	break;
    case SEND_BADARG:
	ERTS_BIF_PREP_ERROR(retval, p, BADARG);
	break;
    case SEND_USER_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_ERROR);
	break;
    case SEND_INTERNAL_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_INTERNAL_ERROR);
	break;
    case SEND_YIELD_CONTINUE:
	BUMP_ALL_REDS(p);
	erts_set_gc_state(p, 0);
	ERTS_BIF_PREP_TRAP1(retval, &dsend_continue_trap_export, p,
			    erts_dsend_export_trap_context(p, ctx));
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "invalid send result %d\n", (int)result);
	break;
    }

done:
    UnUseTmpHeap(sizeof(ErtsSendContext)/sizeof(Eterm), p);
    return retval;
}

/**********************************************************************/
/*
 * apply/3 is implemented as an instruction and as erlang code in the
 * erlang module.
 *
 * There is only one reason that apply/3 is included in the BIF table:
 * The error handling code in the beam emulator passes the pointer to
 * this function to the error handling code if the apply instruction
 * fails.  The error handling use the function pointer to lookup
 * erlang:apply/3 in the BIF table.
 *
 * This function will never be called.  (It could be if init did something
 * like this:  apply(erlang, apply, [M, F, A]). Not recommended.)
 */

BIF_RETTYPE apply_3(BIF_ALIST_3)
{
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/

/* integer to float */

/**********************************************************************/

/* returns the head of a list - this function is unecessary
   and is only here to keep Robert happy (Even more, since it's OP as well) */
BIF_RETTYPE hd_1(BIF_ALIST_1)
{
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(CAR(list_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the tails of a list - same comment as above */

BIF_RETTYPE tl_1(BIF_ALIST_1)
{
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(CDR(list_val(BIF_ARG_1)));
}


/**********************************************************************/
/* return the size of an I/O list */

static Eterm
accumulate(Eterm acc, Uint size)
{
    if (is_non_value(acc)) {
	/*
	 * There is no pre-existing accumulator. Allocate a
	 * bignum buffer with one extra word to be used if
	 * the bignum grows in the future.
	 */
	Eterm* hp = (Eterm *) erts_alloc(ERTS_ALC_T_TEMP_TERM,
					 (BIG_UINT_HEAP_SIZE+1) *
					 sizeof(Eterm));
	return uint_to_big(size, hp);
    } else {
	Eterm* big;
	int need_heap;

	/*
	 * Add 'size' to 'acc' in place. There is always one
	 * extra word allocated in case the bignum grows by one word.
	 */
	big = big_val(acc);
	need_heap = BIG_NEED_SIZE(BIG_SIZE(big));
	acc = big_plus_small(acc, size, big);
	if (BIG_NEED_SIZE(big_size(acc)) > need_heap) {
	    /*
	     * The extra word has been consumed. Grow the
	     * allocation by one word.
	     */
	    big = (Eterm *) erts_realloc(ERTS_ALC_T_TEMP_TERM,
					 big_val(acc),
					 (need_heap+1) * sizeof(Eterm));
	    acc = make_big(big);
	}
	return acc;
    }
}

static Eterm
consolidate(Process* p, Eterm acc, Uint size)
{
    Eterm* hp;

    if (is_non_value(acc)) {
	return erts_make_integer(size, p);
    } else {
	Eterm* big;
	Uint sz;
	Eterm res;
	
	acc = accumulate(acc, size);
	big = big_val(acc);
	sz = BIG_NEED_SIZE(BIG_SIZE(big));
	hp = HAlloc(p, sz);
	res = make_big(hp);
	while (sz--) {
	    *hp++ = *big++;
	}
	erts_free(ERTS_ALC_T_TEMP_TERM, (void *) big_val(acc));
	return res;
    }
}

BIF_RETTYPE iolist_size_1(BIF_ALIST_1)
{
    Eterm obj, hd;
    Eterm* objp;
    Uint size = 0;
    Uint cur_size;
    Uint new_size;
    Eterm acc = THE_NON_VALUE;
    DECLARE_ESTACK(s);

    obj = BIF_ARG_1;
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    hd = CAR(objp);
	    obj = CDR(objp);
	    /* Head */
	    if (is_byte(hd)) {
		size++;
		if (size == 0) {
		    acc = accumulate(acc, (Uint) -1);
		    size = 1;
		}
	    } else if (is_binary(hd) && binary_bitsize(hd) == 0) {
		cur_size = binary_size(hd);
		if ((new_size = size + cur_size) >= size) {
		    size = new_size;
		} else {
		    acc = accumulate(acc, size);
		    size = cur_size;
		}
	    } else if (is_list(hd)) {
		ESTACK_PUSH(s, obj);
		obj = hd;
		goto L_iter_list;
	    } else if (is_not_nil(hd)) {
		goto L_type_error;
	    }
	    /* Tail */
	    if (is_list(obj)) {
		goto L_iter_list;
	    } else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		cur_size = binary_size(obj);
		if ((new_size = size + cur_size) >= size) {
		    size = new_size;
		} else {
		    acc = accumulate(acc, size);
		    size = cur_size;
		}
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj) && binary_bitsize(obj) == 0) {
	    cur_size = binary_size(obj);
	    if ((new_size = size + cur_size) >= size) {
		size = new_size;
	    } else {
		acc = accumulate(acc, size);
		size = cur_size;
	    }
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    BIF_RET(consolidate(BIF_P, acc, size));

 L_type_error:
    DESTROY_ESTACK(s);
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the N'th element of a tuple */

BIF_RETTYPE element_2(BIF_ALIST_2)
{
    if (is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_2)) {
	Eterm* tuple_ptr = tuple_val(BIF_ARG_2);
	Sint ix = signed_val(BIF_ARG_1);

	if ((ix >= 1) && (ix <= arityval(*tuple_ptr)))
	    BIF_RET(tuple_ptr[ix]);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the arity of a tuple */

BIF_RETTYPE tuple_size_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	return make_small(arityval(*tuple_val(BIF_ARG_1)));
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* set the n'th element in a tuple */

BIF_RETTYPE setelement_3(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Eterm* resp;
    Uint ix;
    Uint size;

    if (is_not_small(BIF_ARG_1) || is_not_tuple(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = tuple_val(BIF_ARG_2);
    ix = signed_val(BIF_ARG_1);
    size = arityval(*ptr) + 1;   /* include arity */
    if ((ix < 1) || (ix >= size)) {
	goto error;
    }

    hp = HAlloc(BIF_P, size);

    /* copy the tuple */
    resp = hp;
    sys_memcpy(hp, ptr, sizeof(Eterm)*size);
    resp[ix] = BIF_ARG_3;
    BIF_RET(make_tuple(resp));
}

/**********************************************************************/

BIF_RETTYPE make_tuple_2(BIF_ALIST_2)
{
    Sint n;
    Eterm* hp;
    Eterm res;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0 || n > ERTS_MAX_TUPLE_SIZE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, n+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(n);
    while (n--) {
	*hp++ = BIF_ARG_2;
    }
    BIF_RET(res);
}

BIF_RETTYPE make_tuple_3(BIF_ALIST_3)
{
    Sint n;
    Uint limit;
    Eterm* hp;
    Eterm res;
    Eterm list = BIF_ARG_3;
    Eterm* tup;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0 || n > ERTS_MAX_TUPLE_SIZE) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    limit = (Uint) n;
    hp = HAlloc(BIF_P, n+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(n);
    tup = hp;
    while (n--) {
	*hp++ = BIF_ARG_2;
    }
    while(is_list(list)) {
	Eterm* cons;
	Eterm hd;
	Eterm* tp;
	Eterm index;
	Uint index_val;

	cons = list_val(list);
	hd = CAR(cons);
	list = CDR(cons);
	if (is_not_tuple_arity(hd, 2)) {
	    goto error;
	}
	tp = tuple_val(hd);
	if (is_not_small(index = tp[1])) {
	    goto error;
	}
	if ((index_val = unsigned_val(index) - 1) < limit) {
	    tup[index_val] = tp[2];
	} else {
	    goto error;
	}
    }
    if (is_not_nil(list)) {
	goto error;
    }
    BIF_RET(res);
}


/**********************************************************************/

BIF_RETTYPE append_element_2(BIF_ALIST_2)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;

    if (is_not_tuple(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr   = tuple_val(BIF_ARG_1);
    arity = arityval(*ptr);

    if (arity + 1 > ERTS_MAX_TUPLE_SIZE)
	goto error;

    hp  = HAlloc(BIF_P, arity + 2);
    res = make_tuple(hp);
    *hp = make_arityval(arity+1);
    while (arity--) {
	*++hp = *++ptr;
    }
    *++hp = BIF_ARG_2;
    BIF_RET(res);
}

BIF_RETTYPE insert_element_3(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;
    Sint ix, c1, c2;

    if (is_not_tuple(BIF_ARG_2) || is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    ptr   = tuple_val(BIF_ARG_2);
    arity = arityval(*ptr);
    ix    = signed_val(BIF_ARG_1);

    if ((ix < 1) || (ix > (arity + 1))) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp  = HAlloc(BIF_P, arity + 1 + 1);
    res = make_tuple(hp);
    *hp = make_arityval(arity + 1);

    c1 = ix - 1;
    c2 = arity - ix + 1;

    while (c1--) { *++hp = *++ptr; }
    *++hp = BIF_ARG_3;
    while (c2--) { *++hp = *++ptr; }

    BIF_RET(res);
}

BIF_RETTYPE delete_element_2(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;
    Sint ix, c1, c2;

    if (is_not_tuple(BIF_ARG_2) || is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    ptr   = tuple_val(BIF_ARG_2);
    arity = arityval(*ptr);
    ix    = signed_val(BIF_ARG_1);

    if ((ix < 1) || (ix > arity) || (arity == 0)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp  = HAlloc(BIF_P, arity + 1 - 1);
    res = make_tuple(hp);
    *hp = make_arityval(arity - 1);

    c1  = ix - 1;
    c2  = arity - ix;

    while (c1--) { *++hp = *++ptr; }
    ++ptr;
    while (c2--) { *++hp = *++ptr; }

    BIF_RET(res);
}

/**********************************************************************/

/* convert an atom to a list of ascii integer */

BIF_RETTYPE atom_to_list_1(BIF_ALIST_1)
{
    Atom* ap;
    Uint num_chars, num_built, num_eaten;
    byte* err_pos;
    Eterm res;
#ifdef DEBUG
    int ares;
#endif

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
     
    /* read data from atom table */
    ap = atom_tab(atom_val(BIF_ARG_1));
    if (ap->len == 0)
	BIF_RET(NIL);	/* the empty atom */

#ifdef DEBUG
    ares =
#endif
	erts_analyze_utf8(ap->name, ap->len, &err_pos, &num_chars, NULL);
    ASSERT(ares == ERTS_UTF8_OK);
    
    res = erts_utf8_to_list(BIF_P, num_chars, ap->name, ap->len, ap->len,
			    &num_built, &num_eaten, NIL);
    ASSERT(num_built == num_chars);
    ASSERT(num_eaten == ap->len);
    BIF_RET(res);
}

/**********************************************************************/

/* convert a list of ascii integers to an atom */
 
BIF_RETTYPE list_to_atom_1(BIF_ALIST_1)
{
    Eterm res;
    char *buf = (char *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_CHARACTERS);
    Sint i = intlist_to_buf(BIF_ARG_1, buf, MAX_ATOM_CHARACTERS);

    if (i < 0) {
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	i = erts_list_length(BIF_ARG_1);
	if (i > MAX_ATOM_CHARACTERS) {
	    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    res = erts_atom_put((byte *) buf, i, ERTS_ATOM_ENC_LATIN1, 1);
    ASSERT(is_atom(res));
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
}

/* conditionally convert a list of ascii integers to an atom */
 
BIF_RETTYPE list_to_existing_atom_1(BIF_ALIST_1)
{
    Sint i;
    char *buf = (char *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_CHARACTERS);

    if ((i = intlist_to_buf(BIF_ARG_1, buf, MAX_ATOM_CHARACTERS)) < 0) {
    error:
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(BIF_P, BADARG);
    } else {
	Eterm a;
	
	if (erts_atom_get(buf, i, &a, ERTS_ATOM_ENC_LATIN1)) {
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    BIF_RET(a);
	} else {
	    goto error;
	}
    }
}


/**********************************************************************/

/* convert an integer to a list of ascii integers */

BIF_RETTYPE integer_to_list_1(BIF_ALIST_1)
{
    Eterm* hp;
    Uint need;

    if (is_not_integer(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_1)) {
	char *c;
	int n;
	struct Sint_buf ibuf;

	c = Sint_to_buf(signed_val(BIF_ARG_1), &ibuf);
	n = sys_strlen(c);
	need = 2*n;
	hp = HAlloc(BIF_P, need);
	BIF_RET(buf_to_intlist(&hp, c, n, NIL));
    }
    else {
	int n = big_decimal_estimate(BIF_ARG_1);
	Eterm res;
        Eterm* hp_end;

	need = 2*n;
	hp = HAlloc(BIF_P, need);
        hp_end = hp + need;
	res = erts_big_to_list(BIF_ARG_1, &hp);
        HRelease(BIF_P,hp_end,hp);
	BIF_RET(res);
    }
}

/**********************************************************************/

/*
 * Converts a list of ascii base10 digits to an integer fully or partially.
 * Returns result and the remaining tail.
 * On error returns: {error,not_a_list}, or {error, no_integer}
 */

BIF_RETTYPE string_to_integer_1(BIF_ALIST_1)
{
     Eterm res;
     Eterm tail;
     Eterm *hp;
     /* must be a list */
     switch (erts_list_to_integer(BIF_P, BIF_ARG_1, 10, &res, &tail)) {
     /* HAlloc after erts_list_to_integer as it might HAlloc itself (bignum) */
     case LTI_BAD_STRUCTURE:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, am_error, am_not_a_list));
     case LTI_NO_INTEGER:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, am_error, am_no_integer));
     default:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, res, tail));
     }
}

BIF_RETTYPE list_to_integer_1(BIF_ALIST_1)
 {
   /* Using erts_list_to_integer is about twice as fast as using
      erts_chars_to_integer because we do not have to copy the 
      entire list */
     Eterm res;
     Eterm dummy;
     /* must be a list */
     if (erts_list_to_integer(BIF_P, BIF_ARG_1, 10,
                              &res, &dummy) != LTI_ALL_INTEGER) {
	 BIF_ERROR(BIF_P,BADARG);
     }
     BIF_RET(res);
 }

BIF_RETTYPE list_to_integer_2(BIF_ALIST_2)
{
  /* Bif implementation is about 50% faster than pure erlang,
     and since we have erts_chars_to_integer now it is simpler
     as well. This could be optmized further if we did not have to
     copy the list to buf. */
    Sint i;
    Eterm res, dummy;
    int base;

    i = erts_list_length(BIF_ARG_1);
    if (i < 0)
      BIF_ERROR(BIF_P, BADARG);
    
    base = signed_val(BIF_ARG_2);

    if (base < 2 || base > 36) 
      BIF_ERROR(BIF_P, BADARG);

    if (erts_list_to_integer(BIF_P, BIF_ARG_1, base,
                             &res, &dummy) != LTI_ALL_INTEGER) {
        BIF_ERROR(BIF_P,BADARG);
    }
    BIF_RET(res);
}

/**********************************************************************/

static int do_float_to_charbuf(Process *p, Eterm efloat, Eterm list, 
			char *fbuf, int sizeof_fbuf) {

    const static int arity_two = make_arityval(2);
    int decimals = SYS_DEFAULT_FLOAT_DECIMALS;
    int compact = 0;
    enum fmt_type_ {
        FMT_LEGACY,
        FMT_FIXED,
        FMT_SCIENTIFIC
    } fmt_type = FMT_LEGACY;
    Eterm arg;
    FloatDef f;

    /* check the arguments */
    if (is_not_float(efloat))
        goto badarg;

    for(; is_list(list); list = CDR(list_val(list))) {
        arg = CAR(list_val(list));
        if (arg == am_compact) {
            compact = 1;
            continue;
        } else if (is_tuple(arg)) {
            Eterm* tp = tuple_val(arg);
            if (*tp == arity_two && is_small(tp[2])) {
                decimals = signed_val(tp[2]);
                switch (tp[1]) {
                    case am_decimals:
                        fmt_type = FMT_FIXED;
                        continue;
                    case am_scientific:
                        fmt_type = FMT_SCIENTIFIC;
                        continue;
                }
            }
        }
        goto badarg;
    }
    if (is_not_nil(list)) {
        goto badarg;
    }

    GET_DOUBLE(efloat, f);

    if (fmt_type == FMT_FIXED) {
        return sys_double_to_chars_fast(f.fd, fbuf, sizeof_fbuf,
                decimals, compact);
    } else {
        return sys_double_to_chars_ext(f.fd, fbuf, sizeof_fbuf, decimals);
    }

badarg:
    return -1;
}

/* convert a float to a list of ascii characters */

static BIF_RETTYPE do_float_to_list(Process *BIF_P, Eterm arg, Eterm opts) {
  int used;
  Eterm* hp;
  char fbuf[256];
  
  if ((used = do_float_to_charbuf(BIF_P,arg,opts,fbuf,sizeof(fbuf))) <= 0) {
    BIF_ERROR(BIF_P, BADARG);
  }
  hp = HAlloc(BIF_P, (Uint)used*2);
  BIF_RET(buf_to_intlist(&hp, fbuf, (Uint)used, NIL));
}
  

BIF_RETTYPE float_to_list_1(BIF_ALIST_1)
{
  return do_float_to_list(BIF_P,BIF_ARG_1,NIL);
}

BIF_RETTYPE float_to_list_2(BIF_ALIST_2)
{
  return do_float_to_list(BIF_P,BIF_ARG_1,BIF_ARG_2);
}

/* convert a float to a binary of ascii characters */

static BIF_RETTYPE do_float_to_binary(Process *BIF_P, Eterm arg, Eterm opts) {
  int used;
  char fbuf[256];
  
  if ((used = do_float_to_charbuf(BIF_P,arg,opts,fbuf,sizeof(fbuf))) <= 0) {
    BIF_ERROR(BIF_P, BADARG);
  }
  BIF_RET(new_binary(BIF_P, (byte*)fbuf, (Uint)used));
}

BIF_RETTYPE float_to_binary_1(BIF_ALIST_1)
{
  return do_float_to_binary(BIF_P,BIF_ARG_1,NIL);
}

BIF_RETTYPE float_to_binary_2(BIF_ALIST_2)
{
  return do_float_to_binary(BIF_P,BIF_ARG_1,BIF_ARG_2);
}

/**********************************************************************/

/* convert a list of ascii  integer values e's +'s and -'s to a float */


#define SIGN      0
#define INT       1
#define FRAC      2
#define EXP_SIGN  3
#define EXP0      4
#define EXP1      5
#define END       6

#define IS_DOT(x) (unsigned_val((x)) == '.' || unsigned_val((x)) == ',')
#define IS_E(x) (unsigned_val((x)) == 'e' || unsigned_val((x)) == 'E')
#define IS_DIGIT(x) (unsigned_val((x)) >= '0' && unsigned_val((x)) <= '9')
#define SAVE_E(xi,xim,xl,xlm) ((xim)=(xi), (xlm)=(xl))
#define LOAD_E(xi,xim,xl,xlm) ((xi)=(xim), (xl)=(xlm))

#define STRING_TO_FLOAT_BUF_INC_SZ (128)
BIF_RETTYPE string_to_float_1(BIF_ALIST_1)
{
    Eterm orig = BIF_ARG_1;
    Eterm list = orig;
    Eterm list_mem = list;
    int i = 0;
    int i_mem = 0;
    Eterm* hp;
    Eterm error_res = NIL;
    int part = SIGN;	/* expect a + or - (or a digit) first */
    FloatDef f;
    Eterm tup;
    byte *buf = NULL;
    Uint bufsz = STRING_TO_FLOAT_BUF_INC_SZ;

    /* check it's a valid list to start with */
    if (is_nil(list)) {
	error_res = am_no_float;
    error:
	if (buf)
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	hp = HAlloc(BIF_P, 3);    
	BIF_RET(TUPLE2(hp, am_error, error_res));
    }
    if (is_not_list(list)) {
	error_res = am_not_a_list;
	goto error;
    }

    buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, bufsz);

    /*
       The float might start with a SIGN (+ | -). It must contain an integer 
       part, INT, followed by a delimiter (. | ,) and a fractional, FRAC, 
       part. The float might also contain an exponent. If e or E indicates 
       this we will look for a possible EXP_SIGN (+ | -) followed by the
       exponential number, EXP. (EXP0 is the first digit and EXP1 the rest).

       When we encounter an expected e or E, we can't tell if it's part of
       the float or the rest of the string. We save the current position 
       with SAVE_E. If we later find out it was not part of the float, we
       restore the position (end of the float) with LOAD_E.
    */
    while(1) {
	if (is_not_small(CAR(list_val(list)))) 
	    goto back_to_e;
	if (CAR(list_val(list)) == make_small('-')) {
	    switch (part) {
	    case SIGN:		/* expect integer part next */
		part = INT;		
		break;
	    case EXP_SIGN:	/* expect first digit in exp */
		part = EXP0;
		break;
	    case EXP0:		/* example: "2.3e--" */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (CAR(list_val(list)) == make_small('+')) {
	    switch (part) {
	    case SIGN:		/* expect integer part next */
		part = INT;
		goto skip;
	    case EXP_SIGN:	/* expect first digit in exp */
		part = EXP0;
		break;
	    case EXP0:		/* example: "2.3e++" */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_DOT(CAR(list_val(list)))) { /* . or , */
	    switch (part) {
	    case INT:		/* expect fractional part next */
		part = FRAC;
		break;
	    case EXP_SIGN:	/* example: "2.3e." */
		LOAD_E(i, i_mem, list, list_mem);
	    case EXP0:		/* example: "2.3e+." */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_E(CAR(list_val(list)))) {	/* e or E */
	    switch (part) {
	    case FRAC:		/* expect a + or - (or a digit) next */
		/* 
		   remember the position of e in case we find out later
		   that it was not part of the float, e.g. "2.3eh?" 
		*/
		SAVE_E(i, i_mem, list, list_mem);
		part = EXP_SIGN;
		break;
	    case EXP0:		/* example: "2.3e+e" */
	    case EXP_SIGN:	/* example: "2.3ee" */
		LOAD_E(i, i_mem, list, list_mem);
	    case INT:		/* would like this to be ok, example "2e2",
				   but it's not compatible with list_to_float */
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_DIGIT(CAR(list_val(list)))) { /* digit */
	    switch (part) {
	    case SIGN:		/* got initial digit in integer part */
		part = INT;	/* expect more digits to follow */
		break;
	    case EXP_SIGN:	/* expect exponential part */
	    case EXP0:		/* expect rest of exponential */
		part = EXP1;
		break;
	    }
	} else			/* character not part of float - done */
	    goto back_to_e;
	
	if (part == END) {
	    if (i < 3) {	/* we require a fractional part */
		error_res = am_no_float;
		goto error;
	    }
	    break;
	}

	buf[i++] = unsigned_val(CAR(list_val(list)));

	if (i == bufsz - 1)
	    buf = (byte *) erts_realloc(ERTS_ALC_T_TMP,
					(void *) buf,
					bufsz += STRING_TO_FLOAT_BUF_INC_SZ);
    skip:
	list = CDR(list_val(list)); /* next element */

	if (is_nil(list))
	    goto back_to_e;

	if (is_not_list(list)) {
	back_to_e:
	    if (part == EXP_SIGN || part == EXP0) {
		LOAD_E(i, i_mem, list, list_mem);
	    }
	    break;
	}
    }
      
    if (i == 0) {		/* no float first in list */
	error_res = am_no_float;
	goto error;
    }

    buf[i] = '\0';		/* null terminal */
    ASSERT(bufsz >= i + 1);
    if (sys_chars_to_double((char*) buf, &f.fd) != 0) {
	error_res = am_no_float;
	goto error;
    }
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT + 3);
    tup = TUPLE2(hp+FLOAT_SIZE_OBJECT, make_float(hp), list);
    PUT_DOUBLE(f, hp);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(tup);
}

static BIF_RETTYPE do_charbuf_to_float(Process *BIF_P,char *buf) {
  FloatDef f;
  Eterm res;
  Eterm* hp;

  if (sys_chars_to_double(buf, &f.fd) != 0)
    BIF_ERROR(BIF_P, BADARG);

  hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
  res = make_float(hp);
  PUT_DOUBLE(f, hp);
  BIF_RET(res);

}

BIF_RETTYPE list_to_float_1(BIF_ALIST_1)
{
    Sint i;
    Eterm res;
    char *buf = NULL;

    i = erts_list_length(BIF_ARG_1);
    if (i < 0)
      BIF_ERROR(BIF_P, BADARG);
    
    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
    
    if (intlist_to_buf(BIF_ARG_1, buf, i) < 0)
      goto list_to_float_1_error;
    buf[i] = '\0';		/* null terminal */
    
    if ((res = do_charbuf_to_float(BIF_P,buf)) == THE_NON_VALUE)
      goto list_to_float_1_error;
    
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
    
 list_to_float_1_error:
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);

}

BIF_RETTYPE binary_to_float_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm binary = BIF_ARG_1;
    Sint size;
    byte* bytes, *buf;
    Eterm* real_bin;
    Uint offs = 0;
    Uint bit_offs = 0;

    if (is_not_binary(binary) || (size = binary_size(binary)) == 0)
      BIF_ERROR(BIF_P, BADARG);

    /* 
     *  Unfortunately we have to copy the binary because we have to insert
     *  the '\0' at the end of the binary for strtod to work 
     *  (there is no nstrtod :( )
     */

    buf = erts_alloc(ERTS_ALC_T_TMP, size + 1);

    real_bin = binary_val(binary);
    if (*real_bin == HEADER_SUB_BIN) {
	ErlSubBin* sb = (ErlSubBin *) real_bin;
	if (sb->bitsize) {
	    goto binary_to_float_1_error;
	}
	offs = sb->offs;
	bit_offs = sb->bitoffs;
	real_bin = binary_val(sb->orig);
    } 
    if (*real_bin == HEADER_PROC_BIN) {
	bytes = ((ProcBin *) real_bin)->bytes + offs;
    } else {
	bytes = (byte *)(&(((ErlHeapBin *) real_bin)->data)) + offs;
    }
    if (bit_offs)
      erts_copy_bits(bytes, bit_offs, 1, buf, 0, 1, size*8);
    else
      memcpy(buf, bytes, size);
    
    buf[size] = '\0';
    
    if ((res = do_charbuf_to_float(BIF_P,(char*)buf)) == THE_NON_VALUE)
	goto binary_to_float_1_error;

    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);

 binary_to_float_1_error:
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* convert a tuple to a list */

BIF_RETTYPE tuple_to_list_1(BIF_ALIST_1)
{
    Uint n;
    Eterm *tupleptr;
    Eterm list = NIL;
    Eterm* hp;

    if (is_not_tuple(BIF_ARG_1))  {
	BIF_ERROR(BIF_P, BADARG);
    }

    tupleptr = tuple_val(BIF_ARG_1);
    n = arityval(*tupleptr);
    hp = HAlloc(BIF_P, 2 * n);
    tupleptr++;

    while(n--) {
	list = CONS(hp, tupleptr[n], list);
	hp += 2;
    }
    BIF_RET(list);
}

/**********************************************************************/

/* convert a list to a tuple */

BIF_RETTYPE list_to_tuple_1(BIF_ALIST_1)
{
    Eterm list = BIF_ARG_1;
    Eterm* cons;
    Eterm res;
    Eterm* hp;
    Sint len;

    if ((len = erts_list_length(list)) < 0 || len > ERTS_MAX_TUPLE_SIZE) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, len+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(len);
    while(is_list(list)) {
	cons = list_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    BIF_RET(res);
}

/**********************************************************************/

/* return the pid of our own process, in most cases this has been replaced by
   a machine instruction */

BIF_RETTYPE self_0(BIF_ALIST_0)
{
     BIF_RET(BIF_P->common.id);
}

/**********************************************************************/

/* return the time of day */

BIF_RETTYPE time_0(BIF_ALIST_0)
{
     int hour, minute, second;
     Eterm* hp;

     get_time(&hour, &minute, &second);
     hp = HAlloc(BIF_P, 4);	/* {hour, minute, second}  + arity */
     BIF_RET(TUPLE3(hp, make_small(hour), make_small(minute),
		    make_small(second)));
}
/**********************************************************************/

/* return the date */

BIF_RETTYPE date_0(BIF_ALIST_0)
{
     int year, month, day;
     Eterm* hp;
     
     get_date(&year, &month, &day);
     hp = HAlloc(BIF_P, 4);	/* {year, month, day}  + arity */
     BIF_RET(TUPLE3(hp, make_small(year), make_small(month), make_small(day)));
}

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_0(BIF_ALIST_0)
{
     int year, month, day;
     int hour, minute, second;
     Eterm res1, res2;
     Eterm* hp;

     /* read the clock */
     get_universaltime(&year, &month, &day, &hour, &minute, &second);

     hp = HAlloc(BIF_P, 4+4+3);

     /* and return the tuple */
     res1 = TUPLE3(hp,make_small(year),make_small(month),make_small(day));
     hp += 4;
     res2 = TUPLE3(hp,make_small(hour),make_small(minute),make_small(second));
     hp += 4;
     BIF_RET(TUPLE2(hp, res1, res2));
 }

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE localtime_0(BIF_ALIST_0)
{
     int year, month, day;
     int hour, minute, second;
     Eterm res1, res2;
     Eterm* hp;

     /* read the clock */
     get_localtime(&year, &month, &day, &hour, &minute, &second);

     hp = HAlloc(BIF_P, 4+4+3);

     /* and return the tuple */
     res1 = TUPLE3(hp,make_small(year),make_small(month),make_small(day));
     hp += 4;
     res2 = TUPLE3(hp,make_small(hour),make_small(minute),make_small(second));
     hp += 4;
     BIF_RET(TUPLE2(hp, res1, res2));
}
/**********************************************************************/

/* type check and extract components from a tuple on form: {{Y,M,D},{H,M,S}} */
static int 
time_to_parts(Eterm date, Sint* year, Sint* month, Sint* day,
	      Sint* hour, Sint* minute, Sint* second)
{
    Eterm* t1;
    Eterm* t2;

    if (is_not_tuple(date)) {
	return 0;
    }
    t1 = tuple_val(date);
    if (arityval(t1[0]) !=2 || 
	is_not_tuple(t1[1]) || is_not_tuple(t1[2]))
	return 0;
    t2 = tuple_val(t1[1]);
    t1 = tuple_val(t1[2]);
    if (arityval(t2[0]) != 3 || 
	is_not_small(t2[1]) || is_not_small(t2[2]) || is_not_small(t2[3]))
	return 0;
    *year  = signed_val(t2[1]);
    *month = signed_val(t2[2]);
    *day   = signed_val(t2[3]);
    if (arityval(t1[0]) != 3 || 
	is_not_small(t1[1]) || is_not_small(t1[2]) || is_not_small(t1[3]))
	return 0;
    *hour   = signed_val(t1[1]);
    *minute = signed_val(t1[2]);
    *second = signed_val(t1[3]);
    return 1;
}


/* return the universal time */

BIF_RETTYPE 
localtime_to_universaltime_2(BIF_ALIST_2)
{
    Process *p = BIF_P;
    Eterm localtime = BIF_ARG_1;
    Eterm dst = BIF_ARG_2;
    Sint year, month, day;
    Sint hour, minute, second;
    int isdst;
    Eterm res1, res2;
    Eterm* hp;
    
    if (dst == am_true) isdst = 1;
    else if (dst == am_false) isdst = 0;
    else if (dst == am_undefined) isdst = -1;
    else goto error;
    
    if (!time_to_parts(localtime, &year, &month, &day, 
		       &hour, &minute, &second)) goto error;
    if (!local_to_univ(&year, &month, &day, 
		       &hour, &minute, &second, isdst)) goto error;
    
    hp = HAlloc(p, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
 error:
    BIF_ERROR(p, BADARG);
 }
	 

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_to_localtime_1(BIF_ALIST_1)
{
    Sint year, month, day;
    Sint hour, minute, second;
    Eterm res1, res2;
    Eterm* hp;

    if (!time_to_parts(BIF_ARG_1, &year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    if (!univ_to_local(&year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    
    hp = HAlloc(BIF_P, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
}

/* convert calendar:universaltime_to_seconds/1 */

BIF_RETTYPE universaltime_to_posixtime_1(BIF_ALIST_1)
{
    Sint year, month, day;
    Sint hour, minute, second;

    Sint64 seconds = 0;
    Eterm *hp;
    Uint hsz = 0;

    if (!time_to_parts(BIF_ARG_1, &year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);

    if (!univ_to_seconds(year, month, day, hour, minute, second, &seconds)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    erts_bld_sint64(NULL, &hsz, seconds);
    hp = HAlloc(BIF_P, hsz);
    BIF_RET(erts_bld_sint64(&hp, NULL, seconds));
}

/* convert calendar:seconds_to_universaltime/1 */

BIF_RETTYPE posixtime_to_universaltime_1(BIF_ALIST_1)
{
    Sint year, month, day;
    Sint hour, minute, second;
    Eterm res1, res2;
    Eterm* hp;

    Sint64 time = 0;

    if (!term_to_Sint64(BIF_ARG_1, &time)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!seconds_to_univ(time, &year, &month, &day,
		&hour, &minute, &second)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
}


/**********************************************************************/


 /* return a timestamp */
BIF_RETTYPE now_0(BIF_ALIST_0)
{
    Uint megasec, sec, microsec;
    Eterm* hp;

    get_now(&megasec, &sec, &microsec);
    hp = HAlloc(BIF_P, 4);
    BIF_RET(TUPLE3(hp, make_small(megasec), make_small(sec),
		   make_small(microsec)));
}

/**********************************************************************/

BIF_RETTYPE garbage_collect_0(BIF_ALIST_0)
{
    FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
    erts_garbage_collect(BIF_P, 0, NULL, 0);
    BIF_RET(am_true);
}

/**********************************************************************/
/*
 * The erlang:processes/0 BIF.
 */

BIF_RETTYPE processes_0(BIF_ALIST_0)
{
    return erts_ptab_list(BIF_P, &erts_proc);
}

/**********************************************************************/
/*
 * The erlang:ports/0 BIF.
 */

BIF_RETTYPE ports_0(BIF_ALIST_0)
{
    return erts_ptab_list(BIF_P, &erts_port);
}

/**********************************************************************/

BIF_RETTYPE throw_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_THROWN);
}

/**********************************************************************/


/* 
 * Non-standard, undocumented, dirty BIF, meant for debugging.
 *
 */
BIF_RETTYPE display_1(BIF_ALIST_1)
{
    erts_printf("%.*T\n", INT_MAX, BIF_ARG_1);
    BIF_RET(am_true);
}

/*
 * erts_debug:display/1 is for debugging erlang:display/1
 */
BIF_RETTYPE erts_debug_display_1(BIF_ALIST_1)
{
    int pres;
    Eterm res;
    Eterm *hp;
    erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(64);       
    pres = erts_dsprintf(dsbufp, "%.*T\n", INT_MAX, BIF_ARG_1);
    if (pres < 0)
	erts_exit(ERTS_ERROR_EXIT, "Failed to convert term to string: %d (%s)\n",
		 -pres, erl_errno_id(-pres));
    hp = HAlloc(BIF_P, 2*dsbufp->str_len); /* we need length * 2 heap words */
    res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
    erts_printf("%s", dsbufp->str);
    erts_destroy_tmp_dsbuf(dsbufp);
    BIF_RET(res);
}


BIF_RETTYPE display_string_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm string = BIF_ARG_1;
    Sint len = is_string(string);
    char *str;

    if (len <= 0) {
	BIF_ERROR(p, BADARG);
    }
    str = (char *) erts_alloc(ERTS_ALC_T_TMP, sizeof(char)*(len + 1));
    if (intlist_to_buf(string, str, len) != len)
	erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error\n", __FILE__, __LINE__);
    str[len] = '\0';
    erts_fprintf(stderr, "%s", str);
    erts_free(ERTS_ALC_T_TMP, (void *) str);
    BIF_RET(am_true);
}

BIF_RETTYPE display_nl_0(BIF_ALIST_0)
{
    erts_fprintf(stderr, "\n");
    BIF_RET(am_true);
}

/**********************************************************************/


#define HALT_MSG_SIZE	200
static char halt_msg[HALT_MSG_SIZE+1];

/* stop the system with exit code and flags */
BIF_RETTYPE halt_2(BIF_ALIST_2)
{
    Uint code;
    Eterm optlist = BIF_ARG_2;
    int flush = 1;

    for (optlist = BIF_ARG_2;
	 is_list(optlist);
	 optlist = CDR(list_val(optlist))) {
	Eterm *tp, opt = CAR(list_val(optlist));
	if (is_not_tuple(opt))
	    goto error;
	tp = tuple_val(opt);
	if (tp[0] != make_arityval(2))
	    goto error;
	if (tp[1] == am_flush) {
	    if (tp[2] == am_true)
		flush = 1;
	    else if (tp[2] == am_false)
		flush = 0;
	    else
		goto error;
	}
	else
	    goto error;
    }
    if (is_not_nil(optlist))
	goto error;

    if (term_to_Uint_mask(BIF_ARG_1, &code)) {
	int pos_int_code = (int) (code & INT_MAX);
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	if (flush) {
	    erts_halt(pos_int_code);
	    ERTS_BIF_YIELD2(bif_export[BIF_halt_2], BIF_P, am_undefined, am_undefined);
	}
	else {
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
            erts_exit(pos_int_code, "");
	}
    }
    else if (ERTS_IS_ATOM_STR("abort", BIF_ARG_1)) {
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_exit(ERTS_ABORT_EXIT, "");
    }
    else if (is_string(BIF_ARG_1) || BIF_ARG_1 == NIL) {
	Sint i;

        if ((i = intlist_to_buf(BIF_ARG_1, halt_msg, HALT_MSG_SIZE)) == -1) {
            goto error;
        }
        if (i == -2) /* truncated string */
            i = HALT_MSG_SIZE;
        ASSERT(i >= 0 && i <= HALT_MSG_SIZE);
	halt_msg[i] = '\0';
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_exit(ERTS_DUMP_EXIT, "%s\n", halt_msg);
    }
    else
	goto error;
    return NIL;  /* Pedantic (lint does not know about erts_exit) */
 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE function_exported_3(BIF_ALIST_3)
{
    int arity;
    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) || 
	is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    arity = signed_val(BIF_ARG_3);
    if (erts_find_function(BIF_ARG_1, BIF_ARG_2, arity,
			   erts_active_code_ix()) != NULL ||
	erts_is_builtin(BIF_ARG_1, BIF_ARG_2, arity)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

/**********************************************************************/    

BIF_RETTYPE is_builtin_3(BIF_ALIST_3)
{
    Process* p = BIF_P;
    Eterm Mod = BIF_ARG_1;
    Eterm Name = BIF_ARG_2;
    Eterm Arity = BIF_ARG_3;

    if (is_not_atom(Mod) || is_not_atom(Name) || is_not_small(Arity)) {
	BIF_ERROR(p, BADARG);
    }
    BIF_RET(erts_is_builtin(Mod, Name, signed_val(Arity)) ?
	    am_true : am_false);
}

/**********************************************************************/    

/* NOTE: Cannot be used in all *_to_list() bifs. erts_dsprintf() prints
 *       some terms on other formats than what is desired as results
 *       from *_to_list() bifs.
 */

static Eterm
term2list_dsprintf(Process *p, Eterm term)
{
    int pres;
    Eterm res;
    Eterm *hp;
    erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(64);       
    pres = erts_dsprintf(dsbufp, "%T", term);
    if (pres < 0)
	erts_exit(ERTS_ERROR_EXIT, "Failed to convert term to list: %d (%s)\n",
		 -pres, erl_errno_id(-pres));
    hp = HAlloc(p, 2*dsbufp->str_len); /* we need length * 2 heap words */
    res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
    erts_destroy_tmp_dsbuf(dsbufp);
    return res;
}

BIF_RETTYPE ref_to_list_1(BIF_ALIST_1)
{
    if (is_not_ref(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

BIF_RETTYPE make_fun_3(BIF_ALIST_3)
{
    Eterm* hp;
    Sint arity;

    if (is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2) || is_not_small(BIF_ARG_3)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    arity = signed_val(BIF_ARG_3);
    if (arity < 0) {
	goto error;
    }
    hp = HAlloc(BIF_P, 2);
    hp[0] = HEADER_EXPORT;
    hp[1] = (Eterm) erts_export_get_or_make_stub(BIF_ARG_1, BIF_ARG_2, (Uint) arity);
    BIF_RET(make_export(hp));
}

BIF_RETTYPE fun_to_list_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;

    if (is_not_any_fun(fun))
	BIF_ERROR(p, BADARG);
    BIF_RET(term2list_dsprintf(p, fun));
}

/**********************************************************************/    

/* convert a pid to an erlang list (for the linked cons cells) of the form
   <node.number.serial> to a PID
 */

BIF_RETTYPE pid_to_list_1(BIF_ALIST_1)
{
    if (is_not_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

BIF_RETTYPE port_to_list_1(BIF_ALIST_1)
{
    if (is_not_port(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

/**********************************************************************/

/* convert a list of ascii characeters of the form
   <node.number.serial> to a PID
*/

BIF_RETTYPE list_to_pid_1(BIF_ALIST_1)
{
    Uint a = 0, b = 0, c = 0;
    char* cp;
    Sint i;
    DistEntry *dep = NULL;
    char *buf = (char *) erts_alloc(ERTS_ALC_T_TMP, 65);
    /*
     * Max 'Uint64' has 20 decimal digits. If X, Y, Z in <X.Y.Z>
     * are 'Uint64's. Max chars are 1 + 20 + 1 + 20 + 1 + 20 + 1 = 64,
     * i.e, if the input list is longer than 64 it does not represent
     * a pid.
     */

    /* walk down the list and create a C string */
    if ((i = intlist_to_buf(BIF_ARG_1, buf, 64)) < 0)
	goto bad;

    buf[i] = '\0';		/* null terminal */

    cp = buf;
    if (*cp++ != '<') goto bad;
    
    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { a = 10*a + (*cp - '0'); cp++; }

    if (*cp++ != '.') goto bad;

    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { b = 10*b + (*cp - '0'); cp++; }

    if (*cp++ != '.') goto bad;

    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { c = 10*c + (*cp - '0'); cp++; }

    if (*cp++ != '>') goto bad;
    if (*cp != '\0') goto bad;

    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    buf = NULL;

    /* <a.b.c> a = node, b = process number, c = serial */

    dep = erts_channel_no_to_dist_entry(a);

    if (!dep)
	goto bad;


    if (c > ERTS_MAX_PID_SERIAL || b > ERTS_MAX_PID_NUMBER)
	goto bad;

    if(dep == erts_this_dist_entry) {
	erts_deref_dist_entry(dep);
	BIF_RET(make_internal_pid(make_pid_data(c, b)));
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;

      if (is_nil(dep->cid))
	  goto bad;
      
      enp = erts_find_or_insert_node(dep->sysname, dep->creation);
      ASSERT(enp != erts_this_node);

      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_THING_HEAD_SIZE + 1);
      etp->header = make_external_pid_header(1);
      etp->next = MSO(BIF_P).first;
      etp->node = enp;
      etp->data.ui[0] = make_pid_data(c, b);

      MSO(BIF_P).first = (struct erl_off_heap_header*) etp;
      erts_deref_dist_entry(dep);
      BIF_RET(make_external_pid(etp));
    }

 bad:
    if (dep)
	erts_deref_dist_entry(dep);
    if (buf)
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE group_leader_0(BIF_ALIST_0)
{
    BIF_RET(BIF_P->group_leader);
}

/**********************************************************************/
/* arg1 == leader, arg2 == new member */

BIF_RETTYPE group_leader_2(BIF_ALIST_2)
{
    Process* new_member;

    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_external_pid(BIF_ARG_2)) {
	DistEntry *dep;
	int code;
	ErtsDSigData dsd;
	dep = external_pid_dist_entry(BIF_ARG_2);
	if(dep == erts_this_dist_entry)
	    BIF_ERROR(BIF_P, BADARG);

	code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_DSP_NO_LOCK, 0);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	    BIF_RET(am_true);
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    BIF_TRAP2(dgroup_leader_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	case ERTS_DSIG_PREP_CONNECTED:
	    code = erts_dsig_send_group_leader(&dsd, BIF_ARG_1, BIF_ARG_2);
	    if (code == ERTS_DSIG_SEND_YIELD)
		ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
	    BIF_RET(am_true);
	default:
	    ASSERT(! "Invalid dsig prepare result");
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}
    }
    else if (is_internal_pid(BIF_ARG_2)) {
	int await_x;
	ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS;
	new_member = erts_pid2proc_nropt(BIF_P, ERTS_PROC_LOCK_MAIN,
					 BIF_ARG_2, locks);
	if (!new_member)
	    BIF_ERROR(BIF_P, BADARG);

	if (new_member == ERTS_PROC_LOCK_BUSY)
	    ERTS_BIF_YIELD2(bif_export[BIF_group_leader_2], BIF_P,
			    BIF_ARG_1, BIF_ARG_2);

	await_x = (new_member != BIF_P
		   && ERTS_PROC_PENDING_EXIT(new_member));
	if (!await_x) {
	    if (is_immed(BIF_ARG_1))
		new_member->group_leader = BIF_ARG_1;
	    else {
		locks &= ~ERTS_PROC_LOCK_STATUS;
		erts_smp_proc_unlock(new_member, ERTS_PROC_LOCK_STATUS);
		if (new_member == BIF_P
		    || !(erts_smp_atomic32_read_nob(&new_member->state)
			 & (ERTS_PSFLG_DIRTY_RUNNING|ERTS_PSFLG_DIRTY_RUNNING_SYS))) {
		    new_member->group_leader = STORE_NC_IN_PROC(new_member,
								BIF_ARG_1);
		}
		else {
		    ErlHeapFragment *bp;
		    Eterm *hp;
		    /*
		     * Other process executing on a dirty scheduler,
		     * so we are not allowed to write to its heap.
		     * Store in heap fragment.
		     */

		    bp = new_message_buffer(NC_HEAP_SIZE(BIF_ARG_1));
		    hp = bp->mem;
		    new_member->group_leader = STORE_NC(&hp,
							&new_member->off_heap,
							BIF_ARG_1);
		    bp->next = new_member->mbuf;
		    new_member->mbuf = bp;
		    new_member->mbuf_sz += bp->used_size;
		}
	    }
	}

	if (new_member == BIF_P)
	    locks &= ~ERTS_PROC_LOCK_MAIN;
	if (locks)
	    erts_smp_proc_unlock(new_member, locks);

	if (await_x) {
	    /* Wait for new_member to terminate; then badarg */
	    Eterm args[2] = {BIF_ARG_1, BIF_ARG_2};
	    ERTS_BIF_AWAIT_X_APPLY_TRAP(BIF_P,
					BIF_ARG_2,
					am_erlang,
					am_group_leader,
					args,
					2);
	}

	BIF_RET(am_true);
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
}
    
BIF_RETTYPE system_flag_2(BIF_ALIST_2)    
{
    Sint n;

    if (BIF_ARG_1 == am_multi_scheduling) {
	if (BIF_ARG_2 == am_block || BIF_ARG_2 == am_unblock
	    || BIF_ARG_2 == am_block_normal || BIF_ARG_2 == am_unblock_normal) {
#ifndef ERTS_SMP
	    BIF_RET(am_disabled);
#else
	    int block = (BIF_ARG_2 == am_block
			 || BIF_ARG_2 == am_block_normal);
	    int normal = (BIF_ARG_2 == am_block_normal
			  || BIF_ARG_2 == am_unblock_normal);
	    if (erts_no_schedulers == 1)
		BIF_RET(am_disabled);
	    else {
		switch (erts_block_multi_scheduling(BIF_P,
						    ERTS_PROC_LOCK_MAIN,
						    block,
						    normal,
						    0)) {
		case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
		    BIF_RET(am_blocked);
		case ERTS_SCHDLR_SSPND_DONE_NMSCHED_BLOCKED:
		    BIF_RET(am_blocked_normal);
		case ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED:
		    ERTS_BIF_YIELD_RETURN_X(BIF_P, am_blocked,
					    am_multi_scheduling);
		case ERTS_SCHDLR_SSPND_YIELD_DONE_NMSCHED_BLOCKED:
		    ERTS_BIF_YIELD_RETURN_X(BIF_P, am_blocked_normal,
					    am_multi_scheduling);
		case ERTS_SCHDLR_SSPND_DONE:
		    BIF_RET(am_enabled);
		case ERTS_SCHDLR_SSPND_YIELD_RESTART:
		    ERTS_VBUMP_ALL_REDS(BIF_P);
		    BIF_TRAP2(bif_export[BIF_system_flag_2],
			      BIF_P, BIF_ARG_1, BIF_ARG_2);
		case ERTS_SCHDLR_SSPND_YIELD_DONE:
		    ERTS_BIF_YIELD_RETURN_X(BIF_P, am_enabled,
					    am_multi_scheduling);
		case ERTS_SCHDLR_SSPND_EINVAL:
		    goto error;
		default:
		    ASSERT(0);
		    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
		    break;
		}
	    }
#endif
	}
    } else if (BIF_ARG_1 == am_schedulers_online) {
#ifndef ERTS_SMP
	if (BIF_ARG_2 != make_small(1))
	    goto error;
	else
	    BIF_RET(make_small(1));
#else
	Sint old_no;
	if (!is_small(BIF_ARG_2))
	    goto error;
	switch (erts_set_schedulers_online(BIF_P,
					   ERTS_PROC_LOCK_MAIN,
					   signed_val(BIF_ARG_2),
					   &old_no, 0)) {
	case ERTS_SCHDLR_SSPND_DONE:
	    BIF_RET(make_small(old_no));
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    ERTS_VBUMP_ALL_REDS(BIF_P);
	    BIF_TRAP2(bif_export[BIF_system_flag_2],
		      BIF_P, BIF_ARG_1, BIF_ARG_2);
	case ERTS_SCHDLR_SSPND_YIELD_DONE:
	    ERTS_BIF_YIELD_RETURN_X(BIF_P, make_small(old_no),
				    am_schedulers_online);
	case ERTS_SCHDLR_SSPND_EINVAL:
	    goto error;
	default:
	    ASSERT(0);
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	    break;
	}
#endif
    } else if (BIF_ARG_1 == am_fullsweep_after) {
	Uint16 nval;
	Uint oval;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	nval = (n > (Sint) ((Uint16) -1)) ? ((Uint16) -1) : ((Uint16) n);
	oval = (Uint) erts_smp_atomic32_xchg_nob(&erts_max_gen_gcs,
						 (erts_aint32_t) nval);
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_heap_size) {
	int oval = H_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	H_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_bin_vheap_size) {
	int oval = BIN_VH_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	BIN_VH_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_max_heap_size) {

        Eterm *hp, old_value;
        Uint sz = 0, max_heap_size, max_heap_flags;

        if (!erts_max_heap_size(BIF_ARG_2, &max_heap_size, &max_heap_flags))
            goto error;

        if (max_heap_size < H_MIN_SIZE && max_heap_size != 0)
            goto error;

        erts_max_heap_size_map(H_MAX_SIZE, H_MAX_FLAGS, NULL, &sz);
        hp = HAlloc(BIF_P, sz);
        old_value = erts_max_heap_size_map(H_MAX_SIZE, H_MAX_FLAGS, &hp, NULL);

        erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
        erts_smp_thr_progress_block();

        H_MAX_SIZE = max_heap_size;
        H_MAX_FLAGS = max_heap_flags;

        erts_smp_thr_progress_unblock();
        erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

        BIF_RET(old_value);
    } else if (BIF_ARG_1 == am_display_items) {
	int oval = display_items;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	display_items = n < 32 ? 32 : n;
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_debug_flags) {
	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_backtrace_depth) {
	int oval = erts_backtrace_depth;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	if (n > MAX_BACKTRACE_SIZE) n = MAX_BACKTRACE_SIZE;
	erts_backtrace_depth = n;
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_trace_control_word) {
	BIF_RET(db_set_trace_control_word(BIF_P, BIF_ARG_2));
    } else if (BIF_ARG_1 == am_sequential_tracer) {
        ErtsTracer new_seq_tracer, old_seq_tracer;
        Eterm ret;

        if (BIF_ARG_2 == am_false)
            new_seq_tracer = erts_tracer_nil;
        else
            new_seq_tracer = erts_term_to_tracer(THE_NON_VALUE, BIF_ARG_2);

        if (new_seq_tracer == THE_NON_VALUE)
            goto error;

        old_seq_tracer = erts_set_system_seq_tracer(BIF_P,
                                                    ERTS_PROC_LOCK_MAIN,
                                                    new_seq_tracer);

        ERTS_TRACER_CLEAR(&new_seq_tracer);

        if (old_seq_tracer == THE_NON_VALUE)
            goto error;

        if (ERTS_TRACER_IS_NIL(old_seq_tracer))
            BIF_RET(am_false);

        ret = erts_tracer_to_term(BIF_P, old_seq_tracer);

        ERTS_TRACER_CLEAR(&old_seq_tracer);

        BIF_RET(ret);
    } else if (BIF_ARG_1 == make_small(1)) {
	int i, max;
	ErtsMessage* mp;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	max = erts_ptab_max(&erts_proc);
	for (i = 0; i < max; i++) {
	    Process *p = erts_pix2proc(i);
	    if (p) {
#ifdef USE_VM_PROBES
		p->seq_trace_token = (p->dt_utag != NIL) ? am_have_dt_utag : NIL;
#else
		p->seq_trace_token = NIL;
#endif
		p->seq_trace_clock = 0;
		p->seq_trace_lastcnt = 0;
		ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
		mp = p->msg.first;
		while(mp != NULL) {
#ifdef USE_VM_PROBES
		    ERL_MESSAGE_TOKEN(mp) = (ERL_MESSAGE_DT_UTAG(mp) != NIL) ? am_have_dt_utag : NIL;
#else
		    ERL_MESSAGE_TOKEN(mp) = NIL;
#endif
		    mp = mp->next;
		}
	    }
	}

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_scheduler_wall_time) {
	if (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false) {
	    erts_aint32_t new = BIF_ARG_2 == am_true ? 1 : 0;
	    erts_aint32_t old = erts_smp_atomic32_xchg_nob(&sched_wall_time,
							   new);
	    Eterm ref = erts_sched_wall_time_request(BIF_P, 1, new);
	    ASSERT(is_value(ref));
	    BIF_TRAP2(await_sched_wall_time_mod_trap,
		      BIF_P,
		      ref,
		      old ? am_true : am_false);
	}
#if defined(ERTS_SMP) && defined(ERTS_DIRTY_SCHEDULERS)
    } else if (BIF_ARG_1 == am_dirty_cpu_schedulers_online) {
	Sint old_no;
	if (!is_small(BIF_ARG_2))
	    goto error;
	switch (erts_set_schedulers_online(BIF_P,
					   ERTS_PROC_LOCK_MAIN,
					   signed_val(BIF_ARG_2),
					   &old_no,
					   1)) {
	case ERTS_SCHDLR_SSPND_DONE:
	    BIF_RET(make_small(old_no));
	case ERTS_SCHDLR_SSPND_YIELD_RESTART:
	    ERTS_VBUMP_ALL_REDS(BIF_P);
	    BIF_TRAP2(bif_export[BIF_system_flag_2],
		      BIF_P, BIF_ARG_1, BIF_ARG_2);
	case ERTS_SCHDLR_SSPND_YIELD_DONE:
	    ERTS_BIF_YIELD_RETURN_X(BIF_P, make_small(old_no),
				    am_dirty_cpu_schedulers_online);
	case ERTS_SCHDLR_SSPND_EINVAL:
	    goto error;
	default:
	    ASSERT(0);
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	    break;
	}
#endif
    } else if (BIF_ARG_1 == am_time_offset
	       && ERTS_IS_ATOM_STR("finalize", BIF_ARG_2)) {
	ErtsTimeOffsetState res;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	res = erts_finalize_time_offset();
        erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	switch (res) {
	case ERTS_TIME_OFFSET_PRELIMINARY: {
	    DECL_AM(preliminary);
	    BIF_RET(AM_preliminary);
	}
	case ERTS_TIME_OFFSET_FINAL: {
	    DECL_AM(final);
	    BIF_RET(AM_final);
	}
	case ERTS_TIME_OFFSET_VOLATILE: {
	    DECL_AM(volatile);
	    BIF_RET(AM_volatile);
	}
	default:
	    ERTS_INTERNAL_ERROR("Unknown state");
	}
#ifdef ERTS_ENABLE_MSACC
    } else if (BIF_ARG_1 == am_microstate_accounting) {
      Eterm threads;
      if (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false) {
        erts_aint32_t new = BIF_ARG_2 == am_true ? ERTS_MSACC_ENABLE : ERTS_MSACC_DISABLE;
	erts_aint32_t old = erts_smp_atomic32_xchg_nob(&msacc, new);
	Eterm ref = erts_msacc_request(BIF_P, new, &threads);
        if (is_non_value(ref))
            BIF_RET(old ? am_true : am_false);
	BIF_TRAP3(await_msacc_mod_trap,
		  BIF_P,
		  ref,
		  old ? am_true : am_false,
		  threads);
      } else if (BIF_ARG_2 == am_reset) {
	Eterm ref = erts_msacc_request(BIF_P, ERTS_MSACC_RESET, &threads);
	erts_aint32_t old = erts_smp_atomic32_read_nob(&msacc);
	ASSERT(is_value(ref));
	BIF_TRAP3(await_msacc_mod_trap,
		  BIF_P,
		  ref,
		  old ? am_true : am_false,
		  threads);
      }
#endif
    } else if (ERTS_IS_ATOM_STR("scheduling_statistics", BIF_ARG_1)) {
	int what;
	if (ERTS_IS_ATOM_STR("disable", BIF_ARG_2))
	    what = ERTS_SCHED_STAT_MODIFY_DISABLE;
	else if (ERTS_IS_ATOM_STR("enable", BIF_ARG_2))
	    what = ERTS_SCHED_STAT_MODIFY_ENABLE;
	else if (ERTS_IS_ATOM_STR("clear", BIF_ARG_2))
	    what = ERTS_SCHED_STAT_MODIFY_CLEAR;
	else
	    goto error;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_sched_stat_modify(what);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_RET(am_true);
    } else if (ERTS_IS_ATOM_STR("internal_cpu_topology", BIF_ARG_1)) {
	Eterm res = erts_set_cpu_topology(BIF_P, BIF_ARG_2);
	if (is_value(res))
	    BIF_RET(res);
    } else if (ERTS_IS_ATOM_STR("cpu_topology", BIF_ARG_1)) {
	erts_send_warning_to_logger_str(
	    BIF_P->group_leader,
	    "A call to erlang:system_flag(cpu_topology, _) was made.\n"
	    "The cpu_topology argument is deprecated and scheduled\n"
	    "for removal in Erlang/OTP 18. For more information\n"
	    "see the erlang:system_flag/2 documentation.\n");
	BIF_TRAP1(set_cpu_topology_trap, BIF_P, BIF_ARG_2);
    } else if (ERTS_IS_ATOM_STR("scheduler_bind_type", BIF_ARG_1)) {
	erts_send_warning_to_logger_str(
	    BIF_P->group_leader,
	    "A call to erlang:system_flag(scheduler_bind_type, _) was\n"
	    "made. The scheduler_bind_type argument is deprecated and\n"
	    "scheduled for removal in Erlang/OTP 18. For more\n"
	    "information see the erlang:system_flag/2 documentation.\n");
	return erts_bind_schedulers(BIF_P, BIF_ARG_2);
    }
    error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE hash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Sint range;

    if (is_not_small(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((range = signed_val(BIF_ARG_2)) <= 0) {  /* [1..MAX_SMALL] */
	BIF_ERROR(BIF_P, BADARG);
    }
#if defined(ARCH_64)
    if (range > ((1L << 27) - 1))
	BIF_ERROR(BIF_P, BADARG);
#endif
    hash = make_broken_hash(BIF_ARG_1);
    BIF_RET(make_small(1 + (hash % range)));   /* [1..range] */
}

BIF_RETTYPE phash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;

    /* Check for special case 2^32 */
    if (term_equals_2pow32(BIF_ARG_2)) {
	range = 0;
    } else {
	Uint u;
	if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0 || !u) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	range = (Uint32) u;
    }
    hash = make_hash(BIF_ARG_1);
    if (range) {
	final_hash = 1 + (hash % range); /* [1..range] */
    } else if ((final_hash = hash + 1) == 0) {
	/*
	 * XXX In this case, there will still be a ArithAlloc() in erts_mixed_plus().
	 */
	BIF_RET(erts_mixed_plus(BIF_P,
				erts_make_integer(hash, BIF_P),
				make_small(1)));
    }

    BIF_RET(erts_make_integer(final_hash, BIF_P));
}

BIF_RETTYPE phash2_1(BIF_ALIST_1)
{
    Uint32 hash;

    hash = make_hash2(BIF_ARG_1);
    BIF_RET(make_small(hash & ((1L << 27) - 1)));
}

BIF_RETTYPE phash2_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;

    /* Check for special case 2^32 */
    if (term_equals_2pow32(BIF_ARG_2)) {
	range = 0;
    } else {
	Uint u;
	if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0 || !u) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	range = (Uint32) u;
    }
    hash = make_hash2(BIF_ARG_1);
    if (range) {
	final_hash = hash % range; /* [0..range-1] */
    } else {
	final_hash = hash;
    }
    /*
     * Return either a small or a big. Use the heap for bigs if there is room.
     */
#if defined(ARCH_64)
    BIF_RET(make_small(final_hash));
#else
    if (IS_USMALL(0, final_hash)) {
	BIF_RET(make_small(final_hash));
    } else {
	Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(final_hash, hp));
    }
#endif
}

BIF_RETTYPE bump_reductions_1(BIF_ALIST_1)
{
    Sint reds;
	
    if (is_not_small(BIF_ARG_1) || ((reds = signed_val(BIF_ARG_1)) < 0)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    if (reds > CONTEXT_REDS) {
        reds = CONTEXT_REDS;
    }
    BIF_RET2(am_true, reds);
}

BIF_RETTYPE erts_internal_cmp_term_2(BIF_ALIST_2) {
    Sint res = CMP_TERM(BIF_ARG_1,BIF_ARG_2);

    /* ensure -1, 0, 1 result */
    if (res < 0) {
	BIF_RET(make_small(-1));
    } else if (res > 0) {
	BIF_RET(make_small(1));
    }
    BIF_RET(make_small(0));
}
/*
 * Processes doing yield on return in a bif ends up in bif_return_trap().
 */
static BIF_RETTYPE bif_return_trap(BIF_ALIST_2)
{
    Eterm res = BIF_ARG_1;

    switch (BIF_ARG_2) {
#ifdef ERTS_SMP
    case am_multi_scheduling: {
	int msb = erts_is_multi_scheduling_blocked();
	if (msb > 0)
	    res = am_blocked;
	else if (msb < 0)
	    res = am_blocked_normal;
	else
	    ERTS_INTERNAL_ERROR("Unexpected multi scheduling block state");
	break;
    }
#endif
    default:
	break;
    }
    BIF_RET(res);
}

/*
 * NOTE: The erts_bif_prep_await_proc_exit_*() functions are
 * tightly coupled with the implementation of erlang:await_proc_exit/3.
 * The erts_bif_prep_await_proc_exit_*() functions can safely call
 * skip_current_msgq() since they know that erlang:await_proc_exit/3
 * unconditionally will do a monitor and then unconditionally will
 * wait for the corresponding 'DOWN' message in a receive, and no other
 * receive is done before this receive. This optimization removes an
 * unnecessary scan of the currently existing message queue (which
 * can be large). If the erlang:await_proc_exit/3 implementation
 * is changed so that the above isn't true, nasty bugs in later
 * receives, etc, may appear.
 */

static ERTS_INLINE int
skip_current_msgq(Process *c_p)
{
    int res;
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
    erts_proc_lc_chk_only_proc_main(c_p);
#endif

    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    if (ERTS_PROC_PENDING_EXIT(c_p)) {
	KILL_CATCHES(c_p);
	c_p->freason = EXC_EXIT;
	res = 0;
    }
    else {
        ERTS_SMP_MSGQ_MV_INQ2PRIVQ(c_p);
	c_p->msg.save = c_p->msg.last;
	res = 1;
    }
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    return res;
}

void
erts_bif_prep_await_proc_exit_data_trap(Process *c_p, Eterm pid, Eterm ret)
{
    if (skip_current_msgq(c_p)) {
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p, pid, am_data, ret);
    }
}

void
erts_bif_prep_await_proc_exit_reason_trap(Process *c_p, Eterm pid)
{
    if (skip_current_msgq(c_p)) {
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p,
			    pid, am_reason, am_undefined);
    }
}

void
erts_bif_prep_await_proc_exit_apply_trap(Process *c_p,
					 Eterm pid,
					 Eterm module,
					 Eterm function,
					 Eterm args[],
					 int nargs)
{
    ASSERT(is_atom(module) && is_atom(function));
    if (skip_current_msgq(c_p)) {
	Eterm term;
	Eterm *hp;
	int i;

	hp = HAlloc(c_p, 4+2*nargs);
	term = NIL;
	for (i = nargs-1; i >= 0; i--) {
	    term = CONS(hp, args[i], term);
	    hp += 2;
	}
	term = TUPLE3(hp, module, function, term);
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p, pid, am_apply, term);
    }
}

Export bif_return_trap_export;

void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(BIF_ALIST_0))
{
    int i;
    sys_memset((void *) ep, 0, sizeof(Export));
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	ep->addressv[i] = &ep->code[3];
    }
    ep->code[0] = m;
    ep->code[1] = f;
    ep->code[2] = a;
    ep->code[3] = (BeamInstr) em_apply_bif;
    ep->code[4] = (BeamInstr) bif;
}

void erts_init_bif(void)
{
    erts_smp_mtx_init(&ports_snapshot_mtx, "ports_snapshot");
    erts_smp_atomic_init_nob(&erts_dead_ports_ptr, (erts_aint_t) NULL);

    /*
     * bif_return_trap/2 is a hidden BIF that bifs that need to
     * yield the calling process traps to.
     */
    erts_init_trap_export(&bif_return_trap_export,
			  am_erlang, am_bif_return_trap, 2,
			  &bif_return_trap);

    erts_await_result = erts_export_put(am_erts_internal,
					am_await_result,
					1);

    erts_init_trap_export(&dsend_continue_trap_export,
			  am_erts_internal, am_dsend_continue_trap, 1,
			  dsend_continue_trap_1);

    flush_monitor_messages_trap = erts_export_put(am_erts_internal,
						  am_flush_monitor_messages,
						  3);

    erts_convert_time_unit_trap = erts_export_put(am_erlang,
						  am_convert_time_unit,
						  3);

    set_cpu_topology_trap = erts_export_put(am_erlang,
					    am_set_cpu_topology,
					    1);
    erts_format_cpu_topology_trap = erts_export_put(am_erlang,
						    am_format_cpu_topology,
						    1);
    await_proc_exit_trap = erts_export_put(am_erlang,am_await_proc_exit,3);
    await_port_send_result_trap
	= erts_export_put(am_erts_internal, am_await_port_send_result, 3);
    await_sched_wall_time_mod_trap
        = erts_export_put(am_erlang, am_await_sched_wall_time_modifications, 2);
    await_msacc_mod_trap
	= erts_export_put(am_erts_internal, am_await_microstate_accounting_modifications, 3);

    erts_smp_atomic32_init_nob(&sched_wall_time, 0);
    erts_smp_atomic32_init_nob(&msacc, ERTS_MSACC_IS_ENABLED());
}

#ifdef HARDDEBUG
/*
You'll need this line in bif.tab to be able to use this debug bif

bif erlang:send_to_logger/2

*/
BIF_RETTYPE send_to_logger_2(BIF_ALIST_2)
{
    byte *buf;
    ErlDrvSizeT len;
    if (!is_atom(BIF_ARG_1) || !(is_list(BIF_ARG_2) ||
				 is_nil(BIF_ARG_1))) {
	BIF_ERROR(BIF_P,BADARG);
    }
    if (erts_iolist_size(BIF_ARG_2, &len) != 0)
	BIF_ERROR(BIF_P,BADARG);
    else if (len == 0)
	buf = "";
    else {
#ifdef DEBUG
	ErlDrvSizeT len2;
#endif
	buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, len+1);
#ifdef DEBUG
	len2 =
#else
	(void)
#endif
	    erts_iolist_to_buf(BIF_ARG_2, buf, len);
	ASSERT(len2 == len);
	buf[len] = '\0';
	switch (BIF_ARG_1) {
	case am_info:
	    erts_send_info_to_logger(BIF_P->group_leader, buf, len);
	    break;
	case am_warning:
	    erts_send_warning_to_logger(BIF_P->group_leader, buf, len);
	    break;
	case am_error:
	    erts_send_error_to_logger(BIF_P->group_leader, buf, len);
	    break;
	default:
	{
	    BIF_ERROR(BIF_P,BADARG);
	}
	}
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    }
    BIF_RET(am_true);
}
#endif /* HARDDEBUG */

BIF_RETTYPE get_module_info_1(BIF_ALIST_1)
{
    Eterm ret = erts_module_info_0(BIF_P, BIF_ARG_1);

    if (is_non_value(ret)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}


BIF_RETTYPE get_module_info_2(BIF_ALIST_2)
{
    Eterm ret = erts_module_info_1(BIF_P, BIF_ARG_1, BIF_ARG_2);

    if (is_non_value(ret)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

BIF_RETTYPE dt_put_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm otag;
    if (BIF_ARG_1 == am_undefined) {
	otag = (DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P);
	DT_UTAG(BIF_P) = NIL;
	DT_UTAG_FLAGS(BIF_P) = 0;
	if (SEQ_TRACE_TOKEN(BIF_P) == am_have_dt_utag) {
	    SEQ_TRACE_TOKEN(BIF_P) = NIL;
	}
	BIF_RET(otag);
    }
    if (!is_binary(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    otag = (DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P);
    DT_UTAG(BIF_P) = BIF_ARG_1;
    DT_UTAG_FLAGS(BIF_P) |= DT_UTAG_PERMANENT;
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	SEQ_TRACE_TOKEN(BIF_P) = am_have_dt_utag;
    }
    BIF_RET(otag);
#else
    BIF_RET(am_undefined);
#endif
}

BIF_RETTYPE dt_get_tag_0(BIF_ALIST_0)
{
#ifdef USE_VM_PROBES
    BIF_RET((DT_UTAG(BIF_P) == NIL || !(DT_UTAG_FLAGS(BIF_P) & DT_UTAG_PERMANENT)) ? am_undefined : DT_UTAG(BIF_P));
#else
    BIF_RET(am_undefined);
#endif
}
BIF_RETTYPE dt_get_tag_data_0(BIF_ALIST_0)
{
#ifdef USE_VM_PROBES
    BIF_RET((DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P));
#else
    BIF_RET(am_undefined);
#endif
}
BIF_RETTYPE dt_prepend_vm_tag_data_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm b; 
    Eterm *hp;
    if (is_binary((DT_UTAG(BIF_P)))) {
	Uint sz = binary_size(DT_UTAG(BIF_P));
	int i;
	unsigned char *p,*q;
	byte *temp_alloc = NULL;
	b = new_binary(BIF_P,NULL,sz+1);
	q = binary_bytes(b);
	p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),&temp_alloc);
	for(i=0;i<sz;++i) {
	    q[i] = p[i];
	} 
	erts_free_aligned_binary_bytes(temp_alloc);
	q[sz] = '\0';
    } else {
	b = new_binary(BIF_P,(byte *)"\0",1);
    }
    hp = HAlloc(BIF_P,2);
    BIF_RET(CONS(hp,b,BIF_ARG_1));
#else
    BIF_RET(BIF_ARG_1);
#endif
}
BIF_RETTYPE dt_append_vm_tag_data_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm b; 
    Eterm *hp;
    if (is_binary((DT_UTAG(BIF_P)))) {
	Uint sz = binary_size(DT_UTAG(BIF_P));
	int i;
	unsigned char *p,*q;
	byte *temp_alloc = NULL;
	b = new_binary(BIF_P,NULL,sz+1);
	q = binary_bytes(b);
	p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),&temp_alloc);
	for(i=0;i<sz;++i) {
	    q[i] = p[i];
	} 
	erts_free_aligned_binary_bytes(temp_alloc);
	q[sz] = '\0';
    } else {
	b = new_binary(BIF_P,(byte *)"\0",1);
    }
    hp = HAlloc(BIF_P,2);
    BIF_RET(CONS(hp,BIF_ARG_1,b));
#else
    BIF_RET(BIF_ARG_1);
#endif
}
BIF_RETTYPE dt_spread_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm ret;
    Eterm *hp;
#endif
    if (BIF_ARG_1 != am_true && BIF_ARG_1 != am_false) {
	BIF_ERROR(BIF_P,BADARG);
    }
#ifdef USE_VM_PROBES
    hp = HAlloc(BIF_P,3);
    ret = TUPLE2(hp,make_small(DT_UTAG_FLAGS(BIF_P)),DT_UTAG(BIF_P));
    if (DT_UTAG(BIF_P) != NIL) {
	if (BIF_ARG_1 == am_true) {
	    DT_UTAG_FLAGS(BIF_P) |= DT_UTAG_SPREADING;
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) start spreading tag %T\r\n",
			 BIF_P->common.id,DT_UTAG(BIF_P));
#endif
	} else {
	    DT_UTAG_FLAGS(BIF_P) &= ~DT_UTAG_SPREADING;
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) stop spreading tag %T\r\n",
			 BIF_P->common.id,DT_UTAG(BIF_P));
#endif
	}
    }
    BIF_RET(ret);
#else
    BIF_RET(am_true);
#endif
}
BIF_RETTYPE dt_restore_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm *tpl;
    Uint x;
    if (is_not_tuple(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    tpl = tuple_val(BIF_ARG_1);
    if(arityval(*tpl) != 2 || is_not_small(tpl[1]) || (is_not_binary(tpl[2]) && tpl[2] != NIL)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    if (tpl[2] == NIL) {
	if (DT_UTAG(BIF_P) != NIL) {
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore Killing tag!\r\n",
			 BIF_P->common.id);
#endif
	}
	DT_UTAG(BIF_P) = NIL;
	if (SEQ_TRACE_TOKEN(BIF_P) == am_have_dt_utag) {
	    SEQ_TRACE_TOKEN(BIF_P) = NIL;
	}
	DT_UTAG_FLAGS(BIF_P) = 0;
    } else {
	x = unsigned_val(tpl[1]) & (DT_UTAG_SPREADING | DT_UTAG_PERMANENT);
#ifdef DTRACE_TAG_HARDDEBUG

	if (!(x & DT_UTAG_SPREADING) && (DT_UTAG_FLAGS(BIF_P) & 
					 DT_UTAG_SPREADING)) {
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore stop spreading "
			 "tag %T\r\n",
			 BIF_P->common.id, tpl[2]);
	} else if ((x & DT_UTAG_SPREADING) && 
		   !(DT_UTAG_FLAGS(BIF_P) & DT_UTAG_SPREADING)) {
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore start spreading "
			 "tag %T\r\n",BIF_P->common.id,tpl[2]);
	}
#endif
	DT_UTAG_FLAGS(BIF_P) = x;
	DT_UTAG(BIF_P) = tpl[2];
	if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	    SEQ_TRACE_TOKEN(BIF_P) = am_have_dt_utag;
	}
    }
#else    
    if (BIF_ARG_1 != am_true) {
	BIF_ERROR(BIF_P,BADARG);
    }
#endif
    BIF_RET(am_true);
}


