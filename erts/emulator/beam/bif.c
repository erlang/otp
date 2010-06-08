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
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_db_util.h"
#include "register.h"

static Export* flush_monitor_message_trap = NULL;
static Export* set_cpu_topology_trap = NULL;
static Export* await_proc_exit_trap = NULL;
Export* erts_format_cpu_topology_trap = NULL;

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

/*
 * The BIF's now follow, see the Erlang Manual for a description of what
 * each individual BIF does.
 */

BIF_RETTYPE spawn_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = 0;
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
    if (IS_TRACED(p) && (p->trace_flags & (F_TRACE_SOL|F_TRACE_SOL1)))
	rp_locks = ERTS_PROC_LOCKS_ALL;

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
	erts_add_link(&(p->nlinks), LINK_PID, rp->id);
	erts_add_link(&(rp->nlinks), LINK_PID, p->id);

	ASSERT(is_nil(p->tracer_proc)
	       || is_internal_pid(p->tracer_proc)
	       || is_internal_port(p->tracer_proc));

	if (IS_TRACED(p)) {
	    if (p->trace_flags & (F_TRACE_SOL|F_TRACE_SOL1))  {
		rp->trace_flags |= (p->trace_flags & TRACEE_FLAGS);
		rp->tracer_proc = p->tracer_proc; /* maybe steal */

		if (p->trace_flags & F_TRACE_SOL1)  { /* maybe override */
		    rp->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    p->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
	    }
	}
    }
    if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	trace_proc(p, rp, am_getting_linked, p->id);

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
	trace_proc(BIF_P, BIF_P, am_link, BIF_ARG_1);
    }
    /* check that the pid or port which is our argument is OK */

    if (is_internal_pid(BIF_ARG_1)) {
	if (internal_pid_index(BIF_ARG_1) >= erts_max_processes) {
	    BIF_ERROR(BIF_P, BADARG);
	}

	if (insert_internal_link(BIF_P, BIF_ARG_1)) {
	    BIF_RET(am_true);
	}
	else {
	    goto res_no_proc;
	}
    }

    if (is_internal_port(BIF_ARG_1)) {
	Port *pt = erts_id2port(BIF_ARG_1, BIF_P, ERTS_PROC_LOCK_MAIN);
	if (!pt) {
	    goto res_no_proc;
	}

	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);
	
	if (erts_add_link(&(BIF_P->nlinks), LINK_PID, BIF_ARG_1) >= 0)
	    erts_add_link(&(pt->nlinks), LINK_PID, BIF_P->id);
	/* else: already linked */

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);
	erts_smp_port_unlock(pt);
	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	goto res_no_proc;
    }

    if (is_external_pid(BIF_ARG_1)) {

	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);

	/* We may earn time by checking first that we're not linked already */
	if (erts_lookup_link(BIF_P->nlinks, BIF_ARG_1) != NULL) {
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

		erts_add_link(&(BIF_P->nlinks), LINK_PID, BIF_ARG_1);
		lnk = erts_add_or_lookup_link(&(dep->nlinks),
					      LINK_PID,
					      BIF_P->id);
		ASSERT(lnk != NULL);
		erts_add_link(&ERTS_LINK_ROOT(lnk), LINK_PID, BIF_ARG_1);

		erts_smp_de_links_unlock(dep);
		erts_smp_de_runlock(dep);
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

		code = erts_dsig_send_link(&dsd, BIF_P->id, BIF_ARG_1);
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

 res_no_proc:
    if (BIF_P->flags & F_TRAPEXIT) {
	ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
	erts_deliver_exit_message(BIF_ARG_1, BIF_P, &locks, am_noproc, NIL);
	erts_smp_proc_unlock(BIF_P, ~ERTS_PROC_LOCK_MAIN & locks);
	BIF_RET(am_true);
    }
    else
	BIF_ERROR(BIF_P, EXC_NOPROC);
}

#define ERTS_DEMONITOR_FALSE		2
#define ERTS_DEMONITOR_TRUE		1
#define ERTS_DEMONITOR_BADARG		0
#define ERTS_DEMONITOR_YIELD_TRUE	-1
#define ERTS_DEMONITOR_INTERNAL_ERROR	-2

static int
remote_demonitor(Process *c_p, DistEntry *dep, Eterm ref, Eterm to)
{
    ErtsDSigData dsd;
    ErtsMonitor *dmon;
    ErtsMonitor *mon;
    int code;
    int res;
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
	mon = erts_remove_monitor(&c_p->monitors, ref);
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_LINK);

	res = ERTS_DEMONITOR_TRUE;
	break;

    case ERTS_DSIG_PREP_CONNECTED:

	erts_smp_de_links_lock(dep);
	mon = erts_remove_monitor(&c_p->monitors, ref);
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
	    res = ERTS_DEMONITOR_TRUE;
	}
	else {
	    /*
	     * Soft (no force) send, use ->data in dist slot 
	     * monitor list since in case of monitor name 
	     * the atom is stored there. Yield if necessary.
	     */
	    code = erts_dsig_send_demonitor(&dsd,
					    c_p->id, 
					    (mon->name != NIL
					     ? mon->name
					     : mon->pid), 
					    ref,
					    0);
	    res = (code == ERTS_DSIG_SEND_YIELD
		   ? ERTS_DEMONITOR_YIELD_TRUE
		   : ERTS_DEMONITOR_TRUE);
	    erts_destroy_monitor(dmon);

	}
	break;
    default:
	ASSERT(! "Invalid dsig prepare result");
	res = ERTS_DEMONITOR_INTERNAL_ERROR;
	break;
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

static int demonitor(Process *c_p, Eterm ref)
{
   ErtsMonitor *mon = NULL;  /* The monitor entry to delete */
   Process  *rp;    /* Local target process */
   Eterm     to = NIL;    /* Monitor link traget */
   Eterm     ref_p; /* Pid of this end */
   DistEntry *dep = NULL;  /* Target's distribution entry */
   int deref_de = 0;
   int res;
   int unlock_link = 1;


   erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_LINK);

   if (is_not_internal_ref(ref)) {
       res = ERTS_DEMONITOR_BADARG;
       goto done; /* Cannot be this monitor's ref */
   }
   ref_p = c_p->id;

   mon = erts_lookup_monitor(c_p->monitors, ref);
   if (!mon) {
       res = ERTS_DEMONITOR_FALSE;
       goto done;
   }

   if (mon->type != MON_ORIGIN) {
       res = ERTS_DEMONITOR_BADARG;
       goto done;
   }
   to = mon->pid;

   if (is_atom(to)) {
       /* Monitoring a name at node to */
       ASSERT(is_node_name_atom(to));
       dep = erts_sysname_to_connected_dist_entry(to);
       ASSERT(dep != erts_this_dist_entry);
       if (dep)
	   deref_de = 1;
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
       rp = erts_pid2proc_opt(c_p,
			      ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK,
			      to,
			      ERTS_PROC_LOCK_LINK,
			      ERTS_P2P_FLG_ALLOW_OTHER_X);
       mon = erts_remove_monitor(&c_p->monitors, ref);
#ifndef ERTS_SMP
       ASSERT(mon);
#else
       if (!mon)
	   res = ERTS_DEMONITOR_FALSE;
       else
#endif
       {
	   res = ERTS_DEMONITOR_TRUE;
	   erts_destroy_monitor(mon);
       }
       if (rp) {
	   ErtsMonitor *rmon;
	   rmon = erts_remove_monitor(&(rp->monitors), ref);
	   if (rp != c_p)
	       erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	   if (rmon != NULL)
	       erts_destroy_monitor(rmon);
       }
       else {
	   ERTS_SMP_ASSERT_IS_NOT_EXITING(c_p);
       }

   }

 done:

   if (unlock_link)
       erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_LINK);

   if (deref_de) {
       ASSERT(dep);
       erts_deref_dist_entry(dep);
   }

   ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
   return res;
}

BIF_RETTYPE demonitor_1(BIF_ALIST_1)
{
    switch (demonitor(BIF_P, BIF_ARG_1)) {
    case ERTS_DEMONITOR_FALSE:
    case ERTS_DEMONITOR_TRUE:
	BIF_RET(am_true);
    case ERTS_DEMONITOR_YIELD_TRUE:
	ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    case ERTS_DEMONITOR_BADARG:
	BIF_ERROR(BIF_P, BADARG);
    case ERTS_DEMONITOR_INTERNAL_ERROR:
    default:
	ASSERT(! "demonitor(): internal error");
	BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
    }
}

BIF_RETTYPE demonitor_2(BIF_ALIST_2)
{
    Eterm res = am_true;
    int info = 0;
    int flush = 0;
    Eterm list = BIF_ARG_2;

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

    switch (demonitor(BIF_P, BIF_ARG_1)) {
    case ERTS_DEMONITOR_FALSE:
	if (info)
	    res = am_false;
	if (flush)
	    BIF_TRAP2(flush_monitor_message_trap, BIF_P, BIF_ARG_1, res);
    case ERTS_DEMONITOR_TRUE:
	BIF_RET(res);
    case ERTS_DEMONITOR_YIELD_TRUE:
	ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    case ERTS_DEMONITOR_BADARG:
    badarg:
	BIF_ERROR(BIF_P, BADARG);
    case ERTS_DEMONITOR_INTERNAL_ERROR:
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
    ErlHeapFragment *bp;

    reason_size = IS_CONST(reason) ? 0 : size_object(reason);
    item_size   = IS_CONST(item) ? 0 : size_object(item);
    ref_size    = size_object(ref);

    heap_size = 6+reason_size+ref_size+item_size;

    hp = erts_alloc_message_heap(heap_size,
				 &bp,
				 &ohp,
				 p,
				 p_locksp);

    reason_copy = (IS_CONST(reason)
		   ? reason
		   : copy_struct(reason, reason_size, &hp, ohp));
    item_copy   = (IS_CONST(item)
		   ? item
		   : copy_struct(item, item_size, &hp, ohp));
    ref_copy    = copy_struct(ref, ref_size, &hp, ohp);

    tup = TUPLE5(hp, am_DOWN, ref_copy, type, item_copy, reason_copy);
    erts_queue_message(p, p_locksp, bp, tup, NIL);
}

static BIF_RETTYPE
local_pid_monitor(Process *p, Eterm target)
{
    BIF_RETTYPE ret;
    Eterm mon_ref;
    Process *rp;
    ErtsProcLocks p_locks = ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK;

    mon_ref = erts_make_ref(p);
    ERTS_BIF_PREP_RET(ret, mon_ref);
    if (target == p->id) {
	return ret;
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
    rp = erts_pid2proc_opt(p, p_locks,
			   target, ERTS_PROC_LOCK_LINK,
			   ERTS_P2P_FLG_ALLOW_OTHER_X);
    if (!rp) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	p_locks &= ~ERTS_PROC_LOCK_LINK;
	erts_queue_monitor_message(p, &p_locks,
				   mon_ref, am_process, target, am_noproc);
    }
    else {
	ASSERT(rp != p);

	erts_add_monitor(&(p->monitors), MON_ORIGIN, mon_ref, target, NIL);
	erts_add_monitor(&(rp->monitors), MON_TARGET, mon_ref, p->id, NIL);

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }

    erts_smp_proc_unlock(p, p_locks & ~ERTS_PROC_LOCK_MAIN);

    return ret;
}

static BIF_RETTYPE
local_name_monitor(Process *p, Eterm target_name)
{
    BIF_RETTYPE ret;
    Eterm mon_ref;
    ErtsProcLocks p_locks = ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK;
    Process *rp;

    mon_ref = erts_make_ref(p);
    ERTS_BIF_PREP_RET(ret, mon_ref);
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
    rp = erts_whereis_process(p, p_locks, target_name, ERTS_PROC_LOCK_LINK,
			      ERTS_P2P_FLG_ALLOW_OTHER_X);
    if (!rp) {
	DeclareTmpHeap(lhp,3,p);
	Eterm item;
	UseTmpHeap(3,p);
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	p_locks &= ~ERTS_PROC_LOCK_LINK;
	item = TUPLE2(lhp, target_name, erts_this_dist_entry->sysname);
	erts_queue_monitor_message(p, &p_locks,
				   mon_ref, am_process, item, am_noproc);
	UnUseTmpHeap(3,p);
    }
    else if (rp != p) {
	erts_add_monitor(&(p->monitors), MON_ORIGIN, mon_ref, rp->id,
			 target_name);
	erts_add_monitor(&(rp->monitors), MON_TARGET, mon_ref, p->id,
			 target_name);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }

    erts_smp_proc_unlock(p, p_locks & ~ERTS_PROC_LOCK_MAIN);

    return ret;
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

	    erts_add_monitor(&(p->monitors), MON_ORIGIN, mon_ref, p_trgt,
			     p_name);
	    erts_add_monitor(&(dep->monitors), MON_TARGET, mon_ref, p->id,
			     d_name);

	    erts_smp_de_links_unlock(dep);
	    erts_smp_de_runlock(dep);
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);

	    code = erts_dsig_send_monitor(&dsd, p->id, target, mon_ref);
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

    return ret;
}
	
BIF_RETTYPE monitor_2(BIF_ALIST_2)
{
    Eterm target = BIF_ARG_2;
    BIF_RETTYPE ret;
    DistEntry  *dep = NULL; 
    int deref_de = 0;

    /* Only process monitors are implemented */
    if (BIF_ARG_1 != am_process) {
	goto error;
    }

    if (is_internal_pid(target)) {
    local_pid:
	ret = local_pid_monitor(BIF_P, target);
    } else if (is_external_pid(target)) {
	dep = external_pid_dist_entry(target);
	if (dep == erts_this_dist_entry)
	    goto local_pid;
	ret = remote_monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, dep, target, 0);
    } else if (is_atom(target)) {
	ret = local_name_monitor(BIF_P, target);
    } else if (is_tuple(target)) {
	Eterm *tp = tuple_val(target);
	Eterm remote_node;
	Eterm name;
	if (arityval(*tp) != 2) 
	    goto error;
	remote_node = tp[2];
	name = tp[1];
	if (!is_atom(remote_node) || !is_atom(name)) {
	    goto error;
	}
	if (!erts_is_alive && remote_node != am_Noname) {
	    goto error; /* Remote monitor from (this) undistributed node */
	}
	dep = erts_sysname_to_connected_dist_entry(remote_node);
	if (dep == erts_this_dist_entry) {
	    deref_de = 1;
	    ret = local_name_monitor(BIF_P, name);
	} else {
	    if (dep)
		deref_de = 1;
	    ret = remote_monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, dep, name, 1);
	}
    } else {
    error:
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

    so.flags = SPO_LINK;
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
    so.flags          = SPO_USE_ARGS;
    so.min_heap_size  = H_MIN_SIZE;
    so.min_vheap_size = BIN_VH_MIN_SIZE;
    so.priority       = PRIORITY_NORMAL;
    so.max_gen_gcs    = (Uint16) erts_smp_atomic_read(&erts_max_gen_gcs);
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
	    } else if (arg == am_min_heap_size && is_small(val)) {
		Sint min_heap_size = signed_val(val);
		if (min_heap_size < 0) {
		    goto error;
		} else if (min_heap_size < H_MIN_SIZE) {
		    so.min_heap_size = H_MIN_SIZE;
		} else {
		    so.min_heap_size = erts_next_heap_size(min_heap_size, 0);
		}
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
		if (erts_common_run_queue && erts_no_schedulers > 1)
		    goto error;
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
	trace_proc(BIF_P, BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_internal_port(BIF_ARG_1)) {
	Port *pt = erts_id2port_sflgs(BIF_ARG_1,
				      BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      ERTS_PORT_SFLGS_DEAD);

	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);
#ifdef ERTS_SMP
	if (ERTS_PROC_PENDING_EXIT(BIF_P)) {
	    if (pt)
		erts_smp_port_unlock(pt);
	    goto handle_pending_exit;
	}
#endif

	l = erts_remove_link(&BIF_P->nlinks, BIF_ARG_1);

	ASSERT(pt || !l);

	if (pt) {
	    rl = erts_remove_link(&pt->nlinks, BIF_P->id);
	    erts_smp_port_unlock(pt);
	    if (rl)
		erts_destroy_link(rl);
	}

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

	if (l)
	    erts_destroy_link(l);

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
	l = erts_remove_link(&BIF_P->nlinks,BIF_ARG_1);

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
	    erts_remove_dist_link(&dld, BIF_P->id, BIF_ARG_1, dep);
	    code = erts_dsig_send_unlink(&dsd, BIF_P->id, BIF_ARG_1);
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

     /* process ok ? */
    if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	BIF_ERROR(BIF_P, BADARG);

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

    /* get process struct */
    rp = erts_pid2proc_opt(BIF_P, (ERTS_PROC_LOCK_MAIN
				   | ERTS_PROC_LOCK_LINK
				   | ERTS_PROC_LOCK_STATUS),
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
    l = erts_remove_link(&BIF_P->nlinks,BIF_ARG_1);
    if (l != NULL)
	erts_destroy_link(l);

    if (!rp) {
	ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
    }
    else {
	rl = erts_remove_link(&(rp->nlinks),BIF_P->id);
	if (rl != NULL)
	    erts_destroy_link(rl);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rl != NULL) {
	    trace_proc(BIF_P, rp, am_getting_unlinked, BIF_P->id);
	}

	if (rp != BIF_P)
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
    }
 
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_STATUS);

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
     * hibernate/3 is implemented as an instruction; therefore
     * this function will never be called.
     */
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE get_stacktrace_0(Process* p)
{
    Eterm t = build_stacktrace(p, p->ftrace);
    BIF_RET(t);
}

/**********************************************************************/
/*
 * This is like exit/1, except that errors are logged if they terminate
 * the process, and the final error value will be {Term,StackTrace}.
 */

BIF_RETTYPE error_1(Process* p, Eterm term)
{
    p->fvalue = term;
    BIF_ERROR(p, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like error/1, except that the given 'args' will be included
 * in the stacktrace.
 */

BIF_RETTYPE error_2(Process* p, Eterm value, Eterm args)
{
    Eterm* hp = HAlloc(p, 3);

    p->fvalue = TUPLE2(hp, value, args);
    BIF_ERROR(p, EXC_ERROR_2);
}

/**********************************************************************/
/*
 * This is like exactly like error/1. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_1(Process* p, Eterm term)
{
    p->fvalue = term;
    BIF_ERROR(p, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like exactly like error/2. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_2(Process* p, Eterm value, Eterm args)
{
    Eterm* hp = HAlloc(p, 3);

    p->fvalue = TUPLE2(hp, value, args);
    BIF_ERROR(p, EXC_ERROR_2);
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
Eterm 
raise_3(Process *c_p, Eterm class, Eterm value, Eterm stacktrace) {
    Eterm reason;
    Eterm l, *hp, *hp_end, *tp;
    int depth, cnt;
    size_t sz;
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
     * error_handler may need to give us.
     */
    for (l = stacktrace, depth = 0;  
	 is_list(l);  
	 l = CDR(list_val(l)), depth++) {
	Eterm t = CAR(list_val(l));
	int arity;
	if (is_not_tuple(t)) goto error;
	tp = tuple_val(t);
	arity = arityval(tp[0]);
	if ((arity == 3) && is_atom(tp[1]) && is_atom(tp[2])) continue;
	if ((arity == 2) && is_fun(tp[1])) continue;
	goto error;
    }
    if (is_not_nil(l)) goto error;
    
    /* Create stacktrace and store */
    if (depth <= erts_backtrace_depth) {
	cnt = 0;
	c_p->ftrace = stacktrace;
    } else {
	cnt = depth = erts_backtrace_depth;
	c_p->ftrace = NIL;
    }
    tp = &c_p->ftrace;
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm) - 1) 
	/ sizeof(Eterm);
    hp = HAlloc(c_p, sz + 2*(cnt + 1));
    hp_end = hp + sz + 2*(cnt + 1);
    s = (struct StackTrace *) hp;
    s->header = make_neg_bignum_header(sz - 1);
    s->freason = reason;
    s->pc = NULL;
    s->current = NULL;
    s->depth = 0;
    hp += sz;
    if (cnt > 0) {
	/* Copy list up to depth */
	for (cnt = 0, l = stacktrace;
	     cnt < depth;
	     cnt++, l = CDR(list_val(l))) {
	    ASSERT(*tp == NIL);
	    *tp = CONS(hp, CAR(list_val(l)), *tp);
	    tp = &CDR(list_val(*tp));
	    hp += 2;
	}
    }
    c_p->ftrace = CONS(hp, c_p->ftrace, make_big((Eterm *) s));
    hp += 2;
    ASSERT(hp <= hp_end);
    
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
	 Port *prt;
	 erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	 prt = erts_id2port(BIF_ARG_1, NULL, 0);
	 if (prt) {
	     erts_do_exit_port(prt, BIF_P->id, BIF_ARG_2);
	     erts_port_release(prt);
	 }
	 erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	 ERTS_BIF_CHK_EXITED(BIF_P);
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
	     code = erts_dsig_send_exit2(&dsd, BIF_P->id, BIF_ARG_1, BIF_ARG_2);
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

	 if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	     BIF_ERROR(BIF_P, BADARG);
	 if (BIF_ARG_1 == BIF_P->id) {
	     rp_locks = ERTS_PROC_LOCKS_ALL;
	     rp = BIF_P;
	     erts_smp_proc_lock(rp, ERTS_PROC_LOCKS_ALL_MINOR);
	 }
	 else {
	     rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	     rp = erts_pid2proc_opt(BIF_P, ERTS_PROC_LOCK_MAIN,
				    BIF_ARG_1, rp_locks,
				    ERTS_P2P_FLG_SMP_INC_REFC);
	     if (!rp) {
		 BIF_RET(am_true);
	     }
	 }

	 /*
	  * Send an exit signal.
	  */
	 erts_send_exit_signal(BIF_P,
			       BIF_P->id,
			       rp,
			       &rp_locks,
			       BIF_ARG_2,
			       NIL,
			       NULL,
			       BIF_P == rp ? ERTS_XSIG_FLG_NO_IGN_NORMAL : 0);
#ifdef ERTS_SMP
	 if (rp == BIF_P)
	     rp_locks &= ~ERTS_PROC_LOCK_MAIN;
	 else
	     erts_smp_proc_dec_refc(rp);
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

       scb = ERTS_PROC_SET_SAVED_CALLS_BUF(rp, ERTS_PROC_LOCK_MAIN, scb);

       if (!scb)
	   old_value = make_small(0);
       else {
	   old_value = make_small(scb->len);
	   erts_free(ERTS_ALC_T_CALLS_BUF, (void *) scb);
       }

       /* Make sure the process in question is rescheduled
	  immediately, if it's us, so the call saving takes effect. */
       if (rp == BIF_P)
	   BIF_RET2(old_value, CONTEXT_REDS);
       else
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
      old_value = erts_proc_set_error_handler(BIF_P,
					      ERTS_PROC_LOCK_MAIN,
					      BIF_ARG_2);
      BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_priority) {
       erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_STATUS);
       old_value = erts_set_process_priority(BIF_P, BIF_ARG_2);
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_STATUS);
       if (old_value == THE_NON_VALUE)
	   goto error;
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_trap_exit) {
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
	*       and handle them before flag trap_exit is set to true.
	*       For more info, see implementation of erts_send_exit_signal().
	*/
       erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_STATUS);
       ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
       old_value = ERTS_PROC_IS_TRAPPING_EXITS(BIF_P) ? am_true : am_false;
       if (trap_exit) {
	   ERTS_PROC_SET_TRAP_EXIT(BIF_P);
       } else {
	   ERTS_PROC_UNSET_TRAP_EXIT(BIF_P);
       }
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_STATUS);
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_scheduler) {
       int yield;
       ErtsRunQueue *old;
       ErtsRunQueue *new;
       Sint sched;
       if (erts_common_run_queue && erts_no_schedulers > 1)
	   goto error;
       if (!is_small(BIF_ARG_2))
	   goto error;
       sched = signed_val(BIF_ARG_2);
       if (sched < 0 || erts_no_schedulers < sched)
	   goto error;
       erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_STATUS);
       old = BIF_P->bound_runq;
#ifdef ERTS_SMP
       ASSERT(!old || old == BIF_P->run_queue);
#endif
       new = !sched ? NULL : erts_schedid2runq(sched);
#ifndef ERTS_SMP
       yield = 0;
#else
       if (new == old)
	   yield = 0;
       else {
	   ErtsRunQueue *curr = BIF_P->run_queue;
	   if (!new)
	       erts_smp_runq_lock(curr);
	   else
	       erts_smp_runqs_lock(curr, new);
	   yield = new && BIF_P->run_queue != new;
#endif
	   BIF_P->bound_runq = new;
#ifdef ERTS_SMP
	   if (new)
	       BIF_P->run_queue = new;
	   if (!new)
	       erts_smp_runq_unlock(curr);
	   else
	       erts_smp_runqs_unlock(curr, new);
       }
#endif
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_STATUS);
       old_value = old ? make_small(old->ix+1) : make_small(0);
       if (yield)
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
       old_value = BIF_P->trace_flags & F_SENSITIVE ? am_true : am_false;
       if (is_sensitive) {
	   BIF_P->trace_flags |= F_SENSITIVE;
       } else {
	   BIF_P->trace_flags &= ~F_SENSITIVE;
       }
       erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
       BIF_RET(old_value);
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

   if ((rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
			   BIF_ARG_1, ERTS_PROC_LOCK_MAIN)) == NULL) {
       BIF_ERROR(BIF_P, BADARG);
   }

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

Eterm
ebif_bang_2(Process* p, Eterm To, Eterm Message)
{
    return send_2(p, To, Message);
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

Sint do_send(Process *p, Eterm to, Eterm msg, int suspend);

static Sint remote_send(Process *p, DistEntry *dep,
			Eterm to, Eterm full_to, Eterm msg, int suspend)
{
    Sint res;
    int code;
    ErtsDSigData dsd;

    ASSERT(is_atom(to) || is_external_pid(to));

    code = erts_dsig_prepare(&dsd, dep, p, ERTS_DSP_NO_LOCK, !suspend);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
	res = SEND_TRAP;
	break;
    case ERTS_DSIG_PREP_WOULD_SUSPEND:
	ASSERT(!suspend);
	res = SEND_YIELD;
	break;
    case ERTS_DSIG_PREP_CONNECTED: {

	if (is_atom(to))
	    code = erts_dsig_send_reg_msg(&dsd, to, msg);
	else
	    code = erts_dsig_send_msg(&dsd, to, msg);
	/*
	 * Note that reductions have been bumped on calling
	 * process by erts_dsig_send_reg_msg() or
	 * erts_dsig_send_msg().
	 */
	if (code == ERTS_DSIG_SEND_YIELD)
	    res = SEND_YIELD_RETURN;
	else
	    res = 0;
	break;
    }
    default:
	ASSERT(! "Invalid dsig prepare result");
	res = SEND_INTERNAL_ERROR;
    }

    if (res >= 0) {
	if (IS_TRACED(p))
	    trace_send(p, full_to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
    }

    return res;
}

Sint
do_send(Process *p, Eterm to, Eterm msg, int suspend) {
    Eterm portid;
    Port *pt;
    Process* rp;
    DistEntry *dep;
    Eterm* tp;

    if (is_internal_pid(to)) {
	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
	
	if (internal_pid_index(to) >= erts_max_processes)
	    return SEND_BADARG;

	rp = erts_pid2proc_opt(p, ERTS_PROC_LOCK_MAIN,
			       to, 0, ERTS_P2P_FLG_SMP_INC_REFC);
	
	if (!rp) {
	    ERTS_SMP_ASSERT_IS_NOT_EXITING(p);
	    return 0;
	}
    } else if (is_external_pid(to)) {
	dep = external_pid_dist_entry(to);
	if(dep == erts_this_dist_entry) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Discarding message %T from %T to %T in an old "
			  "incarnation (%d) of this node (%d)\n",
			  msg,
			  p->id,
			  to,
			  external_pid_creation(to),
			  erts_this_node->creation);
	    erts_send_error_to_logger(p->group_leader, dsbufp);
	    return 0;
	}
	return remote_send(p, dep, to, to, msg, suspend);
    } else if (is_atom(to)) {
	
	/* Need to virtual schedule out sending process
	 * because of lock wait. This is only necessary
	 * for internal port calling but the lock is bundled
	 * with name lookup.
	 */
	    
	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(p, am_out);
	}
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    profile_runnable_proc(p, am_inactive);
	}
	erts_whereis_name(p, ERTS_PROC_LOCK_MAIN,
			  to,
			  &rp, 0, ERTS_P2P_FLG_SMP_INC_REFC,
			  &pt);

	if (pt) {
	    portid = pt->id;
	    goto port_common;
	}
	
	/* Not a port virtually schedule the process back in */
	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(p, am_in);
	}
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    profile_runnable_proc(p, am_active);
	}

	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
	
	if (!rp) {
	    return SEND_BADARG;
	}
    } else if (is_external_port(to)
	       && (external_port_dist_entry(to)
		   == erts_this_dist_entry)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Discarding message %T from %T to %T in an old "
		      "incarnation (%d) of this node (%d)\n",
		      msg,
		      p->id,
		      to,
		      external_port_creation(to),
		      erts_this_node->creation);
	erts_send_error_to_logger(p->group_leader, dsbufp);
	return 0;
    } else if (is_internal_port(to)) {
	portid = to;
	/* schedule out calling process, waiting for lock*/
	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(p, am_out);
	}
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    profile_runnable_proc(p, am_inactive);
	}
	pt = erts_id2port(to, p, ERTS_PROC_LOCK_MAIN);
      port_common:
	ERTS_SMP_LC_ASSERT(!pt || erts_lc_is_port_locked(pt));
        
	/* We have waited for locks, trace schedule ports */
	if (pt && IS_TRACED_FL(pt, F_TRACE_SCHED_PORTS)) {
	    trace_sched_ports_where(pt, am_in, am_command);
	}
	if (pt && erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(pt)) {
	    profile_runnable_port(pt, am_active);
	}
	
	/* XXX let port_command handle the busy stuff !!! */
	if (pt && (pt->status & ERTS_PORT_SFLG_PORT_BUSY)) {
	    if (suspend) {
		erts_suspend(p, ERTS_PROC_LOCK_MAIN, pt);
		if (erts_system_monitor_flags.busy_port) {
		    monitor_generic(p, am_busy_port, portid);
		}
	    }
	    /* Virtually schedule out the port before releasing */
	    if (IS_TRACED_FL(pt, F_TRACE_SCHED_PORTS)) {
	    	trace_sched_ports_where(pt, am_out, am_command);
	    }
	    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(pt)) {
	    	profile_runnable_port(pt, am_inactive);
	    }
	    erts_port_release(pt);
	    return SEND_YIELD;
	}
	
	if (IS_TRACED(p)) 	/* trace once only !! */
	    trace_send(p, portid, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
	
	if (SEQ_TRACE_TOKEN(p) != NIL) {
	    seq_trace_update_send(p);
	    seq_trace_output(SEQ_TRACE_TOKEN(p), msg, 
			     SEQ_TRACE_SEND, portid, p);
	}	    
	
	/* XXX NO GC in port command */
	erts_port_command(p, p->id, pt, msg);
	if (pt) {
	    /* Virtually schedule out the port before releasing */
	    if (IS_TRACED_FL(pt, F_TRACE_SCHED_PORTS)) {
	    	trace_sched_ports_where(pt, am_out, am_command);
	    }
	    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(pt)) {
	    	profile_runnable_port(pt, am_inactive);
	    }
	    erts_port_release(pt);
	}
	/* Virtually schedule in process */
	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(p, am_in);
	}
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    profile_runnable_proc(p, am_active);
	}
	if (ERTS_PROC_IS_EXITING(p)) {
	    KILL_CATCHES(p); /* Must exit */
	    return SEND_USER_ERROR;
	}
	return 0;
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
	    erts_deref_dist_entry(dep);
	    if (IS_TRACED(p))
		trace_send(p, to, msg);
	    if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
		save_calls(p, &exp_send);
	    
	    /* Need to virtual schedule out sending process
	     * because of lock wait. This is only necessary
	     * for internal port calling but the lock is bundled.
	     */
	    
	    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    	trace_virtual_sched(p, am_out);
	    }
	    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    	profile_runnable_proc(p, am_inactive);
	    }

	    erts_whereis_name(p, ERTS_PROC_LOCK_MAIN,
			      tp[1],
			      &rp, 0, ERTS_P2P_FLG_SMP_INC_REFC,
			      &pt);
	    if (pt) {
		portid = pt->id;
		goto port_common;
	    }
	    /* Port lookup failed, virtually schedule the process
	     * back in.
	     */

	    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
	    	trace_virtual_sched(p, am_in);
	    }
	    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
	    	profile_runnable_proc(p, am_active);
	    }

	    if (!rp) {
		return 0;
	    }
	    goto send_message;
	}

	ret = remote_send(p, dep, tp[1], to, msg, suspend);
	if (dep)
	    erts_deref_dist_entry(dep);
	return ret;
    } else {
	if (IS_TRACED(p)) /* XXX Is this really neccessary ??? */
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
	erts_send_message(p, rp, &rp_locks, msg, 0);
	if (!erts_use_sender_punish)
	    res = 0;
	else {
#ifdef ERTS_SMP
	    res = rp->msg_inq.len*4;
	    if (ERTS_PROC_LOCK_MAIN & rp_locks)
		res += rp->msg.len*4;
#else
	    res = rp->msg.len*4;
#endif
	}
	erts_smp_proc_unlock(rp,
			     p == rp
			     ? (rp_locks & ~ERTS_PROC_LOCK_MAIN)
			     : rp_locks);
	erts_smp_proc_dec_refc(rp);
	return res;
    }
}


Eterm
send_3(Process *p, Eterm to, Eterm msg, Eterm opts) {
    int connect = !0;
    int suspend = !0;
    Eterm l = opts;
    Sint result;
    
    while (is_list(l)) {
	if (CAR(list_val(l)) == am_noconnect) {
	    connect = 0;
	} else if (CAR(list_val(l)) == am_nosuspend) {
	    suspend = 0;
	} else {
	    BIF_ERROR(p, BADARG);
	}
	l = CDR(list_val(l));
    }
    if(!is_nil(l)) {
	BIF_ERROR(p, BADARG);
    }
    
    result = do_send(p, to, msg, suspend);
    if (result > 0) {
	ERTS_VBUMP_REDS(p, result);
	BIF_RET(am_ok);
    } else switch (result) {
    case 0:
	BIF_RET(am_ok); 
	break;
    case SEND_TRAP:
	if (connect) {
	    BIF_TRAP3(dsend3_trap, p, to, msg, opts); 
	} else {
	    BIF_RET(am_noconnect);
	}
	break;
    case SEND_YIELD:
	if (suspend) {
	    ERTS_BIF_YIELD3(bif_export[BIF_send_3], p, to, msg, opts);
	} else {
	    BIF_RET(am_nosuspend);
	}
	break;
    case SEND_YIELD_RETURN:
	if (suspend)
	    ERTS_BIF_YIELD_RETURN(p, am_ok);
	else
	    BIF_RET(am_nosuspend);
    case SEND_BADARG:
	BIF_ERROR(p, BADARG); 
	break;
    case SEND_USER_ERROR:
	BIF_ERROR(p, EXC_ERROR); 
	break;
    case SEND_INTERNAL_ERROR:
	BIF_ERROR(p, EXC_INTERNAL_ERROR);
	break;
    default:
	ASSERT(! "Illegal send result"); 
	break;
    }
    ASSERT(! "Can not arrive here");
    BIF_ERROR(p, BADARG);
}

Eterm
send_2(Process *p, Eterm to, Eterm msg) {
    Sint result = do_send(p, to, msg, !0);
    
    if (result > 0) {
	ERTS_VBUMP_REDS(p, result);
	BIF_RET(msg);
    } else switch (result) {
    case 0:
	BIF_RET(msg); 
	break;
    case SEND_TRAP:
	BIF_TRAP2(dsend2_trap, p, to, msg); 
	break;
    case SEND_YIELD:
	ERTS_BIF_YIELD2(bif_export[BIF_send_2], p, to, msg);
	break;
    case SEND_YIELD_RETURN:
	ERTS_BIF_YIELD_RETURN(p, msg);
    case SEND_BADARG:
	BIF_ERROR(p, BADARG); 
	break;
    case SEND_USER_ERROR:
	BIF_ERROR(p, EXC_ERROR); 
	break;
    case SEND_INTERNAL_ERROR:
	BIF_ERROR(p, EXC_INTERNAL_ERROR);
	break;
    default:
	ASSERT(! "Illegal send result"); 
	break;
    }
    ASSERT(! "Can not arrive here");
    BIF_ERROR(p, BADARG);
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

BIF_RETTYPE iolist_size_1(BIF_ALIST_1)
{
    Sint size = io_list_len(BIF_ARG_1);

    if (size == -1) {
	BIF_ERROR(BIF_P, BADARG);
    } else if (IS_USMALL(0, (Uint) size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
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
    while (size--) {		/* XXX use memcpy? */
	*hp++ = *ptr++;
    }
    resp[ix] = BIF_ARG_3;
    BIF_RET(make_tuple(resp));
}

/**********************************************************************/

BIF_RETTYPE make_tuple_2(BIF_ALIST_2)
{
    Sint n;
    Eterm* hp;
    Eterm res;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0) {
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

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0) {
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
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = tuple_val(BIF_ARG_1);
    arity = arityval(*ptr);
    hp = HAlloc(BIF_P, arity + 2);
    res = make_tuple(hp);
    *hp = make_arityval(arity+1);
    while (arity--) {
	*++hp = *++ptr;
    }
    *++hp = BIF_ARG_2;
    BIF_RET(res);
}

/**********************************************************************/

/* convert an atom to a list of ascii integer */

BIF_RETTYPE atom_to_list_1(BIF_ALIST_1)
{
    Uint need;
    Eterm* hp;
    Atom* ap;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
     
    /* read data from atom table */
    ap = atom_tab(atom_val(BIF_ARG_1));
    if (ap->len == 0)
	BIF_RET(NIL);	/* the empty atom */
    need = ap->len*2;
    hp = HAlloc(BIF_P, need);
    BIF_RET(buf_to_intlist(&hp,(char*)ap->name,ap->len, NIL));
}

/**********************************************************************/

/* convert a list of ascii integers to an atom */
 
BIF_RETTYPE list_to_atom_1(BIF_ALIST_1)
{
    Eterm res;
    char *buf = (char *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_LENGTH);
    int i = intlist_to_buf(BIF_ARG_1, buf, MAX_ATOM_LENGTH);

    if (i < 0) {
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	i = list_length(BIF_ARG_1);
	if (i > MAX_ATOM_LENGTH) {
	    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    res = am_atom_put(buf, i);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
}

/* conditionally convert a list of ascii integers to an atom */
 
BIF_RETTYPE list_to_existing_atom_1(BIF_ALIST_1)
{
    int i;
    char *buf = (char *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_LENGTH);

    if ((i = intlist_to_buf(BIF_ARG_1, buf, MAX_ATOM_LENGTH)) < 0) {
    error:
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(BIF_P, BADARG);
    } else {
	Eterm a;
	
	if (erts_atom_get(buf, i, &a)) {
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

/* convert a list of ascii ascii integer value to an integer */


#define LTI_BAD_STRUCTURE 0
#define LTI_NO_INTEGER 1
#define LTI_SOME_INTEGER 2
#define LTI_ALL_INTEGER 3

static int do_list_to_integer(Process *p, Eterm orig_list, 
			      Eterm *integer, Eterm *rest)
{
     Sint i = 0;
     int skip = 0;
     int neg = 0;
     int n = 0;
     int m;
     int lg2;
     Eterm res;
     Eterm* hp;
     Eterm *hp_end;
     Eterm lst = orig_list;
     Eterm tail = lst;
     int error_res = LTI_BAD_STRUCTURE;

     if (is_nil(lst)) {
       error_res = LTI_NO_INTEGER;
     error:
	 *rest = tail;
	 *integer = make_small(0);
	 return error_res;
     }       
     if (is_not_list(lst))
       goto error;

     /* if first char is a '-' then it is a negative integer */
     if (CAR(list_val(lst)) == make_small('-')) {
	  neg = 1;
	  skip = 1;
	  lst = CDR(list_val(lst));
	  if (is_not_list(lst)) {
	      tail = lst;
	      error_res = LTI_NO_INTEGER;
	      goto error;
	  }
     } else if (CAR(list_val(lst)) == make_small('+')) {
	 /* ignore plus */
	 skip = 1;
	 lst = CDR(list_val(lst));
	 if (is_not_list(lst)) {
	     tail = lst;
	     error_res = LTI_NO_INTEGER;
	     goto error;
	 }
     }

     /* Calculate size and do type check */

     while(1) {
	 if (is_not_small(CAR(list_val(lst)))) {
	     break;
	 }
	 if (unsigned_val(CAR(list_val(lst))) < '0' ||
	     unsigned_val(CAR(list_val(lst))) > '9') {
	     break;
	 }
	 i = i * 10;
	 i = i + unsigned_val(CAR(list_val(lst))) - '0';
	 n++;
	 lst = CDR(list_val(lst));
	 if (is_nil(lst)) {
	     break;
	 }
	 if (is_not_list(lst)) {
	     break;
	 }
     }

     tail = lst;
     if (!n) {
	 error_res = LTI_NO_INTEGER;
	 goto error;
     } 

	 
      /* If n <= 8 then we know it's a small int 
      ** since 2^27 = 134217728. If n > 8 then we must
      ** construct a bignum and let that routine do the checking
      */

     if (n <= SMALL_DIGITS) {  /* It must be small */
	 if (neg) i = -i;
	 res = make_small(i);
     } else {
	 lg2 =  (n+1)*230/69+1;
	 m  = (lg2+D_EXP-1)/D_EXP; /* number of digits */
	 m  = BIG_NEED_SIZE(m);    /* number of words + thing */

	 hp = HAlloc(p, m);
	 hp_end = hp + m;
	 
	 lst = orig_list;
	 if (skip)
	     lst = CDR(list_val(lst));
	 
	 /* load first digits (at least one digit) */
	 if ((i = (n % D_DECIMAL_EXP)) == 0)
	     i = D_DECIMAL_EXP;
	 n -= i;
	 m = 0;
	 while(i--) {
	     m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
	     lst = CDR(list_val(lst));
	 }
	 res = small_to_big(m, hp);  /* load first digits */
	 
	 while(n) {
	     i = D_DECIMAL_EXP;
	     n -= D_DECIMAL_EXP;
	     m = 0;
	     while(i--) {
		 m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
		 lst = CDR(list_val(lst));
	     }
	     if (is_small(res))
		 res = small_to_big(signed_val(res), hp);
	     res = big_times_small(res, D_DECIMAL_BASE, hp);
	     if (is_small(res))
		 res = small_to_big(signed_val(res), hp);
	     res = big_plus_small(res, m, hp);
	 }

	 if (is_big(res))  /* check if small */
	     res = big_plus_small(res, 0, hp); /* includes conversion to small */
	 
	 if (neg) {
	     if (is_small(res))
		 res = make_small(-signed_val(res));
	     else {
		 Uint *big = big_val(res); /* point to thing */
		 *big = bignum_header_neg(*big);
	     }
	 }

	 if (is_big(res)) {
	     hp += (big_arity(res)+1);
	 }
	 HRelease(p,hp_end,hp);
     }
     *integer = res;
     *rest = tail;
     if (tail != NIL) {
	 return LTI_SOME_INTEGER;
     }
     return LTI_ALL_INTEGER;
}
BIF_RETTYPE string_to_integer_1(BIF_ALIST_1)
{
     Eterm res;
     Eterm tail;
     Eterm *hp;
     /* must be a list */
     switch (do_list_to_integer(BIF_P,BIF_ARG_1,&res,&tail)) {
	 /* HAlloc after do_list_to_integer as it 
	    might HAlloc itself (bignum) */
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
     Eterm res;
     Eterm dummy;
     /* must be a list */
     
     if (do_list_to_integer(BIF_P,BIF_ARG_1,&res,&dummy) != LTI_ALL_INTEGER) {
	 BIF_ERROR(BIF_P,BADARG);
     }
     BIF_RET(res);
 }

/**********************************************************************/

/* convert a float to a list of ascii characters */

BIF_RETTYPE float_to_list_1(BIF_ALIST_1)
{
     int i;
     Uint need;
     Eterm* hp;
     FloatDef f;
     char fbuf[30];
     
     /* check the arguments */
     if (is_not_float(BIF_ARG_1))
	 BIF_ERROR(BIF_P, BADARG);
     GET_DOUBLE(BIF_ARG_1, f);
     if ((i = sys_double_to_chars(f.fd, fbuf)) <= 0)
	 BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
     need = i*2;
     hp = HAlloc(BIF_P, need);
     BIF_RET(buf_to_intlist(&hp, fbuf, i, NIL));
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


BIF_RETTYPE list_to_float_1(BIF_ALIST_1)
{
    int i;
    FloatDef f;
    Eterm res;
    Eterm* hp;
    char *buf = NULL;

    i = list_length(BIF_ARG_1);
    if (i < 0) {
    badarg:
	if (buf)
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(BIF_P, BADARG);
    }

    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
    
    if (intlist_to_buf(BIF_ARG_1, buf, i) < 0)
	goto badarg;
    buf[i] = '\0';		/* null terminal */

    if (sys_chars_to_double(buf, &f.fd) != 0)
	goto badarg;
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
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
    int len;

    if ((len = list_length(list)) < 0) {
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
     BIF_RET(BIF_P->id);
}

/**********************************************************************/

/*
   New representation of refs in R9, see erl_term.h

   In the first data word, only the usual 18 bits are used. Ordinarily,
   in "long refs" all words are used (in other words, practically never
   wrap around), but for compatibility with older nodes, "short refs"
   exist. Short refs come into being by being converted from the old
   external format for refs (tag REFERENCE_EXT). Short refs are
   converted back to the old external format.

   When converting a long ref to the external format in the case of
   preparing for sending to an older node, the ref is truncated by only
   using the first word (with 18 significant bits), and using the old tag
   REFERENCE_EXT.

   When comparing refs or different size, only the parts up to the length
   of the shorter operand are used. This has the desirable effect that a
   long ref sent to an old node and back will be treated as equal to
   the original, although some of the bits have been lost.

   The hash value for a ref always considers only the first word, since
   in the above scenario, the original and the copy should have the same
   hash value.
*/

static Uint32 reference0; /* Initialized in erts_init_bif */
static Uint32 reference1;
static Uint32 reference2;
static erts_smp_spinlock_t make_ref_lock;
static erts_smp_mtx_t ports_snapshot_mtx;
erts_smp_atomic_t erts_dead_ports_ptr; /* To store dying ports during snapshot */

Eterm erts_make_ref_in_buffer(Eterm buffer[REF_THING_SIZE])
{
    Eterm* hp = buffer;
    Uint32 ref0, ref1, ref2;

    erts_smp_spin_lock(&make_ref_lock);

    reference0++;
    if (reference0 >= MAX_REFERENCE) {
	reference0 = 0;
	reference1++;
	if (reference1 == 0) {
	    reference2++;
	}
    }

    ref0 = reference0;
    ref1 = reference1;
    ref2 = reference2;

    erts_smp_spin_unlock(&make_ref_lock);

    write_ref_thing(hp, ref0, ref1, ref2);
    return make_internal_ref(hp);
}

Eterm erts_make_ref(Process *p)
{
    Eterm* hp;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));

    hp = HAlloc(p, REF_THING_SIZE);
    return erts_make_ref_in_buffer(hp);
}

BIF_RETTYPE make_ref_0(BIF_ALIST_0)
{
    return erts_make_ref(BIF_P);
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
localtime_to_universaltime_2(Process *p, Eterm localtime, Eterm dst)
{
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

BIF_RETTYPE garbage_collect_1(BIF_ALIST_1)
{
    int reds;
    Process *rp;

    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				   BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
    if (!rp)
	BIF_RET(am_false);
    if (rp == ERTS_PROC_LOCK_BUSY)
	ERTS_BIF_YIELD1(bif_export[BIF_garbage_collect_1], BIF_P, BIF_ARG_1);

    /* The GC cost is taken for the process executing this BIF. */

    FLAGS(rp) |= F_NEED_FULLSWEEP;
    reds = erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);

    if (BIF_P != rp)
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

    BIF_RET2(am_true, reds);
}

BIF_RETTYPE garbage_collect_0(BIF_ALIST_0)
{
    int reds;

    FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
    reds = erts_garbage_collect(BIF_P, 0, NULL, 0);
    BIF_RET2(am_true, reds);
}

/**********************************************************************/
/* Perform garbage collection of the message area */

BIF_RETTYPE garbage_collect_message_area_0(BIF_ALIST_0)
{
#if defined(HYBRID) && !defined(INCREMENTAL)
    int reds = 0;

    FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
    reds = erts_global_garbage_collect(BIF_P, 0, NULL, 0);
    BIF_RET2(am_true, reds);
#else
    BIF_RET(am_false);
#endif
}

/**********************************************************************/
/* Return a list of active ports */

BIF_RETTYPE ports_0(BIF_ALIST_0)
{
    Eterm res = NIL;
    Eterm* port_buf = erts_alloc(ERTS_ALC_T_TMP,
				 sizeof(Eterm)*erts_max_ports);
    Eterm* pp = port_buf;
    Eterm* dead_ports;
    int alive, dead;
    Uint32 next_ss;

    /* To get a consistent snapshot... 
     * We add alive ports from start of the buffer
     * while dying ports are added from the other end by the killing threads.
     */

    erts_smp_mtx_lock(&ports_snapshot_mtx); /* One snapshot at a time */

    erts_smp_atomic_set(&erts_dead_ports_ptr, (long) (port_buf + erts_max_ports));

    next_ss = erts_smp_atomic_inctest(&erts_ports_snapshot);

    if (erts_smp_atomic_read(&erts_ports_alive) > 0) {
	long i;
	for (i = erts_max_ports-1; i >= 0; i--) {
	    Port* prt = &erts_port[i];
	    erts_smp_port_state_lock(prt);
	    if (!(prt->status & ERTS_PORT_SFLGS_DEAD)
		&& prt->snapshot != next_ss) {
		ASSERT(prt->snapshot == next_ss - 1);
		*pp++ = prt->id;		
		prt->snapshot = next_ss; /* Consumed by this snapshot */
	    }
	    erts_smp_port_state_unlock(prt);
	}
    }

    dead_ports = (Eterm*)erts_smp_atomic_xchg(&erts_dead_ports_ptr,
					      (long)NULL);
    erts_smp_mtx_unlock(&ports_snapshot_mtx);

    ASSERT(pp <= dead_ports);

    alive = pp - port_buf;
    dead = port_buf + erts_max_ports - dead_ports;

    ASSERT((alive+dead) <= erts_max_ports);

    if (alive+dead > 0) {
	long i;
	Eterm *hp = HAlloc(BIF_P, (alive+dead)*2);

	for (i = 0; i < alive; i++) {
	    res = CONS(hp, port_buf[i], res);	    
	    hp += 2;
	}
	for (i = 0; i < dead; i++) {
	    res = CONS(hp, dead_ports[i], res);
	    hp += 2;
	}
    }

    erts_free(ERTS_ALC_T_TMP, port_buf);

    BIF_RET(res);
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
	erl_exit(1, "Failed to convert term to string: %d (s)\n",
		 -pres, erl_errno_id(-pres));
    hp = HAlloc(BIF_P, 2*dsbufp->str_len); /* we need length * 2 heap words */
    res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
    erts_printf("%s", dsbufp->str);
    erts_destroy_tmp_dsbuf(dsbufp);
    BIF_RET(res);
}


Eterm
display_string_1(Process* p, Eterm string)
{
    int len = is_string(string);
    char *str;

    if (len <= 0) {
	BIF_ERROR(p, BADARG);
    }
    str = (char *) erts_alloc(ERTS_ALC_T_TMP, sizeof(char)*(len + 1));
    if (intlist_to_buf(string, str, len) != len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    str[len] = '\0';
    erts_fprintf(stderr, "%s", str);
    erts_free(ERTS_ALC_T_TMP, (void *) str);
    BIF_RET(am_true);
}

Eterm
display_nl_0(Process* p)
{
    erts_fprintf(stderr, "\n");
    BIF_RET(am_true);
}

/**********************************************************************/

/* stop the system */
/* ARGSUSED */
BIF_RETTYPE halt_0(BIF_ALIST_0)
{
    VERBOSE(DEBUG_SYSTEM,("System halted by BIF halt/0\n"));
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erl_exit(0, "");
    return NIL;  /* Pedantic (lint does not know about erl_exit) */
}

/**********************************************************************/

#define MSG_SIZE	200

/* stop the system with exit code */
/* ARGSUSED */
BIF_RETTYPE halt_1(BIF_ALIST_1)
{
    Sint code;
    static char msg[MSG_SIZE];
    int i;
    
    if (is_small(BIF_ARG_1) && (code = signed_val(BIF_ARG_1)) >= 0) {
	VERBOSE(DEBUG_SYSTEM,("System halted by BIF halt(%d)\n", code));
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erl_exit(-code, "");
    } else if (is_string(BIF_ARG_1) || BIF_ARG_1 == NIL) {
	if ((i = intlist_to_buf(BIF_ARG_1, msg, MSG_SIZE-1)) < 0) {
	    goto error;
	}
	msg[i] = '\0';
	VERBOSE(DEBUG_SYSTEM,("System halted by BIF halt(%s)\n", msg));
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erl_exit(ERTS_DUMP_EXIT, "%s\n", msg);
    } else {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    return NIL;  /* Pedantic (lint does not know about erl_exit) */
}

BIF_RETTYPE function_exported_3(BIF_ALIST_3)
{
    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) || 
	is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (erts_find_function(BIF_ARG_1, BIF_ARG_2, signed_val(BIF_ARG_3)) == NULL) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

/**********************************************************************/    

BIF_RETTYPE is_builtin_3(Process* p, Eterm Mod, Eterm Name, Eterm Arity)
{
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
	erl_exit(1, "Failed to convert term to list: %d (s)\n",
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
#if HALFWORD_HEAP
    hp = HAlloc(BIF_P, 3);
    hp[0] = HEADER_EXPORT;
    /* Yes, May be misaligned, but X86_64 will fix it... */
    *((Export **) (hp+1)) = erts_export_get_or_make_stub(BIF_ARG_1, BIF_ARG_2, (Uint) arity);
#else
    hp = HAlloc(BIF_P, 2);
    hp[0] = HEADER_EXPORT;
    hp[1] = (Eterm) erts_export_get_or_make_stub(BIF_ARG_1, BIF_ARG_2, (Uint) arity);
#endif
    BIF_RET(make_export(hp));
}

Eterm
fun_to_list_1(Process* p, Eterm fun)
{
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
    int i;
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

      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_THING_HEAD_SIZE + 1);
      etp->header = make_external_pid_header(1);
      etp->next = MSO(BIF_P).externals;
      etp->node = enp;
      etp->data.ui[0] = make_pid_data(c, b);

      MSO(BIF_P).externals = etp;
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
		new_member->group_leader = STORE_NC_IN_PROC(new_member,
							    BIF_ARG_1);
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
	if (BIF_ARG_2 == am_block || BIF_ARG_2 == am_unblock) {
#ifndef ERTS_SMP
	    BIF_RET(am_disabled);
#else
	    if (erts_no_schedulers == 1)
		BIF_RET(am_disabled);
	    else {
		switch (erts_block_multi_scheduling(BIF_P,
						    ERTS_PROC_LOCK_MAIN,
						    BIF_ARG_2 == am_block,
						    0)) {
		case ERTS_SCHDLR_SSPND_DONE_MSCHED_BLOCKED:
		    BIF_RET(am_blocked);
		case ERTS_SCHDLR_SSPND_YIELD_DONE_MSCHED_BLOCKED:
		    ERTS_BIF_YIELD_RETURN_X(BIF_P, am_blocked,
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
					   &old_no)) {
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
	oval = (Uint) erts_smp_atomic_xchg(&erts_max_gen_gcs, (long) nval);
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_heap_size) {
	int oval = H_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);

	H_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_smp_release_system();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_bin_vheap_size) {
	int oval = BIN_VH_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);

	BIN_VH_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_smp_release_system();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(make_small(oval));
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
	BIF_RET(db_set_trace_control_word_1(BIF_P, BIF_ARG_2));
    } else if (BIF_ARG_1 == am_sequential_tracer) {
        Eterm old_value = erts_set_system_seq_tracer(BIF_P,
						     ERTS_PROC_LOCK_MAIN,
						     BIF_ARG_2);
	if (old_value != THE_NON_VALUE) {
	    BIF_RET(old_value);
	}
    } else if (BIF_ARG_1 == make_small(1)) {
	Uint i;
	ErlMessage* mp;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);

	for (i = 0; i < erts_max_processes; i++) {
	    if (process_tab[i] != (Process*) 0) {
		Process* p = process_tab[i];
		p->seq_trace_token = NIL;
		p->seq_trace_clock = 0;
		p->seq_trace_lastcnt = 0;
		ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
		mp = p->msg.first;
		while(mp != NULL) {
		    ERL_MESSAGE_TOKEN(mp) = NIL;
		    mp = mp->next;
		}
	    }
	}

	erts_smp_release_system();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(am_true);
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
	BIF_TRAP1(set_cpu_topology_trap, BIF_P, BIF_ARG_2);
    } else if (ERTS_IS_ATOM_STR("scheduler_bind_type", BIF_ARG_1)) {
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
#if defined(ARCH_64) && !HALFWORD_HEAP
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
#if defined(ARCH_64) && !HALFWORD_HEAP
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

/*
 * Processes doing yield on return in a bif ends up in bif_return_trap().
 */
static BIF_RETTYPE bif_return_trap(
#ifdef DEBUG
    BIF_ALIST_2
#else
    BIF_ALIST_1
#endif
    )
{
#ifdef DEBUG
    switch (BIF_ARG_2) {
    case am_multi_scheduling:
#ifdef ERTS_SMP
	erts_dbg_multi_scheduling_return_trap(BIF_P, BIF_ARG_1);
#endif
	break;
    case am_schedulers_online:
	break;
    default:
	break;
    }
#endif

    BIF_RET(BIF_ARG_1);
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
	Eterm unused;
	ERTS_BIF_PREP_TRAP3(unused, await_proc_exit_trap, c_p, pid, am_data, ret);
    }
}

void
erts_bif_prep_await_proc_exit_reason_trap(Process *c_p, Eterm pid)
{
    if (skip_current_msgq(c_p)) {
	Eterm unused;
	ERTS_BIF_PREP_TRAP3(unused, await_proc_exit_trap, c_p,
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
	Eterm unused;
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
	ERTS_BIF_PREP_TRAP3(unused, await_proc_exit_trap, c_p, pid, am_apply, term);
    }
}

Export bif_return_trap_export;

void erts_init_bif(void)
{
    reference0 = 0;
    reference1 = 0;
    reference2 = 0;

    erts_smp_spinlock_init(&make_ref_lock, "make_ref");
    erts_smp_mtx_init(&ports_snapshot_mtx, "ports_snapshot");
    erts_smp_atomic_init(&erts_dead_ports_ptr, (long)NULL);

    /*
     * bif_return_trap/1 is a hidden BIF that bifs that need to
     * yield the calling process traps to. The only thing it does:
     * return the value passed as argument.
     */
    sys_memset((void *) &bif_return_trap_export, 0, sizeof(Export));
    bif_return_trap_export.address = &bif_return_trap_export.code[3];
    bif_return_trap_export.code[0] = am_erlang;
    bif_return_trap_export.code[1] = am_bif_return_trap;
#ifdef DEBUG
    bif_return_trap_export.code[2] = 2;
#else
    bif_return_trap_export.code[2] = 1;
#endif
    bif_return_trap_export.code[3] = (BeamInstr) em_apply_bif;
    bif_return_trap_export.code[4] = (BeamInstr) &bif_return_trap;

    flush_monitor_message_trap = erts_export_put(am_erlang,
						 am_flush_monitor_message,
						 2);

    set_cpu_topology_trap = erts_export_put(am_erlang,
					    am_set_cpu_topology,
					    1);
    erts_format_cpu_topology_trap = erts_export_put(am_erlang,
						    am_format_cpu_topology,
						    1);
    await_proc_exit_trap = erts_export_put(am_erlang,am_await_proc_exit,3);
}

#ifdef HARDDEBUG
/*
You'll need this line in bif.tab to be able to use this debug bif

bif erlang:send_to_logger/2

*/
BIF_RETTYPE send_to_logger_2(BIF_ALIST_2)
{
    byte *buf;
    int len;
    if (!is_atom(BIF_ARG_1) || !(is_list(BIF_ARG_2) ||
				 is_nil(BIF_ARG_1))) {
	BIF_ERROR(BIF_P,BADARG);
    }
    len = io_list_len(BIF_ARG_2);
    if (len < 0)
	BIF_ERROR(BIF_P,BADARG);
    else if (len == 0)
	buf = "";
    else {
#ifdef DEBUG
	int len2;
#endif
	buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, len+1);
#ifdef DEBUG
	len2 =
#else
	(void)
#endif
	    io_list_to_buf(BIF_ARG_2, buf, len);
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
