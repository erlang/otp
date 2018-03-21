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
#include "erl_proc_sig_queue.h"

Export *erts_await_result;
static Export await_exit_trap;
static Export* flush_monitor_messages_trap = NULL;
static Export* set_cpu_topology_trap = NULL;
static Export* await_port_send_result_trap = NULL;
Export* erts_format_cpu_topology_trap = NULL;
static Export dsend_continue_trap_export;
Export *erts_convert_time_unit_trap = NULL;

static Export *await_msacc_mod_trap = NULL;
static erts_atomic32_t msacc;

static Export *system_flag_scheduler_wall_time_trap;
static Export *await_sched_wall_time_mod_trap;
static erts_atomic32_t sched_wall_time;

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

/* create a link to the process */
BIF_RETTYPE link_1(BIF_ALIST_1)
{
    if (IS_TRACED_FL(BIF_P, F_TRACE_PROCS)) {
	trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN, BIF_P, am_link, BIF_ARG_1);
    }
    /* check that the pid or port which is our argument is OK */

    if (is_internal_pid(BIF_ARG_1)) {
        int created;
        ErtsLinkData *ldp;
        ErtsLink *lnk;

        if (BIF_P->common.id == BIF_ARG_1)
            BIF_RET(am_true);

        if (!erts_proc_lookup(BIF_ARG_1))
            goto res_no_proc;

        lnk = erts_link_tree_lookup_create(&ERTS_P_LINKS(BIF_P),
                                           &created,
                                           ERTS_LNK_TYPE_PROC,
                                           BIF_P->common.id,
                                           BIF_ARG_1);
        if (!created)
            BIF_RET(am_true);

        ldp = erts_link_to_data(lnk);
        

        if (erts_proc_sig_send_link(BIF_P, BIF_ARG_1, &ldp->b))
            BIF_RET(am_true);

        erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), lnk);
        erts_link_release_both(ldp);
        goto res_no_proc;
    }

    if (is_internal_port(BIF_ARG_1)) {
        int created;
        ErtsLinkData *ldp;
        ErtsLink *lnk;
        Eterm ref;
        Eterm *refp;
	Port *prt = erts_port_lookup(BIF_ARG_1,
				     (erts_port_synchronous_ops
				      ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
				      : ERTS_PORT_SFLGS_INVALID_LOOKUP));
	if (!prt) {
	    goto res_no_proc;
	}

        lnk = erts_link_tree_lookup_create(&ERTS_P_LINKS(BIF_P),
                                           &created,
                                           ERTS_LNK_TYPE_PORT,
                                           BIF_P->common.id,
                                           BIF_ARG_1);
        if (!created)
            BIF_RET(am_true);

        ldp = erts_link_to_data(lnk);
        refp = erts_port_synchronous_ops ? &ref : NULL;

        switch (erts_port_link(BIF_P, prt, &ldp->b, refp)) {
        case ERTS_PORT_OP_DROPPED:
        case ERTS_PORT_OP_BADARG:
            erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), lnk);
            erts_link_release_both(ldp);
            goto res_no_proc;
        case ERTS_PORT_OP_SCHEDULED:
            if (refp) {
                ASSERT(is_internal_ordinary_ref(ref));
                BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
            }
        default:
            break;
        }
	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	goto res_no_proc;
    }

    if (is_external_pid(BIF_ARG_1)) {
        ErtsLinkData *ldp;
        int created;
        DistEntry *dep;
        ErtsLink *lnk;
        int code;
        ErtsDSigData dsd;

        dep = external_pid_dist_entry(BIF_ARG_1);
        if (dep == erts_this_dist_entry)
            goto res_no_proc;

        lnk = erts_link_tree_lookup_create(&ERTS_P_LINKS(BIF_P),
                                           &created,
                                           ERTS_LNK_TYPE_DIST_PROC,
                                           BIF_P->common.id,
                                           BIF_ARG_1);

        if (!created)
            BIF_RET(am_true); /* Already present... */

        ldp = erts_link_to_data(lnk);

        code = erts_dsig_prepare(&dsd, dep, BIF_P,
                                 ERTS_PROC_LOCK_MAIN,
                                 ERTS_DSP_RLOCK, 0, 1);
        switch (code) {
        case ERTS_DSIG_PREP_NOT_ALIVE:
        case ERTS_DSIG_PREP_NOT_CONNECTED:
            erts_link_set_dead_dist(&ldp->b, dep->sysname);
            erts_proc_sig_send_link_exit(NULL, BIF_ARG_1, &ldp->b,
                                         am_noconnection, NIL);
            BIF_RET(am_true);

        case ERTS_DSIG_PREP_PENDING:
        case ERTS_DSIG_PREP_CONNECTED: {
            /*
             * We have (pending) connection.
             * Setup link and enqueue link signal.
             */
#ifdef DEBUG
            int inserted =
#endif
                erts_link_dist_insert(&ldp->b, dep->mld);
            ASSERT(inserted);
            erts_de_runlock(dep);

            code = erts_dsig_send_link(&dsd, BIF_P->common.id, BIF_ARG_1);
            if (code == ERTS_DSIG_SEND_YIELD)
                ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
            BIF_RET(am_true);
            break;
        }
        default:
            ERTS_ASSERT(! "Invalid dsig prepare result");
        }
    }

    BIF_ERROR(BIF_P, BADARG);

res_no_proc:
    if (BIF_P->flags & F_TRAP_EXIT) {
        ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
        erts_deliver_exit_message(BIF_ARG_1, BIF_P, &locks, am_noproc, NIL);
        erts_proc_unlock(BIF_P, ~ERTS_PROC_LOCK_MAIN & locks);
        BIF_RET(am_true);
    }
    else {
        /*
         * This behaviour is *really* sad but link/1 has
         * behaved like this for ages (and this behaviour is
         * actually documented)... :'-(
         *
         * The proper behavior would have been to
         * send calling process an exit signal..
         */
        BIF_ERROR(BIF_P, EXC_NOPROC);
    }
}

static Eterm
demonitor(Process *c_p, Eterm ref, Eterm *multip)
{
    ErtsMonitor  *mon;  /* The monitor entry to delete */

   *multip = am_false;

   if (is_not_internal_ref(ref)) {
       if (is_external_ref(ref)
           && (erts_this_dist_entry
               == external_ref_dist_entry(ref))) {
           return am_false;
       }
       return am_badarg; /* Not monitored by this monitor's ref */
   }

   mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), ref);
   if (!mon)
       return am_false;

   if (!erts_monitor_is_origin(mon))
       return am_badarg;

   erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), mon);

   switch (mon->type) {

   case ERTS_MON_TYPE_TIME_OFFSET:
       *multip = am_true;
       erts_demonitor_time_offset(mon);
       return am_true;

   case ERTS_MON_TYPE_PORT: {
       Port *prt;
       ASSERT(is_internal_port(mon->other.item));
       prt = erts_port_lookup(mon->other.item, ERTS_PORT_SFLGS_DEAD);
       if (!prt || erts_port_demonitor(c_p, prt, mon) == ERTS_PORT_OP_DROPPED)
           erts_monitor_release(mon);
       return am_true;
   }

   case ERTS_MON_TYPE_PROC:
       erts_proc_sig_send_demonitor(mon);
       return am_true;

   case ERTS_MON_TYPE_DIST_PROC: {
       ErtsMonitorData *mdp = erts_monitor_to_data(mon);
       Eterm to = mon->other.item;
       DistEntry *dep;
       int code = ERTS_DSIG_SEND_OK;
       int deleted;
       ErtsDSigData dsd;

       ASSERT(is_external_pid(to) || is_node_name_atom(to));

       if (is_external_pid(to))
           dep = external_pid_dist_entry(to);
       else {
           /* Monitoring a name at node to */
           dep = erts_sysname_to_connected_dist_entry(to);
           ASSERT(dep != erts_this_dist_entry);
           if (!dep) {
               erts_monitor_release(mon);
               return am_false;
           }
       }

       code = erts_dsig_prepare(&dsd, dep, c_p, ERTS_PROC_LOCK_MAIN,
                                ERTS_DSP_RLOCK, 0, 0);

       deleted = erts_monitor_dist_delete(&mdp->target);

       switch (code) {
       case ERTS_DSIG_PREP_NOT_ALIVE:
       case ERTS_DSIG_PREP_NOT_CONNECTED:
           /*
            * In the smp case this is possible if the node goes
            * down just before the call to demonitor.
            */
           break;

       case ERTS_DSIG_PREP_PENDING:
       case ERTS_DSIG_PREP_CONNECTED: {
           Eterm watched;

           erts_de_runlock(dep);

           if (mon->flags & ERTS_ML_FLG_NAME)
               watched = ((ErtsMonitorDataExtended *) mdp)->u.name;
           else
               watched = to;

           /*
            * Soft (no force) send, use ->data in dist slot 
            * monitor list since in case of monitor name 
            * the atom is stored there. Yield if necessary.
            */
           code = erts_dsig_send_demonitor(&dsd, c_p->common.id,
                                           watched, mdp->ref, 0);
           break;
       }

       default:
           ERTS_INTERNAL_ERROR("invalid result from erts_dsig_prepare()");
           break;
       }

       if (deleted)
           erts_monitor_release(&mdp->target);

       erts_monitor_release(mon);
       return code == ERTS_DSIG_SEND_YIELD ? am_yield : am_true;
   }

   default:
       ERTS_INTERNAL_ERROR("Unexpected monitor type");
       return am_false;
   }
}

BIF_RETTYPE demonitor_1(BIF_ALIST_1)
{
    Eterm multi;
    switch (demonitor(BIF_P, BIF_ARG_1, &multi)) {
    case am_false:
    case am_true:
        BIF_RET(am_true);
    case am_yield:
        ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    case am_badarg:
    default:
        BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE demonitor_2(BIF_ALIST_2)
{
    BIF_RETTYPE res;
    Eterm multi = am_false;
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

    res = am_true;
    switch (demonitor(BIF_P, BIF_ARG_1, &multi)) {

    case am_false:
	if (info)
	    res = am_false;
	if (flush) {
flush_messages:
	    BIF_TRAP3(flush_monitor_messages_trap, BIF_P,
		      BIF_ARG_1, multi, res);
	}
        /* Fall through... */

    case am_true:
	if (multi == am_true && flush)
	    goto flush_messages;
	BIF_RET(res);

    case am_yield:
        /* return true after yield... */
        if (flush) {
            ERTS_VBUMP_ALL_REDS(BIF_P);
            goto flush_messages;
        }
        ERTS_BIF_YIELD_RETURN(BIF_P, am_true);

    case am_badarg:
    default:
        break;

    }

badarg:
    BIF_ERROR(BIF_P, BADARG);
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

BIF_RETTYPE monitor_2(BIF_ALIST_2)
{
    Eterm target = BIF_ARG_2;
    Eterm tmp_heap[3];
    Eterm ref, id, name;
    ErtsMonitorData *mdp;

    if (BIF_ARG_1 == am_process) {
        DistEntry *dep;
        int byname;

        if (is_internal_pid(target)) {
            name = NIL;
            id = target;

        local_process:

            ref = erts_make_ref(BIF_P);
            if (id != BIF_P->common.id) {
                mdp = erts_monitor_create(ERTS_MON_TYPE_PROC,
                                          ref, BIF_P->common.id,
                                          id, name);
                erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P),
                                         &mdp->origin);

                if (!erts_proc_sig_send_monitor(&mdp->target, id))
                    erts_proc_sig_send_monitor_down(&mdp->target,
                                                    am_noproc);
            }
            BIF_RET(ref);
        }

        if (is_atom(target)) {
        local_named_process:
            name = target;
            id = erts_whereis_name_to_id(BIF_P, target);
            if (is_internal_pid(id))
                goto local_process;
            target = TUPLE2(&tmp_heap[0], name,
                            erts_this_dist_entry->sysname);
            goto noproc;
        }

        if (is_external_pid(target)) {
            ErtsDSigData dsd;
            int code;

            dep = external_pid_dist_entry(target);
            if (dep == erts_this_dist_entry)
                goto noproc;

            id = target;
            name = NIL;
            byname = 0;

        remote_process:

            ref = erts_make_ref(BIF_P);
            mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC, ref,
                                      BIF_P->common.id, id, name);
            erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P), &mdp->origin);

            code = erts_dsig_prepare(&dsd, dep,
                                     BIF_P, ERTS_PROC_LOCK_MAIN,
                                     ERTS_DSP_RLOCK, 0, 1);
            switch (code) {
            case ERTS_DSIG_PREP_NOT_ALIVE:
            case ERTS_DSIG_PREP_NOT_CONNECTED:
                erts_monitor_set_dead_dist(&mdp->target, dep->sysname);
                erts_proc_sig_send_monitor_down(&mdp->target, am_noconnection);
                code = ERTS_DSIG_SEND_OK;
                break;

            case ERTS_DSIG_PREP_PENDING:
            case ERTS_DSIG_PREP_CONNECTED: {
#ifdef DEBUG
                int inserted =
#endif

                erts_monitor_dist_insert(&mdp->target, dep->mld);
                ASSERT(inserted);
                erts_de_runlock(dep);

                code = erts_dsig_send_monitor(&dsd, BIF_P->common.id, target, ref);
                break;
            }

            default:
                ERTS_ASSERT(! "Invalid dsig prepare result");
                code = ERTS_DSIG_SEND_OK;
                break;
            }

            if (byname)
                erts_deref_dist_entry(dep);

            if (code == ERTS_DSIG_SEND_YIELD)
                ERTS_BIF_YIELD_RETURN(BIF_P, ref);
            BIF_RET(ref);
        }

        if (is_tuple(target)) {
            Eterm *tpl = tuple_val(target);
            if (arityval(tpl[0]) != 2)
                goto badarg;
            if (is_not_atom(tpl[1]) || is_not_atom(tpl[2]))
                goto badarg;
            if (!erts_is_alive && tpl[2] != am_Noname)
                goto badarg;
            target = tpl[1];
            dep = erts_find_or_insert_dist_entry(tpl[2]);
            if (dep == erts_this_dist_entry) {
                erts_deref_dist_entry(dep);
                goto local_named_process;
            }

            id = dep->sysname;
            name = target;
            byname = 1;
            goto remote_process;
        }

        /* badarg... */
    }
    else if (BIF_ARG_1 == am_port) {

        if (is_internal_port(target)) {
            Port *prt;
            name = NIL;
            id = target;
        local_port:
            ref = erts_make_ref(BIF_P);
            mdp = erts_monitor_create(ERTS_MON_TYPE_PORT, ref,
                                      BIF_P->common.id, id, name);
            erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P), &mdp->origin);
            prt = erts_port_lookup(id, ERTS_PORT_SFLGS_INVALID_LOOKUP);
            if (!prt || erts_port_monitor(BIF_P, prt, &mdp->target) == ERTS_PORT_OP_DROPPED)
                erts_proc_sig_send_monitor_down(&mdp->target, am_noproc);
            BIF_RET(ref);
        }

        if (is_atom(target)) {
        local_named_port:
            name = target;
            id = erts_whereis_name_to_id(BIF_P, target);
            if (is_internal_port(id))
                goto local_port;
            target = TUPLE2(&tmp_heap[0], name,
                            erts_this_dist_entry->sysname);
            goto noproc;
        }

        if (is_external_port(target)) {
            if (erts_this_dist_entry == external_port_dist_entry(target))
                goto noproc;
            goto badarg;
        }

        if (is_tuple(target)) {
            Eterm *tpl = tuple_val(target);
            if (arityval(tpl[0]) != 2)
                goto badarg;
            if (is_not_atom(tpl[1]) || is_not_atom(tpl[2]))
                goto badarg;
            if (tpl[2] == erts_this_dist_entry->sysname) {
                target = tpl[1];
                goto local_named_port;
            }
        }

        /* badarg... */
    }
    else if (BIF_ARG_1 == am_time_offset) {

        if (target != am_clock_service)
            goto badarg;
	ref = erts_make_ref(BIF_P);
        mdp = erts_monitor_create(ERTS_MON_TYPE_TIME_OFFSET,
                                  ref, BIF_P->common.id,
                                  am_clock_service, NIL);
        erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P), &mdp->origin);

	erts_monitor_time_offset(&mdp->target);

        BIF_RET(ref);
    }

badarg:

    BIF_ERROR(BIF_P, BADARG);

noproc: {
        ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;

        ref = erts_make_ref(BIF_P);
        erts_queue_monitor_message(BIF_P,
                                   &locks,
                                   ref,
                                   BIF_ARG_1,
                                   target,
                                   am_noproc);
        if (locks != ERTS_PROC_LOCK_MAIN)
            erts_proc_unlock(BIF_P, locks & ~ERTS_PROC_LOCK_MAIN);

        BIF_RET(ref);
    }
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
    so.max_gen_gcs    = (Uint16) erts_atomic32_read_nob(&erts_max_gen_gcs);
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
    if (IS_TRACED_FL(BIF_P, F_TRACE_PROCS)) {
        trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN,
                   BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_internal_pid(BIF_ARG_1)) {
        ErtsLink *lnk = erts_link_tree_lookup(ERTS_P_LINKS(BIF_P), BIF_ARG_1);
        if (lnk) {
            erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), lnk);
            erts_proc_sig_send_unlink(BIF_P, lnk);
        }
        BIF_RET(am_true);
    }

    if (is_internal_port(BIF_ARG_1)) {
        ErtsLink *lnk = erts_link_tree_lookup(ERTS_P_LINKS(BIF_P), BIF_ARG_1);

	if (lnk) {
            Eterm ref;
            Eterm *refp = erts_port_synchronous_ops ? &ref : NULL;
            ErtsPortOpResult res = ERTS_PORT_OP_DROPPED;
	    Port *prt;

            erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), lnk);

	    /* Send unlink signal */
	    prt = erts_port_lookup(BIF_ARG_1, ERTS_PORT_SFLGS_DEAD);
	    if (prt) {
#ifdef DEBUG
		ref = NIL;
#endif
		res = erts_port_unlink(BIF_P, prt, lnk, refp);

	    }

            if (res == ERTS_PORT_OP_DROPPED)
                erts_link_release(lnk);
            else if (refp && res == ERTS_PORT_OP_SCHEDULED) {
                ASSERT(is_internal_ordinary_ref(ref));
                BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
            }
	}

	BIF_RET(am_true);
    }

    if (is_external_pid(BIF_ARG_1)) {
        ErtsLink *lnk, *dlnk;
        ErtsLinkData *ldp;
        DistEntry *dep;
	int code;
	ErtsDSigData dsd;

	dep = external_pid_dist_entry(BIF_ARG_1);
	if (dep == erts_this_dist_entry)
	    BIF_RET(am_true);

        lnk = erts_link_tree_lookup(ERTS_P_LINKS(BIF_P), BIF_ARG_1);
        if (!lnk)
            BIF_RET(am_true);

        erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), lnk);
        dlnk = erts_link_to_other(lnk, &ldp);

        if (erts_link_dist_delete(dlnk))
            erts_link_release_both(ldp);
        else
            erts_link_release(lnk);

	code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_PROC_LOCK_MAIN,
				 ERTS_DSP_NO_LOCK, 0, 0);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    BIF_RET(am_true);
	case ERTS_DSIG_PREP_PENDING:
	case ERTS_DSIG_PREP_CONNECTED:
	    code = erts_dsig_send_unlink(&dsd, BIF_P->common.id, BIF_ARG_1);
	    if (code == ERTS_DSIG_SEND_YIELD)
		ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
            break;
	default:
	    ASSERT(! "Invalid dsig prepare result");
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}

        BIF_RET(am_true);
    }

    if (is_external_port(BIF_ARG_1)) {
        if (external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
            BIF_RET(am_true);
        /* Links to Remote ports not supported... */
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE hibernate_3(BIF_ALIST_3)
{
    /*
     * hibernate/3 is usually translated to an instruction; therefore
     * this function is only called from HiPE or when the call could not
     * be translated.
     */
    Eterm reg[3];

    reg[0] = BIF_ARG_1;
    reg[1] = BIF_ARG_2;
    reg[2] = BIF_ARG_3;

    if (erts_hibernate(BIF_P, reg)) {
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

static BIF_RETTYPE
erts_internal_await_exit_trap(BIF_ALIST_0)
{
    /*
     * We have sent ourselves an exit signal which will
     * terminate ourselves. Handle all signals until
     * terminated in order to ensure that signal order
     * is preserved. Yield if necessary.
     */
    erts_aint32_t state;
    int reds = ERTS_BIF_REDS_LEFT(BIF_P);
    (void) erts_proc_sig_handle_incoming(BIF_P, &state, &reds,
                                         reds, !0);
    BUMP_REDS(BIF_P, reds);
    if (state & ERTS_PSFLG_EXITING)
        ERTS_BIF_EXITED(BIF_P);

    ERTS_BIF_YIELD0(&await_exit_trap, BIF_P);
}

/**********************************************************************/
/* send an exit signal to another process */

static BIF_RETTYPE send_exit_signal_bif(Process *c_p, Eterm id, Eterm reason, int exit2)
{
    BIF_RETTYPE ret_val;

    /*
     * 'id' not a process id, nor a local port id is a 'badarg' error.
     */

     if (is_internal_pid(id)) {
         /*
          * Preserve the very old and *very strange* behaviour
          * of erlang:exit/2...
          *
          * - terminate ourselves even though exit reason
          *   is normal (unless we trap exit)
          * - terminate ourselves before exit/2 return
          */
         int exit2_suicide = (exit2
                              && c_p->common.id == id
                              && (reason == am_kill
                                  || !(c_p->flags & F_TRAP_EXIT)));
         erts_proc_sig_send_exit(c_p, c_p->common.id, id,
                                 reason, NIL, exit2_suicide);
         if (!exit2_suicide)
             ERTS_BIF_PREP_RET(ret_val, am_true);
         else {
             erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
             erts_proc_sig_fetch(c_p);
             erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
             ERTS_BIF_PREP_TRAP0(ret_val, &await_exit_trap, c_p);
         }
     }
     else if (is_internal_port(id)) {
	 Eterm ref, *refp;
	 Uint32 invalid_flags;
	 Port *prt;
         ErtsPortOpResult res = ERTS_PORT_OP_DONE;
#ifdef DEBUG
         ref = NIL;
#endif

	 if (erts_port_synchronous_ops) {
	     refp = &ref;
	     invalid_flags = ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP;
	 }
	 else {
	     refp = NULL;
	     invalid_flags = ERTS_PORT_SFLGS_INVALID_LOOKUP;
	 }

	 prt = erts_port_lookup(id, invalid_flags);
	 if (prt)
	     res = erts_port_exit(c_p, 0, prt, c_p->common.id, reason, refp);

         if (!refp || res != ERTS_PORT_OP_SCHEDULED)
             ERTS_BIF_PREP_RET(ret_val, am_true);
         else {
             ASSERT(is_internal_ordinary_ref(ref));
             ERTS_BIF_PREP_TRAP3(ret_val, await_port_send_result_trap,
                                 c_p, ref, am_true, am_true);
         }
     }
     else if (is_external_pid(id)) {
	 DistEntry *dep = external_pid_dist_entry(id);
	 if (dep == erts_this_dist_entry)
             ERTS_BIF_PREP_RET(ret_val, am_true); /* Old incarnation of this node... */
         else {
             int code;
             ErtsDSigData dsd;

             code = erts_dsig_prepare(&dsd, dep, c_p, ERTS_PROC_LOCK_MAIN,
                                      ERTS_DSP_NO_LOCK, 0, 1);
             switch (code) {
             case ERTS_DSIG_PREP_NOT_ALIVE:
             case ERTS_DSIG_PREP_NOT_CONNECTED:
                 ERTS_BIF_PREP_RET(ret_val, am_true);
                 break;
             case ERTS_DSIG_PREP_PENDING:
             case ERTS_DSIG_PREP_CONNECTED:
                 code = erts_dsig_send_exit2(&dsd, c_p->common.id, id, reason);
                 if (code == ERTS_DSIG_SEND_YIELD)
                     ERTS_BIF_PREP_YIELD_RETURN(ret_val, c_p, am_true);
                 else
                     ERTS_BIF_PREP_RET(ret_val, am_true);
                 break;
             default:
                 ASSERT(! "Invalid dsig prepare result");
                 ERTS_BIF_PREP_ERROR(ret_val, c_p, EXC_INTERNAL_ERROR);
                 break;
             }
         }
     }
     else if (is_external_port(id)) {
	 DistEntry *dep = external_port_dist_entry(id);
	 if(dep == erts_this_dist_entry)
             ERTS_BIF_PREP_RET(ret_val, am_true); /* Old incarnation of this node... */
         else
             ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG);
     }
     else {
         /* Not an id of a process or a port... */

         ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG);
     }

     return ret_val;
}

BIF_RETTYPE exit_2(BIF_ALIST_2)
{
    return send_exit_signal_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, !0);
}

BIF_RETTYPE exit_signal_2(BIF_ALIST_2)
{
    return send_exit_signal_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, 0);
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
       old_value = (BIF_P->flags & F_TRAP_EXIT) ? am_true : am_false;
       if (BIF_ARG_2 == am_true)
           BIF_P->flags |= F_TRAP_EXIT;
       else if (BIF_ARG_2 == am_false)
           BIF_P->flags &= ~F_TRAP_EXIT;
       else
	   goto error;
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_scheduler) {
       ErtsRunQueue *old, *new, *curr;
       Sint sched;

       if (!is_small(BIF_ARG_2))
	   goto error;
       sched = signed_val(BIF_ARG_2);
       if (sched < 0 || erts_no_schedulers < sched)
	   goto error;

       if (sched == 0) {
           old = erts_bind_runq_proc(BIF_P, 0);
	   new = NULL;
       }
       else {
           int bound = !0;
	   new = erts_schedid2runq(sched);
           old = erts_set_runq_proc(BIF_P, new, &bound);
           if (!bound)
               old = NULL;
       }

       old_value = old ? make_small(old->ix+1) : make_small(0);

       curr = erts_proc_sched_data(BIF_P)->run_queue;

       ASSERT(!old || old == curr);

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
       erts_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
       old_value = (ERTS_TRACE_FLAGS(BIF_P) & F_SENSITIVE
		    ? am_true
		    : am_false);
       if (is_sensitive) {
	   ERTS_TRACE_FLAGS(BIF_P) |= F_SENSITIVE;
       } else {
	   ERTS_TRACE_FLAGS(BIF_P) &= ~F_SENSITIVE;
       }
       erts_proc_unlock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
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

   rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				  BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
   if (rp == ERTS_PROC_LOCK_BUSY)
       ERTS_BIF_YIELD3(bif_export[BIF_process_flag_3], BIF_P,
		       BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

   if (!rp)
       BIF_ERROR(BIF_P, BADARG);

   res = process_flag_aux(BIF_P, rp, BIF_ARG_2, BIF_ARG_3);

   if (rp != BIF_P)
       erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

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
#define SEND_NOCONNECT		(-1)
#define SEND_YIELD		(-2)
#define SEND_YIELD_RETURN	(-3)
#define SEND_BADARG		(-4)
#define SEND_USER_ERROR		(-5)
#define SEND_INTERNAL_ERROR	(-6)
#define SEND_AWAIT_RESULT	(-7)
#define SEND_YIELD_CONTINUE     (-8)


static Sint remote_send(Process *p, DistEntry *dep,
			Eterm to, Eterm full_to, Eterm msg,
			ErtsSendContext* ctx)
{
    Sint res;
    int code;
    ASSERT(is_atom(to) || is_external_pid(to));

    ctx->dep = dep;
    code = erts_dsig_prepare(&ctx->dsd, dep, p, ERTS_PROC_LOCK_MAIN,
			     ERTS_DSP_NO_LOCK,
			     !ctx->suspend, ctx->connect);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
	res = SEND_NOCONNECT;
	break;
    case ERTS_DSIG_PREP_WOULD_SUSPEND:
	ASSERT(!ctx->suspend);
	res = SEND_YIELD;
	break;
    case ERTS_DSIG_PREP_PENDING:
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

static Sint
do_send(Process *p, Eterm to, Eterm msg, Eterm *refp, ErtsSendContext *ctx)
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
		    ASSERT(is_internal_ordinary_ref(*refp));
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
        int deref_dep = 0;
	int ret;
	tp = tuple_val(to);
	if (*tp != make_arityval(2))
	    return SEND_BADARG;
	if (is_not_atom(tp[1]) || is_not_atom(tp[2]))
	    return SEND_BADARG;
	
	/* erts_find_dist_entry will return NULL if there is no dist_entry
	   but remote_send() will handle that. */

	dep = erts_find_dist_entry(tp[2]);

	if (dep == erts_this_dist_entry) {
	    Eterm id;
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
        if (dep == NULL) {
            dep = erts_find_or_insert_dist_entry(tp[2]);
            ASSERT(dep != erts_this_dist_entry);
            deref_dep = 1;
        }
	ctx->dsd.node = tp[2];

	ret = remote_send(p, dep, tp[1], to, msg, ctx);
	if (ret == SEND_YIELD_CONTINUE) {
            erts_ref_dist_entry(ctx->dep);
            ctx->deref_dep = 1;
	}
        if (deref_dep)
            erts_deref_dist_entry(dep);
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
	if (p == rp)
	    rp_locks |= ERTS_PROC_LOCK_MAIN;
	/* send to local process */
	erts_send_message(p, rp, &rp_locks, msg, 0);
	erts_proc_unlock(rp,
			     p == rp
			     ? (rp_locks & ~ERTS_PROC_LOCK_MAIN)
			     : rp_locks);
	return 0;
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

    Eterm l = opts;
    Sint result;

    DeclareTypedTmpHeap(ErtsSendContext, ctx, BIF_P);

    ERTS_MSACC_PUSH_STATE_M_X();

    UseTmpHeap(sizeof(ErtsSendContext)/sizeof(Eterm), BIF_P);

    ctx->suspend = !0;
    ctx->connect = !0;
    ctx->deref_dep = 0;
    ctx->return_term = am_ok;
    ctx->dss.reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
    ctx->dss.phase = ERTS_DSIG_SEND_PHASE_INIT;

    while (is_list(l)) {
	if (CAR(list_val(l)) == am_noconnect) {
	    ctx->connect = 0;
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

    if (result >= 0) {
	ERTS_VBUMP_REDS(p, 4);
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, am_ok);
	goto done;
    }

    switch (result) {
    case SEND_NOCONNECT:
	if (ctx->connect) {
	    ERTS_BIF_PREP_RET(retval, am_ok);
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
	ASSERT(is_internal_ordinary_ref(ref));
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
    Binary* bin = erts_magic_ref2bin(BIF_ARG_1);
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
    ctx->connect = !0;
    ctx->deref_dep = 0;
    ctx->return_term = msg;
    ctx->dss.reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
    ctx->dss.phase = ERTS_DSIG_SEND_PHASE_INIT;

    result = do_send(p, to, msg, &ref, ctx);

    ERTS_MSACC_POP_STATE_M_X();

    if (result >= 0) {
	ERTS_VBUMP_REDS(p, 4);
	if (ERTS_IS_PROC_OUT_OF_REDS(p))
	    goto yield_return;
	ERTS_BIF_PREP_RET(retval, msg);
	goto done;
    }

    switch (result) {
    case SEND_NOCONNECT:
	ERTS_BIF_PREP_RET(retval, msg);
	break;
    case SEND_YIELD:
	ERTS_BIF_PREP_YIELD2(retval, bif_export[BIF_send_2], p, to, msg);
	break;
    case SEND_YIELD_RETURN:
    yield_return:
	ERTS_BIF_PREP_YIELD_RETURN(retval, p, msg);
        break;
    case SEND_AWAIT_RESULT:
	ASSERT(is_internal_ordinary_ref(ref));
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
    byte *buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_SZ_LIMIT);
    Sint written;
    int i = erts_unicode_list_to_buf(BIF_ARG_1, buf, MAX_ATOM_CHARACTERS,
                                     &written);
    if (i < 0) {
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	if (i == -2) {
	    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    res = erts_atom_put(buf, written, ERTS_ATOM_ENC_UTF8, 1);
    ASSERT(is_atom(res));
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
}

/* conditionally convert a list of ascii integers to an atom */
 
BIF_RETTYPE list_to_existing_atom_1(BIF_ALIST_1)
{
    byte *buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, MAX_ATOM_SZ_LIMIT);
    Sint written;
    int i = erts_unicode_list_to_buf(BIF_ARG_1, buf, MAX_ATOM_CHARACTERS,
                                     &written);
    if (i < 0) {
    error:
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(BIF_P, BADARG);
    } else {
	Eterm a;
	
	if (erts_atom_get((char *) buf, written, &a, ERTS_ATOM_ENC_UTF8)) {
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

BIF_RETTYPE string_list_to_integer_1(BIF_ALIST_1)
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

    Eterm arity_two = make_arityval(2);
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
BIF_RETTYPE string_list_to_float_1(BIF_ALIST_1)
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
      sys_memcpy(buf, bytes, size);
    
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

/*
 * Pass atom 'minor' for relaxed generational GC run. This is only
 * recommendation, major run may still be chosen by VM.
 * Pass atom 'major' for default behaviour - major GC run (fullsweep)
 */
BIF_RETTYPE
erts_internal_garbage_collect_1(BIF_ALIST_1)
{
    switch (BIF_ARG_1) {
    case am_minor:  break;
    case am_major:  FLAGS(BIF_P) |= F_NEED_FULLSWEEP; break;
    default:        BIF_ERROR(BIF_P, BADARG);
    }
    erts_garbage_collect(BIF_P, 0, NULL, 0);
    return am_true;
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
    Sint len = erts_unicode_list_to_buf_len(string);
    Sint written;
    byte *str;
    int res;

    if (len < 0) {
	BIF_ERROR(p, BADARG);
    }
    str = (byte *) erts_alloc(ERTS_ALC_T_TMP, sizeof(char)*(len + 1));
    res = erts_unicode_list_to_buf(string, str, len, &written);
    if (res != 0 || written != len)
	erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error (%d)\n", __FILE__, __LINE__, res);
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
	    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
            erts_exit(pos_int_code, "");
	}
    }
    else if (ERTS_IS_ATOM_STR("abort", BIF_ARG_1)) {
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_exit(ERTS_ABORT_EXIT, "");
    }
    else if (is_list(BIF_ARG_1) || BIF_ARG_1 == NIL) {
#       define HALT_MSG_SIZE 200
        static byte halt_msg[4*HALT_MSG_SIZE+1];
        Sint written;

        if (erts_unicode_list_to_buf(BIF_ARG_1, halt_msg, HALT_MSG_SIZE,
                                     &written) == -1 ) {
            goto error;
        }
        ASSERT(written >= 0 && written < sizeof(halt_msg));
	halt_msg[written] = '\0';
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
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
    erts_magic_ref_save_bin(BIF_ARG_1);
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
      BIF_RET(make_external_pid(etp));
    }

 bad:
    if (buf)
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE list_to_port_1(BIF_ALIST_1)
{
    /*
     * A valid port identifier is on the format
     * "#Port<N.P>" where N is node and P is
     * the port id. Both N and P are of type Uint32.
     */
    Uint32 n, p;
    char* cp;
    int i;
    DistEntry *dep = NULL;
    char buf[6 /* #Port< */
             + (2)*(10 + 1) /* N.P> */
             + 1 /* \0 */];

    /* walk down the list and create a C string */
    if ((i = intlist_to_buf(BIF_ARG_1, buf, sizeof(buf)-1)) < 0)
	goto bad;

    buf[i] = '\0';		/* null terminal */

    cp = &buf[0];
    if (sys_strncmp("#Port<", cp, 6) != 0)
        goto bad;

    cp += 6; /* sys_strlen("#Port<") */

    if (sscanf(cp, "%u.%u>", (unsigned int*)&n, (unsigned int*)&p) < 2)
        goto bad;

    if (p > ERTS_MAX_PORT_NUMBER)
	goto bad;

    dep = erts_channel_no_to_dist_entry(n);

    if (!dep)
	goto bad;

    if(dep == erts_this_dist_entry) {
	BIF_RET(make_internal_port(p));
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;

      if (is_nil(dep->cid))
	  goto bad;

      enp = erts_find_or_insert_node(dep->sysname, dep->creation);
      ASSERT(enp != erts_this_node);

      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_THING_HEAD_SIZE + 1);
      etp->header = make_external_port_header(1);
      etp->next = MSO(BIF_P).first;
      etp->node = enp;
      etp->data.ui[0] = p;

      MSO(BIF_P).first = (struct erl_off_heap_header*) etp;
      BIF_RET(make_external_port(etp));
    }

 bad:
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE list_to_ref_1(BIF_ALIST_1)
{
    /*
     * A valid reference is on the format
     * "#Ref<N.X.Y.Z>" where N, X, Y, and Z are
     * 32-bit integers (i.e., max 10 characters).
     */
    Eterm *hp;
    Eterm res;
    Uint32 refn[ERTS_MAX_REF_NUMBERS];
    int n = 0;
    Uint ints[1 + ERTS_MAX_REF_NUMBERS] = {0};
    char* cp;
    Sint i;
    DistEntry *dep = NULL;
    char buf[5 /* #Ref< */
             + (1 + ERTS_MAX_REF_NUMBERS)*(10 + 1) /* N.X.Y.Z> */
             + 1 /* \0 */];

    /* walk down the list and create a C string */
    if ((i = intlist_to_buf(BIF_ARG_1, buf, sizeof(buf)-1)) < 0)
	goto bad;

    buf[i] = '\0';		/* null terminal */

    cp = &buf[0];
    if (*cp++ != '#') goto bad;
    if (*cp++ != 'R') goto bad;
    if (*cp++ != 'e') goto bad;
    if (*cp++ != 'f') goto bad;
    if (*cp++ != '<') goto bad;

    for (i = 0; i < sizeof(ints)/sizeof(Uint); i++) {
        if (*cp < '0' || *cp > '9') goto bad;

        while (*cp >= '0' && *cp <= '9') {
            ints[i] = 10*ints[i] + (*cp - '0');
            cp++;
        }

        n++;
        if (ints[i] > ~((Uint32) 0)) goto bad;
        if (*cp == '>') break;
        if (*cp++ != '.') goto bad;
    }

    if (*cp++ != '>') goto bad;
    if (*cp != '\0') goto bad;

    if (n < 2) goto bad;

    for (n = 0; i > 0; i--)
        refn[n++] = (Uint32) ints[i];

    ASSERT(n <= ERTS_MAX_REF_NUMBERS);

    dep = erts_channel_no_to_dist_entry(ints[0]);

    if (!dep)
	goto bad;

    if(dep == erts_this_dist_entry) {
        ErtsMagicBinary *mb;
        Uint32 sid;
        if (refn[0] > MAX_REFERENCE) goto bad;
        if (n != ERTS_REF_NUMBERS) goto bad;
        sid = erts_get_ref_numbers_thr_id(refn);
        if (sid > erts_no_schedulers) goto bad;
        mb = erts_magic_ref_lookup_bin(refn);
        if (mb) {
            hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
            res = erts_mk_magic_ref(&hp, &BIF_P->off_heap,
                                    (Binary *) mb);
        }
        else {
            hp = HAlloc(BIF_P, ERTS_REF_THING_SIZE);
            write_ref_thing(hp, refn[0], refn[1], refn[2]);
            res = make_internal_ref(hp);
        }
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;
      Uint hsz;
      int j;

      if (is_nil(dep->cid))
	  goto bad;
      
      enp = erts_find_or_insert_node(dep->sysname, dep->creation);
      ASSERT(enp != erts_this_node);

      hsz = EXTERNAL_THING_HEAD_SIZE;
#if defined(ARCH_64)
      hsz += n/2 + 1;
#else
      hsz += n;
#endif

      etp = (ExternalThing *) HAlloc(BIF_P, hsz);
      etp->header = make_external_ref_header(n/2);
      etp->next = BIF_P->off_heap.first;
      etp->node = enp;
      i = 0;
#if defined(ARCH_64)
      etp->data.ui32[i] = n;
#endif
      for (j = 0; j < n; j++) {
          etp->data.ui32[i] = refn[j];
          i++;
      }

      BIF_P->off_heap.first = (struct erl_off_heap_header*) etp;
      res = make_external_ref(etp);
    }

    BIF_RET(res);

 bad:
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/

BIF_RETTYPE group_leader_0(BIF_ALIST_0)
{
    BIF_RET(BIF_P->group_leader);
}

/**********************************************************************/
/* set group leader */

int
erts_set_group_leader(Process *proc, Eterm new_gl)
{
    
    erts_aint32_t state;

    ASSERT(is_pid(new_gl));

    state = erts_atomic32_read_nob(&proc->state);

    if (state & ERTS_PSFLG_EXITING)
        return 0;
        
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(proc));

    if (!(state & ERTS_PSFLG_DIRTY_RUNNING))
        proc->group_leader = STORE_NC_IN_PROC(proc, new_gl);
    else {
        ErlHeapFragment *bp;
        Eterm *hp;
        /*
         * Currently executing on a dirty scheduler,
         * so we are not allowed to write to its heap.
         * Store group leader pid in heap fragment.
         */
        bp = new_message_buffer(NC_HEAP_SIZE(new_gl));
        hp = bp->mem;
        proc->group_leader = STORE_NC(&hp,
                                            &proc->off_heap,
                                            new_gl);
        bp->next = proc->mbuf;
        proc->mbuf = bp;
        proc->mbuf_sz += bp->used_size;
    }

    return !0;
}

BIF_RETTYPE erts_internal_group_leader_3(BIF_ALIST_3)
{
    if (is_not_pid(BIF_ARG_1))
        BIF_ERROR(BIF_P, BADARG);
    if (is_not_internal_pid(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);
    if (is_not_internal_ref(BIF_ARG_3))
        BIF_ERROR(BIF_P, BADARG);

    erts_proc_sig_send_group_leader(BIF_P,
                                    BIF_ARG_2,
                                    BIF_ARG_1,
                                    BIF_ARG_3);
    BIF_RET(am_ok);
}

BIF_RETTYPE erts_internal_group_leader_2(BIF_ALIST_2)
{
    if (is_not_pid(BIF_ARG_1))
        BIF_RET(am_badarg);

    if (is_internal_pid(BIF_ARG_2)) {
        Process *rp;
        int res;

        if (BIF_ARG_2 == BIF_P->common.id)
            rp = BIF_P;
        else {
            rp = erts_try_lock_sig_free_proc(BIF_ARG_2,
                                             ERTS_PROC_LOCK_MAIN);
            if (!rp)
                BIF_RET(am_badarg);
            if (rp == ERTS_PROC_LOCK_BUSY)
                BIF_RET(am_false);
        }

        res = erts_set_group_leader(rp, BIF_ARG_1);

        if (rp != BIF_P)
            erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

        BIF_RET(res ? am_true : am_badarg);
    }

    if (is_external_pid(BIF_ARG_2)) {
	DistEntry *dep;
	int code;
	ErtsDSigData dsd;
	dep = external_pid_dist_entry(BIF_ARG_2);
	ERTS_ASSERT(dep);
	if(dep == erts_this_dist_entry)
	    BIF_ERROR(BIF_P, BADARG);

	code = erts_dsig_prepare(&dsd, dep, BIF_P, ERTS_PROC_LOCK_MAIN,
				 ERTS_DSP_NO_LOCK, 0, 1);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    BIF_RET(am_true);
	case ERTS_DSIG_PREP_PENDING:
	case ERTS_DSIG_PREP_CONNECTED:
	    code = erts_dsig_send_group_leader(&dsd, BIF_ARG_1, BIF_ARG_2);
	    if (code == ERTS_DSIG_SEND_YIELD)
		ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
	    BIF_RET(am_true);
	default:
	    ERTS_ASSERT(! "Invalid dsig prepare result");
	}
    }

    BIF_RET(am_badarg);
}

BIF_RETTYPE system_flag_2(BIF_ALIST_2)    
{
    Sint n;

    if (BIF_ARG_1 == am_multi_scheduling) {
	if (BIF_ARG_2 == am_block || BIF_ARG_2 == am_unblock
	    || BIF_ARG_2 == am_block_normal || BIF_ARG_2 == am_unblock_normal) {
	    int block = (BIF_ARG_2 == am_block
			 || BIF_ARG_2 == am_block_normal);
	    int normal = (BIF_ARG_2 == am_block_normal
			  || BIF_ARG_2 == am_unblock_normal);
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
    } else if (BIF_ARG_1 == am_schedulers_online) {
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
    } else if (BIF_ARG_1 == am_fullsweep_after) {
	Uint16 nval;
	Uint oval;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	nval = (n > (Sint) ((Uint16) -1)) ? ((Uint16) -1) : ((Uint16) n);
	oval = (Uint) erts_atomic32_xchg_nob(&erts_max_gen_gcs,
						 (erts_aint32_t) nval);
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_heap_size) {
	int oval = H_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();

	H_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_thr_progress_unblock();
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_bin_vheap_size) {
	int oval = BIN_VH_MIN_SIZE;

	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}

	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();

	BIN_VH_MIN_SIZE = erts_next_heap_size(n, 0);

	erts_thr_progress_unblock();
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

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

        erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
        erts_thr_progress_block();

        H_MAX_SIZE = max_heap_size;
        H_MAX_FLAGS = max_heap_flags;

        erts_thr_progress_unblock();
        erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

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
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();

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

                erts_proc_sig_clear_seq_trace_tokens(p);
	    }
	}

	erts_thr_progress_unblock();
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_scheduler_wall_time) {
	if (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false)
            BIF_TRAP1(system_flag_scheduler_wall_time_trap,
                      BIF_P, BIF_ARG_2);
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
    } else if (BIF_ARG_1 == am_time_offset
	       && ERTS_IS_ATOM_STR("finalize", BIF_ARG_2)) {
	ErtsTimeOffsetState res;
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	res = erts_finalize_time_offset();
        erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
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
	erts_aint32_t old = erts_atomic32_xchg_nob(&msacc, new);
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
	erts_aint32_t old = erts_atomic32_read_nob(&msacc);
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
	erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_sched_stat_modify(what);
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
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
    } else if (ERTS_IS_ATOM_STR("erts_alloc", BIF_ARG_1)) {
        return erts_alloc_set_dyn_param(BIF_P, BIF_ARG_2);
    }
    error:
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_internal_scheduler_wall_time_1(BIF_ALIST_1)
{
    erts_aint32_t new = BIF_ARG_1 == am_true ? 1 : 0;
    erts_aint32_t old = erts_atomic32_xchg_nob(&sched_wall_time,
                                               new);
    Eterm ref = erts_sched_wall_time_request(BIF_P, 1, new, 0, 0);
    ASSERT(is_value(ref));
    BIF_TRAP2(await_sched_wall_time_mod_trap,
              BIF_P, ref, old ? am_true : am_false);
}

/**********************************************************************/

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
    default:
	break;
    }
    BIF_RET(res);
}

Export bif_return_trap_export;

void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(BIF_ALIST))
{
    int i;
    sys_memset((void *) ep, 0, sizeof(Export));
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	ep->addressv[i] = ep->beam;
    }
    ep->info.mfa.module = m;
    ep->info.mfa.function = f;
    ep->info.mfa.arity = a;
    ep->beam[0] = BeamOpCodeAddr(op_apply_bif);
    ep->beam[1] = (BeamInstr) bif;
}

void erts_init_bif(void)
{
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

    erts_init_trap_export(&await_exit_trap, am_erts_internal,
                          am_await_exit, 0, erts_internal_await_exit_trap);

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
    await_port_send_result_trap
	= erts_export_put(am_erts_internal, am_await_port_send_result, 3);
    system_flag_scheduler_wall_time_trap
        = erts_export_put(am_erts_internal, am_system_flag_scheduler_wall_time, 1);
    await_sched_wall_time_mod_trap
        = erts_export_put(am_erts_internal, am_await_sched_wall_time_modifications, 2);
    await_msacc_mod_trap
	= erts_export_put(am_erts_internal, am_await_microstate_accounting_modifications, 3);

    erts_atomic32_init_nob(&sched_wall_time, 0);
    erts_atomic32_init_nob(&msacc, ERTS_MSACC_IS_ENABLED());
}

/*
 * Scheduling of BIFs via NifExport...
 */
#define ERTS_WANT_NFUNC_SCHED_INTERNALS__
#include "erl_nfunc_sched.h"

#define ERTS_SCHED_BIF_TRAP_MARKER ((void *) (UWord) 1)

static ERTS_INLINE void
schedule(Process *c_p, Process *dirty_shadow_proc,
	 ErtsCodeMFA *mfa, BeamInstr *pc,
	 ErtsBifFunc dfunc, void *ifunc,
	 Eterm module, Eterm function,
	 int argc, Eterm *argv)
{
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(c_p));
    (void) erts_nif_export_schedule(c_p, dirty_shadow_proc,
				    mfa, pc, BeamOpCodeAddr(op_apply_bif),
				    dfunc, ifunc,
				    module, function,
				    argc, argv);
}


static BIF_RETTYPE dirty_bif_result(BIF_ALIST_1)
{
    NifExport *nep = (NifExport *) ERTS_PROC_GET_NIF_TRAP_EXPORT(BIF_P);
    erts_nif_export_restore(BIF_P, nep, BIF_ARG_1);
    BIF_RET(BIF_ARG_1);
}

static BIF_RETTYPE dirty_bif_trap(BIF_ALIST)
{
    NifExport *nep = (NifExport *) ERTS_PROC_GET_NIF_TRAP_EXPORT(BIF_P);

    /*
     * Arity and argument registers already set
     * correct by call to dirty_bif_trap()...
     */

    ASSERT(BIF_P->arity == nep->exp.info.mfa.arity);

    erts_nif_export_restore(BIF_P, nep, THE_NON_VALUE);

    BIF_P->i = (BeamInstr *) nep->func;
    BIF_P->freason = TRAP;
    return THE_NON_VALUE;
}

static BIF_RETTYPE dirty_bif_exception(BIF_ALIST_2)
{
    Eterm freason;

    ASSERT(is_small(BIF_ARG_1));

    freason = signed_val(BIF_ARG_1);

    /* Restore orig info for error and clear nif export in handle_error() */
    freason |= EXF_RESTORE_NIF;

    BIF_P->fvalue = BIF_ARG_2;

    BIF_ERROR(BIF_P, freason);
}


static BIF_RETTYPE call_bif(Process *c_p, Eterm *reg, BeamInstr *I);

BIF_RETTYPE
erts_schedule_bif(Process *proc,
		  Eterm *argv,
		  BeamInstr *i,
		  ErtsBifFunc bif,
		  ErtsSchedType sched_type,
		  Eterm mod,
		  Eterm func,
		  int argc)
{
    Process *c_p, *dirty_shadow_proc;
    ErtsCodeMFA *mfa;

    if (proc->static_flags & ERTS_STC_FLG_SHADOW_PROC) {
	dirty_shadow_proc = proc;
	c_p = proc->next;
	ASSERT(c_p->common.id == dirty_shadow_proc->common.id);
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
    else
    {
	dirty_shadow_proc = NULL;
	c_p = proc;
    }

    if (!ERTS_PROC_IS_EXITING(c_p)) {
	Export *exp;
	BifFunction dbif, ibif;
	BeamInstr *pc;

	/*
	 * dbif - direct bif
	 * ibif - indirect bif
	 */

	erts_aint32_t set, mask;
	mask = (ERTS_PSFLG_DIRTY_CPU_PROC
		| ERTS_PSFLG_DIRTY_IO_PROC);
	switch (sched_type) {
	case ERTS_SCHED_DIRTY_CPU:
	    set = ERTS_PSFLG_DIRTY_CPU_PROC;
	    dbif = bif;
	    ibif = NULL;
	    break;
	case ERTS_SCHED_DIRTY_IO:
	    set = ERTS_PSFLG_DIRTY_IO_PROC;
	    dbif = bif;
	    ibif = NULL;
	    break;
	case ERTS_SCHED_NORMAL:
	default:
	    set = 0;
	    dbif = call_bif;
	    ibif = bif;
	    break;
	}

	(void) erts_atomic32_read_bset_nob(&c_p->state, mask, set);

	if (i == NULL) {
	    ERTS_INTERNAL_ERROR("Missing instruction pointer");
	}
#ifdef HIPE
	else if (proc->flags & F_HIPE_MODE) {
	    /* Pointer to bif export in i */
	    exp = (Export *) i;
	    pc = c_p->cp;
	    mfa = &exp->info.mfa;
	}
#endif
	else if (BeamIsOpCode(*i, op_call_bif_e)) {
	    /* Pointer to bif export in i+1 */
	    exp = (Export *) i[1];
	    pc = i;
	    mfa = &exp->info.mfa;
	}
	else if (BeamIsOpCode(*i, op_apply_bif)) {
	    /* Pointer to bif in i+1, and mfa in i-3 */	    
	    pc = c_p->cp;
	    mfa = erts_code_to_codemfa(i);
	}
	else {
	    ERTS_INTERNAL_ERROR("erts_schedule_bif() called "
				"from unexpected instruction");
	}
	ASSERT(bif);

	if (argc < 0) { /* reschedule original call */
	    mod = mfa->module;
	    func = mfa->function;
	    argc = (int) mfa->arity;
	}

	schedule(c_p, dirty_shadow_proc, mfa, pc, dbif, ibif,
		 mod, func, argc, argv);
    }

    if (dirty_shadow_proc)
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);

    return THE_NON_VALUE;
}

static BIF_RETTYPE
call_bif(Process *c_p, Eterm *reg, BeamInstr *I)
{
    NifExport *nep = ERTS_I_BEAM_OP_TO_NIF_EXPORT(I);
    ErtsBifFunc bif = (ErtsBifFunc) nep->func;
    BIF_RETTYPE ret;

    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));

    nep->func = ERTS_SCHED_BIF_TRAP_MARKER;

    ASSERT(bif);

    ret = (*bif)(c_p, reg, I);

    if (is_value(ret))
	erts_nif_export_restore(c_p, nep, ret);
    else if (c_p->freason != TRAP)
	c_p->freason |= EXF_RESTORE_NIF; /* restore in handle_error() */
    else if (nep->func == ERTS_SCHED_BIF_TRAP_MARKER) {
	/* BIF did an ordinary trap... */
	erts_nif_export_restore(c_p, nep, ret);
    }
    /* else:
     *   BIF rescheduled itself using erts_schedule_bif().
     */

    return ret;
}


int
erts_call_dirty_bif(ErtsSchedulerData *esdp, Process *c_p, BeamInstr *I, Eterm *reg)
{
    BIF_RETTYPE result;
    int exiting;
    Process *dirty_shadow_proc;
    ErtsBifFunc bf;
    NifExport *nep;
#ifdef DEBUG
    Eterm *c_p_htop;
    erts_aint32_t state;

    ASSERT(!c_p->scheduler_data);
    state = erts_atomic32_read_nob(&c_p->state);
    ASSERT((state & ERTS_PSFLG_DIRTY_RUNNING)
	   && !(state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)));
    ASSERT(esdp);

#endif

    nep = ERTS_I_BEAM_OP_TO_NIF_EXPORT(I);
    ASSERT(nep == ERTS_PROC_GET_NIF_TRAP_EXPORT(c_p));

    nep->func = ERTS_SCHED_BIF_TRAP_MARKER;

    bf = (ErtsBifFunc) I[1];

    erts_atomic32_read_band_mb(&c_p->state, ~(ERTS_PSFLG_DIRTY_CPU_PROC
						  | ERTS_PSFLG_DIRTY_IO_PROC));

    dirty_shadow_proc = erts_make_dirty_shadow_proc(esdp, c_p);

    dirty_shadow_proc->freason = c_p->freason;
    dirty_shadow_proc->fvalue = c_p->fvalue;
    dirty_shadow_proc->ftrace = c_p->ftrace;
    dirty_shadow_proc->cp = c_p->cp;
    dirty_shadow_proc->i = c_p->i;

#ifdef DEBUG
    c_p_htop = c_p->htop;
#endif

    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);

    result = (*bf)(dirty_shadow_proc, reg, I);

    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

    ASSERT(c_p_htop == c_p->htop);
    ASSERT(dirty_shadow_proc->static_flags & ERTS_STC_FLG_SHADOW_PROC);
    ASSERT(dirty_shadow_proc->next == c_p);

    exiting = ERTS_PROC_IS_EXITING(c_p);

    if (!exiting) {
	if (is_value(result))
	    schedule(c_p, dirty_shadow_proc, NULL, NULL, dirty_bif_result,
		     NULL, am_erts_internal, am_dirty_bif_result, 1, &result);
	else if (dirty_shadow_proc->freason != TRAP) {
	    Eterm argv[2];
	    ASSERT(dirty_shadow_proc->freason <= MAX_SMALL);
	    argv[0] = make_small(dirty_shadow_proc->freason);
	    argv[1] = dirty_shadow_proc->fvalue;
	    schedule(c_p, dirty_shadow_proc, NULL, NULL,
		     dirty_bif_exception, NULL, am_erts_internal,
		     am_dirty_bif_exception, 2, argv);
	}
	else if (nep->func == ERTS_SCHED_BIF_TRAP_MARKER) {
	    /* Dirty BIF did an ordinary trap... */
	    ASSERT(!(erts_atomic32_read_nob(&c_p->state)
		     & (ERTS_PSFLG_DIRTY_CPU_PROC|ERTS_PSFLG_DIRTY_IO_PROC)));
	    schedule(c_p, dirty_shadow_proc, NULL, NULL,
		     dirty_bif_trap, (void *) dirty_shadow_proc->i,
		     am_erts_internal, am_dirty_bif_trap,
		     dirty_shadow_proc->arity, reg);
	}
	/* else:
	 *   BIF rescheduled itself using erts_schedule_bif().
	 */
	c_p->freason = dirty_shadow_proc->freason;
	c_p->fvalue = dirty_shadow_proc->fvalue;
	c_p->ftrace = dirty_shadow_proc->ftrace;
	c_p->cp = dirty_shadow_proc->cp;
	c_p->i = dirty_shadow_proc->i;
	c_p->arity = dirty_shadow_proc->arity;
    }

    erts_flush_dirty_shadow_proc(dirty_shadow_proc);

    return exiting;
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
