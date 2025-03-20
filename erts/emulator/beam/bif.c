/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#define WANT_NONBLOCKING
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
#include "erl_thr_progress.h"
#define ERTS_PTAB_WANT_BIF_IMPL__
#include "erl_ptab.h"
#include "erl_bits.h"
#include "erl_bif_unique.h"
#include "erl_map.h"
#include "erl_msacc.h"
#include "erl_proc_sig_queue.h"
#include "erl_fun.h"
#include "ryu.h"
#include "jit/beam_asm.h"
#include "erl_global_literals.h"
#include "beam_load.h"
#include "beam_common.h"
#include "dtrace-wrapper.h"

Export *erts_await_result;
static Export await_exit_trap;
static Export* flush_monitor_messages_trap = NULL;
static Export* set_cpu_topology_trap = NULL;
static Export* await_port_send_result_trap = NULL;
Export* erts_format_cpu_topology_trap = NULL;
#ifndef DEBUG
static
#endif
Export dsend_continue_trap_export;
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

    ERTS_SET_DEFAULT_SPAWN_OPTS(&so);

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

/*
 * link/1 and link/2
 */

static ERTS_INLINE int
link_modify_flags(Uint32 *flagsp, Uint32 add_flags, Uint32 rm_flags)
{
    Uint32 fs, fs_before;

    fs = fs_before = *flagsp;

    fs |= add_flags;
    fs &= ~rm_flags;

    *flagsp = fs;

    if (!(fs_before & ERTS_ML_FLG_PRIO_ML) & !!(fs & ERTS_ML_FLG_PRIO_ML))
        return 1;

    if (!!(fs_before & ERTS_ML_FLG_PRIO_ML) & !(fs & ERTS_ML_FLG_PRIO_ML))
        return -1;

    return 0;
}

/* create a link to the process */
static BIF_RETTYPE link_opt(Process *c_p, Eterm other, Eterm opts)
{
    BIF_RETTYPE ret_val;
    int prio_change = 0;
    Uint32 add_flags = 0, rm_flags = 0;

    if (ERTS_IS_P_TRACED_FL(c_p, F_TRACE_PROCS)) {
	trace_proc(c_p, ERTS_PROC_LOCK_MAIN, c_p, am_link, other);
    }

    add_flags = erts_link_opts(opts, &rm_flags);
    if (add_flags == (Uint32) ~0) {
        c_p->fvalue = am_badopt;
        ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG | EXF_HAS_EXT_INFO);
        goto done;
    }

    ERTS_BIF_PREP_RET(ret_val, am_true); /* Prepare for success... */

    if (is_internal_pid(other)) {
        int created;
        ErtsLink *lnk, *rlnk;

        if (c_p->common.id == other)
            goto done;

        if (!erts_proc_lookup(other) && !(c_p->flags & F_TRAP_EXIT))
            goto res_no_proc;

        lnk = erts_link_internal_tree_lookup_create(&ERTS_P_LINKS(c_p),
                                                    &created,
                                                    ERTS_LNK_TYPE_PROC,
                                                    other);
        prio_change = link_modify_flags(&lnk->flags, add_flags, rm_flags);
        if (!created) {
            ErtsILink *ilnk = (ErtsILink *) lnk;
            if (!ilnk->unlinking) {
                goto done;
            }
            ilnk->unlinking = 0;
        }

        rlnk = erts_link_internal_create(ERTS_LNK_TYPE_PROC, c_p->common.id);

        if (!erts_proc_sig_send_link(&c_p->common, c_p->common.id,
                                    other, rlnk)) {
            erts_proc_sig_send_link_exit(NULL, other, rlnk, am_noproc, NIL);
        }

        goto done;
    }

    if (is_internal_port(other)) {
        int created;
        ErtsLink *lnk, *rlnk;
        Eterm ref;
        Eterm *refp;
	Port *prt = erts_port_lookup(other,
				     (erts_port_synchronous_ops
				      ? ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP
				      : ERTS_PORT_SFLGS_INVALID_LOOKUP));
	if (!prt && !(c_p->flags & F_TRAP_EXIT))
            goto res_no_proc;

        lnk = erts_link_internal_tree_lookup_create(&ERTS_P_LINKS(c_p),
                                                    &created,
                                                    ERTS_LNK_TYPE_PORT,
                                                    other);
        prio_change = link_modify_flags(&lnk->flags, add_flags, rm_flags);
        if (!created) {
            ErtsILink *ilnk = (ErtsILink *) lnk;
            if (!ilnk->unlinking) {
                goto done;
            }
            ilnk->unlinking = 0;
        }

        rlnk = erts_link_internal_create(ERTS_LNK_TYPE_PROC, c_p->common.id);
        refp = erts_port_synchronous_ops ? &ref : NULL;

        switch (!prt
                ? ERTS_PORT_OP_BADARG
                : erts_port_link(c_p, prt, rlnk, refp)) {
        case ERTS_PORT_OP_BADARG:
            erts_proc_sig_send_link_exit(NULL, other, rlnk, am_noproc, NIL);
            break;
        case ERTS_PORT_OP_DROPPED:
        case ERTS_PORT_OP_SCHEDULED:
            if (refp) {
                ASSERT(is_internal_ordinary_ref(ref));
                BIF_TRAP3(await_port_send_result_trap, c_p, ref, am_true, am_true);
            }
        default:
            break;
        }
	goto done;
    }
    else if (is_external_port(other)
	     && external_port_dist_entry(other) == erts_this_dist_entry) {
        ErtsLink *lnk;
        int created;

        if (!(c_p->flags & F_TRAP_EXIT))
            goto res_no_proc;

        lnk = erts_link_external_tree_lookup_create(&ERTS_P_LINKS(c_p),
                                                    &created,
                                                    ERTS_LNK_TYPE_DIST_PORT,
                                                    c_p->common.id,
                                                    other);
        prio_change = link_modify_flags(&lnk->flags, add_flags, rm_flags);
        if (!created) {
            ErtsELink *elnk = erts_link_to_elink(lnk);
            if (!elnk->unlinking) {
                goto done;
            }
            elnk->unlinking = 0;
        }

        erts_proc_sig_send_dist_link_exit(erts_this_dist_entry, other,
                                          c_p->common.id, NULL, NULL,
                                          am_noproc, NIL);
        goto done;
    }

    if (is_external_pid(other)) {
        ErtsELink *elnk, *relnk, *pelnk;
        int created, replace;
        DistEntry *dep;
        ErtsLink *lnk, *rlnk;
        int code;
        ErtsDSigSendContext ctx;

        dep = external_pid_dist_entry(other);
        if (dep == erts_this_dist_entry && !(c_p->flags & F_TRAP_EXIT))
            goto res_no_proc;

        lnk = erts_link_external_tree_lookup_create(&ERTS_P_LINKS(c_p),
                                                    &created,
                                                    ERTS_LNK_TYPE_DIST_PROC,
                                                    c_p->common.id,
                                                    other);
        prio_change = link_modify_flags(&lnk->flags, add_flags, rm_flags);
        elnk = erts_link_to_elink(lnk);

        if (dep == erts_this_dist_entry) {
            elnk->unlinking = 0;
            erts_proc_sig_send_dist_link_exit(erts_this_dist_entry, other,
                                              c_p->common.id, NULL, NULL,
                                              am_noproc, NIL);
            goto done;
        }

        if (created) {
            pelnk = NULL;
            relnk = NULL;
            rlnk = NULL;
        }
        else {
            if (!elnk->unlinking) {
                goto done; /* Already present... */
            }
            /*
             * We need to replace the link if the connection has changed.
             * Prepare a link...
             */
            pelnk = (ErtsELink *) erts_link_external_create(ERTS_LNK_TYPE_DIST_PROC,
                                                            c_p->common.id,
                                                            other);
            pelnk->ld.proc.flags |= add_flags;
            pelnk->ld.proc.flags &= ~rm_flags;
            ASSERT(eq(pelnk->ld.proc.other.item, other));
            ASSERT(pelnk->ld.dist.other.item == c_p->common.id);
            /* Release pelnk if not used as replacement... */
            relnk = pelnk;
            rlnk = &pelnk->ld.proc;
        }
        replace = 0;

        ASSERT(eq(elnk->ld.proc.other.item, other));
        ASSERT(elnk->ld.dist.other.item == c_p->common.id);

        code = erts_dsig_prepare(&ctx, dep, c_p,
                                 ERTS_PROC_LOCK_MAIN,
                                 ERTS_DSP_RLOCK, 0, 1, 1);
        switch (code) {
        case ERTS_DSIG_PREP_NOT_ALIVE:
        case ERTS_DSIG_PREP_NOT_CONNECTED:
            if (created || elnk->unlinking) {
                if (elnk->unlinking) {
                    /*
                     * Currently unlinking an old link from an old connection; replace
                     * old link with the prepared one...
                     */
                    relnk = NULL;
                    rlnk = lnk;
                    elnk = pelnk;
                    replace = !0;
                }
                erts_link_set_dead_dist(&elnk->ld.dist, dep->sysname);
            }
            erts_proc_sig_send_link_exit_noconnection(&elnk->ld.dist);
            break;

        case ERTS_DSIG_PREP_PENDING:
        case ERTS_DSIG_PREP_CONNECTED: {
            /*
             * We have a connection (or a pending connection).
             * Setup link and enqueue link signal.
             */
            if (created
                || (elnk->unlinking
                    && elnk->dist->connection_id != ctx.connection_id)) {
                int inserted;
                if (!created) {
                    /*
                     * Currently unlinking an old link from an old connection; replace
                     * old link with the prepared one...
                     */
                    rlnk = lnk;
                    if (erts_link_dist_delete(&elnk->ld.dist))
                        relnk = elnk;
                    else
                        relnk = NULL;
                    elnk = pelnk;
                    replace = !0;
                }
                inserted = erts_link_dist_insert(&elnk->ld.dist, dep->mld);
                ASSERT(inserted); (void)inserted;
            }

            erts_de_runlock(dep);

            code = erts_dsig_send_link(&ctx, c_p->common.id, other);
            if (code == ERTS_DSIG_SEND_YIELD)
                ERTS_BIF_YIELD_RETURN(c_p, am_true);
            ASSERT(code == ERTS_DSIG_SEND_OK);
            break;
        }
        default:
            ERTS_ASSERT(! "Invalid dsig prepare result");
        }

        if (replace) {
            ASSERT(pelnk);
            erts_link_tree_replace(&ERTS_P_LINKS(c_p), rlnk, &pelnk->ld.proc);
        }

        if (relnk)
            erts_link_release_both(&relnk->ld);
        else if (rlnk)
            erts_link_release(rlnk);

        elnk->unlinking = 0;

        goto done;
    }

    ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG);

done:

    if (prio_change) {
        if (prio_change > 0)
            erts_proc_sig_prio_item_added(c_p, ERTS_PRIO_ITEM_TYPE_LINK);
        else
            erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_LINK);
    }

    return ret_val;

res_no_proc: {
        /*
         * This behaviour is *really* sad but link/1 has
         * behaved like this for ages (and this behaviour is
         * actually documented)... :'-(
         *
         * The proper behavior would have been to
         * send calling process an exit signal..
         */
        ERTS_BIF_PREP_ERROR(ret_val, c_p, EXC_NOPROC);

        return ret_val;
    }
}

BIF_RETTYPE link_1(BIF_ALIST_1)
{
    return link_opt(BIF_P, BIF_ARG_1, NIL);
}

BIF_RETTYPE link_2(BIF_ALIST_2)
{
    return link_opt(BIF_P, BIF_ARG_1, BIF_ARG_2);
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

   if (ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_ALIAS)
       return am_false; /* Not a monitor (may have been...) */

   if (mon->flags & ERTS_ML_FLG_SPAWN_PENDING) {
       /*
        * Not allowed to remove this until spawn
        * operation has succeeded; restore monitor...
        */
       return am_false;
   }

   switch (mon->flags & ERTS_ML_STATE_ALIAS_MASK) {
   case ERTS_ML_STATE_ALIAS_UNALIAS: {
       ErtsMonitorData *amdp = erts_monitor_create(ERTS_MON_TYPE_ALIAS,
                                                   ref,
                                                   c_p->common.id,
                                                   NIL,
                                                   NIL,
                                                   THE_NON_VALUE);
       Uint32 add_flags = mon->flags & (ERTS_ML_STATE_ALIAS_MASK
                                        | ERTS_ML_FLG_PRIO_ALIAS);
       amdp->origin.flags |= add_flags;
       mon->flags &= ~(ERTS_ML_STATE_ALIAS_MASK | ERTS_ML_FLG_PRIO_ALIAS);
       erts_monitor_tree_replace(&ERTS_P_MONITORS(c_p), mon, &amdp->origin);
       break;
   }
   case ERTS_ML_STATE_ALIAS_ONCE:
   case ERTS_ML_STATE_ALIAS_DEMONITOR:
       /* fall through... */
   default:
       erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), mon);
       if (mon->flags & ERTS_ML_FLG_PRIO_ML)
           erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_MONITOR);
       break;
   }

   switch (ERTS_ML_GET_TYPE(mon)) {

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

   case ERTS_MON_TYPE_DIST_PORT: {
       ASSERT(is_external_port(mon->other.item));
       ASSERT(external_pid_dist_entry(mon->other.item)
              == erts_this_dist_entry);
       erts_monitor_release(mon);
       return am_true;
   }

   case ERTS_MON_TYPE_PROC:
       erts_proc_sig_send_demonitor(&c_p->common, c_p->common.id, 0, mon);
       return am_true;

   case ERTS_MON_TYPE_DIST_PROC: {
       ErtsMonitorData *mdp = erts_monitor_to_data(mon);
       Eterm to = mon->other.item;
       DistEntry *dep;
       int code = ERTS_DSIG_SEND_OK;
       int deleted;
       ErtsDSigSendContext ctx;

       ASSERT(is_external_pid(to) || is_node_name_atom(to));

       if (is_external_pid(to))
           dep = external_pid_dist_entry(to);
       else /* Monitoring a name at node to */
           dep = erts_sysname_to_connected_dist_entry(to);

       if (!dep || dep == erts_this_dist_entry) {
           erts_monitor_release(mon);
           return am_false;
       }

       code = erts_dsig_prepare(&ctx, dep, c_p, ERTS_PROC_LOCK_MAIN,
                                ERTS_DSP_RLOCK, 0, 1, 0);

       deleted = erts_monitor_dist_delete(&mdp->u.target);

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
           code = erts_dsig_send_demonitor(&ctx, c_p->common.id,
                                           watched, mdp->ref);
           break;
       }

       default:
           ERTS_INTERNAL_ERROR("invalid result from erts_dsig_prepare()");
           break;
       }

       if (deleted)
           erts_monitor_release(&mdp->u.target);

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

Uint32
erts_link_opts(Eterm opts, Uint32 *rm_oflagsp)
{
    Uint32 add_oflags = 0;
    Uint32 rm_oflags = ERTS_ML_FLG_PRIO_ML;

    while (is_list(opts)) {
        Eterm *cons, opt;
        cons = list_val(opts);
        opt = CAR(cons);
        switch (opt) {
        case am_priority:
            add_oflags |= ERTS_ML_FLG_PRIO_ML;
            rm_oflags &= ~ERTS_ML_FLG_PRIO_ML;
            break;
        default:
            return (Uint32) ~0;
        }
        opts = CDR(cons);
    }
    if (is_not_nil(opts))
        return (Uint32) ~0;
    if (rm_oflagsp)
        *rm_oflagsp = rm_oflags;
    return add_oflags;
}

Uint32
erts_monitor_opts(Eterm opts, Eterm *tag)
{
    Uint32 add_oflags = 0;

    *tag = THE_NON_VALUE;

    while (is_list(opts)) {
        Eterm *tpl, *cons, opt;
        cons = list_val(opts);
        opt = CAR(cons);
        switch (opt) {
        case am_priority:
            add_oflags |= ERTS_ML_FLG_PRIO_ML;
            break;
        default:
            if (is_not_tuple(opt))
                return (Uint32) ~0;
            tpl = tuple_val(opt);
            switch (arityval(tpl[0])) {
            case 2:
                switch (tpl[1]) {
                case am_alias:
                    add_oflags &= ~ERTS_ML_STATE_ALIAS_MASK;
                    switch (tpl[2]) {
                    case am_explicit_unalias:
                        add_oflags |= ERTS_ML_STATE_ALIAS_UNALIAS;
                        break;
                    case am_demonitor:
                        add_oflags |= ERTS_ML_STATE_ALIAS_DEMONITOR;
                        break;
                    case am_reply_demonitor:
                        add_oflags |= ERTS_ML_STATE_ALIAS_ONCE;
                        break;
                    default:
                        return (Uint32) ~0;
                    }
                    break;
                case am_tag:
                    *tag = tpl[2];
                    break;
                default:
                    return (Uint32) ~0;
                }
                break;
            default:
                return (Uint32) ~0;
            }
            break;
        }
	opts = CDR(cons);
    }
    if (is_not_nil(opts))
        return (Uint32) ~0;
    return add_oflags;
}

static BIF_RETTYPE monitor(Process *c_p, Eterm type, Eterm target,
                           Uint32 add_oflags, Eterm tag) 
{
    Eterm ref, id, name;
    ErtsMonitorData *mdp;
    BIF_RETTYPE ret_val;

    ref = ((add_oflags & ERTS_ML_STATE_ALIAS_MASK)
           ? erts_make_pid_ref(c_p)
           : erts_make_ref(c_p));

    ERTS_BIF_PREP_RET(ret_val, ref); /* Prepare for success... */
    
    if (type == am_process) {
        DistEntry *dep;
        int byname;

        if (is_internal_pid(target)) {
            name = NIL;
            id = target;

        local_process:

            if (id != c_p->common.id) {
                mdp = erts_monitor_create(ERTS_MON_TYPE_PROC,
                                          ref, c_p->common.id,
                                          id, name, tag);
                mdp->origin.flags |= add_oflags;
                erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p),
                                         &mdp->origin);
                if (is_not_internal_pid(id)
                    || !erts_proc_sig_send_monitor(&c_p->common, c_p->common.id,
                                                &mdp->u.target, id)) {
                    erts_proc_sig_send_monitor_down(NULL, id,
                                                    &mdp->u.target,
                                                    am_noproc);
                }
            }

            goto done;
        }

        if (is_atom(target)) {
        local_named_process:
            name = target;
            id = erts_whereis_name_to_id(c_p, target);
            goto local_process;
        }

        if (is_external_pid(target)) {
            ErtsDSigSendContext ctx;
            int code;

            dep = external_pid_dist_entry(target);
            if (dep == erts_this_dist_entry) {
                mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC, ref,
                                          c_p->common.id, target,
                                          NIL, tag);
                mdp->origin.flags |= add_oflags;
                erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), &mdp->origin);
                erts_proc_sig_send_monitor_down(NULL, target,
                                                &mdp->u.target, am_noproc);
                goto done;
            }

            id = target;
            name = NIL;
            byname = 0;

        remote_process:

            mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC, ref,
                                      c_p->common.id, id, name, tag);
            mdp->origin.flags |= add_oflags;
            erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), &mdp->origin);

            code = erts_dsig_prepare(&ctx, dep,
                                     c_p, ERTS_PROC_LOCK_MAIN,
                                     ERTS_DSP_RLOCK, 0, 1, 1);
            switch (code) {
            case ERTS_DSIG_PREP_NOT_ALIVE:
            case ERTS_DSIG_PREP_NOT_CONNECTED:
                erts_monitor_set_dead_dist(&mdp->u.target, dep->sysname);
                erts_proc_sig_send_monitor_down(NULL, id,
                                                &mdp->u.target,
                                                am_noconnection);
                code = ERTS_DSIG_SEND_OK;
                break;

            case ERTS_DSIG_PREP_PENDING:
            case ERTS_DSIG_PREP_CONNECTED: {
                int inserted = erts_monitor_dist_insert(&mdp->u.target, dep->mld);
                ASSERT(inserted); (void)inserted;
                erts_de_runlock(dep);

                code = erts_dsig_send_monitor(&ctx, c_p->common.id, target, ref);
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
                ERTS_BIF_PREP_YIELD_RETURN(ret_val, c_p, ref);
            goto done;
        }

        if (is_tuple(target)) {
            Eterm *tpl = tuple_val(target);
            if (arityval(tpl[0]) != 2)
                goto badarg;
            if (is_not_atom(tpl[1]) || is_not_atom(tpl[2]))
                goto badarg;
            if (tpl[2] != am_Noname && !erts_is_this_node_alive())
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
    else if (type == am_port) {

        if (is_internal_port(target)) {
            Port *prt;
            name = NIL;
            id = target;
        local_port:
            mdp = erts_monitor_create(ERTS_MON_TYPE_PORT, ref,
                                      c_p->common.id, id,
                                      name, tag);
            mdp->origin.flags |= add_oflags;
            erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), &mdp->origin);
            prt = erts_port_lookup(id, ERTS_PORT_SFLGS_INVALID_LOOKUP);
            if (!prt || erts_port_monitor(c_p, prt, &mdp->u.target) == ERTS_PORT_OP_DROPPED) {
                erts_proc_sig_send_monitor_down(prt ? &prt->common : NULL, id,
                                                &mdp->u.target, am_noproc);
            }
            goto done;
        }

        if (is_atom(target)) {
        local_named_port:
            name = target;
            id = erts_whereis_name_to_id(c_p, target);
            goto local_port;
        }

        if (is_external_port(target)) {
            if (erts_this_dist_entry == external_port_dist_entry(target)) {
                mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PORT, ref,
                                          c_p->common.id, target,
                                          NIL, tag);
                mdp->origin.flags |= add_oflags;
                erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), &mdp->origin);
                erts_proc_sig_send_monitor_down(NULL, target, &mdp->u.target, am_noproc);
                goto done;
            }
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
    else if (type == am_time_offset) {

        if (target != am_clock_service)
            goto badarg;
        mdp = erts_monitor_create(ERTS_MON_TYPE_TIME_OFFSET,
                                  ref, c_p->common.id,
                                  am_clock_service, NIL, tag);
        mdp->origin.flags |= add_oflags;
        erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), &mdp->origin);

	erts_monitor_time_offset(&mdp->u.target);

        goto done;
    } else {
        c_p->fvalue = am_badtype;
        BIF_ERROR(c_p, BADARG | EXF_HAS_EXT_INFO);
    }

badarg:

    ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG);
    
done:

    if (add_oflags & ERTS_ML_FLG_PRIO_ML)
        erts_proc_sig_prio_item_added(c_p, ERTS_PRIO_ITEM_TYPE_MONITOR);

    return ret_val;
}

BIF_RETTYPE monitor_2(BIF_ALIST_2)
{
    return monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, (Uint32) 0, THE_NON_VALUE);
}

BIF_RETTYPE monitor_3(BIF_ALIST_3)
{
    Eterm tag;
    Uint32 oflags = erts_monitor_opts(BIF_ARG_3, &tag);
    if (oflags == (Uint32) ~0) {
        BIF_P->fvalue = am_badopt;
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
    }
    return monitor(BIF_P, BIF_ARG_1, BIF_ARG_2, oflags, tag);
}


/**********************************************************************/
/* this is a combination of the spawn and link BIFs */

BIF_RETTYPE spawn_link_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;
    Eterm tmp_heap[2];

    ERTS_SET_DEFAULT_SPAWN_OPTS(&so);

    so.flags |= SPO_LINK;
    so.opts = CONS(&tmp_heap[0], am_link, so.opts);

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

BIF_RETTYPE spawn_opt_4(BIF_ALIST_4)
{
    ErlSpawnOpts so;
    Eterm pid;
    Eterm res;
    int opts_error;

    /*
     * Fail order:
     * - Bad types
     * - Bad options
     */
    opts_error = erts_parse_spawn_opts(&so, BIF_ARG_4, NULL, 0);
    if (opts_error) {
        Sint arity;
        BIF_P->fvalue = am_badopt;
        if (is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2))
            BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
        arity = erts_list_length(BIF_ARG_3);
        if (arity < 0)
            BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
        if (arity > MAX_SMALL)
            BIF_ERROR(BIF_P, SYSTEM_LIMIT | EXF_HAS_EXT_INFO);
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
    }

    /*
     * Spawn the process.
     */
    so.opts = BIF_ARG_4;
    so.tag = am_spawn_reply;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
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

BIF_RETTYPE erts_internal_spawn_request_4(BIF_ALIST_4)
{
    ErlSpawnOpts so;
    Eterm *hp;
    Sint arity;
    int opts_error;
    Eterm tag, tmp, error;

    if (!is_atom(BIF_ARG_1))
        goto badarg;
    if (!is_atom(BIF_ARG_2))
        goto badarg;
    arity = erts_list_length(BIF_ARG_3);
    if (arity < 0)
        goto badarg;

    /*
     * Fail order:
     * - Bad types
     * - Bad options
     */
    opts_error = erts_parse_spawn_opts(&so, BIF_ARG_4, &tag, !0);
    if (arity > MAX_SMALL)
        goto system_limit;
    if (opts_error) {
        if (opts_error > 0) {
            /* Return `badopt`, meaning that the Options argument
             * is not a proper list. */
            BIF_RET(am_badopt);
        }
        goto badopt;
    }

    hp = HAlloc(BIF_P, 4 + 4 + 2);

    /* Make argument list for erts_internal:spawn_init/1 */
    tmp = TUPLE3(hp, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3); hp += 4;
    tmp = CONS(hp, tmp, NIL); hp += 2;

    so.mfa = TUPLE3(hp, BIF_ARG_1, BIF_ARG_2, make_small(arity)); hp += 4;
    so.flags |= SPO_ASYNC;
    so.mref = THE_NON_VALUE;
    so.tag = tag;
    so.opts = BIF_ARG_4;

    /*
     * Spawn the process.
     */
    tmp = erl_create_process(BIF_P, am_erts_internal, am_spawn_init, tmp, &so);
    if (is_non_value(tmp)) {
        switch (so.error_code) {
        case SYSTEM_LIMIT:
            goto system_limit;
        case BADARG:
        default:
            ERTS_INTERNAL_ERROR("Unexpected error from erl_create_process()");
            BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
        }
    }

    ASSERT(is_internal_pid(tmp));

    if (ERTS_USE_MODIFIED_TIMING()) {
	BIF_TRAP2(erts_delay_trap, BIF_P, so.mref, ERTS_MODIFIED_TIMING_DELAY);
    }
    else {
	BIF_RET(so.mref);
    }

badarg:
    BIF_RET(am_badarg);
system_limit:
    error = am_system_limit;
    goto send_error;
badopt:
    error = am_badopt;
    /* fall through... */
send_error: {
        Eterm ref = erts_make_ref(BIF_P);
        if (!(so.flags & SPO_NO_EMSG))
            erts_send_local_spawn_reply(BIF_P, ERTS_PROC_LOCK_MAIN, NULL,
                                        tag, ref, error, am_undefined);
        BIF_RET(ref);
    }
    
}

BIF_RETTYPE spawn_request_abandon_1(BIF_ALIST_1)
{
    ErtsMonitor *omon;

    if (is_not_internal_ref(BIF_ARG_1)) {
        if (is_not_ref(BIF_ARG_1))
            BIF_ERROR(BIF_P, BADARG);
        /* Not an outstanding spawn_request of this process... */
        BIF_RET(am_false);
    }

    omon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P), BIF_ARG_1);
    if (!omon
        || ((omon->flags & (ERTS_ML_FLG_SPAWN_PENDING
                            | ERTS_ML_FLG_SPAWN_ABANDONED))
            != ERTS_ML_FLG_SPAWN_PENDING)) {
        /* Not an outstanding spawn_request of this process... */
        BIF_RET(am_false);
    }

    ASSERT(erts_monitor_is_origin(omon));

    if (omon->flags & ERTS_ML_FLG_PRIO_ML) {
        omon->flags &= ~ERTS_ML_FLG_PRIO_ML;
        erts_proc_sig_prio_item_deleted(BIF_P, ERTS_PRIO_ITEM_TYPE_MONITOR);
    }

    if (omon->flags & ERTS_ML_FLG_SPAWN_LINK) {
        /* Leave it for reply... */
        omon->flags |= ERTS_ML_FLG_SPAWN_ABANDONED;
    }
    else {
        /* We don't need it anymore; remove it... */
        ErtsMonitorData *mdp;
        erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), omon);
        mdp = erts_monitor_to_data(omon);
        if (erts_monitor_dist_delete(&mdp->u.target))
            erts_monitor_release_both(mdp);
        else
            erts_monitor_release(omon);
    }
    BIF_RET(am_true);
}

  
/**********************************************************************/

static ERTS_INLINE void
unlink_clear_prio(Process *c_p, Uint32 *flagsp)
{
    if ((*flagsp) & ERTS_ML_FLG_PRIO_ML) {
        *flagsp &= ~ERTS_ML_FLG_PRIO_ML;
        erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_LINK);
    }
}

/* remove a link from a process */
BIF_RETTYPE unlink_1(BIF_ALIST_1)
{

    if (ERTS_IS_P_TRACED_FL(BIF_P, F_TRACE_PROCS)) {
        trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN,
                   BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_internal_pid(BIF_ARG_1)) {
        ErtsILink *ilnk;
        ilnk = (ErtsILink *) erts_link_tree_lookup(ERTS_P_LINKS(BIF_P),
                                                   BIF_ARG_1);
        ASSERT(!ilnk || !(ilnk->link.flags & ERTS_ML_FLG_PRIO_ML)
               || !ilnk->unlinking);
        if (ilnk && !ilnk->unlinking) {
            Uint64 id = erts_proc_sig_send_unlink(&BIF_P->common,
                                                  BIF_P->common.id,
                                                  &ilnk->link);
            unlink_clear_prio(BIF_P, &ilnk->link.flags);
            if (id)
                ilnk->unlinking = id;
            else {
                erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), &ilnk->link);
                erts_link_internal_release(&ilnk->link);
            }
        }
        BIF_RET(am_true);
    }

    if (is_internal_port(BIF_ARG_1)) {
        ErtsILink *ilnk;
        ilnk = (ErtsILink *) erts_link_tree_lookup(ERTS_P_LINKS(BIF_P),
                                                   BIF_ARG_1);

        ASSERT(!ilnk || !(ilnk->link.flags & ERTS_ML_FLG_PRIO_ML)
               || !ilnk->unlinking);
	if (ilnk && !ilnk->unlinking) {
            Eterm ref;
            Eterm *refp = erts_port_synchronous_ops ? &ref : NULL;
            ErtsPortOpResult res = ERTS_PORT_OP_DROPPED;
	    Port *prt;

            unlink_clear_prio(BIF_P, &ilnk->link.flags);

	    /* Send unlink signal */
	    prt = erts_port_lookup(BIF_ARG_1, ERTS_PORT_SFLGS_DEAD);
	    if (!prt) {
                erts_link_tree_delete(&ERTS_P_LINKS(BIF_P), &ilnk->link);
                erts_link_internal_release(&ilnk->link);
            }
            else {
                ErtsSigUnlinkOp *sulnk;

                sulnk = erts_proc_sig_make_unlink_op(&BIF_P->common,
                                                     BIF_P->common.id);
                ilnk->unlinking = sulnk->id;
#ifdef DEBUG
		ref = NIL;
#endif
		res = erts_port_unlink(BIF_P, prt, sulnk, refp);
	    }
            if (refp && res == ERTS_PORT_OP_SCHEDULED) {
                ASSERT(is_internal_ordinary_ref(ref));
                BIF_TRAP3(await_port_send_result_trap, BIF_P, ref, am_true, am_true);
            }
	}

	BIF_RET(am_true);
    }

    if (is_external_pid(BIF_ARG_1)) {
        ErtsLink *lnk;
        ErtsELink *elnk;
        DistEntry *dep;
        Uint64 unlink_id;
	int code;
	ErtsDSigSendContext ctx;

	dep = external_pid_dist_entry(BIF_ARG_1);
	if (dep == erts_this_dist_entry)
	    BIF_RET(am_true);

        lnk = erts_link_tree_lookup(ERTS_P_LINKS(BIF_P), BIF_ARG_1);
        if (!lnk)
            BIF_RET(am_true);

        elnk = erts_link_to_elink(lnk);

        ASSERT(!(lnk->flags & ERTS_ML_FLG_PRIO_ML) || !elnk->unlinking);

        if (elnk->unlinking)
            BIF_RET(am_true);

        unlink_clear_prio(BIF_P, &lnk->flags);

        unlink_id = erts_proc_sig_new_unlink_id(&BIF_P->common);
        elnk->unlinking = unlink_id;

	code = erts_dsig_prepare(&ctx, dep, BIF_P, ERTS_PROC_LOCK_MAIN,
				 ERTS_DSP_NO_LOCK, 0, 1, 0);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    BIF_RET(am_true);
	case ERTS_DSIG_PREP_PENDING:
	case ERTS_DSIG_PREP_CONNECTED:
            /*
             * Do not send unlink signal on another connection than
             * the one which the link was set up on.
             */
            if (elnk->dist->connection_id == ctx.connection_id) {
                code = erts_dsig_send_unlink(&ctx, BIF_P->common.id, BIF_ARG_1,
                                             unlink_id);
                if (code == ERTS_DSIG_SEND_YIELD)
                    ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
            }
            break;
	default:
	    ASSERT(! "Invalid dsig prepare result");
	    BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	}

        BIF_RET(am_true);
    }

    if (is_external_port(BIF_ARG_1)
        && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
        ErtsLink *lnk = erts_link_tree_lookup(ERTS_P_LINKS(BIF_P), BIF_ARG_1);
        if (lnk) {
            ErtsELink *elnk;
            unlink_clear_prio(BIF_P, &lnk->flags);
            erts_link_to_other(lnk, &elnk);
            erts_link_release_both(&elnk->ld);
        }
        BIF_RET(am_true);
    }
    /* Links to Remote ports not supported... */

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE hibernate_3(BIF_ALIST_3)
{
    Eterm module = BIF_ARG_1, function = BIF_ARG_2, args = BIF_ARG_3;
    Uint arity = 0;

    /* Check for obvious errors as a courtesy to the user; while apply/3 will
     * fail later on if there's anything wrong with the arguments (e.g. the
     * callee does not exist), we have more helpful context now than after
     * discarding the stack. */
    if (is_not_atom(module) || is_not_atom(function)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    while (is_list(args) && arity <= MAX_ARG) {
        args = CDR(list_val(args));
        arity++;
    }

    if (is_not_nil(args)) {
        if (arity > MAX_ARG) {
            BIF_ERROR(BIF_P, SYSTEM_LIMIT);
        }

        BIF_ERROR(BIF_P, BADARG);
    }

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_hibernate)) {
        ErtsCodeMFA cmfa = { module, function, arity };
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(mfa_buf, DTRACE_TERM_BUF_SIZE);
        dtrace_fun_decode(BIF_P, &cmfa, process_name, mfa_buf);
        DTRACE2(process_hibernate, process_name, mfa_buf);
    }
#endif

    /* Discard our execution state and prepare to resume with apply/3 after
     * waking up from hibernation.
     *
     * Note that BIF_P->current has already been set to hibernate/3 as this is
     * a heavy BIF. */
    BIF_P->stop = BIF_P->hend - CP_SIZE;
    BIF_P->return_trace_frames = 0;
    BIF_P->catches = 0;

    switch(erts_frame_layout) {
    case ERTS_FRAME_LAYOUT_RA:
        ASSERT(BIF_P->stop[0] == make_cp(beam_normal_exit));
        break;
    case ERTS_FRAME_LAYOUT_FP_RA:
        FRAME_POINTER(BIF_P) = &BIF_P->stop[0];
        ASSERT(BIF_P->stop[0] == make_cp(NULL));
        ASSERT(BIF_P->stop[1] == make_cp(beam_normal_exit));
        break;
    }

    /* Normally, the X register array is filled when trapping out. We do NOT do
     * this here as there is special magic involved when trapping out after
     * hibernation; `erts_hibernate` populates the process' argument registers
     * and then the BIF epilogue jumps straight into do_schedule. */
    erts_hibernate(BIF_P, BIF__ARGS, 3);
    BIF_TRAP_CODE_PTR(BIF_P, beam_run_process, 3);
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
 * This is like error/1, except that the given 'args' and 'error_info'
 * will be included in the stacktrace.
 */

BIF_RETTYPE error_3(BIF_ALIST_3)
{
    Eterm error_info = THE_NON_VALUE;
    Eterm list = BIF_ARG_3;

    while (is_list(list)) {
        Eterm* tuple;
        Eterm term = CAR(list_val(list));

        if (is_not_tuple(term)) {
            break;
        }
        tuple = tuple_val(term);
        if (arityval(tuple[0]) != 2 || tuple[1] != am_error_info) {
            break;
        }
        error_info = tuple[2];
        if (is_not_map(error_info)) {
            break;
        }
        list = CDR(list_val(list));
    }

    if (is_not_nil(list)) {
        /* Improper list. Invalidate any error info. */
        error_info = THE_NON_VALUE;
    }

    if (is_value(error_info)) {
        Eterm* hp = HAlloc(BIF_P, 4);
        BIF_P->fvalue = TUPLE3(hp, BIF_ARG_1, BIF_ARG_2, error_info);
        BIF_ERROR(BIF_P, EXC_ERROR_3);
    } else {
        /*
         * Either the option argument was an empty list, an improper
         * list, or a list with an illegal term. Fall back to error/2.
         */
        Eterm* hp = HAlloc(BIF_P, 3);
        BIF_P->fvalue = TUPLE2(hp, BIF_ARG_1, BIF_ARG_2);
        BIF_ERROR(BIF_P, EXC_ERROR_2);
    }
}

/**********************************************************************/
/*
 * This is like exactly like error/1. The only difference is
 * that Dialyzer thinks that it will return an arbitrary term.
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
 * that Dialyzer thinks that it will return an arbitrary term.
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
    size_t alloc_size;
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

    /* Check syntax of stacktrace, and count depth.  Accept anything
     * that can be returned from a stacktrace from try/catch, as well
     * as a 2-tuple with a fun as first element that the error_handler
     * may need to give us. Also allow old-style MFA three-tuples.
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
	    if (is_any_fun(tp[1])) {
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
	    if (is_any_fun(tp[1])) {
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
        if (depth == 0) {
            /*
             * For consistency with stacktraces generated
             * automatically, always include one element.
             */
            depth = 1;
        }
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
    alloc_size = sz + 4 + (2+6) * (cnt + 1);
    hp = HAlloc(c_p, alloc_size);
    hp_end = hp + alloc_size;
    s = (struct StackTrace *) hp;
    s->header = make_neg_bignum_header(sz - 1);
    s->freason = reason;
    s->pc = NULL;
    s->current = NULL;
    s->depth = 0;
    s->max_depth = 0;
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
    c_p->ftrace = TUPLE3(hp, make_big((Eterm *) s), c_p->ftrace, NIL);
    hp += 4;
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
    erts_aint32_t state = erts_atomic32_read_nob(&BIF_P->state);
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

static BIF_RETTYPE send_exit_signal_bif(Process *c_p, Eterm id, Eterm reason,
                                        Eterm opts, int exit2)
{
    BIF_RETTYPE ret_val;
    int prio = 0;

    while (is_list(opts)) {
        Eterm *cons = list_val(opts);
        switch (CAR(cons)) {
        case am_priority:
            prio = !0;
            break;
        default:
            goto badopt;
        }
	opts = CDR(cons);
    }

    if (is_not_nil(opts)) {
    badopt:
        c_p->fvalue = am_badopt;
        ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG | EXF_HAS_EXT_INFO);
        return ret_val;
    }

    /*
     * 'id' not a process id, nor a local port id is a 'badarg' error.
     */

     if (is_internal_pid(id) || is_internal_ref(id)) {
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
         erts_proc_sig_send_exit(&c_p->common, c_p->common.id, id,
                                 reason, NIL, exit2_suicide, prio);
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
     else if (is_external_pid(id) || is_external_ref(id)) {
	 DistEntry *dep = external_dist_entry(id);
	 if (dep == erts_this_dist_entry)
             ERTS_BIF_PREP_RET(ret_val, am_true); /* Old incarnation of this node... */
         else {
             int code;
             ErtsDSigSendContext ctx;

             code = erts_dsig_prepare(&ctx, dep, c_p, ERTS_PROC_LOCK_MAIN,
                                      ERTS_DSP_NO_LOCK, 0, 0, 1);

             switch (code) {
             case ERTS_DSIG_PREP_NOT_ALIVE:
             case ERTS_DSIG_PREP_NOT_CONNECTED:
                 ERTS_BIF_PREP_RET(ret_val, am_true);
                 break;
             case ERTS_DSIG_PREP_PENDING:
             case ERTS_DSIG_PREP_CONNECTED:
                 code = erts_dsig_send_exit2(&ctx, c_p->common.id, id,
                                             reason, prio);
                 switch (code) {
                 case ERTS_DSIG_SEND_YIELD:
                     ERTS_BIF_PREP_YIELD_RETURN(ret_val, c_p, am_true);
                     break;
                 case ERTS_DSIG_SEND_CONTINUE:
                     BUMP_ALL_REDS(c_p);
                     erts_set_gc_state(c_p, 0);
                     ERTS_BIF_PREP_TRAP1(ret_val,
                                         &dsend_continue_trap_export,
                                         c_p,
                                         erts_dsend_export_trap_context(c_p,
                                                                        &ctx));
                     break;
                 case ERTS_DSIG_SEND_OK:
                     ERTS_BIF_PREP_RET(ret_val, am_true);
                     break;
                 case ERTS_DSIG_SEND_TOO_LRG:
                     erts_set_gc_state(c_p, 1);
                     ERTS_BIF_PREP_ERROR(ret_val, c_p, SYSTEM_LIMIT);
                     break;
                 default:
                     ASSERT(! "Invalid dsig send exit2 result");
                     ERTS_BIF_PREP_ERROR(ret_val, c_p, EXC_INTERNAL_ERROR);
                     break;
                 }
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
    return send_exit_signal_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, NIL, !0);
}

BIF_RETTYPE exit_3(BIF_ALIST_3)
{
    return send_exit_signal_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, !0);
}

BIF_RETTYPE exit_signal_2(BIF_ALIST_2)
{
    return send_exit_signal_bif(BIF_P, BIF_ARG_1, BIF_ARG_2, NIL, 0);
}


/**********************************************************************/
/* this sets some process info- trapping exits or the error handler */


/* Handle flags common to both process_flag_2 and process_flag_3. */
static Eterm process_flag_aux(Process *c_p, int *redsp, Eterm flag, Eterm val)
{
   Eterm old_value = NIL;	/* shut up warning about use before set */
   Sint i;

   c_p->fvalue = am_badopt;

   if (redsp)
       *redsp = 1;

   if (flag == am_save_calls) {
       struct saved_calls *scb;

       c_p->fvalue = am_none;
       if (!is_small(val))
	   goto error;
       i = signed_val(val);
       if (i < 0 || i > 10000)
	   goto error;

       if (i == 0)
	   scb = NULL;
       else {
	   Uint sz = sizeof(*scb) + (i-1) * sizeof(*scb->ct);
	   scb = erts_alloc(ERTS_ALC_T_CALLS_BUF, sz);
	   scb->len = i;
	   scb->cur = 0;
	   scb->n = 0;
       }

       {
	   scb = ERTS_PROC_SET_SAVED_CALLS_BUF(c_p, scb);

	   if (((scb && i == 0) || (!scb && i != 0))) {

               /*
                * Make sure we reschedule immediately so the
                * change take effect at once.
                */
               if (!redsp) {
                   /* Executed via BIF call.. */
               via_bif:

/* BEAMASM doesn't use negative reductions for save_calls */
#ifndef BEAMASM
                   /* Adjust fcalls to match save calls setting... */
                   if (i == 0)
                       c_p->fcalls += CONTEXT_REDS; /* disabled it */
                   else
                       c_p->fcalls -= CONTEXT_REDS; /* enabled it */
#endif

                   ERTS_VBUMP_ALL_REDS(c_p);
               }
               else {
                   erts_aint32_t state;
                   /*
                    * Executed via signal handler. Try to figure
                    * out in what context we are executing...
                    */

                   state = erts_atomic32_read_nob(&c_p->state);
                   if (!(state & ERTS_PSFLG_RUNNING)) {
                       /*
                        * We are either processing signals before
                        * being executed or executing dirty. That
                        * is, no need to adjust anything...
                        */
                       *redsp = 1;
                   }
                   else {

                       /*
                        * F_DELAY_GC is currently only set when
                        * we handle signals in state running via
                        * receive helper...
                        */

                       if (!(c_p->flags & F_DELAY_GC)) {
                           *redsp = 1;
                           goto via_bif;
                       }

                       /*
                        * Executing via receive helper...
                        *
                        * We utilize the virtual reds counter
                        * in order to get correct calculation
                        * of reductions consumed when scheduling
                        * out the process...
                        */


/* BEAMASM doesn't use negative reductions for save_calls */
#ifndef BEAMASM
                        {
                            ErtsSchedulerData *esdp;

                            esdp = erts_get_scheduler_data();

                            if (i == 0) {
                                /* disabled it */
                                esdp->virtual_reds += CONTEXT_REDS;
                            } else {
                                /* enabled it */
                                esdp->virtual_reds -= CONTEXT_REDS;
                            }
                        }
#endif

                       *redsp = -1;
                   }
               }
           }
       }

       if (!scb)
	   old_value = make_small(0);
       else {
	   old_value = make_small(scb->len);
	   erts_free(ERTS_ALC_T_CALLS_BUF, (void *) scb);
       }

       ASSERT(is_immed(old_value));
       return old_value;
   }

 error:
   return am_badarg;
}

BIF_RETTYPE process_flag_2(BIF_ALIST_2)
{
   Eterm old_value;

   if (BIF_ARG_1 == am_async_dist) {
       old_value = (BIF_P->flags & F_ASYNC_DIST) ? am_true : am_false;
       if (BIF_ARG_2 == am_false) {
           BIF_P->flags &= ~F_ASYNC_DIST;
       }
       else if (BIF_ARG_2 == am_true) {
           BIF_P->flags |= F_ASYNC_DIST;
       }
       else {
           goto error;
       }
       BIF_RET(old_value);
   }
   else if (BIF_ARG_1 == am_error_handler) {
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
   else if (BIF_ARG_1 == am_fullsweep_after) {
       Sint i;
       if (!is_small(BIF_ARG_2)) {
	   goto error;
       }
       i = signed_val(BIF_ARG_2);
       if (i < 0) {
	   goto error;
       }
       old_value = make_small(BIF_P->max_gen_gcs);
       BIF_P->max_gen_gcs = i;
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
       ErtsHeapFactory factory;
       Uint max_heap_size, max_heap_flags;

       if (!erts_max_heap_size(BIF_ARG_2, &max_heap_size, &max_heap_flags))
           goto error;

       if ((max_heap_size < MIN_HEAP_SIZE(BIF_P) && max_heap_size != 0))
	   goto error;

       erts_factory_proc_init(&factory, BIF_P);
       old_value = erts_max_heap_size_map(&factory,
                                          MAX_HEAP_SIZE_GET(BIF_P),
                                          MAX_HEAP_SIZE_FLAGS_GET(BIF_P));
       erts_factory_close(&factory);
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
       ErtsTracerRef* ref;
       Uint is_sensitive;
       if (BIF_ARG_2 == am_true) {
	   is_sensitive = 1;
       } else if (BIF_ARG_2 == am_false) {
	   is_sensitive = 0;
       } else {
	   goto error;
       }
       erts_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
       old_value = (ERTS_P_ALL_TRACE_FLAGS(BIF_P) & F_SENSITIVE
		    ? am_true
		    : am_false);
       if (is_sensitive) {
           for (ref = BIF_P->common.tracee.first_ref; ref; ref = ref->next)
               ref->flags |= F_SENSITIVE;
	   ERTS_P_ALL_TRACE_FLAGS(BIF_P) = F_SENSITIVE;
       } else {
           for (ref = BIF_P->common.tracee.first_ref; ref; ref = ref->next)
               ref->flags &= ~F_SENSITIVE;
	   ERTS_P_ALL_TRACE_FLAGS(BIF_P) = 0;
           ERTS_P_ALL_TRACE_FLAGS(BIF_P) = erts_sum_all_trace_flags(&BIF_P->common);
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

   old_value = process_flag_aux(BIF_P, NULL, BIF_ARG_1, BIF_ARG_2);
   if (old_value != am_badarg)
       BIF_RET(old_value);

   BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);

 error:
   BIF_ERROR(BIF_P, BADARG);
}

typedef struct {
    Eterm flag;
    Eterm value;
    ErlOffHeap oh;
    Eterm heap[1];
} ErtsProcessFlag3Args;

static Eterm
exec_process_flag_3(Process *c_p, void *arg, int *redsp, ErlHeapFragment **bpp)
{
    ErtsProcessFlag3Args *pf3a = arg;
    Eterm res;

    if (ERTS_PROC_IS_EXITING(c_p))
        res = am_badarg;
    else
        res = process_flag_aux(c_p, redsp, pf3a->flag, pf3a->value);
    erts_cleanup_offheap(&pf3a->oh);
    erts_free(ERTS_ALC_T_PF3_ARGS, arg);
    return res;
}


BIF_RETTYPE erts_internal_process_flag_3(BIF_ALIST_3)
{
   Eterm res, *hp;
   ErlOffHeap *ohp;
   ErtsProcessFlag3Args *pf3a;
   Uint flag_sz, value_sz;

   if (BIF_P->common.id == BIF_ARG_1) {
       res = process_flag_aux(BIF_P, NULL, BIF_ARG_2, BIF_ARG_3);
       BIF_RET(res);
   }

   if (is_not_internal_pid(BIF_ARG_1))
       BIF_RET(am_badtype);

   flag_sz = is_immed(BIF_ARG_2) ? 0 : size_object(BIF_ARG_2);
   value_sz = is_immed(BIF_ARG_3) ? 0 : size_object(BIF_ARG_3);

   pf3a = erts_alloc(ERTS_ALC_T_PF3_ARGS,
                     sizeof(ErtsProcessFlag3Args)
                     + sizeof(Eterm)*(flag_sz+value_sz-1));

   ohp = &pf3a->oh;
   ERTS_INIT_OFF_HEAP(&pf3a->oh);

   hp = &pf3a->heap[0];

   pf3a->flag = copy_struct(BIF_ARG_2, flag_sz, &hp, ohp);
   pf3a->value = copy_struct(BIF_ARG_3, value_sz, &hp, ohp);

   res = erts_proc_sig_send_rpc_request(BIF_P, BIF_ARG_1,
                                        !0,
                                        exec_process_flag_3,
                                        (void *) pf3a);

   if (is_non_value(res)) {
       erts_free(ERTS_ALC_T_PF3_ARGS, pf3a);
       BIF_RET(am_badtype);
   }

   return res;
}

/**********************************************************************/

/* register(atom, Process|Port) registers a global process or port
   (for this node) */

BIF_RETTYPE register_2(BIF_ALIST_2)   /* (Atom, Pid|Port)   */
{
    if (erts_register_name(BIF_P, BIF_ARG_1, BIF_ARG_2)) {
	BIF_RET(am_true);
    } else {
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
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
#define SEND_SYSTEM_LIMIT	(-9)


static Sint remote_send(Process *p, DistEntry *dep,
			Eterm to, Eterm node, Eterm full_to, Eterm msg,
                        Eterm return_term, Eterm *ctxpp,
                        int connect, int suspend, int prio)
{
    Sint res;
    int code;
    ErtsDSigSendContext ctx;
    ASSERT(is_atom(to) || is_external_pid(to) || is_external_ref(to));

    code = erts_dsig_prepare(&ctx, dep, p, ERTS_PROC_LOCK_MAIN,
			     ERTS_DSP_NO_LOCK,
			     !suspend, 0, connect);
    ctx.return_term = return_term;
    ctx.node = node;
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:
	res = SEND_NOCONNECT;
	break;
    case ERTS_DSIG_PREP_WOULD_SUSPEND:
	ASSERT(!suspend);
	res = SEND_YIELD;
	break;
    case ERTS_DSIG_PREP_CONNECTED:
        if (!(ctx.dflags & DFLAG_ALIAS) && is_external_ref(to)) {
            /*
             * Receiver does not support alias, so the alias is
             * obviously not present at the receiver. Just drop
             * it...
             */
            res = 0;
            break;
        }
        /* Fall through... */
    case ERTS_DSIG_PREP_PENDING: {

        code = erts_dsig_send_msg(&ctx, to, full_to, msg, prio);
	/*
	 * Note that reductions have been bumped on calling
	 * process by erts_dsig_send_msg().
	 */
	if (code == ERTS_DSIG_SEND_YIELD)
	    res = SEND_YIELD_RETURN;
	else if (code == ERTS_DSIG_SEND_CONTINUE) {
            erts_set_gc_state(p, 0);

            /* Keep a reference to the dist entry if the
               name is an not a pid. */
            if (is_atom(to)) {
                erts_ref_dist_entry(ctx.dep);
                ctx.deref_dep = 1;
            }

            *ctxpp = erts_dsend_export_trap_context(p, &ctx);
	    res = SEND_YIELD_CONTINUE;
	} else if (code == ERTS_DSIG_SEND_TOO_LRG)
	    res = SEND_SYSTEM_LIMIT;
	else
	    res = 0;
	break;
    }
    default:
	ASSERT(! "Invalid dsig prepare result");
	res = SEND_INTERNAL_ERROR;
    }

    if (res >= 0) {
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, full_to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
    }

    return res;
}

static Sint
do_send(Process *p, Eterm to, Eterm msg, Eterm return_term, Eterm *refp,
        Eterm *dist_ctx, int connect, int suspend, int prio)
{
    Eterm portid;
    Port *pt;
    Process* rp;
    DistEntry *dep;
    Eterm* tp;
    Eterm to_proc;

    if (is_internal_pid(to)) {
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
        if (prio) {
            to_proc = to;
            goto send_altact_message;
        }
	rp = erts_proc_lookup_raw(to);	
	if (!rp)
	    return 0;
    }
    else if (is_internal_ref(to)) {
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
	    trace_send(p, to, msg);
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
	    save_calls(p, &exp_send);
        to_proc = to;
        goto send_altact_message;
    } else if (is_external_pid(to) || is_external_ref(to)) {
	dep = external_dist_entry(to);
	if(dep == erts_this_dist_entry) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Discarding message %T from %T to %T in an old "
			  "incarnation (%u) of this node (%u)\n",
			  msg,
			  p->common.id,
			  to,
			  external_pid_creation(to),
			  erts_this_node->creation);
	    erts_send_error_to_logger(p->group_leader, dsbufp);
	    return 0;
	}
	return remote_send(p, dep, to, dep->sysname, to, msg, return_term,
                           dist_ctx, connect, suspend, prio);
    } else if (is_atom(to)) {
	Eterm id = erts_whereis_name_to_id(p, to);

	rp = erts_proc_lookup_raw(id);
	if (rp) {
	    if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
		trace_send(p, to, msg);
	    if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
		save_calls(p, &exp_send);
            if (prio) {
                to_proc = id;
                goto send_altact_message;
            }
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

	if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
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
		      "incarnation (%u) of this node (%u)\n",
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
	    int ps_flags = suspend ? 0 : ERTS_PORT_SIG_FLG_NOSUSPEND;
	    *refp = NIL;

            if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
                trace_send(p, portid, msg);

            if (have_seqtrace(SEQ_TRACE_TOKEN(p))) {
                seq_trace_update_serial(p);
                seq_trace_output(SEQ_TRACE_TOKEN(p), msg,
                                 SEQ_TRACE_SEND, portid, p);
            }

	    switch (erts_port_command(p, ps_flags, pt, msg, refp)) {
	    case ERTS_PORT_OP_BUSY:
		/* Nothing has been sent */
		if (suspend)
		    erts_suspend(p, ERTS_PROC_LOCK_MAIN, pt);
		return SEND_YIELD;
	    case ERTS_PORT_OP_BUSY_SCHEDULED:
		/* Message was sent */
		if (suspend) {
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

            if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
		trace_send(p, to, msg);
	    if (ERTS_PROC_GET_SAVED_CALLS_BUF(p))
		save_calls(p, &exp_send);

            id = erts_whereis_name_to_id(p, tp[1]);

	    rp = erts_proc_lookup_raw(id);
	    if (rp) {
                if (prio) {
                    to_proc = id;
                    goto send_altact_message;
                }
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
	    return 0;
	}
        if (dep == NULL) {
            dep = erts_find_or_insert_dist_entry(tp[2]);
            ASSERT(dep != erts_this_dist_entry);
            deref_dep = 1;
        }

	ret = remote_send(p, dep, tp[1], tp[2], to, msg, return_term,
                          dist_ctx, connect, suspend, prio);

        if (deref_dep)
            erts_deref_dist_entry(dep);
	return ret;
    } else {
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SEND))
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
	erts_send_message(p, rp, &rp_locks, msg);
	erts_proc_unlock(rp,
			     p == rp
			     ? (rp_locks & ~ERTS_PROC_LOCK_MAIN)
			     : rp_locks);
	return 0;
    }

send_altact_message: {
        erts_proc_sig_send_altact_msg(p, p->common.id, to_proc, msg,
                                      SEQ_TRACE_TOKEN(p), prio);
        return 0;
    }

}

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
    int connect = 1, suspend = 1, prio = 0;
    Eterm ctx;

    ERTS_MSACC_PUSH_STATE_M_X();

    while (is_list(l)) {
	if (CAR(list_val(l)) == am_noconnect) {
	    connect = 0;
	} else if (CAR(list_val(l)) == am_nosuspend) {
	    suspend = 0;
	} else if (CAR(list_val(l)) == am_priority) {
	    prio = !0;
	} else {
            BIF_P->fvalue = am_badopt;
            ERTS_BIF_PREP_ERROR(retval, p, BADARG | EXF_HAS_EXT_INFO);
	    goto done;
	}
	l = CDR(list_val(l));
    }
    if(!is_nil(l)) {
	BIF_P->fvalue = am_badopt;
	ERTS_BIF_PREP_ERROR(retval, p, BADARG | EXF_HAS_EXT_INFO);
	goto done;
    }

#ifdef DEBUG
    ref = NIL;
#endif

    ERTS_MSACC_SET_STATE_CACHED_M_X(ERTS_MSACC_STATE_SEND);
    result = do_send(p, to, msg, am_ok, &ref, &ctx, connect, suspend, prio);
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
	if (connect) {
	    ERTS_BIF_PREP_RET(retval, am_ok);
	} else {
	    ERTS_BIF_PREP_RET(retval, am_noconnect);
	}
	break;
    case SEND_YIELD:
	if (suspend) {
	    ERTS_BIF_PREP_YIELD3(retval, BIF_TRAP_EXPORT(BIF_send_3), p, to, msg, opts);
	} else {
	    ERTS_BIF_PREP_RET(retval, am_nosuspend);
	}
	break;
    case SEND_YIELD_RETURN:
	if (!suspend) {
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
    case SEND_SYSTEM_LIMIT:
	ERTS_BIF_PREP_ERROR(retval, p, SYSTEM_LIMIT);
	break;
    case SEND_USER_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_ERROR);
	break;
    case SEND_INTERNAL_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_INTERNAL_ERROR);
	break;
    case SEND_YIELD_CONTINUE:
	BUMP_ALL_REDS(p);
	ERTS_BIF_PREP_TRAP1(retval, &dsend_continue_trap_export, p, ctx);
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "send_3 invalid result %d\n", (int)result);
	break;
    }

done:
    return retval;
}

BIF_RETTYPE send_2(BIF_ALIST_2)
{
    return erl_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

static BIF_RETTYPE dsend_continue_trap_1(BIF_ALIST_1)
{
    Binary* bin = erts_magic_ref2bin(BIF_ARG_1);
    ErtsDSigSendContext *ctx = (ErtsDSigSendContext*) ERTS_MAGIC_BIN_DATA(bin);
    Sint initial_reds = (Sint) (ERTS_BIF_REDS_LEFT(BIF_P) * TERM_TO_BINARY_LOOP_FACTOR);
    int result;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == erts_dsend_context_dtor);

    ctx->reds = initial_reds;
    result = erts_dsig_send(ctx);

    switch (result) {
    case ERTS_DSIG_SEND_OK:
	erts_set_gc_state(BIF_P, 1);
	BIF_RET(ctx->return_term);
	break;
    case ERTS_DSIG_SEND_YIELD: /*SEND_YIELD_RETURN*/
	erts_set_gc_state(BIF_P, 1);
	if (ctx->no_suspend)
	    BIF_RET(am_nosuspend);
	ERTS_BIF_YIELD_RETURN(BIF_P, ctx->return_term);

    case ERTS_DSIG_SEND_CONTINUE: { /*SEND_YIELD_CONTINUE*/
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP1(&dsend_continue_trap_export, BIF_P, BIF_ARG_1);
    }
    case ERTS_DSIG_SEND_TOO_LRG: { /*SEND_SYSTEM_LIMIT*/
	erts_set_gc_state(BIF_P, 1);
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
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
    Eterm ctx;
    ERTS_MSACC_PUSH_AND_SET_STATE_M_X(ERTS_MSACC_STATE_SEND);

#ifdef DEBUG
    ref = NIL;
#endif

    result = do_send(p, to, msg, msg, &ref, &ctx, 1, 1, 0);

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
	ERTS_BIF_PREP_YIELD2(retval, BIF_TRAP_EXPORT(BIF_send_2), p, to, msg);
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
    case SEND_SYSTEM_LIMIT:
	ERTS_BIF_PREP_ERROR(retval, p, SYSTEM_LIMIT);
	break;
    case SEND_USER_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_ERROR);
	break;
    case SEND_INTERNAL_ERROR:
	ERTS_BIF_PREP_ERROR(retval, p, EXC_INTERNAL_ERROR);
	break;
    case SEND_YIELD_CONTINUE:
	BUMP_ALL_REDS(p);
	ERTS_BIF_PREP_TRAP1(retval, &dsend_continue_trap_export, p, ctx);
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "invalid send result %d\n", (int)result);
	break;
    }

done:
    return retval;
}

/**********************************************************************/

/* integer to float */

/**********************************************************************/

/* returns the head of a list - this function is unnecessary
   and is only here to keep Robert happy (Even more, since it's OP as well) */
BIF_RETTYPE hd_1(BIF_ALIST_1)
{
    /* NOTE: The JIT has its own implementation of this BIF. */
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(CAR(list_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the tails of a list - same comment as above */

BIF_RETTYPE tl_1(BIF_ALIST_1)
{
    /* NOTE: The JIT has its own implementation of this BIF. */
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(CDR(list_val(BIF_ARG_1)));
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
    /* NOTE: The JIT has its own implementation of this BIF. */
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
    if (n == 0) {
        return ERTS_GLOBAL_LIT_EMPTY_TUPLE;
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
    Eterm res;
    Eterm list = BIF_ARG_3;
    Eterm* tup = NULL;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0 || n > ERTS_MAX_TUPLE_SIZE) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    limit = (Uint) n;
    if (n == 0) {
        res = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    } else {
        Eterm* hp = HAlloc(BIF_P, n+1);
        res = make_tuple(hp);
        *hp++ = make_arityval(n);
        tup = hp;
        while (n--) {
            *hp++ = BIF_ARG_2;
        }
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

    if (arity == 1) {
        return ERTS_GLOBAL_LIT_EMPTY_TUPLE;
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
    const byte* err_pos;
    Eterm res;
    int ares;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
     
    /* read data from atom table */
    ap = atom_tab(atom_val(BIF_ARG_1));
    if (ap->len == 0)
	BIF_RET(NIL);	/* the empty atom */

    ares =
	erts_analyze_utf8(erts_atom_get_name(ap), ap->len, &err_pos, &num_chars, NULL);
    ASSERT(ares == ERTS_UTF8_OK); (void)ares;
    
    res = erts_utf8_to_list(BIF_P, num_chars, erts_atom_get_name(ap), ap->len, ap->len,
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
    int i = erts_unicode_list_to_buf(BIF_ARG_1, buf, MAX_ATOM_SZ_LIMIT,
                                     MAX_ATOM_CHARACTERS, &written);
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
    int i = erts_unicode_list_to_buf(BIF_ARG_1, buf, MAX_ATOM_SZ_LIMIT,
                                     MAX_ATOM_CHARACTERS, &written);
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

static Eterm integer_to_list(Process *c_p, Eterm num, int base)
{
    Eterm *hp;
    Eterm res;
    Uint need;

    if (is_small(num)) {
        char s[128];
        char *c = s;
        Uint digits;

        digits = Sint_to_buf(signed_val(num), base, &c, sizeof(s));
        need = 2 * digits;

        hp = HAlloc(c_p, need);
        res = buf_to_intlist(&hp, c, digits, NIL);
    } else {
        const int DIGITS_PER_RED = 16;
        Eterm *hp_end;
        Uint digits;

        digits = big_integer_estimate(num, base);

        if ((digits / DIGITS_PER_RED) > ERTS_BIF_REDS_LEFT(c_p)) {
            ErtsSchedulerData *esdp = erts_get_scheduler_data();

            /* This could take a very long time, tell the caller to reschedule
             * us to a dirty CPU scheduler if we aren't already on one. */
            if (esdp->type == ERTS_SCHED_NORMAL) {
                return THE_NON_VALUE;
            }
        } else {
            BUMP_REDS(c_p, digits / DIGITS_PER_RED);
        }

        need = 2 * digits;

        hp = HAlloc(c_p, need);
        hp_end = hp + need;

        res = erts_big_to_list(num, base, &hp);
        HRelease(c_p, hp_end, hp);
    }

    return res;
}

BIF_RETTYPE integer_to_list_1(BIF_ALIST_1)
{
    Eterm res;

    if (is_not_integer(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    res = integer_to_list(BIF_P, BIF_ARG_1, 10);

    if (is_non_value(res)) {
        ErtsCodeMFA *mfa = &BIF_TRAP_EXPORT(BIF_integer_to_list_1)->info.mfa;
        Eterm args[1];
        args[0] = BIF_ARG_1;
        return erts_schedule_bif(BIF_P,
                                 args,
                                 BIF_I,
                                 mfa,
                                 integer_to_list_1,
                                 ERTS_SCHED_DIRTY_CPU,
                                 am_erlang,
                                 am_integer_to_list,
                                 1);
    }

    return res;
}

BIF_RETTYPE integer_to_list_2(BIF_ALIST_2)
{
    Eterm res;
    SWord base;

    if (is_not_integer(BIF_ARG_1) || is_not_small(BIF_ARG_2)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    base = signed_val(BIF_ARG_2);
    if (base < 2 || base > 36) {
        BIF_ERROR(BIF_P, BADARG);
    }

    res = integer_to_list(BIF_P, BIF_ARG_1, base);

    if (is_non_value(res)) {
        ErtsCodeMFA *mfa = &BIF_TRAP_EXPORT(BIF_integer_to_list_2)->info.mfa;
        Eterm args[2];
        args[0] = BIF_ARG_1;
        args[1] = BIF_ARG_2;
        return erts_schedule_bif(BIF_P,
                                 args,
                                 BIF_I,
                                 mfa,
                                 integer_to_list_2,
                                 ERTS_SCHED_DIRTY_CPU,
                                 am_erlang,
                                 am_integer_to_list,
                                 2);
    }

    return res;
}

/**********************************************************************/

static int do_float_to_charbuf(Process *p, Eterm efloat, Eterm list, 
			char *fbuf, int sizeof_fbuf) {

    Eterm arity_two = make_arityval(2);
    int decimals = SYS_DEFAULT_FLOAT_DECIMALS;
    int compact = 0;
    enum fmt_type_ {
        FMT_LEGACY,
        FMT_SHORT,
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
        } else if (arg == am_short) {
            fmt_type = FMT_SHORT;
            continue;
        }
        goto badarg;
    }

    if (is_not_nil(list)) {
        goto badarg;
    }

    GET_DOUBLE(efloat, f);

    if (fmt_type == FMT_SHORT) {
        const int index = d2s_buffered_n(f.fd, fbuf);

        /* Terminate the string. */
        fbuf[index] = '\0';
        return index;
    } else if (fmt_type == FMT_FIXED) {
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
  char fbuf[256];
  int used;
  
  if ((used = do_float_to_charbuf(BIF_P,arg,opts,fbuf,sizeof(fbuf))) <= 0) {
    BIF_ERROR(BIF_P, BADARG);
  }

  BIF_RET(erts_new_binary_from_data(BIF_P, (Uint)used, (byte*)fbuf));
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
                ERTS_FALLTHROUGH();
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
                ERTS_FALLTHROUGH();
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
                ERTS_FALLTHROUGH();
	    case EXP0:		/* example: "2.3e+." */
		LOAD_E(i, i_mem, list, list_mem);
                ERTS_FALLTHROUGH();
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
                ERTS_FALLTHROUGH();
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
    if (is_bitstring(BIF_ARG_1)) {
        Uint offset, size;
        byte *base;

        ERTS_GET_BITSTRING(BIF_ARG_1, base, offset, size);

        if (size > 0 && TAIL_BITS(size) == 0) {
            byte *char_buf;
            Eterm res;

            /* Unfortunately, there is no nstrtod and strtod requires
             * NUL-termination, so we have to copy the binary to insert
             * that. :( */
            char_buf = erts_alloc(ERTS_ALC_T_TMP, NBYTES(size) + 1);
            copy_binary_to_buffer(char_buf, 0, base, offset, size);
            char_buf[NBYTES(size)] = '\0';

            res = do_charbuf_to_float(BIF_P, (char*)char_buf);
            erts_free(ERTS_ALC_T_TMP, (void*)char_buf);

            if (is_value(res)) {
                BIF_RET(res);
            }
        }
    }

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
    if (len == 0) {
        BIF_RET(ERTS_GLOBAL_LIT_EMPTY_TUPLE);
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
    if (ERTS_PROC_IS_EXITING(BIF_P)) {
        /* The max heap size limit was reached. */
        return THE_NON_VALUE;
    }
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
 * The erts_internal:processes_next/1 BIF.
 */

BIF_RETTYPE erts_internal_processes_next_1(BIF_ALIST_1)
{
    Eterm res;
    if (is_not_small(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    res = erts_ptab_processes_next(BIF_P, &erts_proc, unsigned_val(BIF_ARG_1));
    if (is_non_value(res)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
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
 * erts_internal:term_to_string/2 is an internal and undocumented function for
 * formatting terms during init or other times when io_lib is unavailable.
 * It can also be used to debug functions that rely on the internal term
 * printing such as erlang:display/1
 */
BIF_RETTYPE erts_internal_term_to_string_2(BIF_ALIST_2)
{
    erts_dsprintf_buf_t *dsbufp;
    int limit;
    int pres;
    Eterm res;
    Eterm *hp;

    if (is_small(BIF_ARG_2) &&
        (signed_val(BIF_ARG_2) > 1 &&
         signed_val(BIF_ARG_2) < INT_MAX)) {
        limit = signed_val(BIF_ARG_2);
    } else if (BIF_ARG_2 == am_undefined) {
        limit = INT_MAX;
    } else {
        BIF_ERROR(BIF_P, BADARG);
    }

    dsbufp = erts_create_tmp_dsbuf(64);
    pres = erts_dsprintf(dsbufp, "%.*T", limit, BIF_ARG_1);

    if (pres < 0) {
        erts_exit(ERTS_ERROR_EXIT,
                  "Failed to convert term to string: %d (%s)\n",
                  -pres,
                  erl_errno_id(-pres));
    }

    hp = HAlloc(BIF_P, 2 * dsbufp->str_len);
    res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);

    erts_destroy_tmp_dsbuf(dsbufp);

    BIF_RET(res);
}

BIF_RETTYPE display_string_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm string = BIF_ARG_2;
    Sint written;
    Sint len;
    const byte *temp_alloc = NULL, *str;
    int res;

#ifdef __WIN32__
    HANDLE fd;
    if (ERTS_IS_ATOM_STR("stdout", BIF_ARG_1)) {
        fd = GetStdHandle(STD_OUTPUT_HANDLE);
    } else if (ERTS_IS_ATOM_STR("stderr", BIF_ARG_1)) {
        fd = GetStdHandle(STD_ERROR_HANDLE);
    }
#else
    int fd;
    if (ERTS_IS_ATOM_STR("stdout", BIF_ARG_1)) {
        fd = fileno(stdout);
    } else if (ERTS_IS_ATOM_STR("stderr", BIF_ARG_1)) {
        fd = fileno(stderr);
    }
#if defined(HAVE_SYS_IOCTL_H) && defined(TIOCSTI)
    else if (ERTS_IS_ATOM_STR("stdin", BIF_ARG_1)) {
        const char stdin_fname[] = "/dev/tty";
        fd = open(stdin_fname,0);
        if (fd < 0) {
            fprintf(stderr,"failed to open %s (%s)\r\n", stdin_fname,
                    strerror(errno));
            goto error;
        }
    }
#endif
#endif
    else {
        BIF_ERROR(p, BADARG);
    }
    if (is_list(string) || is_nil(string)) {
        len = erts_unicode_list_to_buf_len(string);
        if (len < 0) BIF_ERROR(p, BADARG);
        str = temp_alloc = (byte*)erts_alloc(ERTS_ALC_T_TMP, sizeof(char)*len);
        res = erts_unicode_list_to_buf(string, (byte*)str, len, len, &written);
        if (res != 0 || written != len)
            erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error (%d)\n", __FILE__, __LINE__, res);
    } else if (is_bitstring(string)) {
        /* The cast to Uint* is safe since the length of any binary expressed
         * in bytes cannot exceed ERTS_SINT_MAX. */
        str = erts_get_aligned_binary_bytes(string,
                                            (Uint*)&len,
                                            &temp_alloc);
        if (str == NULL) {
            BIF_ERROR(p, BADARG);
        }
    } else {
        BIF_ERROR(p, BADARG);
    }

#if defined(HAVE_SYS_IOCTL_H) && defined(TIOCSTI)
    if (ERTS_IS_ATOM_STR("stdin", BIF_ARG_1)) {
        for (int i = 0; i < len; i++) {
            if (ioctl(fd, TIOCSTI, str+i) < 0) {
                fprintf(stderr,"failed to write to %s (%s)\r\n"
                        "to solve this you may need to enable legacy tiocsti\r\n"
                        "  sudo sysctl -w dev.tty.legacy_tiocsti=1\r\n",
                        "/proc/self/fd/0", strerror(errno));
                close(fd);
                goto error;
            }
        }
        close(fd);
    } else
#endif
    {
#ifdef __WIN32__
        Uint32 w;
        if (!WriteFile(fd, str, len, &w, NULL)) {
            goto error;
        }
        written = (Sint)w;
#else
        written = 0;
        do {
            res = write(fd, str+written, len-written);
            if (res < 0 && errno != ERRNO_BLOCK && errno != EINTR)
                goto error;
            written += res;
        } while (written < len);
#endif
    }
    if (temp_alloc) {
        erts_free(ERTS_ALC_T_TMP, (void*)temp_alloc);
    }
    BIF_RET(am_true);

error: {
#ifdef __WIN32__
        char *errnostr = last_error();
#else
        char *errnostr = erl_errno_id(errno);
#endif
        BIF_P->fvalue = am_atom_put(errnostr, strlen(errnostr));
        if (temp_alloc) {
            erts_free(ERTS_ALC_T_TMP, (void*)temp_alloc);
        }
        BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }
}

/**********************************************************************/


/* stop the system with exit code and flags */
#if defined(BEAMASM) && defined(BEAMASM_DUMP_SIZES)
void beamasm_dump_sizes(void);
#endif

BIF_RETTYPE halt_2(BIF_ALIST_2)
{
    Uint code;
    Eterm optlist = BIF_ARG_2;
    int flush = 1;
    ErtsMonotonicTime halt_flush_timeout = -1; /* default infinity */

    for (optlist = BIF_ARG_2;
	 is_list(optlist);
	 optlist = CDR(list_val(optlist))) {
	Eterm *tp, opt = CAR(list_val(optlist));
	if (is_not_tuple(opt))
	    goto bad_options;
	tp = tuple_val(opt);
	if (tp[0] != make_arityval(2))
            goto bad_options;
	if (tp[1] == am_flush) {
	    if (tp[2] == am_true)
		flush = 1;
	    else if (tp[2] == am_false)
		flush = 0;
	    else
                goto bad_options;
	}
	else if (tp[1] == am_flush_timeout) {
            Uint32 tmo;
            if (!term_to_Uint32(tp[2], &tmo))
                goto bad_options;
            halt_flush_timeout = (ErtsMonotonicTime) tmo;
	}
	else
            goto bad_options;
    }
    if (is_not_nil(optlist))
        goto bad_options;

#if defined(BEAMASM) && defined(BEAMASM_DUMP_SIZES)
    beamasm_dump_sizes();
#endif

    if (term_to_Uint_mask(BIF_ARG_1, &code)) {
	int pos_int_code = (int) (code & INT_MAX);
	VERBOSE(DEBUG_SYSTEM,
		("System halted by BIF halt(%T, %T)\n", BIF_ARG_1, BIF_ARG_2));
	if (flush) {
	    erts_halt(pos_int_code, halt_flush_timeout);
            /* We lost race against other halt call.
               Suspend this process and do a dummy trap while waiting
               for other halt call to terminate the beam. */
            erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
	    ERTS_BIF_YIELD2(BIF_TRAP_EXPORT(BIF_halt_2), BIF_P, am_undefined, am_undefined);
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
#       define HALT_MSG_SIZE 1023
        static byte halt_msg[4*HALT_MSG_SIZE+1];
        Sint written;

        if (erts_unicode_list_to_buf(BIF_ARG_1, halt_msg, 4 * HALT_MSG_SIZE,
                                     HALT_MSG_SIZE, &written) == -1 ) {
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

 bad_options:
    BIF_P->fvalue = am_badopt;
    BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
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
    Export *ep;
    Sint arity;

    if (is_not_atom(BIF_ARG_1) ||
        is_not_atom(BIF_ARG_2) ||
        is_not_small(BIF_ARG_3)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    arity = signed_val(BIF_ARG_3);
    if (arity < 0 || arity > MAX_ARG) {
        BIF_ERROR(BIF_P, BADARG);
    }

    ep = erts_export_get_or_make_stub(BIF_ARG_1, BIF_ARG_2, (Uint) arity);
    BIF_RET(ep->lambda);
}

BIF_RETTYPE fun_to_list_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;

    if (is_not_any_fun(fun)) {
        BIF_ERROR(p, BADARG);
    }

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

/* convert a list of ascii characters of the form
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

    if(dep == erts_this_dist_entry) {
        if (c > ERTS_MAX_INTERNAL_PID_SERIAL || b > ERTS_MAX_INTERNAL_PID_NUMBER)
            goto bad;
	BIF_RET(make_internal_pid(make_pid_data(c, b)));
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;

      if (is_nil(dep->cid))
	  goto bad;
      
      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_PID_HEAP_SIZE);
      
      enp = erts_find_or_insert_node(dep->sysname, dep->creation,
                                     make_boxed(&etp->header));
      ASSERT(enp != erts_this_node);

      etp->header = make_external_pid_header();
      etp->next = MSO(BIF_P).first;
      etp->node = enp;
      etp->data.pid.num = b;
      etp->data.pid.ser = c;

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
     * "#Port<Node.Num>" where Node is node and Num is
     * the port id.
     */
    Uint32 node;
    Uint64 num;
    char c, *cp;
    int i;
    DistEntry *dep = NULL;
    char buf[6 /* #Port< */
	     + 11 /* Node. */
	     + 21 /* Num> */
             + 1 /* \0 */];

    /* walk down the list and create a C string */
    if ((i = intlist_to_buf(BIF_ARG_1, buf, sizeof(buf)-1)) < 0)
	goto bad;

    buf[i] = '\0';		/* null terminal */

    cp = &buf[0];
    if (sys_strncmp("#Port<", cp, 6) != 0)
        goto bad;

    cp += 6; /* sys_strlen("#Port<") */

    node = 0;
    c = *cp;
    if (!('0' <= c && c <= '9'))
	goto bad;
    do {
	node *= 10;
	node += (Uint32) (c - '0');
	c = *(++cp);
    } while ('0' <= c && c <= '9');

    if (*(cp++) != '.')
	goto bad;

    c = *cp;
    num = 0;
    if (!('0' <= c && c <= '9'))
	goto bad;
    do {
	num *= 10;
	num += (Uint64) (c - '0');
	c = *(++cp);
    } while ('0' <= c && c <= '9');

    if (*(cp++) != '>')
	goto bad;
    if (*cp != '\0')
	goto bad;
    
    dep = erts_channel_no_to_dist_entry(node);

    if (!dep)
	goto bad;

    if(dep == erts_this_dist_entry) {
	if (num > ERTS_MAX_INTERNAL_PORT_NUMBER)
	    goto bad;
	BIF_RET(make_internal_port(num));
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;

      if (is_nil(dep->cid))
	  goto bad;

      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_PORT_HEAP_SIZE);
      enp = erts_find_or_insert_node(dep->sysname, dep->creation,
                                     make_boxed(&etp->header));
      ASSERT(enp != erts_this_node);

      etp->header = make_external_port_header();
      etp->next = MSO(BIF_P).first;
      etp->node = enp;
#ifdef ARCH_64
      etp->data.port.id = num;
#else
      etp->data.port.low = (Uint32) (num & 0xffffffff);
      etp->data.port.high = (Uint32) ((num >> 32) & 0xffffffff);
#endif

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
        if (refn[0] >= MAX_REFERENCE) goto bad;
        if (n != ERTS_REF_NUMBERS && n != ERTS_PID_REF_NUMBERS) goto bad;
        sid = erts_get_ref_numbers_thr_id(refn);
        if (sid > erts_no_schedulers) goto bad;
        if (erts_is_ordinary_ref_numbers(refn)) {
        make_ordinary_internal_ref:
            hp = HAlloc(BIF_P, ERTS_REF_THING_SIZE);
            write_ref_thing(hp, refn[0], refn[1], refn[2]);
            res = make_internal_ref(hp);
        }
        else {
            /* Check if it is a pid reference... */
            Eterm pid = erts_pid_ref_lookup(refn, n);
            if (is_internal_pid(pid)) {
                hp = HAlloc(BIF_P, ERTS_PID_REF_THING_SIZE);
                write_pid_ref_thing(hp, refn[0], refn[1], refn[2], pid);
                res = make_internal_ref(hp);
            }
            else if (is_non_value(pid)) {
                goto bad;
            }
            else {
                /* Check if it is a magic reference... */
                mb = erts_magic_ref_lookup_bin(refn);
                if (!mb)
                    goto make_ordinary_internal_ref;
                hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
                res = erts_mk_magic_ref(&hp, &BIF_P->off_heap,
                                        (Binary *) mb);
            }
        }
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;
      Uint hsz;
      int j;

      if (is_nil(dep->cid))
	  goto bad;
      
      hsz = EXTERNAL_THING_HEAD_SIZE;
#if defined(ARCH_64)
      hsz += n/2 + 1;
#else
      hsz += n;
#endif

      etp = (ExternalThing *) HAlloc(BIF_P, hsz);

      enp = erts_find_or_insert_node(dep->sysname, dep->creation,
                                     make_boxed(&etp->header));
      ASSERT(enp != erts_this_node);

#if defined(ARCH_64)
      etp->header = make_external_ref_header(n/2 + 1);
#else
      etp->header = make_external_ref_header(n);
#endif
      etp->next = BIF_P->off_heap.first;
      etp->node = enp;
      i = 0;
#if defined(ARCH_64)
      etp->data.ui32[i++] = n;
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
                                             ERTS_PROC_LOCK_MAIN,
                                             NULL);
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
	ErtsDSigSendContext ctx;
	dep = external_pid_dist_entry(BIF_ARG_2);
	ERTS_ASSERT(dep);
	if(dep == erts_this_dist_entry)
	    BIF_ERROR(BIF_P, BADARG);

	code = erts_dsig_prepare(&ctx, dep, BIF_P, ERTS_PROC_LOCK_MAIN,
				 ERTS_DSP_NO_LOCK, 0, 1, 1);
	switch (code) {
	case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    BIF_RET(am_true);
	case ERTS_DSIG_PREP_PENDING:
	case ERTS_DSIG_PREP_CONNECTED:
	    code = erts_dsig_send_group_leader(&ctx, BIF_ARG_1, BIF_ARG_2);
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
                BIF_TRAP2(BIF_TRAP_EXPORT(BIF_system_flag_2),
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
	    BIF_TRAP2(BIF_TRAP_EXPORT(BIF_system_flag_2),
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
        ErtsHeapFactory factory;
        Eterm old_value;
        Uint max_heap_size, max_heap_flags;

        if (!erts_max_heap_size(BIF_ARG_2, &max_heap_size, &max_heap_flags))
            goto error;

        if (max_heap_size < H_MIN_SIZE && max_heap_size != 0)
            goto error;

        erts_factory_proc_init(&factory, BIF_P);
        old_value = erts_max_heap_size_map(&factory, H_MAX_SIZE, H_MAX_FLAGS);
        erts_factory_close(&factory);

        erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
        erts_thr_progress_block();

        H_MAX_SIZE = max_heap_size;
        H_MAX_FLAGS = max_heap_flags;

        erts_thr_progress_unblock();
        erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

        BIF_RET(old_value);
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
    } else if (BIF_ARG_1 == am_reset_seq_trace) {
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
                erts_proc_lock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_MSGQ);
                erts_proc_sig_clear_seq_trace_tokens(p);
                erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_MSGQ);
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
	    BIF_TRAP2(BIF_TRAP_EXPORT(BIF_system_flag_2),
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
    } else if (BIF_ARG_1 == am_outstanding_system_requests_limit) {
        Uint val;
	if (!term_to_Uint(BIF_ARG_2, &val))
            goto error;
        val = erts_set_outstanding_system_requests_limit(val);
        if (!val)
            goto error;
        BIF_RET(make_small(val));
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
    } else if (ERTS_IS_ATOM_STR("system_logger", BIF_ARG_1)) {
        Eterm res = erts_set_system_logger(BIF_ARG_2);
        if (is_value(res)) BIF_RET(res);
    } else {
        BIF_P->fvalue = am_badopt;
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
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
    Eterm trap_state = THE_NON_VALUE;
    hash = trapping_make_hash2(BIF_ARG_1, &trap_state, BIF_P);
    if (trap_state == THE_NON_VALUE) {
        BIF_RET(make_small(hash & ((1L << 27) - 1)));
    } else {
        BIF_TRAP1(BIF_TRAP_EXPORT(BIF_phash2_1), BIF_P, trap_state);
    }
}

BIF_RETTYPE phash2_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;
    Eterm trap_state = THE_NON_VALUE;

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
    hash = trapping_make_hash2(BIF_ARG_1, &trap_state, BIF_P);
    if (trap_state != THE_NON_VALUE) {
        BIF_TRAP2(BIF_TRAP_EXPORT(BIF_phash2_2), BIF_P, trap_state, BIF_ARG_2);
    }
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

static BIF_RETTYPE
bif_handle_signals_return(BIF_ALIST_2)
{
    int reds_left;
    erts_aint32_t state;

    if (BIF_P->sig_qs.flags & FS_FLUSHED_SIGS) {
    flushed:
	ASSERT(BIF_P->sig_qs.flags & FS_FLUSHING_SIGS);
	BIF_P->sig_qs.flags &= ~(FS_FLUSHED_SIGS|FS_FLUSHING_SIGS);
        erts_set_gc_state(BIF_P, !0); /* Allow GC again... */
	BIF_RET(BIF_ARG_2);
    }
    
    if (!(BIF_P->sig_qs.flags & FS_FLUSHING_SIGS)) {
        int flags = ((is_internal_pid(BIF_ARG_1)
                      || is_internal_port(BIF_ARG_1))
                     ? ERTS_PROC_SIG_FLUSH_FLG_FROM_ID
                     : ERTS_PROC_SIG_FLUSH_FLG_FROM_ALL);
	erts_proc_sig_init_flush_signals(BIF_P, flags, BIF_ARG_1);
        if (BIF_P->sig_qs.flags & FS_FLUSHED_SIGS)
            goto flushed;
    }
    
    ASSERT(BIF_P->sig_qs.flags & FS_FLUSHING_SIGS);

    reds_left = ERTS_BIF_REDS_LEFT(BIF_P);

    state = erts_atomic32_read_nob(&BIF_P->state);
    do {
	int sreds = reds_left;
	(void) erts_proc_sig_handle_incoming(BIF_P, &state, &sreds,
					     sreds, !0);
	BUMP_REDS(BIF_P, (int) sreds);
	if (state & ERTS_PSFLG_EXITING)
	    ERTS_BIF_EXITED(BIF_P);
	if (BIF_P->sig_qs.flags & FS_FLUSHED_SIGS)
            goto flushed;
	reds_left -= sreds;
    } while (reds_left > 0);

    /*
     * More signals to handle, but out of reductions. Yield
     * and come back here and continue...
     */
    ERTS_BIF_YIELD2(&erts_bif_handle_signals_return_export,
		    BIF_P, BIF_ARG_1, BIF_ARG_2);
}

Export bif_return_trap_export;
Export erts_bif_handle_signals_return_export;

void erts_init_trap_export(Export *ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(BIF_ALIST))
{
    int i;

    sys_memset((void *) ep, 0, sizeof(Export));

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
        erts_activate_export_trampoline(ep, i);
    }

#ifdef BEAMASM
    ep->dispatch.addresses[ERTS_SAVE_CALLS_CODE_IX] = beam_save_calls_export;
#endif

    ep->bif_number = -1;
    ep->info.mfa.module = m;
    ep->info.mfa.function = f;
    ep->info.mfa.arity = a;

    ep->trampoline.common.op = BeamOpCodeAddr(op_call_bif_W);
    ep->trampoline.bif.address = (BeamInstr)bif;
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

    erts_init_trap_export(&erts_bif_handle_signals_return_export,
			  am_erlang, am_bif_handle_signals_return, 2,
			  &bif_handle_signals_return);

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
 * Scheduling of BIFs via ErtsNativeFunc...
 */
#define ERTS_WANT_NFUNC_SCHED_INTERNALS__
#include "erl_nfunc_sched.h"

#define ERTS_SCHED_BIF_TRAP_MARKER ((void *) (UWord) 1)

static ERTS_INLINE void
schedule(Process *c_p, Process *dirty_shadow_proc,
	 const ErtsCodeMFA *mfa, ErtsCodePtr pc,
	 ErtsBifFunc dfunc, void *ifunc,
	 Eterm module, Eterm function,
	 int argc, Eterm *argv)
{
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(c_p));
    (void) erts_nfunc_schedule(c_p, dirty_shadow_proc,
				    mfa, pc,
                                    BeamOpCodeAddr(op_call_bif_W),
				    dfunc, ifunc,
				    module, function,
				    argc, argv);
}


static BIF_RETTYPE dirty_bif_result(BIF_ALIST_1)
{
    ErtsNativeFunc *nep = (ErtsNativeFunc *) ERTS_PROC_GET_NFUNC_TRAP_WRAPPER(BIF_P);
    erts_nfunc_restore(BIF_P, nep, BIF_ARG_1);
    BIF_RET(BIF_ARG_1);
}

static BIF_RETTYPE dirty_bif_trap(BIF_ALIST)
{
    ErtsNativeFunc *nep = (ErtsNativeFunc *) ERTS_PROC_GET_NFUNC_TRAP_WRAPPER(BIF_P);

    /*
     * Arity and argument registers already set
     * correct by call to dirty_bif_trap()...
     */

    ASSERT(BIF_P->arity == nep->trampoline.info.mfa.arity);

    erts_nfunc_restore(BIF_P, nep, THE_NON_VALUE);

    BIF_P->i = (ErtsCodePtr) nep->func;
    BIF_P->freason = TRAP;
    return THE_NON_VALUE;
}

static BIF_RETTYPE dirty_bif_exception(BIF_ALIST_2)
{
    Eterm freason;

    ASSERT(is_small(BIF_ARG_1));

    freason = signed_val(BIF_ARG_1);

    /* Restore orig info for error and clear nif wrapper in handle_error() */
    freason |= EXF_RESTORE_NFUNC;

    BIF_P->fvalue = BIF_ARG_2;

    BIF_ERROR(BIF_P, freason);
}


static BIF_RETTYPE call_bif(Process *c_p, Eterm *reg, ErtsCodePtr I);

BIF_RETTYPE
erts_schedule_bif(Process *proc,
		  Eterm *argv,
		  ErtsCodePtr i,
		  const ErtsCodeMFA *mfa,
		  ErtsBifFunc bif,
		  ErtsSchedType sched_type,
		  Eterm mod,
		  Eterm func,
		  int argc)
{
    Process *c_p, *dirty_shadow_proc;

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
	BifFunction dbif, ibif;

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

	ASSERT(bif);

	if (argc < 0) { /* reschedule original call */
	    mod = mfa->module;
	    func = mfa->function;
	    argc = (int) mfa->arity;
	}

	schedule(c_p, dirty_shadow_proc, mfa, i, dbif, ibif,
		 mod, func, argc, argv);
    }

    if (dirty_shadow_proc)
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);

    return THE_NON_VALUE;
}

static BIF_RETTYPE
call_bif(Process *c_p, Eterm *reg, ErtsCodePtr I)
{
    ErtsNativeFunc *nep = ERTS_I_BEAM_OP_TO_NFUNC(I);
    ErtsBifFunc bif = (ErtsBifFunc) nep->func;
    BIF_RETTYPE ret;

    ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));

    nep->func = ERTS_SCHED_BIF_TRAP_MARKER;

    ASSERT(bif);

    ret = (*bif)(c_p, reg, I);

    if (is_value(ret))
	erts_nfunc_restore(c_p, nep, ret);
    else if (c_p->freason != TRAP)
	c_p->freason |= EXF_RESTORE_NFUNC; /* restore in handle_error() */
    else if (nep->func == ERTS_SCHED_BIF_TRAP_MARKER) {
	/* BIF did an ordinary trap... */
	erts_nfunc_restore(c_p, nep, ret);
    }
    /* else:
     *   BIF rescheduled itself using erts_schedule_bif().
     */

    return ret;
}


int
erts_call_dirty_bif(ErtsSchedulerData *esdp,
                    Process *c_p,
                    ErtsCodePtr I,
                    Eterm *reg)
{
    BIF_RETTYPE result;
    int exiting;
    Process *dirty_shadow_proc;
    ErtsBifFunc bf;
    ErtsNativeFunc *nep;
#ifdef DEBUG
    Eterm *c_p_htop;
    erts_aint32_t state;

    ASSERT(!c_p->scheduler_data);
    state = erts_atomic32_read_nob(&c_p->state);
    ASSERT((state & ERTS_PSFLG_DIRTY_RUNNING)
	   && !(state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)));
    ASSERT(esdp);

#endif

    nep = ERTS_I_BEAM_OP_TO_NFUNC(I);
    ASSERT(nep == ERTS_PROC_GET_NFUNC_TRAP_WRAPPER(c_p));

    nep->func = ERTS_SCHED_BIF_TRAP_MARKER;

    bf = (ErtsBifFunc) nep->trampoline.dfunc;

    erts_atomic32_read_band_mb(&c_p->state, ~(ERTS_PSFLG_DIRTY_CPU_PROC
						  | ERTS_PSFLG_DIRTY_IO_PROC));

    dirty_shadow_proc = erts_make_dirty_shadow_proc(esdp, c_p);

    dirty_shadow_proc->freason = c_p->freason;
    dirty_shadow_proc->fvalue = c_p->fvalue;
    dirty_shadow_proc->ftrace = c_p->ftrace;
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
	c_p->i = dirty_shadow_proc->i;
	c_p->arity = dirty_shadow_proc->arity;
    }

    erts_flush_dirty_shadow_proc(dirty_shadow_proc);

    return exiting;
}

BIF_RETTYPE alias_1(BIF_ALIST_1)
{
    Eterm ref, opts = BIF_ARG_1;
    ErtsMonitorData *mdp;
    Uint32 flags = ERTS_ML_STATE_ALIAS_UNALIAS;
    
    while (is_list(opts)) {
        Eterm *cons = list_val(opts);
        switch (CAR(cons)) {
        case am_explicit_unalias:
            flags &= ~ERTS_ML_STATE_ALIAS_MASK;
            flags |= ERTS_ML_STATE_ALIAS_UNALIAS;
            break;
        case am_reply:
            flags &= ~ERTS_ML_STATE_ALIAS_MASK;
            flags |= ERTS_ML_STATE_ALIAS_ONCE;
            break;
        case am_priority:
            flags |= ERTS_ML_FLG_PRIO_ALIAS;
            break;
        default:
            BIF_ERROR(BIF_P, BADARG);
        }
    	opts = CDR(cons);
    }
    if (is_not_nil(opts)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    ref = erts_make_pid_ref(BIF_P);
    mdp = erts_monitor_create(ERTS_MON_TYPE_ALIAS, ref,
                              BIF_P->common.id, NIL, NIL,
                              THE_NON_VALUE);
    mdp->origin.flags |= flags;
    erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P),
                             &mdp->origin);

    if (flags & ERTS_ML_FLG_PRIO_ALIAS)
        erts_proc_sig_prio_item_added(BIF_P, ERTS_PRIO_ITEM_TYPE_ALIAS);

    BIF_RET(ref);

}

BIF_RETTYPE unalias_1(BIF_ALIST_1)
{
    ErtsMonitor *mon;
    Eterm pid;
    int prio;

    pid = erts_get_pid_of_ref(BIF_ARG_1);
    if (BIF_P->common.id != pid) {
        if (is_non_value(pid))
            BIF_ERROR(BIF_P, BADARG);
        BIF_RET(am_false);
    }

    mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P),
                                   BIF_ARG_1);
    if (!mon || !(mon->flags & ERTS_ML_STATE_ALIAS_MASK))
        BIF_RET(am_false);

    ASSERT(erts_monitor_is_origin(mon));

    prio = !!(mon->flags & ERTS_ML_FLG_PRIO_ALIAS);
    mon->flags &= ~(ERTS_ML_STATE_ALIAS_MASK|ERTS_ML_FLG_PRIO_ALIAS);
    if (ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_ALIAS) {
        erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), mon);
        erts_monitor_release(mon);
    }

    if (prio)
        erts_proc_sig_prio_item_deleted(BIF_P, ERTS_PRIO_ITEM_TYPE_ALIAS);

    BIF_RET(am_true);
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
    if (!is_bitstring(BIF_ARG_1) || TAIL_BITS(bitstring_size(BIF_ARG_1)) != 0) {
        BIF_ERROR(BIF_P, BADARG);
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
    const byte *temp_alloc = NULL;
    const byte *p;
    Uint size;
    if (p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),
                                          &size,
                                          &temp_alloc)) {
        byte *q;
        Uint i;
        b = erts_new_binary(BIF_P, (size + 1), &q);
        for(i = 0; i < size; i++) {
            q[i] = p[i];
        } 
        erts_free_aligned_binary_bytes(temp_alloc);
        q[size] = '\0';
    } else {
        b = erts_new_binary_from_data(BIF_P, 1, (const byte*)"");
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
    const byte *temp_alloc = NULL;
    const byte *p;
    Uint size;
    if (p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),
                                          &size,
                                          &temp_alloc)) {
        byte *q;
        Uint i;
        p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),
                                          &size,
                                          &temp_alloc);
        b = erts_new_binary(BIF_P, size + 1, &q);
        for(i = 0; i < size; i++) {
            q[i] = p[i];
        } 
        erts_free_aligned_binary_bytes(temp_alloc);
        q[size] = '\0';
    } else {
        b = erts_new_binary_from_data(BIF_P, 1, (const byte*)"");
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
    if(arityval(*tpl) != 2 || is_not_small(tpl[1]) || (is_not_bitstring(tpl[2]) && tpl[2] != NIL)) {
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
