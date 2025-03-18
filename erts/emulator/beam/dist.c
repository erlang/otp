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

/*
 * distribution of erlang messages to other nodes.
 */


/* define this to get a lot of debug output */
/* #define ERTS_DIST_MSG_DBG */
/* #define ERTS_RAW_DIST_MSG_DBG */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_EXTERNAL_TAGS

#include <stddef.h>
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "dist.h"
#include "bif.h"
#include "external.h"
#include "erl_binary.h"
#include "erl_thr_progress.h"
#include "dtrace-wrapper.h"
#include "erl_proc_sig_queue.h"
#include "erl_global_literals.h"
#include "erl_map.h"

#define DIST_CTL_DEFAULT_SIZE 64

/* Turn this on to get printouts of all distribution messages
 * which go on the line. Enabling this may make some testcases
 * fail. Especially the broken dist testcases in distribution_SUITE.
 */
#if 0
#define ERTS_DIST_MSG_DBG
#endif
#if 0
/* Enable this to print the dist debug messages to a file instead */
#define ERTS_DIST_MSG_DBG_FILE "/tmp/dist_dbg.%d"
#endif
#if 0
/* Enable this to print the raw bytes sent and received */
#define ERTS_RAW_DIST_MSG_DBG
#endif

#if defined(ERTS_DIST_MSG_DBG) || defined(ERTS_RAW_DIST_MSG_DBG)
FILE *dbg_file;
static void bw(byte *buf, ErlDrvSizeT sz)
{
    bin_write(ERTS_PRINT_FILE, dbg_file, buf, sz);
}
#endif

#ifdef ERTS_DIST_MSG_DBG
static void
dist_msg_dbg(ErtsDistExternal *edep, char *what, byte *buf, int sz)
{
    byte *extp = edep->data->extp;
    Eterm msg;
    Sint ctl_len;
    Sint size = ctl_len = erts_decode_dist_ext_size(edep, 0, 0);
    if (size < 0) {
	erts_fprintf(dbg_file,
		     "DIST MSG DEBUG: erts_decode_dist_ext_size(%s) failed:\n",
		     what);
	bw(buf, sz);
    }
    else {
        ErtsHeapFactory factory;
	ErtsMessage *mbuf = erts_factory_message_create(&factory, NULL, 0, ctl_len);
        /* Set mbuf msg to NIL as erts_factory_undo will fail otherwise */
        ERL_MESSAGE_TERM(mbuf) = NIL;
        msg = erts_decode_dist_ext(&factory, edep, 0);
	if (is_value(msg))
	    erts_fprintf(dbg_file, "    %s: %.80T\n", what, msg);
	else {
	    erts_fprintf(dbg_file,
			 "DIST MSG DEBUG: erts_decode_dist_ext(%s) failed:\n",
			 what);
	    bw(buf, sz);
	}
        erts_factory_undo(&factory);
	edep->data->extp = extp;
    }
}

static char *erts_dop_to_string(enum dop dop) {
    if (dop == DOP_LINK)
        return "LINK";
    if (dop == DOP_SEND)
        return "SEND";
    if (dop == DOP_EXIT)
        return "EXIT";
    if (dop == DOP_UNLINK)
        return "UNLINK";
    if (dop == DOP_REG_SEND)
        return "REG_SEND";
    if (dop == DOP_GROUP_LEADER)
        return "GROUP_LEADER";
    if (dop == DOP_EXIT2)
        return "EXIT2";
    if (dop == DOP_SEND_TT)
        return "SEND_TT";
    if (dop == DOP_EXIT_TT)
        return "EXIT_TT";
    if (dop == DOP_REG_SEND_TT)
        return "REG_SEND_TT";
    if (dop == DOP_EXIT2_TT)
        return "EXIT2_TT";
    if (dop == DOP_MONITOR_P)
        return "MONITOR_P";
    if (dop == DOP_DEMONITOR_P)
        return "DEMONITOR_P";
    if (dop == DOP_MONITOR_P_EXIT)
        return "MONITOR_P_EXIT";
    if (dop == DOP_SEND_SENDER)
        return "SEND_SENDER";
    if (dop == DOP_SEND_SENDER_TT)
        return "SEND_SENDER_TT";
    if (dop == DOP_PAYLOAD_EXIT)
        return "PAYLOAD_EXIT";
    if (dop == DOP_PAYLOAD_EXIT_TT)
        return "PAYLOAD_EXIT_TT";
    if (dop == DOP_PAYLOAD_EXIT2)
        return "PAYLOAD_EXIT2";
    if (dop == DOP_PAYLOAD_EXIT2_TT)
        return "PAYLOAD_EXIT2_TT";
    if (dop == DOP_PAYLOAD_MONITOR_P_EXIT)
        return "PAYLOAD_MONITOR_P_EXIT";
    if (dop == DOP_SPAWN_REQUEST)
        return "SPAWN_REQUEST";
    if (dop == DOP_SPAWN_REQUEST_TT)
        return "SPAWN_REQUEST_TT";
    if (dop == DOP_SPAWN_REPLY)
        return "SPAWN_REPLY";
    if (dop == DOP_UNLINK_ID)
        return "UNLINK_ID";
    if (dop == DOP_UNLINK_ID_ACK)
        return "UNLINK_ID_ACK";
    ASSERT(0);
    return "UNKNOWN";
}

#endif

#if defined(VALGRIND)
#  include <valgrind/valgrind.h>
#  include <valgrind/memcheck.h>
#  define VALGRIND_MSG(msg) VALGRIND_PRINTF("%s, line %d: %s", __FILE__, __LINE__, msg)
#else
#  define VALGRIND_MSG(msg)
#endif

int erts_is_alive; /* System must be blocked on change */
int erts_dist_buf_busy_limit;

Uint64 erts_dflags_test_remove;

Export spawn_request_yield_export;

/* distribution trap functions */
Export* dmonitor_node_trap = NULL;

/* local variables */
static Export *dist_ctrl_put_data_trap;

/* forward declarations */

static void erts_schedule_dist_command(Port *, DistEntry *);
static int dsig_send_exit(ErtsDSigSendContext *ctx, Eterm ctl, Eterm msg);
static int dsig_send_ctl(ErtsDSigSendContext *ctx, Eterm ctl);
static void send_nodes_mon_msgs(Process *, Eterm, Eterm, Uint32, Eterm, Eterm);
static void init_nodes_monitors(void);
static Sint abort_pending_connection(DistEntry* dep, Uint32 conn_id,
                                     int *was_connected_p);
static ErtsDistOutputBuf* clear_de_out_queues(DistEntry*);
static void free_de_out_queues(DistEntry*, ErtsDistOutputBuf*);
int erts_dist_seq_tree_foreach_delete_yielding(DistSeqNode **root,
                                               void **vyspp,
                                               Sint limit);

static erts_atomic_t no_caches;
static erts_atomic_t no_nodes;

struct {
    Eterm reason;
    ErlHeapFragment *bp;
} nodedown;

/*
 * Dist entry queue flags are only modified while
 * the dist entry queue lock is held...
 */
static ERTS_INLINE erts_aint32_t
de_qflags_read(DistEntry *dep)
{
    return erts_atomic32_read_nob(&dep->qflgs);
}

static ERTS_INLINE erts_aint32_t
de_qflags_read_set(DistEntry *dep, erts_aint32_t set)
{
    erts_aint32_t qflgs, new_qflgs;
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&dep->qlock));
    new_qflgs = qflgs = erts_atomic32_read_nob(&dep->qflgs);
    new_qflgs |= set;
    erts_atomic32_set_nob(&dep->qflgs, new_qflgs);
    return qflgs;
}

static ERTS_INLINE erts_aint32_t
de_qflags_read_unset(DistEntry *dep, erts_aint32_t unset)
{
    erts_aint32_t qflgs, new_qflgs;
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&dep->qlock));
    new_qflgs = qflgs = erts_atomic32_read_nob(&dep->qflgs);
    new_qflgs &= ~unset;
    erts_atomic32_set_nob(&dep->qflgs, new_qflgs);
    return qflgs;
}

static void
delete_cache(ErtsAtomCache *cache)
{
    if (cache) {
	erts_free(ERTS_ALC_T_DCACHE, (void *) cache);
	ASSERT(erts_atomic_read_nob(&no_caches) > 0);
	erts_atomic_dec_nob(&no_caches);
    }
}

static void
create_cache(DistEntry *dep)
{
    int i;
    ErtsAtomCache *cp;

    ERTS_LC_ASSERT(is_nil(dep->cid));
    ASSERT(!dep->cache);

    dep->cache = cp = (ErtsAtomCache*) erts_alloc(ERTS_ALC_T_DCACHE,
						  sizeof(ErtsAtomCache));
    erts_atomic_inc_nob(&no_caches);
    for (i = 0; i < sizeof(cp->in_arr)/sizeof(cp->in_arr[0]); i++) {
	cp->in_arr[i] = THE_NON_VALUE;
	cp->out_arr[i] = THE_NON_VALUE;
    }
}

Uint erts_dist_cache_size(void)
{
    return (Uint) erts_atomic_read_mb(&no_caches)*sizeof(ErtsAtomCache);
}

static ErtsProcList *
get_suspended_on_de(DistEntry *dep, erts_aint32_t unset_qflgs)
{
    erts_aint32_t qflgs;
    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&dep->qlock));
    qflgs = de_qflags_read_unset(dep, unset_qflgs);
    qflgs &= ~unset_qflgs;
    if (qflgs & ERTS_DE_QFLG_EXIT) {
	/* No resume when exit has been scheduled */
	return NULL;
    }
    else {
	ErtsProcList *suspended = dep->suspended;
	dep->suspended = NULL;
	erts_proclist_fetch(&suspended, NULL);
	return suspended;
    }
}

#define ERTS_MON_LNK_FIRE_REDS 40

static int monitor_connection_down(ErtsMonitor *mon, void *unused, Sint reds)
{
    const ErtsMonitorData *data = erts_monitor_to_data(mon);
    Eterm from;

    if (erts_monitor_is_origin(mon)) {
        from = data->u.target.other.item;
    } else {
        from = data->origin.other.item;
    }

    ASSERT(!is_internal_pid(from) && is_internal_pid(mon->other.item));

    if (erts_monitor_is_origin(mon)) {
        erts_proc_sig_send_demonitor(NULL, from, 0, mon);
    } else {
        erts_proc_sig_send_monitor_down(NULL, from, mon, am_noconnection);
    }

    return ERTS_MON_LNK_FIRE_REDS;
}

static int dist_pend_spawn_exit_connection_down(ErtsMonitor *mon, void *unused, Sint reds)
{
    Process *proc = mon->other.ptr;
    ASSERT(!erts_monitor_is_origin(mon));
    if (proc) {
        ErtsMonitorData *mdp = erts_monitor_to_data(mon);
        if (mdp->origin.other.item == am_pending) {
            /* Resume the parent process waiting for a result... */
            erts_resume(proc, 0);
        }
    }
    erts_monitor_release(mon);
    return 1;
}

static int link_connection_down(ErtsLink *lnk, void *vdist, Sint reds)
{
    erts_proc_sig_send_link_exit_noconnection(lnk);
    return ERTS_MON_LNK_FIRE_REDS;
}

typedef enum {
    ERTS_CML_CLEANUP_STATE_LINKS,
    ERTS_CML_CLEANUP_STATE_MONITORS,
    ERTS_CML_CLEANUP_STATE_ONAME_MONITORS,
    ERTS_CML_CLEANUP_STATE_PEND_SPAWN_EXIT_MONITORS,
    ERTS_CML_CLEANUP_STATE_SEQUENCES,
    ERTS_CML_CLEANUP_STATE_NODE_MONITORS
} ErtsConMonLnkSeqCleanupState;

typedef struct {
    ErtsConMonLnkSeqCleanupState state;
    DistEntry* dep;
    Uint32 connection_id;
    ErtsMonLnkDist *dist;
    DistSeqNode *seq;
    void *yield_state;
    int trigger_node_monitors;
    Eterm nodename;
    Eterm visability;
    Eterm reason;
    ErlOffHeap oh;
    Eterm heap[1];
} ErtsConMonLnkSeqCleanup;

static void
con_monitor_link_seq_cleanup(void *vcmlcp)
{
    ErtsConMonLnkSeqCleanup *cmlcp = vcmlcp;
    ErtsMonLnkDist *dist = cmlcp->dist;
    ErtsSchedulerData *esdp;
    int reds = CONTEXT_REDS;

    switch (cmlcp->state) {
    case ERTS_CML_CLEANUP_STATE_LINKS:
        reds = erts_link_list_foreach_delete_yielding(&dist->links,
                                                      link_connection_down,
                                                      NULL, &cmlcp->yield_state,
                                                      reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_MONITORS;
        ERTS_FALLTHROUGH();
    case ERTS_CML_CLEANUP_STATE_MONITORS:
        reds = erts_monitor_list_foreach_delete_yielding(&dist->monitors,
                                                         monitor_connection_down,
                                                         NULL, &cmlcp->yield_state,
                                                         reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_ONAME_MONITORS;
        ERTS_FALLTHROUGH();
    case ERTS_CML_CLEANUP_STATE_ONAME_MONITORS:
        reds = erts_monitor_tree_foreach_delete_yielding(&dist->orig_name_monitors,
                                                         monitor_connection_down,
                                                         NULL, &cmlcp->yield_state,
                                                         reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_PEND_SPAWN_EXIT_MONITORS;
        ERTS_FALLTHROUGH();
    case ERTS_CML_CLEANUP_STATE_PEND_SPAWN_EXIT_MONITORS:
        reds = erts_monitor_tree_foreach_delete_yielding(&dist->dist_pend_spawn_exit,
                                                         dist_pend_spawn_exit_connection_down,
                                                         NULL, &cmlcp->yield_state,
                                                         reds);
        if (reds <= 0)
            break;

        cmlcp->dist = NULL;
        erts_mon_link_dist_dec_refc(dist);

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_SEQUENCES;
        ERTS_FALLTHROUGH();
    case ERTS_CML_CLEANUP_STATE_SEQUENCES:
        reds = erts_dist_seq_tree_foreach_delete_yielding(&cmlcp->seq,
                                                          &cmlcp->yield_state,
                                                          reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_NODE_MONITORS;
        ERTS_FALLTHROUGH();
    case ERTS_CML_CLEANUP_STATE_NODE_MONITORS:
        if (cmlcp->trigger_node_monitors) {
            Process* waiter;
            send_nodes_mon_msgs(NULL,
                                am_nodedown,
                                cmlcp->nodename,
                                cmlcp->connection_id,
                                cmlcp->visability,
                                cmlcp->reason);
            erts_de_rwlock(cmlcp->dep);
            ASSERT(cmlcp->dep->state == ERTS_DE_STATE_IDLE ||
                   cmlcp->dep->state == ERTS_DE_STATE_PENDING);
            ASSERT(cmlcp->dep->pending_nodedown);
            waiter = cmlcp->dep->suspended_nodeup;
            cmlcp->dep->suspended_nodeup = NULL;
            cmlcp->dep->pending_nodedown = 0;
            erts_de_rwunlock(cmlcp->dep);
            erts_deref_dist_entry(cmlcp->dep);

            if (waiter) {
                erts_proc_lock(waiter, ERTS_PROC_LOCK_STATUS);
                if (!ERTS_PROC_IS_EXITING(waiter)) {
                    erts_resume(waiter, ERTS_PROC_LOCK_STATUS);
                }
                erts_proc_unlock(waiter, ERTS_PROC_LOCK_STATUS);
                erts_proc_dec_refc(waiter);
            }
        }
        erts_cleanup_offheap(&cmlcp->oh);
        erts_free(ERTS_ALC_T_CML_CLEANUP, vcmlcp);
        return; /* done */
    }

    /* yield... */

    esdp = erts_get_scheduler_data();
    ASSERT(esdp && esdp->type == ERTS_SCHED_NORMAL);
    erts_schedule_misc_aux_work((int) esdp->no,
                                con_monitor_link_seq_cleanup,
                                (void *) cmlcp);
}

static void
schedule_con_monitor_link_seq_cleanup(DistEntry* dep,
                                      ErtsMonLnkDist *dist,
                                      DistSeqNode *seq,
                                      Eterm nodename,
                                      Eterm visability,
                                      Eterm reason)
{
    if (dist || is_value(nodename) || seq) {
        ErtsSchedulerData *esdp;
        ErtsConMonLnkSeqCleanup *cmlcp;
        Uint rsz, size;

        size = sizeof(ErtsConMonLnkSeqCleanup);

        if (is_non_value(reason) || is_immed(reason)) {
            rsz = 0;
            size -= sizeof(Eterm);
        }
        else {
            rsz = size_object(reason);
            size += sizeof(Eterm) * (rsz - 1);
        }

        cmlcp = erts_alloc(ERTS_ALC_T_CML_CLEANUP, size);

        ERTS_INIT_OFF_HEAP(&cmlcp->oh);

        cmlcp->yield_state = NULL;
        cmlcp->dist = dist;
        if (!dist) {
            cmlcp->state = ERTS_CML_CLEANUP_STATE_NODE_MONITORS;
            cmlcp->connection_id = 0;
        }
        else {
            cmlcp->state = ERTS_CML_CLEANUP_STATE_LINKS;
            cmlcp->connection_id = dist->connection_id;
            erts_mtx_lock(&dist->mtx);
            ASSERT(dist->alive);
            dist->alive = 0;
            erts_mtx_unlock(&dist->mtx);
        }

        cmlcp->seq = seq;

        if (is_value(nodename)) {
            ASSERT(dep);
            cmlcp->trigger_node_monitors = 1;
            cmlcp->dep = dep;
            erts_ref_dist_entry(dep);
        }
        else {
            cmlcp->trigger_node_monitors = 0;
            cmlcp->dep = NULL;
        }
        cmlcp->nodename = nodename;
        cmlcp->visability = visability;
        if (rsz == 0)
            cmlcp->reason = reason;
        else {
            Eterm *hp = &cmlcp->heap[0];
            cmlcp->reason = copy_struct(reason, rsz, &hp, &cmlcp->oh);
        }

        esdp = erts_get_scheduler_data();
        ASSERT(esdp && esdp->type == ERTS_SCHED_NORMAL);
        erts_schedule_misc_aux_work((int) esdp->no,
                                    con_monitor_link_seq_cleanup,
                                    (void *) cmlcp);
    }
}

static void
dist_pend_spawn_exit_save_child_result(Eterm result, Eterm ref, ErtsMonLnkDist *dist)
{
    ErtsMonitorData *new_mdp = NULL;
    Process *proc = NULL;
    int done = 0;

    while (1) {
        erts_mtx_lock(&dist->mtx);

        if (!dist->alive)
            done = !0;
        else {
            ErtsMonitor *mon;
            mon = erts_monitor_tree_lookup(dist->dist_pend_spawn_exit, ref);
            if (!mon) {
                if (new_mdp) {
                    /*
                     * Save info so parent can get child pid when handling
                     * links during termination
                     */
                    erts_monitor_tree_insert(&dist->dist_pend_spawn_exit,
                                             &new_mdp->u.target);
                    done = !0;
                    new_mdp = NULL;
                }
            }
            else {
                ErtsMonitorDataExtended *mdep;
                /*
                 * The terminating parent is waiting for this signal.
                 * Store childs pid and resume parent if it has
                 * suspended (i.e, mon->other.ptr != NULL)...
                 */
                proc = mon->other.ptr;
                mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
                if (is_atom(result))
                    mdep->md.origin.other.item = result;
                else {
                    Eterm *hp;
                    ErlOffHeap oh;
#ifdef DEBUG
                    int i;
                    for (i = 0; i < EXTERNAL_PID_HEAP_SIZE; i++) {
                        ASSERT(is_non_value(mdep->heap[i]));
                    }
#endif
                    ASSERT(is_external_pid(result));
                    hp = &(mdep)->heap[0];
                    ERTS_INIT_OFF_HEAP(&oh);
                    oh.first = mdep->uptr.ohhp;
                    mdep->md.origin.other.item
                        = copy_struct(result,
                                      EXTERNAL_PID_HEAP_SIZE,
                                      &hp, &oh);
                    mdep->uptr.ohhp = oh.first;
                }
                done = !0;
            }
        }
    
        erts_mtx_unlock(&dist->mtx);

        if (done)
            break;

        /*
         * No monitor previously saved by parent; create one
         * and store child pid...
         */
        new_mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC, ref,
                                      am_undefined, result, NIL,
                                      THE_NON_VALUE);
        ASSERT(new_mdp->u.target.other.item == am_undefined);
        new_mdp->u.target.other.ptr = NULL;

        ((ErtsMonitorDataExtended *) new_mdp)->dist = dist;
        erts_mon_link_dist_inc_refc(dist);
        erts_monitor_release(&new_mdp->origin);
    }

    if (proc)
        erts_resume(proc, 0);

    if (new_mdp)
        erts_monitor_release(&new_mdp->u.target);

}

int
erts_dist_pend_spawn_exit_delete(ErtsMonitor *mon)
{
    ErtsMonitorDataExtended *mdep;
    int delete;
    ErtsMonLnkDist *dist;
    Uint16 flags;

    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
    dist = mdep->dist;
    
    erts_mtx_lock(&dist->mtx);
    
    flags = mon->flags;
    delete = !!dist->alive & !!(flags & ERTS_ML_FLG_IN_TABLE);

    if (delete)
        erts_monitor_tree_delete(&dist->dist_pend_spawn_exit, mon);

    erts_mtx_unlock(&dist->mtx);

    return delete;
}

int
erts_dist_pend_spawn_exit_parent_setup(ErtsMonitor *mon)
{
    /*
     * Return:
     * 0 -> connection is closing
     * !0 -> moved target part of monitor to dist_pend_spawn_exit
     */
    ErtsMonitorData *mdp;
    ErtsMonLnkDist *dist;
    int res;

    ASSERT(erts_monitor_is_origin(mon));

    mdp = (ErtsMonitorData *) erts_monitor_to_data(mon);

    if (!erts_monitor_dist_delete(&mdp->u.target))
        return 0;
    
    dist = ((ErtsMonitorDataExtended *) mdp)->dist;

    while (1) {
        ErtsMonitor *tmp_mon;

        erts_mtx_lock(&dist->mtx);
        mdp->u.target.other.ptr = NULL;

        if (!dist->alive) {
            res = 0;
            tmp_mon = NULL;
        }
        else {
            res = !0;
            tmp_mon = erts_monitor_tree_lookup(dist->dist_pend_spawn_exit,
                                               mdp->ref);
            if (!tmp_mon)
                erts_monitor_tree_insert(&dist->dist_pend_spawn_exit,
                                         &mdp->u.target);
            else
                erts_monitor_tree_delete(&dist->dist_pend_spawn_exit, tmp_mon);
        }
    
        erts_mtx_unlock(&dist->mtx);

        if (!tmp_mon) {
            if (!res)
                erts_monitor_release(&mdp->u.target);
            return res;
        }
        else {
            /*
             * Child had responded; copy its pid then store
             * original target end in 'dist_pend_spawn_exit'
             */
            ErtsMonitorData *tmp_mdp = erts_monitor_to_data(tmp_mon);

            if (is_atom(tmp_mdp->origin.other.item)) {
                erts_monitor_release(tmp_mon);
                erts_monitor_release(&mdp->u.target);
                return 0; /* Spawn failed; drop it... */
            }

            /*
             * If mdp->origin.other.item != am_pending, the other
             * end is behaving badly by sending multiple
             * responses...
             */
            if (mdp->origin.other.item == am_pending) {
                ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
                ErlOffHeap oh;
                Eterm *hp;
#ifdef DEBUG
                int i;

                ASSERT(is_external_pid(tmp_mdp->origin.other.item));
                for (i = 0; i < EXTERNAL_PID_HEAP_SIZE; i++) {
                    ASSERT(is_non_value(mdep->heap[i]));
                }
#endif
                hp = &(mdep)->heap[0];
                ERTS_INIT_OFF_HEAP(&oh);
                oh.first = mdep->uptr.ohhp;
                mdep->md.origin.other.item
                    = copy_struct(tmp_mdp->origin.other.item,
                                  EXTERNAL_PID_HEAP_SIZE,
                                  &hp, &oh);
                mdep->uptr.ohhp = oh.first;
            }
            erts_monitor_release(tmp_mon);
        }
    }
}

int
erts_dist_pend_spawn_exit_parent_wait(Process *c_p,
                                      ErtsProcLocks locks,
                                      ErtsMonitor *mon)
{
    /*
     * return value of
     * > 0 --> Child pid can now be found in monitor
     * 0   --> Connection not alive; drop it
     * < 0 --> Setup completed; later need to wait for child pid
     */
    ErtsMonitorData *mdp;
    ErtsMonLnkDist *dist;
    int res;
    
    ASSERT(erts_monitor_is_origin(mon));

    mdp = (ErtsMonitorData *) erts_monitor_to_data(mon);
    dist = ((ErtsMonitorDataExtended *) mdp)->dist;

    erts_mtx_lock(&dist->mtx);

    if (!dist->alive) {
        res = 0;
    }
    else {
        ASSERT(&mdp->u.target ==
               erts_monitor_tree_lookup(dist->dist_pend_spawn_exit, mdp->ref));
        if (mdp->origin.other.item != am_pending) {
            erts_monitor_tree_delete(&dist->dist_pend_spawn_exit, &mdp->u.target);
            res = 1;
        }
        else {
            /* We need to suspend wait and wait for the result... */
            mdp->u.target.other.ptr = (void *) c_p;
            erts_suspend(c_p, locks, NULL);
            res = -1;
        }
    }
    
    erts_mtx_unlock(&dist->mtx);

    return res;
}

/*
** A full node name consists of a "n@h"
**
** n must be a valid node name: string of ([a-z][A-Z][0-9]_-)+
** 
** h is not checked at all, we assume that we have a properly
** configured machine where the networking is ok for the OS
**
** We do check that there is not a second @ in the string, since
** many distributed operations are guaranteed not to work then.
*/


static int is_node_name(char *ptr, int len)
{
   int c = '\0';		/* suppress use-before-set warning */
   int pos = 0;

   while (pos < len) {
      c = ptr[pos++];
      if (! ((c == '-') || (c == '_') ||
	     ((c >= 'a') && (c <= 'z')) ||
	     ((c >= 'A') && (c <= 'Z')) ||
	     ((c >= '0') && (c <= '9'))))
	 break;
   }

   /* Scanned past the host name: now we want to see a '@', and there
      should be text both before and after it. */
   if (c != '@' || pos < 2 || pos == len)
      return 0;

   while (pos < len) {
      c = ptr[pos++];
      if (c == '@')
	 return 0;
   }

   return 1;
}

int is_node_name_atom(Eterm a)
{
    int i;
    if(is_not_atom(a))
	return 0;
    i = atom_val(a);
    ASSERT((i > 0) && (i < atom_table_size()) &&  (atom_tab(i) != NULL));
    return is_node_name((char*)erts_atom_get_name(atom_tab(i)), atom_tab(i)->len);
}

static void
set_node_not_alive(void *unused)
{
    ErlHeapFragment *bp;
    Eterm nodename = erts_this_dist_entry->sysname;

    ASSERT(erts_atomic_read_nob(&no_nodes) == 0);

    erts_thr_progress_block();
    erts_set_this_node(am_Noname, 0);
    erts_is_alive = 0;
    send_nodes_mon_msgs(NULL, am_nodedown, nodename, ~((Uint32) 0),
                        am_visible, nodedown.reason);
    nodedown.reason = NIL;
    bp = nodedown.bp;
    nodedown.bp = NULL;
    erts_thr_progress_unblock();
    if (bp)
	free_message_buffer(bp);
}

static ERTS_INLINE void
dec_no_nodes(void)
{
    erts_aint_t no = erts_atomic_dec_read_mb(&no_nodes);
    ASSERT(no >= 0);
    ASSERT(erts_get_scheduler_id()); /* Need to be a scheduler */
    if (no == 0)
	erts_schedule_misc_aux_work(erts_get_scheduler_id(),
				    set_node_not_alive,
				    NULL);
}

static ERTS_INLINE void
inc_no_nodes(void)
{
#ifdef DEBUG
    erts_aint_t no = erts_atomic_read_nob(&no_nodes);
    ASSERT(erts_is_alive ? no > 0 : no == 0);
#endif
    erts_atomic_inc_mb(&no_nodes);
}

static void
kill_dist_ctrl_proc(void *vpid)
{
    /* Send a 'kill' exit signal from init process */
    Process *init_proc = erts_proc_lookup_raw(erts_init_process_id);
    erts_proc_sig_send_exit(&init_proc->common, erts_init_process_id,
                            (Eterm)vpid, am_kill, NIL, 0, 0);
}

static void
schedule_kill_dist_ctrl_proc(Eterm pid)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int sched_id = 1;
    if (!esdp || ERTS_SCHEDULER_IS_DIRTY(esdp))
        sched_id = 1;
    else
        sched_id = (int) esdp->no;
    erts_schedule_misc_aux_work(sched_id,
                                kill_dist_ctrl_proc,
                                (void *) (UWord) pid);
}

/*
 * proc is currently running or exiting process.
 */
int erts_do_net_exits(DistEntry *dep, Eterm reason)
{
    Eterm nodename;

    if (dep == erts_this_dist_entry) {  /* Net kernel has died (clean up!!) */
	DistEntry *tdep;
	int no_dist_ctrl;
        int no_pending;
	Eterm nd_reason = (reason == am_no_network
			   ? am_no_network
			   : am_net_kernel_terminated);
        int i = 0;
        Eterm *dist_ctrl;
        DistEntry** pending;

        ERTS_UNDEF(dist_ctrl, NULL);
        ERTS_UNDEF(pending, NULL);

	erts_rwmtx_rlock(&erts_dist_table_rwmtx);

        no_dist_ctrl = (erts_no_of_hidden_dist_entries +
                        erts_no_of_visible_dist_entries);
        no_pending = erts_no_of_pending_dist_entries;

	/* KILL all port controllers */
	if (no_dist_ctrl) {
            dist_ctrl = erts_alloc(ERTS_ALC_T_TMP,
                                   sizeof(Eterm)*no_dist_ctrl);
	    for (tdep = erts_hidden_dist_entries; tdep; tdep = tdep->next) {
		ASSERT(is_internal_port(tdep->cid) || is_internal_pid(tdep->cid));
                ASSERT(i < no_dist_ctrl);
		dist_ctrl[i++] = tdep->cid;
	    }
	    for (tdep = erts_visible_dist_entries; tdep; tdep = tdep->next) {
		ASSERT(is_internal_port(tdep->cid) || is_internal_pid(tdep->cid));
                ASSERT(i < no_dist_ctrl);
		dist_ctrl[i++] = tdep->cid;
	    }
            ASSERT(i == no_dist_ctrl);
        }
        if (no_pending) {
            pending = erts_alloc(ERTS_ALC_T_TMP, sizeof(DistEntry*)*no_pending);
            i = 0;
            for (tdep = erts_pending_dist_entries; tdep; tdep = tdep->next) {
                ASSERT(is_nil(tdep->cid));
                ASSERT(i < no_pending);
                pending[i++] = tdep;
                erts_ref_dist_entry(tdep);
            }
            ASSERT(i == no_pending);
        }
        erts_rwmtx_runlock(&erts_dist_table_rwmtx);

        if (no_dist_ctrl) {
            for (i = 0; i < no_dist_ctrl; i++) {
                if (is_internal_pid(dist_ctrl[i]))
                    schedule_kill_dist_ctrl_proc(dist_ctrl[i]);
                else {
                    Port *prt = erts_port_lookup(dist_ctrl[i],
                                                 ERTS_PORT_SFLGS_INVALID_LOOKUP);
                    if (prt) {
                        ASSERT(erts_atomic32_read_nob(&prt->state)
                               & ERTS_PORT_SFLG_DISTRIBUTION);

                        erts_port_exit(NULL, ERTS_PORT_SIG_FLG_FORCE_SCHED,
                                       prt, dist_ctrl[i], nd_reason, NULL);
                    }
                }
            }
            erts_free(ERTS_ALC_T_TMP, dist_ctrl);
        }

        if (no_pending) {
            for (i = 0; i < no_pending; i++) {
                abort_pending_connection(pending[i], pending[i]->connection_id, NULL);
                erts_deref_dist_entry(pending[i]);
            }
            erts_free(ERTS_ALC_T_TMP, pending);
        }


	/*
	 * When last dist ctrl exits, node will be taken
	 * from alive to not alive.
	 */
	ASSERT(is_nil(nodedown.reason) && !nodedown.bp);
	if (is_immed(nd_reason))
	    nodedown.reason = nd_reason;
	else {
	    Eterm *hp;
	    Uint sz = size_object(nd_reason);
	    nodedown.bp = new_message_buffer(sz);
	    hp = nodedown.bp->mem;
	    nodedown.reason = copy_struct(nd_reason,
					  sz,
					  &hp,
					  &nodedown.bp->off_heap);
	}
    }
    else { /* Call from distribution controller (port/process) */
        ErtsMonLnkDist *mld;
        DistSeqNode *sequences;
        ErtsAtomCache *cache;
        ErtsProcList *suspendees;
        ErtsDistOutputBuf *obuf;
	Uint64 flags;

	erts_atomic_set_mb(&dep->dist_cmd_scheduled, 1);
	erts_de_rwlock(dep);

        if (is_internal_port(dep->cid)) {
            ERTS_LC_ASSERT(erts_lc_is_port_locked(erts_port_lookup_raw(dep->cid)));

            if (erts_port_task_is_scheduled(&dep->dist_cmd))
                erts_port_task_abort(&dep->dist_cmd);
        }
        else {
            ASSERT(is_internal_pid(dep->cid));
            /*
             * Supervised distribution controllers may exit "normally" with
             * {shutdown,Reason}. Unwrap such shutdown tuple to get a correct
             * documented 'nodedown_reason' from net_kernel:montitor_nodes.
             */
            if (is_tuple_arity(reason, 2)) {
                Eterm* tpl = tuple_val(reason);
                if (tpl[1] == am_shutdown)
                    reason = tpl[2];
            }
        }

	if (dep->state == ERTS_DE_STATE_EXITING) {
	    ASSERT(de_qflags_read(dep) & ERTS_DE_QFLG_EXIT);
	}
	else {
            ASSERT(dep->state == ERTS_DE_STATE_CONNECTED);
	    dep->state = ERTS_DE_STATE_EXITING;
	    erts_mtx_lock(&dep->qlock);
	    ASSERT(!(de_qflags_read(dep) & ERTS_DE_QFLG_EXIT));
	    de_qflags_read_set(dep, ERTS_DE_QFLG_EXIT);
	    erts_mtx_unlock(&dep->qlock);
	}

        mld = dep->mld;
        dep->mld = NULL;

        sequences = dep->sequences;
        dep->sequences = NULL;

	nodename = dep->sysname;
	flags = dep->dflags;

        erts_atomic_set_nob(&dep->input_handler, (erts_aint_t) NIL);
        cache = dep->cache;
        dep->cache = NULL;

        erts_mtx_lock(&dep->qlock);

        erts_atomic64_set_nob(&dep->in, 0);
        erts_atomic64_set_nob(&dep->out, 0);

        obuf = clear_de_out_queues(dep);
        suspendees = get_suspended_on_de(dep, ERTS_DE_QFLGS_ALL);

        erts_mtx_unlock(&dep->qlock);

        erts_atomic32_set_relb(&dep->notify, 0);
        erts_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
        dep->send = NULL;

	erts_set_dist_entry_not_connected(dep);
        dep->pending_nodedown = 1;
	erts_de_rwunlock(dep);

        schedule_con_monitor_link_seq_cleanup(dep,
                                              mld,
                                              sequences,
                                              nodename,
                                              (flags & DFLAG_PUBLISHED
                                               ? am_visible
                                               : am_hidden),
                                              (reason == am_normal
                                               ? am_connection_closed
                                               : reason));

        erts_resume_processes(suspendees);

        delete_cache(cache);

        free_de_out_queues(dep, obuf);
    }

    dec_no_nodes();

    return 1;
}

static Export*
trap_function(Eterm func, int arity)
{
    return erts_export_put(am_erlang, func, arity);
}

static BIF_RETTYPE spawn_request_yield_3(BIF_ALIST_3);

void init_dist(void)
{
    init_nodes_monitors();

#ifdef ERTS_DIST_MSG_DBG_FILE
    {
        char buff[255];
        sprintf(buff, ERTS_DIST_MSG_DBG_FILE, getpid());
        dbg_file = fopen(buff,"w+");
    }
#elif defined(ERTS_DIST_MSG_DBG) || defined(ERTS_RAW_DIST_MSG_DBG)
    dbg_file = stderr;
#endif

    nodedown.reason = NIL;
    nodedown.bp = NULL;

    erts_atomic_init_nob(&no_nodes, 0);
    erts_atomic_init_nob(&no_caches, 0);

    /* Lookup/Install all references to trap functions */
    dmonitor_node_trap = trap_function(am_dmonitor_node,3);
    dist_ctrl_put_data_trap = erts_export_put(am_erts_internal,
                                              am_dist_ctrl_put_data,
                                              2);
    erts_init_trap_export(&spawn_request_yield_export,
                          am_erts_internal, am_spawn_request_yield,
                          3, spawn_request_yield_3);
    {
        Eterm *hp_start, *hp, **hpp = NULL, tuple;
        Uint sz = 0, *szp = &sz;
        ERTS_UNDEF(hp, NULL);
        ERTS_UNDEF(hp_start, NULL);
        while (1) {
            struct erl_off_heap_header **ohp;
            /*
             * Sync with dist_util.erl:
             *
             * -record(erts_dflags,
             *         {default, mandatory, addable, rejectable, strict_order}).
             */
            tuple = erts_bld_tuple(hpp, szp, 6,
                               am_erts_dflags,
                               erts_bld_uint64(hpp, szp, DFLAG_DIST_DEFAULT),
                               erts_bld_uint64(hpp, szp, DFLAG_DIST_MANDATORY),
                               erts_bld_uint64(hpp, szp, DFLAG_DIST_ADDABLE),
                               erts_bld_uint64(hpp, szp, DFLAG_DIST_REJECTABLE),
                               erts_bld_uint64(hpp, szp, DFLAG_DIST_STRICT_ORDER));
            if (hpp) {
                ASSERT(is_value(tuple));
                ASSERT(hp == hp_start + sz);
                erts_global_literal_register(&tuple);
                ERTS_GLOBAL_LIT_DFLAGS_RECORD = tuple;
                break;
            }
            hp = hp_start = erts_global_literal_allocate(sz, &ohp);
            hpp = &hp;
            szp = NULL;
        }
    }
    ERTS_CT_ASSERT(sizeof(ErtsDistOutputBuf) % sizeof(void*) == 0);
    ERTS_CT_ASSERT(sizeof(SysIOVec) % sizeof(void*) == 0);
    ERTS_CT_ASSERT(sizeof(ErlIOVec) % sizeof(void*) == 0);
}

static ERTS_INLINE ErtsDistOutputBuf *
alloc_dist_obufs(byte **extp, TTBEncodeContext *ctx,
                 Uint data_size, Uint fragments, Uint vlen,
                 int ignore_busy)
{
    int ix;
    ErtsDistOutputBuf *obuf;
    char *ptr;
    Uint iov_sz, obsz;
    Binary *bin;
    Uint fragment_size;

    obsz = sizeof(ErtsDistOutputBuf)*fragments;

    iov_sz = erts_ttb_iov_size(0, vlen, fragments);
    
    bin = erts_bin_drv_alloc(obsz + iov_sz + data_size);
    ctx->result_bin = bin;
    ptr = (char *) &bin->orig_bytes[0];
    
    obuf = (ErtsDistOutputBuf *) ptr;
    ptr += obsz;

    if (fragments > 1)
        fragment_size = ERTS_DIST_FRAGMENT_SIZE;
    else
        fragment_size = ~((Uint) 0);

    erts_ttb_iov_init(ctx, 0, ptr, vlen, fragments, fragment_size);
    ptr += iov_sz;

    erts_refc_add(&bin->intern.refc, fragments - 1, 1);

    for (ix = 0; ix < fragments; ix++) {
        obuf[ix].ignore_busy = ignore_busy;
        obuf[ix].bin = bin;
        obuf[ix].eiov = &ctx->fragment_eiovs[ix];
#ifdef DEBUG
        obuf[ix].dbg_pattern = ERTS_DIST_OUTPUT_BUF_DBG_PATTERN;
#endif
    }
    *extp = (byte *) ptr;
    return obuf;
}

static ERTS_INLINE void
free_dist_obuf(ErtsDistOutputBuf *obuf, int free_binv)
{
    ASSERT(obuf->dbg_pattern == ERTS_DIST_OUTPUT_BUF_DBG_PATTERN);

    if (free_binv) {
        int i;
        int vlen = obuf->eiov->vsize;
        ErlDrvBinary **binv = obuf->eiov->binv;
        for (i = 0; i < vlen; i++) {
            if (binv[i])
                driver_free_binary(binv[i]);
        }
    }
    erts_bin_release(obuf->bin);
}

static ERTS_INLINE Sint
size_obuf(ErtsDistOutputBuf *obuf)
{
    Sint vlen = obuf->eiov->vsize;
    Sint sz;
#ifdef DEBUG
    Sint i;
    for (i = 0, sz = 0; i < vlen; i++)
        sz += obuf->eiov->iov[i].iov_len;
    ASSERT(sz == obuf->eiov->size);
#endif
    sz = sizeof(ErtsDistOutputBuf) + sizeof(ErlIOVec);
    sz += obuf->eiov->size;
    sz += sizeof(SysIOVec)*vlen;
    sz += sizeof(ErlDrvBinary*)*vlen;
    return sz;
}

static ERTS_INLINE void
get_obuf_sizes(ErtsDistOutputBuf *obuf, Sint *size, Sint *ignore_size)
{
    Sint sz = size_obuf(obuf);
    ASSERT(sz >= 0);
    *size = sz;
    *ignore_size = obuf->ignore_busy ? sz : 0;
}

static ERTS_INLINE void
add_obuf_sizes(ErtsDistOutputBuf *obuf, Sint *size, Sint *ignore_size)
{
    Sint sz, isz;
    get_obuf_sizes(obuf, &sz, &isz);
    *size += sz;
    *ignore_size += isz;
}

static ERTS_INLINE void
subtract_obuf_sizes(ErtsDistOutputBuf *obuf, Sint *size, Sint *ignore_size)
{
    Sint sz, isz;
    get_obuf_sizes(obuf, &sz, &isz);
    *size -= sz;
    *ignore_size -= isz;
}

static ERTS_INLINE void
update_qsizes(DistEntry *dep, int *empty_fillp, Sint *qsizep,
              Sint add_total_qsize, Sint ignore_qsize)
{
    /*
     * All modifications of the 'total_qsize' and 'qsize' fields are
     * made while holding the 'qlock', so read/modify/write of each
     * field does not need to be atomic. Readers without the lock will
     * still see consistent updates of each 'field'.
     */
    erts_aint_t qsize, add_qsize;

    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&dep->qlock));

    if (empty_fillp)
        *empty_fillp = 0;

    if (add_total_qsize) {
        qsize = erts_atomic_read_nob(&dep->total_qsize);
        qsize += (erts_aint_t) add_total_qsize;
        if (empty_fillp && qsize == add_total_qsize)
            *empty_fillp = !0;
        erts_atomic_set_nob(&dep->total_qsize, (erts_aint_t) qsize);
    }

    add_qsize = (erts_aint_t) (add_total_qsize - ignore_qsize);
    if (add_qsize) {
        qsize = erts_atomic_read_nob(&dep->qsize);
        qsize += add_qsize;
        if (qsizep)
            *qsizep = qsize;
        erts_atomic_set_nob(&dep->qsize, (erts_aint_t) qsize);
    }
    else if (qsizep) {
        *qsizep = erts_atomic_read_nob(&dep->qsize);
    }

#ifdef DEBUG
    {
        erts_aint_t tqsize = erts_atomic_read_nob(&dep->total_qsize);
        qsize = erts_atomic_read_nob(&dep->qsize);
        ASSERT(tqsize >= 0);
        ASSERT(qsize >= 0);
        ASSERT(tqsize >= qsize);
    }
#endif
}

static ErtsDistOutputBuf* clear_de_out_queues(DistEntry* dep)
{
    ErtsDistOutputBuf *obuf;

    ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&dep->qlock));

    if (!dep->out_queue.last)
	obuf = dep->finalized_out_queue.first;
    else {
	dep->out_queue.last->next = dep->finalized_out_queue.first;
	obuf = dep->out_queue.first;
    }

    if (dep->tmp_out_queue.first) {
        dep->tmp_out_queue.last->next = obuf;
        obuf = dep->tmp_out_queue.first;
    }

    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    dep->tmp_out_queue.first = NULL;
    dep->tmp_out_queue.last = NULL;
    dep->finalized_out_queue.first = NULL;
    dep->finalized_out_queue.last = NULL;

    return obuf;
}

static void free_de_out_queues(DistEntry* dep, ErtsDistOutputBuf *obuf)
{
    Sint obufsize = 0, ignore_obufsize = 0;

    while (obuf) {
	ErtsDistOutputBuf *fobuf;
	fobuf = obuf;
	obuf = obuf->next;
        add_obuf_sizes(fobuf, &obufsize, &ignore_obufsize);
	free_dist_obuf(fobuf, !0);
    }

    if (obufsize) {
	erts_mtx_lock(&dep->qlock);
        update_qsizes(dep, NULL, NULL, -obufsize, -ignore_obufsize);
	erts_mtx_unlock(&dep->qlock);
    }
}

int erts_dsend_context_dtor(Binary* ctx_bin)
{
    ErtsDSigSendContext* ctx = ERTS_MAGIC_BIN_DATA(ctx_bin);
    switch (ctx->phase) {
    case ERTS_DSIG_SEND_PHASE_MSG_SIZE:
	DESTROY_SAVED_WSTACK(&ctx->u.sc.wstack);
	break;
    case ERTS_DSIG_SEND_PHASE_MSG_ENCODE:
	DESTROY_SAVED_WSTACK(&ctx->u.ec.wstack);
	break;
    default:;
    }
    if (ctx->phase >= ERTS_DSIG_SEND_PHASE_ALLOC && ctx->obuf) {
        int i;
        for (i = 0; i < ctx->fragments; i++)
            free_dist_obuf(&ctx->obuf[i], !0);
    }
    if (ctx->deref_dep)
	erts_deref_dist_entry(ctx->dep);

    return 1;
}

Eterm erts_dsend_export_trap_context(Process* p, ErtsDSigSendContext* ctx)
{
    struct exported_ctx {
	ErtsDSigSendContext ctx;
	ErtsAtomCacheMap acm;
    };
    Binary* ctx_bin = erts_create_magic_binary(sizeof(struct exported_ctx),
					       erts_dsend_context_dtor);
    struct exported_ctx* dst = ERTS_MAGIC_BIN_DATA(ctx_bin);
    Eterm* hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);

    sys_memcpy(&dst->ctx, ctx, sizeof(ErtsDSigSendContext));
    ASSERT(ctx->ctl == make_tuple(ctx->ctl_heap));
    dst->ctx.ctl = make_tuple(dst->ctx.ctl_heap);
    if (ctx->acmp) {
	sys_memcpy(&dst->acm, ctx->acmp, sizeof(ErtsAtomCacheMap));
	dst->ctx.acmp = &dst->acm;
    }
    return erts_mk_magic_ref(&hp, &MSO(p), ctx_bin);
}


/*
 * The erts_dsig_send_*() functions implemented below, sends asynchronous
 * distributed signals to other Erlang nodes. Before sending a distributed
 * signal, you need to prepare the operation by calling erts_dsig_prepare()
 * (see dist.h).
 *
 * Note that the distributed signal send operation is truly asynchronous,
 * and the signal is not guaranteed to reach the receiver if the connection
 * goes down before the signal has reached the receiver.
 */

/*
** Send a DOP_LINK link message
*/
int
erts_dsig_send_link(ErtsDSigSendContext *ctx, Eterm local, Eterm remote)
{
    Eterm ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_LINK), local, remote);
    return dsig_send_ctl(ctx, ctl);
}

int
erts_dsig_send_unlink(ErtsDSigSendContext *ctx, Eterm local, Eterm remote, Uint64 id)
{
    Eterm big_heap[ERTS_MAX_UINT64_HEAP_SIZE];
    Eterm unlink_id;    
    Eterm ctl;
    if (IS_USMALL(0, id))
        unlink_id = make_small(id);
    else {
        Eterm *hp = &big_heap[0];
        unlink_id = erts_uint64_to_big(id, &hp);
    }
    ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_UNLINK_ID),
                 unlink_id, local, remote);
    return dsig_send_ctl(ctx, ctl);
}

int
erts_dsig_send_unlink_ack(ErtsDSigSendContext *ctx, Eterm local, Eterm remote, Uint64 id)
{
    Eterm big_heap[ERTS_MAX_UINT64_HEAP_SIZE];
    Eterm unlink_id;
    Eterm ctl;

    if (IS_USMALL(0, id))
        unlink_id = make_small(id);
    else {
        Eterm *hp = &big_heap[0];
        unlink_id = erts_uint64_to_big(id, &hp);
    }
    ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_UNLINK_ID_ACK),
                 unlink_id, local, remote);
    return dsig_send_ctl(ctx, ctl);
}


/* A local process that's being monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason} */
int
erts_dsig_send_m_exit(ErtsDSigSendContext *ctx, Eterm watcher, Eterm watched,
                      Eterm ref, Eterm reason)
{
    Eterm ctl, msg;

    if (~ctx->dflags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_MONITOR_P_EXIT (see dsig_send_monitor)
         */
        return ERTS_DSIG_SEND_OK;
    }

    if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD) {
        ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_PAYLOAD_MONITOR_P_EXIT),
                     watched, watcher, ref);
        msg = reason;
    } else {
        ctl = TUPLE5(&ctx->ctl_heap[0], make_small(DOP_MONITOR_P_EXIT),
                     watched, watcher, ref, reason);
        msg = THE_NON_VALUE;
    }

    return dsig_send_exit(ctx, ctl, msg);
}

/* We want to monitor a process (named or unnamed) on another node, we send:
   {DOP_MONITOR_P, Local pid, Remote pid or name, Ref}, which is exactly what's
   needed on the other side... */
int
erts_dsig_send_monitor(ErtsDSigSendContext *ctx, Eterm watcher, Eterm watched,
		       Eterm ref)
{
    Eterm ctl;

    if (~ctx->dflags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_MONITOR_P.
         * Just avoid sending it and by doing that reduce this monitor
         * to only supervise the connection. This will work for simple c-nodes
         * with a 1-to-1 relation between "Erlang process" and OS-process.
         */
        return ERTS_DSIG_SEND_OK;
    }

    ctl = TUPLE4(&ctx->ctl_heap[0],
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    return dsig_send_ctl(ctx, ctl);
}

/* A local process monitoring a remote one wants to stop monitoring, either 
   because of a demonitor bif call or because the local process died. We send
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref} */
int
erts_dsig_send_demonitor(ErtsDSigSendContext *ctx, Eterm watcher,
			 Eterm watched, Eterm ref)
{
    Eterm ctl;

    if (~ctx->dflags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_DEMONITOR_P (see dsig_send_monitor)
         */
        return ERTS_DSIG_SEND_OK;
    }

    ctl = TUPLE4(&ctx->ctl_heap[0],
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    return dsig_send_ctl(ctx, ctl);
}

static int can_send_seqtrace_token(ErtsDSigSendContext* ctx, Eterm token) {
    Eterm label;

    if (ctx->dflags & DFLAG_BIG_SEQTRACE_LABELS) {
        /* The other end is capable of handling arbitrary seq_trace labels. */
        return 1;
    }

    /* The other end only tolerates smalls, but since we could potentially be
     * talking to an old 32-bit emulator from a 64-bit one, we have to check
     * whether the label is small on any emulator. */
    label = SEQ_TRACE_T_LABEL(token);

    return is_small(label) &&
        signed_val(label) <= (ERTS_SINT32_MAX >> _TAG_IMMED1_SIZE) &&
        signed_val(label) >= (ERTS_SINT32_MIN >> _TAG_IMMED1_SIZE);
}

int
erts_dsig_send_msg(ErtsDSigSendContext* ctx, Eterm remote, Eterm full_remote,
                   Eterm message, int prio)
{
    Eterm ctl;
    Eterm token = NIL;
    Process *sender = ctx->c_p;
    int res;
#ifdef USE_VM_PROBES
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    Uint msize = 0;
    DTRACE_CHARBUF(node_name, 64);
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(receiver_name, 2050);
#endif

    ASSERT(is_external_pid(remote)
           || is_external_ref(remote)
           || is_atom(remote));

    if (have_seqtrace(SEQ_TRACE_TOKEN(sender))) {
	seq_trace_update_serial(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, full_remote, sender);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *receiver_name = '\0';
    if (DTRACE_ENABLED(message_send) || DTRACE_ENABLED(message_send_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", ctx->dep->sysname);
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
                      "%T", sender->common.id);
        if (is_not_atom(remote))
            erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
                          "%T", remote);
        else
            erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
                          "{%T,%s}", remote, node_name);
        msize = size_object(message);
        if (have_seqtrace(token)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(token);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
        }
    }
#endif

    {
        Eterm dist_op, sender_id;
        int send_token;

        send_token = (token != NIL && can_send_seqtrace_token(ctx, token));

        if ((prio || is_external_ref(remote))
            && (ctx->dflags & DFLAG_ALTACT_SIG)) {
            int fs = 0;
            Eterm flags, *hp = &ctx->ctl_heap[0];
            if (is_external_ref(remote))
                fs |= ERTS_DOP_ALTACT_SIG_FLG_ALIAS;
            else if (is_atom(remote))
                fs |= ERTS_DOP_ALTACT_SIG_FLG_NAME;
            if (prio)
                fs |= ERTS_DOP_ALTACT_SIG_FLG_PRIO;
            if (send_token)
                fs |= ERTS_DOP_ALTACT_SIG_FLG_TOKEN;
            dist_op = make_small(DOP_ALTACT_SIG_SEND);
            flags = make_small(fs);
            sender_id = sender->common.id;
            if (send_token) {
                ctl = TUPLE5(hp, dist_op, flags, sender_id, remote, token);
            }
            else {
                ctl = TUPLE4(hp, dist_op, flags, sender_id, remote);
            }
        }
        else if (is_atom(remote)) {
            if (send_token)
                ctl = TUPLE5(&ctx->ctl_heap[0], make_small(DOP_REG_SEND_TT),
                             sender->common.id, am_Empty, remote, token);
            else
                ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_REG_SEND),
                             sender->common.id, am_Empty, remote);
        }
        else {

            if (is_external_ref(remote)) {
                dist_op = make_small(send_token ?
                                     DOP_ALIAS_SEND_TT :
                                     DOP_ALIAS_SEND);
                sender_id = sender->common.id;
            }
            else if (ctx->dflags & DFLAG_SEND_SENDER) {
                dist_op = make_small(send_token ?
                                     DOP_SEND_SENDER_TT :
                                     DOP_SEND_SENDER);
                sender_id = sender->common.id;
            } else {
                dist_op = make_small(send_token ?
                                     DOP_SEND_TT :
                                     DOP_SEND);
                sender_id = am_Empty;
            }

            if (send_token) {
                ctl = TUPLE4(&ctx->ctl_heap[0], dist_op, sender_id, remote, token);
            } else {
                ctl = TUPLE3(&ctx->ctl_heap[0], dist_op, sender_id, remote);
            }
        }
    }

    DTRACE6(message_send, sender_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    DTRACE7(message_send_remote, sender_name, node_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    ctx->ctl = ctl;
    ctx->msg = message;
    res = erts_dsig_send(ctx);
    return res;
}

/* local has died, deliver the exit signal to remote */
int
erts_dsig_send_exit_tt(ErtsDSigSendContext *ctx, Process *c_p, Eterm remote, 
		       Eterm reason, Eterm token)
{
    Eterm ctl, msg = THE_NON_VALUE, local = c_p->common.id;
#ifdef USE_VM_PROBES
    Process *sender = c_p;
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    DTRACE_CHARBUF(node_name, 64);
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(remote_name, 128);
    DTRACE_CHARBUF(reason_str, 128);
#endif

    if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD)
        msg = reason;

    if (have_seqtrace(token)) {
	seq_trace_update_serial(c_p);
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
        if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD) {
            ctl = TUPLE4(&ctx->ctl_heap[0],
                         make_small(DOP_PAYLOAD_EXIT_TT), local, remote, token);
        } else
            ctl = TUPLE5(&ctx->ctl_heap[0],
                         make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
        if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD)
            ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_PAYLOAD_EXIT), local, remote);
        else
            ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_EXIT), local, remote, reason);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *remote_name = '\0';
    if (DTRACE_ENABLED(process_exit_signal_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", ctx->dep->sysname);
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
                      "%T", sender->common.id);
        erts_snprintf(remote_name, sizeof(DTRACE_CHARBUF_NAME(remote_name)),
                      "{%T,%s}", remote, node_name);
        erts_snprintf(reason_str, sizeof(DTRACE_CHARBUF_NAME(reason_str)),
                      "%T", reason);
        if (have_seqtrace(token)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(token);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
        }
    }
#endif
    DTRACE7(process_exit_signal_remote, sender_name, node_name,
            remote_name, reason_str, tok_label, tok_lastcnt, tok_serial);
    return dsig_send_exit(ctx, ctl, msg);
}

int
erts_dsig_send_exit(ErtsDSigSendContext *ctx, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl, msg = ctx->dep->dflags & DFLAG_EXIT_PAYLOAD ? reason : THE_NON_VALUE;

    if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD) {
        ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_PAYLOAD_EXIT), local, remote);
        msg = reason;
    } else {
        ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_EXIT), local, remote, reason);
        msg = THE_NON_VALUE;
    }
    return dsig_send_exit(ctx, ctl, msg);
}

int
erts_dsig_send_exit2(ErtsDSigSendContext *ctx, Eterm local, Eterm remote,
                     Eterm reason, int prio)
{
    Eterm ctl, msg;

    if (!(prio || is_ref(remote))) {
    non_altact_sig:
        if (ctx->dep->dflags & DFLAG_EXIT_PAYLOAD) {
            ctl = TUPLE3(&ctx->ctl_heap[0],
                         make_small(DOP_PAYLOAD_EXIT2), local, remote);
            msg = reason;
        } else {
            ctl = TUPLE4(&ctx->ctl_heap[0],
                         make_small(DOP_EXIT2), local, remote, reason);
            msg = THE_NON_VALUE;
        }
    }
    else {
        int fs = ERTS_DOP_ALTACT_SIG_FLG_EXIT;
        Eterm flags, *hp = &ctx->ctl_heap[0];

        if (!(ctx->dep->dflags & DFLAG_ALTACT_SIG)) {
            if (is_ref(remote)) {
                /* not supported by receiver node; drop it... */
                return ERTS_DSIG_SEND_OK;
            }
            goto non_altact_sig;
        }

        if (is_ref(remote))
            fs |= ERTS_DOP_ALTACT_SIG_FLG_ALIAS;
        if (prio)
            fs |= ERTS_DOP_ALTACT_SIG_FLG_PRIO;
        flags = make_small(fs);

        ctl = TUPLE4(hp, make_small(DOP_ALTACT_SIG_SEND), flags, local, remote);
        msg = reason;
    }

    return dsig_send_exit(ctx, ctl, msg);
}


int
erts_dsig_send_group_leader(ErtsDSigSendContext *ctx, Eterm leader, Eterm remote)
{
    Eterm ctl;

    ctl = TUPLE3(&ctx->ctl_heap[0],
		 make_small(DOP_GROUP_LEADER), leader, remote);

    return dsig_send_ctl(ctx, ctl);
}

static int
dsig_send_spawn_request(ErtsDSigSendContext *ctx, Eterm ref, Eterm from,
                        Eterm gl, Eterm mfa, Eterm alist, Eterm opts)
{
    Process *sender = ctx->c_p;
    if (!have_seqtrace(SEQ_TRACE_TOKEN(sender))) {
        ctx->ctl = TUPLE6(&ctx->ctl_heap[0], make_small(DOP_SPAWN_REQUEST),
                          ref, from, gl, mfa, opts);
    }
    else {
        Eterm tmp_heap[8];
        Eterm node = ctx->dep ? ctx->dep->sysname : ctx->node;
        Eterm msg;
        Eterm token;
        /*
         * Present this as two messages for the sequence tracing.
         * All data except the argument list in the first message
         * and then the argument list as second message (which
         * will become an actual message). For more info see
         * handling of seq-trace token in erl_create_process().
         */
        
	seq_trace_update_serial(sender);
	token = SEQ_TRACE_TOKEN(sender);
        msg = TUPLE6(&tmp_heap[0], am_spawn_request,
                     ref, from, gl, mfa, opts);
	seq_trace_output(token, msg, SEQ_TRACE_SEND, node, sender);
        
	seq_trace_update_serial(sender);
	token = SEQ_TRACE_TOKEN(sender);        
	seq_trace_output(token, alist, SEQ_TRACE_SEND, node, sender);
        
        ctx->ctl = TUPLE7(&ctx->ctl_heap[0], make_small(DOP_SPAWN_REQUEST_TT),
                          ref, from, gl, mfa, opts, token);
    }
    ctx->msg = alist;
    return erts_dsig_send(ctx);
}

int
erts_dsig_send_spawn_reply(ErtsDSigSendContext *ctx,
                           Eterm ref,
                           Eterm to,
                           Eterm flags,
                           Eterm result,
                           Eterm token)
{
    if (!have_seqtrace(token)) {
        ctx->ctl = TUPLE5(&ctx->ctl_heap[0], make_small(DOP_SPAWN_REPLY),
                          ref, to, flags, result);
    }
    else {
        ctx->ctl = TUPLE6(&ctx->ctl_heap[0], make_small(DOP_SPAWN_REPLY_TT),
                          ref, to, flags, result, token);
    }
    ctx->msg = THE_NON_VALUE;
    return erts_dsig_send(ctx);
}

#define ERTS_RBT_PREFIX dist_seq
#define ERTS_RBT_T DistSeqNode
#define ERTS_RBT_KEY_T Uint
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T)            \
    do {                                        \
        (T)->parent = NULL;                     \
        (T)->left = NULL;                       \
        (T)->right = NULL;                      \
        (T)->is_red = 0;                        \
    } while(0)
#define ERTS_RBT_IS_RED(T) ((T)->is_red)
#define ERTS_RBT_SET_RED(T) ((T)->is_red = 1)
#define ERTS_RBT_IS_BLACK(T) (!ERTS_RBT_IS_RED(T))
#define ERTS_RBT_SET_BLACK(T) ((T)->is_red = 0)
#define ERTS_RBT_GET_FLAGS(T) ((T)->is_red)
#define ERTS_RBT_SET_FLAGS(T, F) ((T)->is_red = F)
#define ERTS_RBT_GET_PARENT(T) ((T)->parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->parent = P)
#define ERTS_RBT_GET_RIGHT(T) ((T)->right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->seq_id)
#define ERTS_RBT_IS_LT(KX, KY) (KX < KY)
#define ERTS_RBT_IS_EQ(KX, KY) (KX == KY)
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_LOOKUP_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_WANT_FOREACH_DESTROY_YIELDING

#include "erl_rbtree.h"

struct erts_debug_dist_seq_tree_foreach_iter_arg {
    int (*func)(DistSeqNode *, void *, Sint);
    void *arg;
};

static int
erts_debug_dist_seq_tree_foreach_iter(DistSeqNode *seq, void *arg, Sint reds)
{
    struct erts_debug_dist_seq_tree_foreach_iter_arg *state = arg;
    return state->func(seq, state->arg, reds);
}

void
erts_debug_dist_seq_tree_foreach(DistEntry *dep, int (*func)(DistSeqNode *, void *, Sint), void *arg)
{
    struct erts_debug_dist_seq_tree_foreach_iter_arg state;
    state.func = func;
    state.arg = arg;
    dist_seq_rbt_foreach(dep->sequences, erts_debug_dist_seq_tree_foreach_iter, &state);
}

static int dist_seq_cleanup(DistSeqNode *seq, void *unused, Sint reds)
{
    erts_free_dist_ext_copy(erts_get_dist_ext(&seq->hfrag));
    free_message_buffer(&seq->hfrag);
    return 1;
}

typedef struct {
    DistSeqNode *root;
    dist_seq_rbt_yield_state_t rbt_ystate;
} DistSeqNodeYieldState;

int
erts_dist_seq_tree_foreach_delete_yielding(DistSeqNode **root,
                                           void **vyspp,
                                           Sint limit)
{
    DistSeqNodeYieldState ys = {*root, ERTS_RBT_YIELD_STAT_INITER};
    DistSeqNodeYieldState *ysp;
    int res;

    ysp = (DistSeqNodeYieldState *) *vyspp;
    if (!ysp) {
        *root = NULL;
	ysp = &ys;
    }
    res = dist_seq_rbt_foreach_destroy_yielding(&ysp->root,
                                               dist_seq_cleanup,
                                               NULL,
                                               &ysp->rbt_ystate,
                                               limit);
    if (res > 0) {
	if (ysp != &ys)
	    erts_free(ERTS_ALC_T_SEQ_YIELD_STATE, ysp);
	*vyspp = NULL;
    }
    else {

	if (ysp == &ys) {
	    ysp = erts_alloc(ERTS_ALC_T_SEQ_YIELD_STATE,
			     sizeof(DistSeqNodeYieldState));
	    sys_memcpy((void *) ysp, (void *) &ys,
		       sizeof(DistSeqNodeYieldState));
	}

	*vyspp = (void *) ysp;
    }

    return res;
}

/*
** Input from distribution port.
**  Input follows the distribution protocol v4.5
**  
**   The protocol is a 4 byte header protocol
**   the DOP_DATA is stripped by driver_output
**
**   assert  hlen == 0 !!!
*/

int erts_net_message(Port *prt,
		     DistEntry *dep,
                     Uint32 conn_id,
		     byte *hbuf,
		     ErlDrvSizeT hlen,
                     Binary *bin,
		     const byte *buf,
		     ErlDrvSizeT len)
{
    ErtsDistExternal ede, *edep = &ede;
    ErtsDistExternalData ede_data;
    ErlHeapFragment *ede_hfrag = NULL;
    Sint ctl_len;
    Eterm arg;
    Eterm from, to;
    Eterm watcher, watched;
    Eterm ref;
    Eterm *tuple;
    Eterm reason;
    Process* rp;
    DeclareTmpHeapNoproc(ctl_default,DIST_CTL_DEFAULT_SIZE);
    Eterm* ctl = ctl_default;
    ErtsHeapFactory factory;
    Sint type;
    Eterm token;
    Uint tuple_arity;
    int res;
#ifdef ERTS_DIST_MSG_DBG
    ErlDrvSizeT orig_len = len;
#endif

    UseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);

    ERTS_CHK_NO_PROC_LOCKS;

    ERTS_LC_ASSERT(!prt || erts_lc_is_port_locked(prt));

    if (!erts_is_alive) {
	UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
	return 0;
    }

    ASSERT(hlen == 0);

    if (len == 0) {  /* HANDLE TICK !!! */
	UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
	return 0;
    }

#ifdef ERTS_RAW_DIST_MSG_DBG
    erts_fprintf(dbg_file, "RECV: ");
    bw(buf, len);
#endif

    ede.data = &ede_data;

    res = erts_prepare_dist_ext(&ede, buf, len, bin, dep, conn_id, dep->cache);

    switch (res) {
    case ERTS_PREP_DIST_EXT_CLOSED:
        return 0; /* Connection not alive; ignore signal... */
    case ERTS_PREP_DIST_EXT_FAILED:
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(dbg_file, "DIST MSG DEBUG: erts_prepare_dist_ext() failed:\n");
	bw(buf, orig_len);
#endif
        goto data_error;
    case ERTS_PREP_DIST_EXT_SUCCESS:
	ctl_len = erts_decode_dist_ext_size(&ede, 1, 0);
        if (ctl_len < 0) {
#ifdef ERTS_DIST_MSG_DBG
            erts_fprintf(dbg_file, "DIST MSG DEBUG: erts_decode_dist_ext_size(CTL) failed:\n");
            bw(buf, orig_len);
#endif
            VALGRIND_MSG("data error");
            goto data_error;
        }

        /* A non-fragmented message */
        if (!ede.data->seq_id) {
            if (ctl_len > DIST_CTL_DEFAULT_SIZE) {
                ctl = erts_alloc(ERTS_ALC_T_DCTRL_BUF, ctl_len * sizeof(Eterm));
            }

            erts_factory_tmp_init(&factory, ctl, ctl_len, ERTS_ALC_T_DCTRL_BUF);
            break;
        } else {
            DistSeqNode *seq;
            Uint sz = erts_dist_ext_size(&ede);
            Uint used_sz = ctl_len * sizeof(Eterm);

            /* We calculate the size of the heap fragment to be allocated.
               The used_size part has to be larger that the ctl data and the
               DistSeqNode. */
            if (used_sz + (sizeof(ErlHeapFragment) - sizeof(Eterm)) < sizeof(DistSeqNode))
                used_sz = sizeof(DistSeqNode) - (sizeof(ErlHeapFragment) - sizeof(Eterm));

            seq = (DistSeqNode *)new_message_buffer((sz + used_sz) / sizeof(Eterm));
            seq->hfrag.used_size = used_sz / sizeof(Eterm);

            seq->ctl_len = ctl_len;
            seq->seq_id = ede.data->seq_id;
            seq->cnt = ede.data->frag_id;
            erts_de_rlock(dep);
            if (dep->state != ERTS_DE_STATE_CONNECTED
                || dep->connection_id != ede.connection_id
                || dist_seq_rbt_lookup_insert(&dep->sequences, seq) != NULL) {
                free_message_buffer(&seq->hfrag);
                goto data_error_runlock;
            }
            erts_de_runlock(dep);

            erts_make_dist_ext_copy(&ede, erts_get_dist_ext(&seq->hfrag));

            if (ede.data->frag_id > 1) {
                seq->cnt--;
                return 0;
            }
        }

        /* fall through, the first fragment in the sequence was the last fragment */
        ERTS_FALLTHROUGH();
    case ERTS_PREP_DIST_EXT_FRAG_CONT: {
        DistSeqNode *seq;
        erts_de_rlock(dep);
        if (dep->state != ERTS_DE_STATE_CONNECTED
            || dep->connection_id != ede.connection_id) {
            goto data_error_runlock;
        }

        seq = dist_seq_rbt_lookup(dep->sequences, ede.data->seq_id);

        if (!seq)
            goto data_error_runlock;

        /* If we did a fall-though we already did this */
        if (res == ERTS_PREP_DIST_EXT_FRAG_CONT)
            erts_dist_ext_frag(&ede_data, erts_get_dist_ext(&seq->hfrag));

        /* Verify that the fragments have arrived in the correct order */
        if (seq->cnt != ede.data->frag_id)
            goto data_error_runlock;

        seq->cnt--;

        /* Check if this was the last fragment */
        if (ede.data->frag_id > 1) {
            erts_de_runlock(dep);
            return 0;
        }

        /* Last fragment arrived, time to dispatch the signal */

        dist_seq_rbt_delete(&dep->sequences, seq);
        erts_de_runlock(dep);
        ctl_len = seq->ctl_len;

        /* Now that we no longer need the DistSeqNode we re-use the heapfragment
           to decode the ctl msg into. We don't need the ctl message to be in
           the heapfragment, but we decode into the heapfragment speculatively
           in case there is a trace token that we need. */
        erts_factory_heap_frag_init(&factory, &seq->hfrag);
        edep = erts_get_dist_ext(&seq->hfrag);
        ede_hfrag = &seq->hfrag;

        break;
    }
    default:
        ERTS_INTERNAL_ERROR("Unexpected result from erts_prepare_dist_ext()");
        break;
    }

    arg = erts_decode_dist_ext(&factory, edep, 1);
    if (is_non_value(arg)) {
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(dbg_file, "DIST MSG DEBUG: erts_decode_dist_ext(CTL) failed:\n");
	bw(buf, orig_len);
#endif
	VALGRIND_MSG("data error");
	goto decode_error;
    }

    /* Fill the unused part of the hfrag */
    if (ede_hfrag && ede_hfrag->mem + ede_hfrag->used_size > factory.hp) {
        Uint slot = factory.hp - ede_hfrag->mem;
        erts_write_heap_filler(&ede_hfrag->mem[slot], ede_hfrag->used_size - slot);
    }

    if (is_not_tuple(arg) || 
	(tuple = tuple_val(arg), (tuple_arity = arityval(*tuple)) < 1) ||
	is_not_small(tuple[1])) {
#ifdef ERTS_DIST_MSG_DBG
        if (is_tuple(arg) && arityval(*tuple) > 1)
            erts_fprintf(dbg_file, "RECV: CTL: %s: %.80T\n",
                         erts_dop_to_string(unsigned_val(tuple[1])), arg);
#endif
 	goto invalid_message;
    }

#ifdef ERTS_DIST_MSG_DBG
    erts_fprintf(dbg_file, "RECV: CTL: %s: %.80T\n",
                 erts_dop_to_string(unsigned_val(tuple[1])), arg);
#endif

    token = NIL;

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK: {
        ErtsDSigSendContext ctx;
        int code;

	if (tuple_arity != 3) {
	    goto invalid_message;
	}
	from = tuple[2];
	to   = tuple[3];  /* local proc to link to */

	if (is_not_external_pid(from))
            goto invalid_message;

        if (dep != external_pid_dist_entry(from))
            goto invalid_message;

        if (is_external_pid(to)) {
            if (external_pid_dist_entry(to) != erts_this_dist_entry)
                goto invalid_message;
            /* old incarnation of node; reply noproc... */
        }
        else if (is_internal_pid(to)) {
            ErtsLinkData *ldp = erts_link_external_create(ERTS_LNK_TYPE_DIST_PROC,
                                                          to, from);
            ASSERT(ldp->dist.other.item == to);
            ASSERT(eq(ldp->proc.other.item, from));

            code = erts_link_dist_insert(&ldp->dist, ede.mld);
            if (erts_proc_sig_send_link(NULL, from, to, &ldp->proc)) {
                if (!code) {
                    /* Race: connection already down => send link exit */
                    erts_proc_sig_send_link_exit_noconnection(&ldp->dist);
                }
                break; /* Done */
            }

            /* Failed to send signal; cleanup and reply noproc... */
            if (code) {
                code = erts_link_dist_delete(&ldp->dist);
                ASSERT(code);
            }
            erts_link_release_both(ldp);
        }

        code = erts_dsig_prepare(&ctx, dep, NULL, 0, ERTS_DSP_NO_LOCK, 1, 1, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED && ctx.connection_id == conn_id) {
            code = erts_dsig_send_exit(&ctx, to, from, am_noproc);
            ASSERT(code == ERTS_DSIG_SEND_OK);
        }

	break;
    }

    case DOP_UNLINK:
        /*
         * DOP_UNLINK should never be passed. The new link protocol is
         * mandatory as of OTP 26.
         */
        goto invalid_message;
        
    case DOP_UNLINK_ID: {
        Eterm *element;
        Uint64 id;
	if (tuple_arity != 4)
	    goto invalid_message;

        element = &tuple[2];
        if (!term_to_Uint64(*(element++), &id))
            goto invalid_message;

        if (id == 0)
            goto invalid_message;

	from = *(element++);
	to = *element;
	if (is_not_external_pid(from))
	    goto invalid_message;
        if (dep != external_pid_dist_entry(from))
	    goto invalid_message;

        if (is_external_pid(to)
            && erts_this_dist_entry == external_pid_dist_entry(from))
            break;

        if (is_not_internal_pid(to))
            goto invalid_message;

        erts_proc_sig_send_dist_unlink(dep, conn_id, from, to, id);
	break;
    }
    
    case DOP_UNLINK_ID_ACK: {
        Uint64 id;
	if (tuple_arity != 4)
	    goto invalid_message;

        if (!term_to_Uint64(tuple[2], &id))
            goto invalid_message;

	from = tuple[3];
	to = tuple[4];
	if (is_not_external_pid(from))
	    goto invalid_message;
        if (dep != external_pid_dist_entry(from))
	    goto invalid_message;

        if (is_external_pid(to)
            && erts_this_dist_entry == external_pid_dist_entry(from))
            break;

        if (is_not_internal_pid(to))
            goto invalid_message;

        erts_proc_sig_send_dist_unlink_ack(dep, conn_id, from, to, id);
	break;
    }

    case DOP_MONITOR_P: {
	/* A remote process wants to monitor us, we get:
	   {DOP_MONITOR_P, Remote pid, local pid or name, ref} */
	Eterm pid, name;
        ErtsDSigSendContext ctx;
        int code;

	if (tuple_arity != 4) {
	    goto invalid_message;
	}

	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

        if (is_not_external_pid(watcher))
            goto invalid_message;
        else if (external_pid_dist_entry(watcher) != dep)
            goto invalid_message;

	if (is_not_ref(ref))
	    goto invalid_message;

        if (is_internal_pid(watched)) {
            name = NIL;
            pid = watched;
        }
        else if (is_atom(watched)) {
            name = watched;
            pid = erts_whereis_name_to_id(NULL, watched);
            /* if port or undefined; reply noproc... */
        }
        else if (is_external_pid(watched)
                 && external_pid_dist_entry(watched) == erts_this_dist_entry) {
            name = NIL;
            pid = am_undefined; /* old incarnation; reply noproc... */
        }
        else
            goto invalid_message;

        if (is_internal_pid(pid)) {
            ErtsMonitorData *mdp;
            mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC,
                                      ref, watcher, pid, name,
                                      THE_NON_VALUE);

            if (!erts_monitor_dist_insert(&mdp->origin, ede.mld)) {
                /* Race: connection down => do nothing */
                erts_monitor_release_both(mdp);
                break;
            }

            if (erts_proc_sig_send_monitor(NULL, watcher,
                                           &mdp->u.target, pid)) {
                break; /* done */
            }

            /* Failed to send to local proc; cleanup reply noproc... */

            code = erts_monitor_dist_delete(&mdp->origin);
            ASSERT(code); (void)code;
            erts_monitor_release_both(mdp);

        }

        code = erts_dsig_prepare(&ctx, dep, NULL, 0, ERTS_DSP_NO_LOCK, 1, 1, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED && ctx.connection_id == conn_id) {
            code = erts_dsig_send_m_exit(&ctx, watcher, watched, ref, am_noproc);
            ASSERT(code == ERTS_DSIG_SEND_OK);
        }

        break;
    }

    case DOP_DEMONITOR_P:
	/* A remote node informs us that a local pid in no longer monitored
	   We get {DOP_DEMONITOR_P, Remote pid, Local pid or name, ref}. */

	if (tuple_arity != 4) {
	    goto invalid_message;
	}

	watcher = tuple[2];
	watched = tuple[3];
	ref = tuple[4];

	if (is_not_ref(ref)) {
	    goto invalid_message;
	}

        if (is_not_external_pid(watcher) || external_pid_dist_entry(watcher) != dep)
            goto invalid_message;

        if (is_internal_pid(watched)) {
            erts_proc_sig_send_dist_demonitor(watcher, watched, ref);
        } else if (is_external_pid(watched)
                 && external_pid_dist_entry(watched) == erts_this_dist_entry) {
            /* old incarnation; ignore it */
            ;
        }
        else if (is_atom(watched)) {
            ErtsMonitor *mon;

            erts_mtx_lock(&ede.mld->mtx);
            if (ede.mld->alive) {
                mon = erts_monitor_tree_lookup(ede.mld->orig_name_monitors, ref);
                if (mon)
                    erts_monitor_tree_delete(&ede.mld->orig_name_monitors, mon);
            } else {
                mon = NULL;
            }
            erts_mtx_unlock(&ede.mld->mtx);

            if (mon) {
                erts_proc_sig_send_demonitor(NULL, watcher, 0, mon);
            }
        }
        else
            goto invalid_message;

	break;

    case DOP_REG_SEND_TT:
	if (tuple_arity != 5) {
	    goto invalid_message;
	}

	/* Fall through ... */
    case DOP_REG_SEND:
	/* {DOP_REG_SEND, From, Cookie, ToName} -- Message */
	/* {DOP_REG_SEND_TT, From, Cookie, ToName, TraceToken} -- Message */

	/*
	 * There is intentionally no testing of the cookie (it is always '')
	 * from R9B and onwards.
	 */
	if (type != DOP_REG_SEND_TT && tuple_arity != 4) {
	    goto invalid_message;
	}

#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(edep, "MSG", buf, orig_len);
#endif

	from = tuple[2];
	to = tuple[4];
	if (is_not_pid(from) || is_not_atom(to)){
	    goto invalid_message;
	}
	rp = erts_whereis_process(NULL, 0, to, 0, 0);
	if (rp) {
	    ErtsProcLocks locks = 0;

	    if (type == DOP_REG_SEND) {
		token = NIL;
	    } else {
		token = tuple[5];
	    }

            erts_queue_dist_message(rp, locks, edep, ede_hfrag, token, from);

	    if (locks)
		erts_proc_unlock(rp, locks);
	} else if (ede_hfrag != NULL) {
            erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
            free_message_buffer(ede_hfrag);
        }
	break;

    case DOP_SEND_SENDER_TT: {
    case DOP_SEND_TT:

	if (tuple_arity != 4) {
	    goto invalid_message;
	}

	token = tuple[4];
        goto send_common;

    case DOP_SEND_SENDER:
    case DOP_SEND:

        token = NIL;
	if (tuple_arity != 3)
	    goto invalid_message;

    send_common:

	/*
         * If DOP_SEND_SENDER or DOP_SEND_SENDER_TT element 2 contains
         * the sender pid (i.e. DFLAG_SEND_SENDER is set); otherwise,
         * the atom '' (empty cookie).
	 */
        ASSERT((type == DOP_SEND_SENDER || type == DOP_SEND_SENDER_TT)
               ? is_pid(tuple[2])
               : tuple[2] == am_Empty);

#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(edep, "MSG", buf, orig_len);
#endif
        from = tuple[2];
        to = tuple[3];
        if (is_not_pid(to))
            goto invalid_message;
        rp = erts_proc_lookup(to);

        if (rp) {
            erts_queue_dist_message(rp, 0, edep, ede_hfrag, token, from);
        } else if (ede_hfrag != NULL) {
            erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
            free_message_buffer(ede_hfrag);
        }
	
	break;
    }

    case DOP_ALIAS_SEND:

        token = NIL;
	if (tuple_arity != 3)
	    goto invalid_message;

        if (0) {
        case DOP_ALIAS_SEND_TT:
            if (tuple_arity != 4)
                goto invalid_message;
            token = tuple[4];
        }

#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(edep, "ALIAS MSG", buf, orig_len);
#endif
        from = tuple[2];
        to = tuple[3];
        if (is_not_ref(to)) {
            goto invalid_message;
        }
        erts_proc_sig_send_dist_altact_msg(from, to, edep, ede_hfrag, token, 0);
        break;

    case DOP_ALTACT_SIG_SEND: {
        /*
         * {DOP_ALTACT_SIG_SEND, Flags, SenderPid, To}
         * | {DOP_ALTACT_SIG_SEND, Flags, SenderPid, To, Token}
         *
         * The tuple might be extended with more elements in the
         * future, so we should only verify that it is large enough.
         *
         * The Flags element is an integer which currently have these
         * bit flags defined in the 4 least significant bits:
         * - ERTS_DOP_ALTACT_SIG_FLG_PRIO
         *   This is a priority message
         * - ERTS_DOP_ALTACT_SIG_FLG_TOKEN
         *   Control message also contains a token (control message is
         *   a 5-tuple instead of a 4-tuple).
         * - ERTS_DOP_ALTACT_SIG_FLG_ALIAS
         *   An alias message (To is a reference)
         * - ERTS_DOP_ALTACT_SIG_FLG_NAME
         *   Send to a registered name (To is an atom)
         * - ERTS_DOP_ALTACT_SIG_FLG_EXIT
         *   The signal is an exit signal; otherwise, a message signal
         * If neither ERTS_DOP_ALTACT_SIG_FLG_ALIAS nor
         * ERTS_DOP_ALTACT_SIG_FLG_NAME is set, 'To' is a process
         * identifier.
         *
         * Other flags should be ignored. However, we should be able to
         * handle a flags field of an arbitrary size (for future use), so
         * we must be prepared for a bignum of an arbitrary size...
         */
        int prio;
        byte flags;

#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(edep, "ALTACT MSG", buf, orig_len);
#endif
	if (tuple_arity < 4)
	    goto invalid_message;

        if (is_small(tuple[2])) {
            Sint val = signed_val(tuple[2]);
            if (val < 0)
                goto invalid_message;
            flags = (byte) (val & 0xff);
        }
        else if (is_big(tuple[2])) {
            Eterm *val = big_val(tuple[2]);
            ASSERT(BIG_ARITY(val) >= 1);
            if (BIG_SIGN(val))
                goto invalid_message;
            flags = (byte) (BIG_V(val)[0] & 0xff); /* least significant byte */
        }
        else {
            /* Not an integer... */
            goto invalid_message;
        }

        from = tuple[3];
        if (is_not_pid(from)) {
            goto invalid_message;
        }
        to = tuple[4];
        if (!(flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)) {
            token = NIL;
        }
	else if (tuple_arity < 5) {
            goto invalid_message;
        }
        else {
            token = tuple[5];
        }
        if ((flags & ERTS_DOP_ALTACT_SIG_FLG_ALIAS)) {
            if (is_not_ref(to) || (flags & ERTS_DOP_ALTACT_SIG_FLG_NAME)) {
                goto invalid_message;
            }
        }
        else if (flags & ERTS_DOP_ALTACT_SIG_FLG_NAME) {
            if (!is_atom(to)) {
                goto invalid_message;
            }
            to = erts_whereis_name_to_id(NULL, to);
            if (is_not_internal_pid(to)) {
                /* No such process registered; drop message... */
                if (ede_hfrag) {
                    erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
                    free_message_buffer(ede_hfrag);
                }
                break;
            }
        }
        else {
            if (is_not_pid(to)) {
                goto invalid_message;
            }
        }

        prio = !!(flags & ERTS_DOP_ALTACT_SIG_FLG_PRIO);
        /* Dispatch it to identified process... */
        if (flags & ERTS_DOP_ALTACT_SIG_FLG_EXIT) {
            erts_proc_sig_send_dist_exit(dep, from, to, edep, ede_hfrag,
                                         THE_NON_VALUE, token, prio);
        }
        else {
            erts_proc_sig_send_dist_altact_msg(from, to, edep, ede_hfrag,
                                               token, prio);
        }
        break;
    }

    case DOP_PAYLOAD_MONITOR_P_EXIT:
    case DOP_MONITOR_P_EXIT: {

	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */

        watched = tuple[2];  /* remote proc or name which died */
        watcher = tuple[3];
	ref     = tuple[4];

        if (type == DOP_PAYLOAD_MONITOR_P_EXIT) {
            if (tuple_arity != 4) {
                goto invalid_message;
            }
            reason = THE_NON_VALUE;
        } else {
            if (tuple_arity != 5) {
                goto invalid_message;
            }
            reason = tuple[5];
            edep = NULL;
        }

	if (is_not_ref(ref))
	    goto invalid_message;

        if (is_not_external_pid(watched) && is_not_atom(watched))
            goto invalid_message;

        if (is_not_internal_pid(watcher)) {
            if (!is_external_pid(watcher))
                goto invalid_message;
            if (erts_this_dist_entry == external_pid_dist_entry(watcher))
                goto monitored_process_not_alive;
            goto invalid_message;
        }

        if (!erts_proc_lookup(watcher)) {
        monitored_process_not_alive:
            if (ede_hfrag != NULL) {
                erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
                free_message_buffer(ede_hfrag);
            }
            break; /* Process not alive */
        }

#ifdef ERTS_DIST_MSG_DBG
        if (reason == THE_NON_VALUE) {
            dist_msg_dbg(edep, "MSG", buf, orig_len);
        }
#endif

        erts_proc_sig_send_dist_monitor_down(
            dep, ref, watched, watcher, edep, ede_hfrag, reason);
	break;
    }

    case DOP_PAYLOAD_EXIT:
    case DOP_PAYLOAD_EXIT_TT:
    case DOP_EXIT_TT:
    case DOP_EXIT: {

	/* 'from', which 'to' is linked to, died */
        from = tuple[2];
        to = tuple[3];

	if (type == DOP_EXIT) {
	    if (tuple_arity != 4) {
		goto invalid_message;
	    }
	    token = NIL;
	    reason = tuple[4];
            edep = NULL;
	} else if (type == DOP_EXIT_TT){
	    if (tuple_arity != 5) {
		goto invalid_message;
	    }
	    token = tuple[4];
	    reason = tuple[5];
            edep = NULL;
	} else if (type == DOP_PAYLOAD_EXIT) {
            if (tuple_arity != 3) {
		goto invalid_message;
	    }
            token = NIL;
            reason = THE_NON_VALUE;
        } else {
            if (tuple_arity != 4) {
		goto invalid_message;
	    }
            token = tuple[4];
            reason = THE_NON_VALUE;
        }
	if (is_not_external_pid(from)
            || dep != external_pid_dist_entry(from)
            || is_not_internal_pid(to)) {
	    goto invalid_message;
	}

        if (!erts_proc_lookup(to)) {
            if (ede_hfrag != NULL) {
                erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
                free_message_buffer(ede_hfrag);
            }
            break; /* Process not alive */
        }

#ifdef ERTS_DIST_MSG_DBG
        if (reason == THE_NON_VALUE) {
            dist_msg_dbg(edep, "MSG", buf, orig_len);
        }
#endif

        erts_proc_sig_send_dist_link_exit(dep,
                                          from, to, edep, ede_hfrag,
                                          reason, token);
	break;
    }
    case DOP_PAYLOAD_EXIT2_TT:
    case DOP_PAYLOAD_EXIT2:
    case DOP_EXIT2_TT:
    case DOP_EXIT2: {

	/* 'from' is send an exit signal to 'to' */
        from = tuple[2];
        to = tuple[3];

	if (type == DOP_EXIT2) {
	    if (tuple_arity != 4) {
		goto invalid_message;
	    }
	    reason = tuple[4];
	    token = NIL;
            edep = NULL;
	} else if (type == DOP_EXIT2_TT) {
	    if (tuple_arity != 5) {
		goto invalid_message;
	    }
	    token = tuple[4];
	    reason = tuple[5];
            edep = NULL;
	} else if (type == DOP_PAYLOAD_EXIT2) {
            if (tuple_arity != 3) {
		goto invalid_message;
	    }
            reason = THE_NON_VALUE;
            token = NIL;
        } else {
            if (tuple_arity != 4) {
		goto invalid_message;
	    }
            reason = THE_NON_VALUE;
            token = tuple[4];
        }
	if (is_not_pid(from)
            || dep != external_pid_dist_entry(from)) {
	    goto invalid_message;
	}
        if (is_not_internal_pid(to)) {
            if (is_external_pid(to)) {
		DistEntry *dep = external_pid_dist_entry(to);
		if (dep == erts_this_dist_entry)
                    break; /* Old incarnation of this node... */
            }
            goto invalid_message;
        }

        if (!erts_proc_lookup(to)) {
            if (ede_hfrag != NULL) {
                erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
                free_message_buffer(ede_hfrag);
            }
            break; /* Process not alive */
        }

#ifdef ERTS_DIST_MSG_DBG
        if (reason == THE_NON_VALUE) {
            dist_msg_dbg(edep, "MSG", buf, orig_len);
        }
#endif

        erts_proc_sig_send_dist_exit(dep, from, to, edep, ede_hfrag, reason, token, 0);
	break;
    }
    case DOP_GROUP_LEADER:
	if (tuple_arity != 3) {
	    goto invalid_message;
	}
	from = tuple[2];   /* Group leader  */
	to = tuple[3];     /* new member */
	if (is_not_pid(from) || is_not_pid(to)) {
	    goto invalid_message;
	}

        (void) erts_proc_sig_send_group_leader(NULL, to, from, NIL);
	break;

    case DOP_SPAWN_REQUEST_TT: {
        Eterm tmp_heap[2];
        ErlSpawnOpts so;
        int code, opts_error;
        Eterm pid, error, ref, from, gl, mfa, opts, token, args;
        /* {DOP_SPAWN_REQUEST_TT, Ref, From, GL, MFA, Opts, Token} */

        if (tuple_arity != 7)
            goto invalid_message;

        token = tuple[7];

        if (0) {
            
        case DOP_SPAWN_REQUEST:
            /* {DOP_SPAWN_REQUEST, Ref, From, GL, MFA, Opts} */
            if (tuple_arity != 6)
                goto invalid_message;

            token = NIL;
        }

        ref = tuple[2];
        from = tuple[3];
        gl = tuple[4];
        mfa = tuple[5];
        opts = tuple[6];
        
        if (is_not_external_ref(ref))
            goto invalid_message;
        if (is_not_external_pid(from))
            goto invalid_message;
        if (is_not_pid(gl))
            goto invalid_message;
        if (is_not_tuple_arity(mfa, 3))
            goto invalid_message;
        else {
            Eterm *tp = tuple_val(tuple[5]);
            if (is_not_atom(tp[1]))
                goto invalid_message;
            if (is_not_atom(tp[2]))
                goto invalid_message;
            if (is_not_small(tp[3]))
                goto invalid_message;
        }

        opts_error = erts_parse_spawn_opts(&so, opts, NULL, 0);
        if (opts_error) {
            ErtsDSigSendContext ctx;
            if (opts_error > 1)
                goto invalid_message;
            error = am_badopt;
        dist_spawn_error:
            if (ede_hfrag) {
                erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
                free_message_buffer(ede_hfrag);
            }
            if (have_seqtrace(token)) {
                /*
                 * See erl_create_process() for why we do this
                 * serial trickery...
                 */
                Eterm tmp_heap[7];
                Eterm seq_msg;
                Eterm serial;
                Uint serial_num;
                /* Receiver spawn_request... */
                serial = SEQ_TRACE_T_SERIAL(token);
                serial_num = unsigned_val(serial);
                serial_num--;
                serial = make_small(serial_num);
                SEQ_TRACE_T_SERIAL(token) = serial;
                seq_msg = TUPLE6(&tmp_heap[0], am_spawn_request,
                                 ref, from, gl, mfa, opts);
                seq_trace_output(token, seq_msg, SEQ_TRACE_RECEIVE,
                                 erts_this_dist_entry->sysname, NULL);
                SEQ_TRACE_T_LASTCNT(token) = serial;
                /* Send spawn_reply... */
                erts_seq_trace_update_node_token(token);
                seq_msg = TUPLE4(&tmp_heap[0],
                                 am_spawn_reply, ref, am_error, error);
                seq_trace_output(token, seq_msg, SEQ_TRACE_SEND, from, NULL);
            }
            code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                     ERTS_DSP_NO_LOCK, 1, 1, 0);
            if (code == ERTS_DSIG_PREP_CONNECTED
                && ctx.connection_id == conn_id) {
                code = erts_dsig_send_spawn_reply(&ctx,
                                                  tuple[2],
                                                  tuple[3],
                                                  make_small(0),
                                                  error,
                                                  token);
                ASSERT(code == ERTS_DSIG_SEND_OK);
            }
            break;
        }

        so.mref = ref;
        so.tag = am_spawn_reply;
        so.parent_id = from;
        so.group_leader = gl;
        so.mfa = mfa;
        so.dist_entry = dep;
        so.conn_id = conn_id;
        so.mld = ede.mld;
        so.edep = edep;
        so.ede_hfrag = ede_hfrag;
        so.token = token;
        so.opts = opts;

        args = CONS(&tmp_heap[0], mfa, NIL);
        pid = erl_create_process(NULL,
                                 am_erts_internal,
                                 am_dist_spawn_init,
                                 args,
                                 &so);
        if (is_non_value(pid)) {
            if (so.error_code == SYSTEM_LIMIT)
                error = am_system_limit;
            else
                goto invalid_message; /* should not happen */
            goto dist_spawn_error;
        }
        
        break;
    }

    case DOP_SPAWN_REPLY_TT: {
        ErtsLinkData *ldp;
        ErtsLink *lnk;
        int monitor;
        int link_inserted;
        Eterm ref, result, flags_term, parent, token;
        Uint flags;

        /* {DOP_SPAWN_REPLY_TT, Ref, To, Flags, From, Token} */
        if (tuple_arity != 6)
            goto invalid_message;

        token = tuple[6];

        if (0) {
        case DOP_SPAWN_REPLY:

            /* {DOP_SPAWN_REPLY, Ref, To, Flags, From} */
            if (tuple_arity != 5)
                goto invalid_message;
            
            token = NIL;
        }

        ldp = NULL;
        lnk = NULL;
        link_inserted = 0;
        monitor = 0;

        ref = tuple[2];
        parent = tuple[3];
        flags_term = tuple[4];
        result = tuple[5];
        
        if (is_not_internal_pid(parent)) {
            if (is_external_pid(parent)) {
		DistEntry *dep = external_pid_dist_entry(parent);
		if (dep == erts_this_dist_entry)
                    break; /* Old incarnation of this node... */
            }
            goto invalid_message;
        }

        if (is_not_internal_ref(ref)) {
            if (is_external_ref(ref)) {
		DistEntry *dep = external_ref_dist_entry(ref);
		if (dep == erts_this_dist_entry)
                    break; /* Old incarnation of this node... */
            }
            goto invalid_message;
        }

        if (is_not_small(flags_term))
            goto invalid_message;

        flags = unsigned_val(flags_term);
        if (flags >= (1 << 27))
            goto invalid_message;

        if (is_not_external_pid(result) && is_not_atom(result))
            goto invalid_message;
        
        if (is_external_pid(result)) {

            monitor = !!(flags & ERTS_DIST_SPAWN_FLAG_MONITOR);

            if (flags & ERTS_DIST_SPAWN_FLAG_LINK) {
                /* Successful spawn-link... */
                ldp = erts_link_external_create(ERTS_LNK_TYPE_DIST_PROC,
                                                parent, result);
                ASSERT(ldp->dist.other.item == parent);
                ASSERT(eq(ldp->proc.other.item, result));
                link_inserted = erts_link_dist_insert(&ldp->dist, ede.mld);
                lnk = &ldp->proc;
            }
        }

        if (!erts_proc_sig_send_dist_spawn_reply(dep->sysname, ref,
                                                 parent, lnk, result,
                                                 token)) {
            ErtsDSigSendContext ctx;
            int code;
            
            if (monitor) {
                code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                         ERTS_DSP_NO_LOCK, 1, 1, 0);
                if (code == ERTS_DSIG_PREP_CONNECTED
                    && ctx.connection_id == conn_id) {
                    code = erts_dsig_send_demonitor(&ctx, parent,
                                                    result, ref);
                    ASSERT(code == ERTS_DSIG_SEND_OK);
                }
            }

            if (lnk) {
                if (link_inserted) {
                    code = erts_link_dist_delete(&ldp->dist);
                    ASSERT(code);
                }
                erts_link_release_both(ldp);
            }

            if (flags & ERTS_DIST_SPAWN_FLAG_LINK) {
                /*
                 * Save info so the terminating parent can send us
                 * an exit signal with the correct exit reason...
                 */
                dist_pend_spawn_exit_save_child_result(result,
                                                       ref,
                                                       ede.mld);
            }
        } else if (lnk && !link_inserted) {
            erts_proc_sig_send_link_exit_noconnection(&ldp->dist);
        }

        break;
    }

    default:
	goto invalid_message;
    }

    if (ede_hfrag == NULL) {
        erts_factory_close(&factory);
        if (ctl != ctl_default) {
            erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
        }
    }
    UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
    ERTS_CHK_NO_PROC_LOCKS;
    return 0;
 invalid_message:
    {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Invalid distribution message: %.200T", arg);
	erts_send_error_to_logger_nogl(dsbufp);
    }
decode_error:
    VALGRIND_MSG("data error");
    if (ede_hfrag == NULL) {
        erts_factory_close(&factory);
        if (ctl != ctl_default) {
            erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
        }
    } else {
        erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
        free_message_buffer(ede_hfrag);
    }
data_error:
    UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
    erts_kill_dist_connection(dep, conn_id);
    ERTS_CHK_NO_PROC_LOCKS;
    return -1;
data_error_runlock:
    erts_de_runlock(dep);
    goto data_error;
}

static int dsig_send_exit(ErtsDSigSendContext *ctx, Eterm ctl, Eterm msg)
{
    ctx->ctl = ctl;
    ctx->msg = msg;
    return erts_dsig_send(ctx);
}

static int dsig_send_ctl(ErtsDSigSendContext *ctx, Eterm ctl)
{
    int ret;
    ctx->ctl = ctl;
    ctx->msg = THE_NON_VALUE;
    ctx->from = THE_NON_VALUE;
    ctx->reds = 1; /* provoke assert below (no reduction count without msg) */
    ret = erts_dsig_send(ctx);
    ASSERT(ret != ERTS_DSIG_SEND_CONTINUE);
    return ret;
}

static ERTS_INLINE void
notify_dist_data(Process *c_p, Eterm pid)
{
    Process *rp;
    ErtsProcLocks rp_locks;

    ASSERT(erts_get_scheduler_data()
           && !ERTS_SCHEDULER_IS_DIRTY(erts_get_scheduler_data()));
    ASSERT(is_internal_pid(pid));

    if (c_p && c_p->common.id == pid) {
        rp = c_p;
        rp_locks = ERTS_PROC_LOCK_MAIN;
    }
    else {
        rp = erts_proc_lookup(pid);
        rp_locks = 0;
    }

    if (rp) {
        ErtsMessage *mp = erts_alloc_message(0, NULL);
        erts_queue_message(rp, rp_locks, mp, am_dist_data, am_system);
    }
}

int
erts_dsig_prepare(ErtsDSigSendContext *ctx,
		  DistEntry *dep,
		  Process *proc,
                  ErtsProcLocks proc_locks,
		  ErtsDSigPrepLock dspl,
		  int no_suspend,
                  int no_trap,
		  int connect)
{
    /*
     * No process imply that we should force data through. That
     * is, ignore busy state of dist entry and allow enqueue
     * regardless of its state...
     */
    int res;

    ASSERT(no_trap || proc);

    if (!erts_is_alive)
	return ERTS_DSIG_PREP_NOT_ALIVE;
    if (!dep) {
        ASSERT(!connect);
        return ERTS_DSIG_PREP_NOT_CONNECTED;
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (connect) {
        erts_proc_lc_might_unlock(proc, proc_locks);
    }
#endif

retry:
    erts_de_rlock(dep);

    if (dep->state == ERTS_DE_STATE_CONNECTED) {
	res = ERTS_DSIG_PREP_CONNECTED;
    }
    else if (dep->state == ERTS_DE_STATE_PENDING) {
	res = ERTS_DSIG_PREP_PENDING;
    }
    else if (dep->state == ERTS_DE_STATE_EXITING) {
	res = ERTS_DSIG_PREP_NOT_CONNECTED;
	goto fail;
    }
    else if (connect) {
        ASSERT(dep->state == ERTS_DE_STATE_IDLE);
        erts_de_runlock(dep);
        if (!erts_auto_connect(dep, proc, proc_locks)) {
            return ERTS_DSIG_PREP_NOT_ALIVE;
        }
	goto retry;
    }
    else {
        ASSERT(dep->state == ERTS_DE_STATE_IDLE);
	res = ERTS_DSIG_PREP_NOT_CONNECTED;
	goto fail;
    }

    if (!proc || (proc->flags & F_ASYNC_DIST)) {
        ctx->ignore_busy = !0;
    }
    else {
        ctx->ignore_busy = 0;
        if (no_suspend) {
            if (de_qflags_read(dep) & ERTS_DE_QFLG_BUSY) {
                res = ERTS_DSIG_PREP_WOULD_SUSPEND;
                goto fail;
            }
        }
    }

    ctx->c_p = proc;
    ctx->dep = dep;
    ctx->deref_dep = 0;
    ctx->cid = dep->cid;
    ctx->connection_id = dep->connection_id;
    ctx->no_suspend = no_suspend;
    ctx->no_trap = no_trap;
    ctx->dflags = dep->dflags;
    ctx->return_term = am_true;
    ctx->phase = ERTS_DSIG_SEND_PHASE_INIT;
    ctx->from = proc ? proc->common.id : am_undefined;
    ctx->reds = no_trap ? 1 : (Sint) (ERTS_BIF_REDS_LEFT(proc) * TERM_TO_BINARY_LOOP_FACTOR);
    if (dspl == ERTS_DSP_NO_LOCK)
	erts_de_runlock(dep);
    return res;

 fail:
    erts_de_runlock(dep);
    return res;
}

static
void erts_schedule_dist_command(Port *prt, DistEntry *dist_entry)
{
    DistEntry *dep;
    Eterm id;

    if (prt) {
	ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));
	ASSERT((erts_atomic32_read_nob(&prt->state)
		& ERTS_PORT_SFLGS_DEAD) == 0);

        dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
        ERTS_ASSUME(dep);
	id = prt->common.id;
    }
    else {
	ERTS_ASSUME(dist_entry);
	ERTS_LC_ASSERT(erts_lc_rwmtx_is_rlocked(&dist_entry->rwmtx)
			   || erts_lc_rwmtx_is_rwlocked(&dist_entry->rwmtx));
	ASSERT(is_internal_port(dist_entry->cid));

 	dep = dist_entry;
	id = dep->cid;
    }

    if (!erts_atomic_xchg_mb(&dep->dist_cmd_scheduled, 1))
	erts_port_task_schedule(id, &dep->dist_cmd, ERTS_PORT_TASK_DIST_CMD);
}


int
erts_dsig_send(ErtsDSigSendContext *ctx)
{
    int retval;
    Sint initial_reds = ctx->reds;
    Eterm cid;

    while (1) {
	switch (ctx->phase) {
	case ERTS_DSIG_SEND_PHASE_INIT:

	    if (!ctx->c_p) {
		ctx->no_trap = 1;
		ctx->no_suspend = 1;
            }

	    ERTS_LC_ASSERT(!ctx->c_p
			       || (ERTS_PROC_LOCK_MAIN
				   == erts_proc_lc_my_proc_locks(ctx->c_p)));

	    if (!erts_is_alive)
		return ERTS_DSIG_SEND_OK;

	    if (ctx->dflags & DFLAG_DIST_HDR_ATOM_CACHE) {
		ctx->acmp = erts_get_atom_cache_map(ctx->c_p);
	    }
	    else {
		ctx->acmp = NULL;
	    }

    #ifdef ERTS_DIST_MSG_DBG
            erts_fprintf(dbg_file, "SEND: CTL: %s: %.80T\n",
                         erts_dop_to_string(unsigned_val(tuple_val(ctx->ctl)[1])),
                         ctx->ctl);
            if (is_value(ctx->msg))
                erts_fprintf(dbg_file, "    MSG: %.160T\n", ctx->msg);
    #endif

	    ctx->data_size = 0;
	    erts_reset_atom_cache_map(ctx->acmp);

            ERTS_INIT_TTBSizeContext(&ctx->u.sc, ctx->dflags);

            while (1) {
                ErtsExtSzRes sz_res;
                Sint reds = CONTEXT_REDS;
                sz_res = erts_encode_dist_ext_size(ctx->ctl,
                                                   ctx->acmp,
                                                   &ctx->u.sc,
                                                   &ctx->data_size,
                                                   &reds,
                                                   &ctx->vlen,
                                                   &ctx->fragments);
                ctx->reds -= CONTEXT_REDS - reds;
                if (sz_res == ERTS_EXT_SZ_OK)
                    break;
                if (sz_res == ERTS_EXT_SZ_SYSTEM_LIMIT) {
                    retval = ERTS_DSIG_SEND_TOO_LRG;
                    goto done;
                }
            }

	    if (is_non_value(ctx->msg)) {
                ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
                break;
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_SIZE;
            ERTS_FALLTHROUGH();
	case ERTS_DSIG_SEND_PHASE_MSG_SIZE: {
            Sint reds, *redsp;
            if (!ctx->no_trap)
                redsp = &ctx->reds;
            else {
                reds = CONTEXT_REDS;
                redsp = &reds;
            }
            while (1) {
                ErtsExtSzRes sz_res;
                sz_res = erts_encode_dist_ext_size(ctx->msg,
                                                   ctx->acmp,
                                                   &ctx->u.sc,
                                                   &ctx->data_size,
                                                   redsp,
                                                   &ctx->vlen,
                                                   &ctx->fragments);
                if (ctx->no_trap) {
                    ASSERT(redsp == &reds); /* silence CodeChecker */
                    ctx->reds -= CONTEXT_REDS - reds;
                    if (sz_res == ERTS_EXT_SZ_YIELD) {
                        reds = CONTEXT_REDS;
                        continue;
                    }
                }
                if (sz_res == ERTS_EXT_SZ_OK)
                    break;
                if (sz_res == ERTS_EXT_SZ_SYSTEM_LIMIT) {
                    retval = ERTS_DSIG_SEND_TOO_LRG;
                    goto done;
                }
                ASSERT(sz_res == ERTS_EXT_SZ_YIELD);
                retval = ERTS_DSIG_SEND_CONTINUE;
                goto done;
            }

	    ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
            ERTS_FALLTHROUGH();
        }
	case ERTS_DSIG_SEND_PHASE_ALLOC: {

	    erts_finalize_atom_cache_map(ctx->acmp, ctx->dflags);

            ERTS_INIT_TTBEncodeContext(&ctx->u.ec, ctx->dflags);
	    ctx->dhdr_ext_size = erts_encode_ext_dist_header_size(&ctx->u.ec,
                                                                  ctx->acmp,
                                                                  ctx->fragments);
            ctx->obuf = alloc_dist_obufs(&ctx->extp,
                                         &ctx->u.ec,
                                         ctx->dhdr_ext_size + ctx->data_size
                                         + ((ctx->fragments - 1)
                                            * ERTS_DIST_FRAGMENT_HEADER_SIZE),
                                         ctx->fragments,
                                         ctx->vlen,
                                         ctx->ignore_busy);
            ctx->alloced_fragments = ctx->fragments;
	    /* Encode internal version of dist header */
            ctx->dhdrp = ctx->extp;
            ctx->extp += ctx->dhdr_ext_size;
	    ctx->dhdrp = erts_encode_ext_dist_header_setup(&ctx->u.ec,
                                                           ctx->extp,
                                                           ctx->acmp,
                                                           ctx->fragments,
                                                           ctx->from);
            ctx->dhdr_ext_size = ctx->extp - ctx->dhdrp;
            while (1) {
                Sint reds = CONTEXT_REDS;
                /* Encode control message */
                int res = erts_encode_dist_ext(ctx->ctl, &ctx->extp,
                                               ctx->dflags, ctx->acmp,
                                               &ctx->u.ec, &ctx->fragments,
                                               &reds);
                ctx->reds -= CONTEXT_REDS - reds;
                if (res == 0)
                    break;
            }

	    if (is_non_value(ctx->msg)) {
                ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
                break;
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_ENCODE;
            ERTS_FALLTHROUGH();
        }
        case ERTS_DSIG_SEND_PHASE_MSG_ENCODE: {
            Sint reds, *redsp;
            if (!ctx->no_trap)
                redsp = &ctx->reds;
            else {
                reds = CONTEXT_REDS;
                redsp = &reds;
            }
            while (1) {
                int res = erts_encode_dist_ext(ctx->msg, &ctx->extp,
                                               ctx->dflags, ctx->acmp,
                                               &ctx->u.ec,
                                               &ctx->fragments,
                                               redsp);
                if (!ctx->no_trap) {
                    if (res == 0)
                        break;
                    retval = ERTS_DSIG_SEND_CONTINUE;
                    goto done;
                }
                else {
                    ASSERT(redsp == &reds); /* silence CodeChecker */
                    ctx->reds -= CONTEXT_REDS - reds;
                    if (res == 0)
                        break;
                    reds = CONTEXT_REDS;
                }
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
            ERTS_FALLTHROUGH();
        }
	case ERTS_DSIG_SEND_PHASE_FIN: {
            Uint fid = ctx->fragments;
            ErtsDistOutputBuf *obuf = ctx->obuf;
            ErlIOVec *eiov;
            Sint fix;
    
            ASSERT(fid >= 1);
            ASSERT(ctx->alloced_fragments >= ctx->fragments);

            eiov = obuf[0].eiov;
            ASSERT(eiov);
            ASSERT(eiov->vsize >= 3);
            ASSERT(!eiov->iov[0].iov_base);
            ASSERT(!eiov->iov[0].iov_len);
            ASSERT(!eiov->binv[0]);
            ASSERT(!eiov->iov[1].iov_base);
            ASSERT(!eiov->iov[1].iov_len);
            ASSERT(!eiov->binv[1]);

            if (ctx->alloced_fragments > 1) {
                ASSERT(get_int64(ctx->dhdrp + 1 + 1 + 8) == ctx->alloced_fragments);
                /* Update the frag_id in the DIST_FRAG_HEADER */
                put_int64(ctx->fragments, ctx->dhdrp + 1 + 1 + 8);
            }
            
            eiov->size += ctx->dhdr_ext_size;
            eiov->iov[1].iov_base = ctx->dhdrp;
            eiov->iov[1].iov_len = ctx->dhdr_ext_size;
            erts_refc_inc(&obuf[0].bin->intern.refc, 2);
            eiov->binv[1] = Binary2ErlDrvBinary(obuf[0].bin);
            obuf[0].next = &obuf[1];

            for (fix = 1; fix < ctx->fragments; fix++) {
                byte *hdr = erts_encode_ext_dist_header_fragment(&ctx->extp,
                                                                 --fid, ctx->from);
                Uint sz = ctx->extp - hdr;

                eiov = obuf[fix].eiov;
                ASSERT(eiov);
                ASSERT(eiov->vsize >= 3);
                ASSERT(!eiov->iov[0].iov_base);
                ASSERT(!eiov->iov[0].iov_len);
                ASSERT(!eiov->binv[0]);
                ASSERT(!eiov->iov[1].iov_base);
                ASSERT(!eiov->iov[1].iov_len);
                ASSERT(!eiov->binv[1]);

                eiov->size += sz;
                eiov->iov[1].iov_base = hdr;
                eiov->iov[1].iov_len = sz;
                erts_refc_inc(&obuf[fix].bin->intern.refc, 2);
                eiov->binv[1] = Binary2ErlDrvBinary(obuf[fix].bin);
                obuf[fix].next = &obuf[fix+1];
            }
            obuf[fix-1].next = NULL;
            ASSERT(fid == 1);
            /* If the initial fragment calculation was incorrect we free the
               remaining output buffers. */
            for (; fix < ctx->alloced_fragments; fix++) {
                free_dist_obuf(&ctx->obuf[fix], 0);
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_SEND;

            if (ctx->reds <= 0 && !ctx->no_trap) {
                retval = ERTS_DSIG_SEND_CONTINUE;
                goto done;
            }
            ERTS_FALLTHROUGH();
        }
        case ERTS_DSIG_SEND_PHASE_SEND: {
            /*
	     * Signal encoded; now verify that the connection still exists,
	     * and if so enqueue the signal and schedule it for send.
	     */
            DistEntry *dep = ctx->dep;
	    int suspended = 0;
	    int resume = 0;
            int i;
	    erts_de_rlock(dep);
	    cid = dep->cid;
	    if (dep->state == ERTS_DE_STATE_EXITING
                || dep->state == ERTS_DE_STATE_IDLE
                || dep->connection_id != ctx->connection_id) {
		/* Not the same connection as when we started; drop message... */
		erts_de_runlock(dep);
                for (i = 0; i < ctx->fragments; i++)
                    free_dist_obuf(&ctx->obuf[i], !0);
                ctx->fragments = 0;
	    }
	    else {
                Sint qsize = (Sint) erts_atomic_read_nob(&dep->qsize);
                erts_aint32_t qflgs;
		ErtsProcList *plp = NULL;
                Eterm notify_proc = NIL;
                Sint obsz;
                int fragments, empty_fill;

                /* Calculate how many fragments to send. This depends on
                   the available space in the distr queue and the amount
                   of remaining reductions. */
                for (fragments = 0, obsz = 0;
                     (fragments < ctx->fragments
                      && ((ctx->reds > 0
                           && (ctx->ignore_busy
                               || (qsize + obsz < erts_dist_buf_busy_limit)))
                          || ctx->no_trap
                          || ctx->no_suspend));
                     fragments++) {
#ifdef DEBUG
                    int reds = 100;
#else
                    int reds = 10;
#endif
                    if (!ctx->no_trap && !ctx->no_suspend)
                        ctx->reds -= reds;
                    obsz += size_obuf(&ctx->obuf[fragments]);
                }

                ASSERT(fragments == ctx->fragments ||
                       (!ctx->no_trap && !ctx->no_suspend));

		erts_mtx_lock(&dep->qlock);
                update_qsizes(dep, &empty_fill, &qsize, obsz,
                              ctx->ignore_busy ? obsz : 0);
                qflgs = de_qflags_read(dep);
		if (!(qflgs & ERTS_DE_QFLG_BUSY)
                    && qsize >= erts_dist_buf_busy_limit) {
		    qflgs = de_qflags_read_set(dep, ERTS_DE_QFLG_BUSY);
                    qflgs |= ERTS_DE_QFLG_BUSY;
                }
                if (empty_fill && is_internal_pid(dep->cid)) {
                    erts_aint32_t notify;
                    notify = erts_atomic32_xchg_mb(&dep->notify,
                                                   (erts_aint32_t) 0);
                    if (notify) {
                        /*
                         * Previously empty queue and notification
                         * requested...
                         */
                        notify_proc = dep->cid;
                        ASSERT(is_internal_pid(notify_proc));
                    }
                }

                ASSERT(fragments < 2
                       || (get_int64(&((char*)ctx->obuf->eiov->iov[1].iov_base)[10])
                           == ctx->fragments));

                if (fragments) {
                    ctx->obuf[fragments-1].next = NULL;
                    if (dep->out_queue.last)
                        dep->out_queue.last->next = ctx->obuf;
                    else
                        dep->out_queue.first = ctx->obuf;
                    dep->out_queue.last = &ctx->obuf[fragments-1];

                    ctx->fragments -= fragments;
                    ctx->obuf = &ctx->obuf[fragments];
                }

		if ((qflgs & ERTS_DE_QFLG_BUSY)
                    && !ctx->ignore_busy
                    && !ctx->no_suspend) {

                    erts_mtx_unlock(&dep->qlock);

                    plp = erts_proclist_create(ctx->c_p);

                    erts_suspend(ctx->c_p, ERTS_PROC_LOCK_MAIN, NULL);
                    suspended = 1;

                    erts_mtx_lock(&dep->qlock);

                    qflgs = de_qflags_read(dep);
                    if (qflgs & ERTS_DE_QFLG_BUSY) {
                        /* Enqueue suspended process on dist entry */
                        ASSERT(plp);
                        erts_proclist_store_last(&dep->suspended, plp);
                    }
                    else {
                        resume = 1; /* was busy, but isn't now */
    #ifdef USE_VM_PROBES
                        if (resume && DTRACE_ENABLED(dist_port_not_busy)) {
                            DTRACE_CHARBUF(port_str, 64);
                            DTRACE_CHARBUF(remote_str, 64);

                            erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                                          "%T", cid);
                            erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                                          "%T", dep->sysname);
                            DTRACE3(dist_port_not_busy, erts_this_node_sysname,
                                    port_str, remote_str);
                        }
    #endif
                    }
		}

		erts_mtx_unlock(&dep->qlock);
		if (dep->state != ERTS_DE_STATE_PENDING) {
                    if (is_internal_port(dep->cid))
                        erts_schedule_dist_command(NULL, dep);
                }
                else {
                    notify_proc = NIL;
                }
		erts_de_runlock(dep);
                if (is_internal_pid(notify_proc))
                    notify_dist_data(ctx->c_p, notify_proc);

		if (resume) {
		    erts_resume(ctx->c_p, ERTS_PROC_LOCK_MAIN);
		    erts_proclist_destroy(plp);
		    /*
		     * Note that the calling process still have to yield as if it
		     * suspended. If not, the calling process could later be
		     * erroneously scheduled when it shouldn't be.
		     */
		}
                /* More fragments left to be sent, yield and re-schedule */
                if (ctx->fragments) {
                    ctx->c_p->flags |= F_FRAGMENTED_SEND;
                    retval = ERTS_DSIG_SEND_CONTINUE;
                    if (!resume && erts_system_monitor_busy_dist_port_cnt)
                        monitor_busy_dist_port(ctx->c_p, cid);
                    goto done;
                }
	    }

            if (ctx->c_p) ctx->c_p->flags &= ~F_FRAGMENTED_SEND;
	    ctx->obuf = NULL;

	    if (suspended) {
    #ifdef USE_VM_PROBES
		if (!resume && DTRACE_ENABLED(dist_port_busy)) {
		    DTRACE_CHARBUF(port_str, 64);
		    DTRACE_CHARBUF(remote_str, 64);
		    DTRACE_CHARBUF(pid_str, 16);

		    erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)), "%T", cid);
		    erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
				  "%T", dep->sysname);
		    erts_snprintf(pid_str, sizeof(DTRACE_CHARBUF_NAME(pid_str)),
				  "%T", ctx->c_p->common.id);
		    DTRACE4(dist_port_busy, erts_this_node_sysname,
			    port_str, remote_str, pid_str);
		}
    #endif
                if (!resume && erts_system_monitor_busy_dist_port_cnt) {
                    monitor_busy_dist_port(ctx->c_p, cid);
                }
		retval = ERTS_DSIG_SEND_YIELD;
	    } else {
		retval = ERTS_DSIG_SEND_OK;
	    }
	    goto done;
	}
	default:
	    erts_exit(ERTS_ABORT_EXIT, "dsig_send invalid phase (%d)\n", (int)ctx->phase);
	}
    }

done:
    if (ctx->msg && ctx->c_p) {
	BUMP_REDS(ctx->c_p, (initial_reds - ctx->reds) / TERM_TO_BINARY_LOOP_FACTOR);
    }
    return retval;
}

static Uint
dist_port_command(Port *prt, ErtsDistOutputBuf *obuf)
{
    ErlDrvSizeT size;
    char *bufp;

    ERTS_CHK_NO_PROC_LOCKS;
    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (!obuf) {
        size = 0;
        bufp = NULL;
    }
    else {
        char *ptr;
        Sint i, vlen = obuf->eiov->vsize;
        SysIOVec *iov = obuf->eiov->iov;
        size = obuf->eiov->size;
        bufp = ptr = erts_alloc(ERTS_ALC_T_TMP, size);
        for (i = 0; i < vlen; i++) {
            Uint len = iov[i].iov_len;
            if (len) {
                sys_memcpy((void *) ptr, (void *) iov[i].iov_base, len);
                ptr += len;
            }
        }
        ASSERT(size == ptr - bufp);
    }

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_output)) {
        DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", dep->sysname);
        DTRACE4(dist_output, erts_this_node_sysname, port_str,
                remote_str, size);
    }
#endif

    prt->caller = NIL;
    (*prt->drv_ptr->output)((ErlDrvData) prt->drv_data, bufp, size);
    erts_free(ERTS_ALC_T_TMP, bufp);
    return size;
}

static Uint
dist_port_commandv(Port *prt, ErtsDistOutputBuf *obuf)
{
    SysIOVec iov[1];
    Uint size;
    ErlDrvBinary* bv[1];
    ErlIOVec eiov, *eiovp;

    ERTS_CHK_NO_PROC_LOCKS;
    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (obuf)
        eiovp = obuf->eiov;
    else {
        iov[0].iov_base = NULL;
        iov[0].iov_len = 0;
        bv[0] = NULL;
        eiov.size = 0;
        eiov.vsize = 1;
        eiov.iov = iov;
        eiov.binv = bv;
        eiovp = &eiov;
    }

#ifdef DEBUG
    {
        Uint sz;
        Sint i;
        for (i = 0, sz = 0; i < eiovp->vsize; i++)
            sz += eiovp->iov[i].iov_len;
        ASSERT(sz == eiovp->size);
    }
#endif

#ifdef ERTS_RAW_DIST_MSG_DBG
    {
        Sint i;
        erts_fprintf(dbg_file, "SEND: ");
        for (i = 0; i < eiovp->vsize; i++) {
            if (eiovp->iov[i].iov_len)
                bw(eiovp->iov[i].iov_base, eiovp->iov[i].iov_len);
        }
    }
#endif

    if (eiovp->size > (Uint) INT_MAX)
	erts_exit(ERTS_DUMP_EXIT,
		 "Absurdly large distribution output data buffer "
		 "(%beu bytes) passed.\n",
                  eiovp->size);

    ASSERT(prt->drv_ptr->outputv);

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_outputv)) {
        DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);

        ERTS_ASSUME(dep);

        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", dep->sysname);
        DTRACE4(dist_outputv, erts_this_node_sysname, port_str,
                remote_str, size);
    }
#endif
    prt->caller = NIL;
    (*prt->drv_ptr->outputv)((ErlDrvData) prt->drv_data, eiovp);

    size = (Uint) eiovp->size;
    /* Remove header used by driver... */
    eiovp->size -= eiovp->iov[0].iov_len;
    eiovp->iov[0].iov_base = NULL;
    eiovp->iov[0].iov_len = 0;
    return size;
}


#if defined(ARCH_64)
#define ERTS_PORT_REDS_MASK__ 0x003fffffffffffffL
#elif defined(ARCH_32)
#define ERTS_PORT_REDS_MASK__ 0x003fffff
#else
#  error "Ohh come on ... !?!"
#endif

#define ERTS_PORT_REDS_DIST_CMD_START 5
#define ERTS_PORT_REDS_DIST_CMD_EXIT 200
#define ERTS_PORT_REDS_DIST_CMD_RESUMED 5
#define ERTS_PORT_REDS_DIST_CMD_DATA(SZ) \
  ((SZ) < (1 << 10) \
   ? ((Sint) 1) \
   : ((((Sint) (SZ)) >> 10) & ((Sint) ERTS_PORT_REDS_MASK__)))

#ifndef DEBUG
#define ERTS_DBG_CHK_DIST_QSIZE(DEP, PRT)
#else
#define ERTS_DBG_CHK_DIST_QSIZE(DEP, PRT)           \
    dbg_check_dist_qsize((DEP), (PRT))

static void
dbg_check_dist_qsize(DistEntry *dep, Port *prt)
{
    int ix;
    Sint sz = 0, isz = 0, tqsz, qsz;
    ErtsDistOutputBuf *qs[2];

    ERTS_LC_ASSERT(dep && erts_lc_mtx_is_locked(&dep->qlock));
    ASSERT(prt && erts_lc_is_port_locked(prt));
    ERTS_LC_ASSERT((erts_atomic32_read_nob(&prt->sched.flags)
                    & ERTS_PTS_FLG_EXIT)
                   || prt->common.id == dep->cid);

    tqsz = erts_atomic_read_nob(&dep->total_qsize);
    qsz = erts_atomic_read_nob(&dep->qsize);

    ASSERT(tqsz >= 0);
    ASSERT(qsz >= 0);
    ASSERT(tqsz >= qsz);

    qs[0] = dep->out_queue.first;
    qs[1] = dep->finalized_out_queue.first;

    for (ix = 0; ix < sizeof(qs)/sizeof(qs[0]); ix++) {
        ErtsDistOutputBuf *obuf = qs[ix];
        while (obuf) {
            add_obuf_sizes(obuf, &sz, &isz);
            obuf = obuf->next;
        }
    }

    ASSERT(tqsz == sz);
    ASSERT(qsz == sz - isz);
}

#endif

int
erts_dist_command(Port *prt, int initial_reds)
{
    Sint reds = initial_reds - ERTS_PORT_REDS_DIST_CMD_START;
    enum dist_entry_state state;
    Uint64 flags;
    /*
     * 'obufsize' and 'ignore_obufsize' contains the number of bytes removed
     * from the queue which will be updated (in dep->total_qsize and
     * dep->qsize) before we return from this function. Note that
     * 'obufsize' and 'ignore_obufsize' may be negative if we added to the
     * queue size. This may occur since finalization of a buffer may increase
     * buffer size.
     */
    Sint qsize, obufsize = 0, ignore_obufsize = 0;
    ErtsDistOutputQueue oq, foq;
    DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
    Uint (*send)(Port *prt, ErtsDistOutputBuf *obuf);
    erts_aint32_t sched_flags;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    ERTS_ASSUME(dep);
    erts_atomic_set_mb(&dep->dist_cmd_scheduled, 0);

    erts_de_rlock(dep);
    flags = dep->dflags;
    state = dep->state;
    send = dep->send;
    erts_de_runlock(dep);

    if (state == ERTS_DE_STATE_EXITING) {
	erts_deliver_port_exit(prt, prt->common.id, am_killed, 0, 1);
        reds -= ERTS_PORT_REDS_DIST_CMD_EXIT;
	return initial_reds - reds;
    }

    ASSERT(state != ERTS_DE_STATE_PENDING);

    ASSERT(send);

    /*
     * We need to remove both out queues from the
     * dist entry while passing it to port command;
     * otherwise, port command will free the buffers
     * in the queues on failure and we'll end up with
     * a mess.
     */

    erts_mtx_lock(&dep->qlock);
    ERTS_DBG_CHK_DIST_QSIZE(dep, prt);
    oq.first = dep->out_queue.first;
    oq.last = dep->out_queue.last;
    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    erts_mtx_unlock(&dep->qlock);

    foq.first = dep->finalized_out_queue.first;
    foq.last = dep->finalized_out_queue.last;
    dep->finalized_out_queue.first = NULL;
    dep->finalized_out_queue.last = NULL;

    sched_flags = erts_atomic32_read_nob(&prt->sched.flags);

    if (reds < 0)
	goto preempted;

    if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT) && foq.first) {
	int preempt = 0;
	do {
            Uint size;
            ErtsDistOutputBuf *fob;
            add_obuf_sizes(foq.first, &obufsize, &ignore_obufsize);
            size = (*send)(prt, foq.first);
            erts_atomic64_inc_nob(&dep->out);
            esdp->io.out += (Uint64) size;
	    reds -= ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = foq.first;
	    foq.first = foq.first->next;
	    free_dist_obuf(fob, !0);
	    sched_flags = erts_atomic32_read_nob(&prt->sched.flags);
	    preempt = reds < 0 || (sched_flags & ERTS_PTS_FLG_EXIT);
	    if (sched_flags & ERTS_PTS_FLG_BUSY_PORT)
		break;
	} while (foq.first && !preempt);
	if (!foq.first)
	    foq.last = NULL;
	if (preempt)
	    goto preempted;
    }

    if (sched_flags & ERTS_PTS_FLG_BUSY_PORT) {
	if (oq.first) {
	    ErtsDistOutputBuf *ob;
            ErtsDistOutputBuf *last_finalized;
	finalize_only:
            last_finalized = NULL;
	    ob = oq.first;
	    ASSERT(ob);
	    do {
                add_obuf_sizes(ob, &obufsize, &ignore_obufsize);
		reds = erts_encode_ext_dist_header_finalize(ob, dep, flags, reds);
                subtract_obuf_sizes(ob, &obufsize, &ignore_obufsize);
                if (reds < 0)
                    break; /* finalize needs to be restarted... */
                last_finalized  = ob;
                ob = ob->next;
	    } while (ob);
            if (last_finalized) {
                /*
                 * At least one buffer was finalized; if we got preempted,
                 * ob points to the next buffer to continue finalize.
                 */
                if (foq.last)
                    foq.last->next = oq.first;
                else
                    foq.first = oq.first;
                foq.last = last_finalized;
                if (!ob) {
                    /* All buffers finalized */
                    ASSERT(foq.last == oq.last);
                    ASSERT(foq.last->next == NULL);
                    oq.first = oq.last = NULL;
                }
                else {
                    /* Not all buffers finalized; split oq. */
                    ASSERT(foq.last->next == ob);
                    foq.last->next = NULL;
                    oq.first = ob;
                }
            }
            if (reds <= 0)
                goto preempted;
	}
    }
    else {
        int de_busy;
	int preempt = 0;
	while (oq.first && !preempt) {
	    ErtsDistOutputBuf *fob;
	    Uint size;
            add_obuf_sizes(oq.first, &obufsize, &ignore_obufsize);
            reds = erts_encode_ext_dist_header_finalize(oq.first, dep, flags, reds);
            if (reds < 0) { /* finalize needs to be restarted... */
                subtract_obuf_sizes(oq.first, &obufsize, &ignore_obufsize);
                preempt = 1;
                break;
            }
	    size = (*send)(prt, oq.first);
            erts_atomic64_inc_nob(&dep->out);
	    esdp->io.out += (Uint64) size;
	    reds -= ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = oq.first;
	    oq.first = oq.first->next;
	    free_dist_obuf(fob, !0);
	    sched_flags = erts_atomic32_read_nob(&prt->sched.flags);
	    preempt = reds <= 0 || (sched_flags & ERTS_PTS_FLG_EXIT);
	    if ((sched_flags & ERTS_PTS_FLG_BUSY_PORT) && oq.first && !preempt)
		goto finalize_only;
	}

	ASSERT(!oq.first || preempt);

	/*
	 * Preempt if not all buffers have been handled.
	 */
	if (preempt && oq.first)
	    goto preempted;

#ifdef DEBUG
	oq.last = NULL;
#endif
	ASSERT(!oq.first);
	ASSERT(!foq.first && !foq.last);

	/*
	 * Everything that was buffered when we started have now been
	 * written to the port. If port isn't busy but dist entry is
	 * and we haven't got too muched queued on dist entry, set
	 * dist entry in a non-busy state and resume suspended
	 * processes.
	 */
	erts_mtx_lock(&dep->qlock);
        de_busy = !!(de_qflags_read(dep) & ERTS_DE_QFLG_BUSY);
        update_qsizes(dep, NULL, &qsize, -obufsize, -ignore_obufsize);
	obufsize = ignore_obufsize = 0;
	if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT)
	    && de_busy
            && qsize < erts_dist_buf_busy_limit) {
	    int resumed;
	    ErtsProcList *suspendees = get_suspended_on_de(dep, ERTS_DE_QFLG_BUSY);
	    erts_mtx_unlock(&dep->qlock);

	    resumed = erts_resume_processes(suspendees);
	    reds -= resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
	}
	else
	    erts_mtx_unlock(&dep->qlock);
    }

    ASSERT(!oq.first && !oq.last);

 done:

    ASSERT(!ignore_obufsize || obufsize);

    ASSERT(!!foq.first == !!foq.last);
    ASSERT(!dep->finalized_out_queue.first);
    ASSERT(!dep->finalized_out_queue.last);

    if (foq.first) {
	dep->finalized_out_queue.first = foq.first;
	dep->finalized_out_queue.last = foq.last;
    }

    if (obufsize != 0) {
	erts_mtx_lock(&dep->qlock);
        update_qsizes(dep, NULL, NULL, -obufsize, -ignore_obufsize);
        ERTS_DBG_CHK_DIST_QSIZE(dep, prt);
	erts_mtx_unlock(&dep->qlock);
    }
#ifdef DEBUG
    else {
        erts_mtx_lock(&dep->qlock);
        ERTS_DBG_CHK_DIST_QSIZE(dep, prt);
	erts_mtx_unlock(&dep->qlock);
    }
#endif

    /* Avoid wrapping reduction counter... */
    if (reds < INT_MIN/2)
	reds = INT_MIN/2;

    return initial_reds - reds;

 preempted:
    /*
     * Here we assume that state has been read
     * since last call to driver.
     */

    ASSERT(!!oq.first == !!oq.last);

    if (sched_flags & ERTS_PTS_FLG_EXIT) {
	/*
	 * Port died during port command; clean up 'oq'
	 * and 'foq'. Things buffered in dist entry after
	 * we begun processing the queues have already been
	 * cleaned up when port terminated.
	 */

	if (oq.first)
	    oq.last->next = foq.first;
	else
	    oq.first = foq.first;

	while (oq.first) {
	    ErtsDistOutputBuf *fob = oq.first;
	    oq.first = oq.first->next;
            add_obuf_sizes(fob, &obufsize, &ignore_obufsize);
	    free_dist_obuf(fob, !0);
	}

	foq.first = NULL;
	foq.last = NULL;
    }
    else {
	if (oq.first) {
	    erts_mtx_lock(&dep->qlock);
            update_qsizes(dep, NULL, NULL, -obufsize, -ignore_obufsize);
	    obufsize = ignore_obufsize = 0;

	    /*
	     * Unhandled buffers need to be put back first
	     * in out_queue.
	     */
            oq.last->next = dep->out_queue.first;
	    dep->out_queue.first = oq.first;
	    if (!dep->out_queue.last)
		dep->out_queue.last = oq.last;
	    erts_mtx_unlock(&dep->qlock);
	}

	erts_schedule_dist_command(prt, NULL);
    }
    goto done;
}

BIF_RETTYPE
dist_ctrl_get_data_notification_1(BIF_ALIST_1)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    erts_aint32_t notify;
    erts_aint_t qsize;
    Eterm receiver = NIL;
    Uint32 conn_id;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
        BIF_ERROR(BIF_P, BADARG);

    /*
     * Caller is the only one that can consume from this queue
     * and the only one that can set the notify field...
     */

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        BIF_ERROR(BIF_P, BADARG);
    }

    ASSERT(dep->cid == BIF_P->common.id);

    notify = erts_atomic32_read_nob(&dep->notify);

    if (!notify) {
        ERTS_THR_READ_MEMORY_BARRIER;
        qsize = erts_atomic_read_nob(&dep->total_qsize);
        ASSERT(qsize >= 0);
        if (qsize > 0)
            receiver = BIF_P->common.id; /* Notify ourselves... */
        else { /* Empty queue; set the notify field... */
            notify = erts_atomic32_xchg_mb(&dep->notify, (erts_aint32_t) !0);
            qsize = erts_atomic_read_nob(&dep->total_qsize);
            ASSERT(qsize >= 0);
            if (qsize > 0) {
                notify = erts_atomic32_xchg_mb(&dep->notify, (erts_aint32_t) 0);
                if (notify)
                    receiver = BIF_P->common.id; /* Notify ourselves... */
                /* else: someone else will notify us... */
            }
            /* else: still empty queue... */
        }
    }
    /* else: Already requested... */

    erts_de_runlock(dep);

    if (is_internal_pid(receiver))
        notify_dist_data(BIF_P, receiver);

    BIF_RET(am_ok);
}

BIF_RETTYPE
dist_ctrl_put_data_2(BIF_ALIST_2)
{
    DistEntry *dep;
    Eterm input_handler;
    Uint32 conn_id;
    Binary *bin = NULL;

    if (is_list(BIF_ARG_2)) {
        BIF_TRAP2(dist_ctrl_put_data_trap,
                  BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    dep = erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id);
    if (!dep) {
        BIF_ERROR(BIF_P, BADARG);
    }

    input_handler = (Eterm)erts_atomic_read_nob(&dep->input_handler);

    if (input_handler != BIF_P->common.id) {
        BIF_ERROR(BIF_P, EXC_NOTSUP);
    }

    erts_atomic64_inc_nob(&dep->in);

    if (is_bitstring(BIF_ARG_2)) {
        ERTS_DECLARE_DUMMY(Eterm br_flags);
        const byte *data, *temp_alloc = NULL;
        Uint offset, size;
        BinRef *br;

        ERTS_PIN_BITSTRING(BIF_ARG_2, br_flags, br, data, offset, size);

        if (TAIL_BITS(size) != 0) {
            BIF_ERROR(BIF_P, BADARG);
        }

        if (BIT_OFFSET(offset) == 0) {
            data = &data[BYTE_OFFSET(offset)];
            size = BYTE_SIZE(size);

            bin = br ? br->val : NULL;
        } else {
            ERTS_DECLARE_DUMMY(Uint dummy);
            data = (byte *) erts_get_aligned_binary_bytes(BIF_ARG_2,
                                                          &size,
                                                          &temp_alloc);
        }

        if (!data) {
            BIF_ERROR(BIF_P, BADARG);
        }

        erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);

        (void) erts_net_message(NULL, dep, conn_id, NULL, 0, bin, data, size);
        /*
         * We ignore any decode failures. On fatal failures the
         * connection will be taken down by killing the
         * distribution channel controller...
         */

        erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

        BUMP_REDS(BIF_P, 5);

        erts_free_aligned_binary_bytes(temp_alloc);

    } else if (is_not_nil(BIF_ARG_2)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    BIF_RET(am_ok);
}

BIF_RETTYPE
dist_ctrl_set_opt_3(BIF_ALIST_3)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    Uint32 conn_id;
    BIF_RETTYPE ret;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id)
        ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    else {

        switch (BIF_ARG_2) {
        case am_get_size:
            ERTS_BIF_PREP_RET(ret, (dep->opts & ERTS_DIST_CTRL_OPT_GET_SIZE
                                    ? am_true
                                    : am_false));
            if (BIF_ARG_3 == am_true) 
                dep->opts |= ERTS_DIST_CTRL_OPT_GET_SIZE;
            else if (BIF_ARG_3 == am_false)
                dep->opts &= ~ERTS_DIST_CTRL_OPT_GET_SIZE;
            else
                ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
            break;
        default:
            ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
            break;
        }

    }

    erts_de_runlock(dep);

    return ret;
}

BIF_RETTYPE
dist_ctrl_get_opt_2(BIF_ALIST_2)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    Uint32 conn_id;
    BIF_RETTYPE ret;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id)
        ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    else {

        switch (BIF_ARG_2) {
        case am_get_size:
            ERTS_BIF_PREP_RET(ret, (dep->opts & ERTS_DIST_CTRL_OPT_GET_SIZE
                                    ? am_true
                                    : am_false));
            break;
        default:
            ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
            break;
        }

    }

    erts_de_runlock(dep);

    return ret;
}
    
BIF_RETTYPE
dist_get_stat_1(BIF_ALIST_1)
{
    Sint64 read, write, pend;
    Eterm res, *hp, **hpp;
    Uint sz, *szp;
    Uint32 conn_id;
    DistEntry *dep = erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id);

    if (!dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        BIF_ERROR(BIF_P, BADARG);
    }
    read = (Sint64) erts_atomic64_read_nob(&dep->in);
    write = (Sint64) erts_atomic64_read_nob(&dep->out);
    pend = (Sint64) erts_atomic_read_nob(&dep->total_qsize);

    erts_de_runlock(dep);

    sz = 0;
    szp = &sz;
    hpp = NULL;

    while (1) {
        res = erts_bld_tuple(hpp, szp, 4,
                             am_ok,
                             erts_bld_sint64(hpp, szp, read),
                             erts_bld_sint64(hpp, szp, write),
                             erts_bld_sint64(hpp, szp, pend));
        if (hpp)
            break;
        hp = HAlloc(BIF_P, sz);
        hpp = &hp;
        szp = NULL;
    }

    BIF_RET(res);
}

BIF_RETTYPE
dist_ctrl_input_handler_2(BIF_ALIST_2)
{
    Uint32 conn_id;
    DistEntry *dep = erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id);

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if ((ERTS_PROC_GET_DIST_ENTRY(BIF_P) != dep) && !is_internal_port(dep->cid))
        BIF_ERROR(BIF_P, BADARG);

    if (is_not_internal_pid(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);
    if (dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        BIF_ERROR(BIF_P, BADARG);
    }
    erts_atomic_set_nob(&dep->input_handler,
                        (erts_aint_t) BIF_ARG_2);
    erts_de_runlock(dep);
    BIF_RET(am_ok);
}

BIF_RETTYPE
dist_ctrl_get_data_1(BIF_ALIST_1)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    const Sint initial_reds = ERTS_BIF_REDS_LEFT(BIF_P);
    Sint reds = initial_reds, ix, vlen;
    /*
     * 'obufsize' and 'ignore_obufsize' contains the number of bytes removed
     * from the queue which will be updated (in dep->total_qsize and
     * dep->qsize) before we return from this function. Note that
     * 'obufsize' and 'ignore_obufsize' may be negative if we added to the
     * queue size. This may occur since finalization of a buffer may increase
     * buffer size.
     */
    Sint obufsize = 0, ignore_obufsize = 0;
    ErtsDistOutputBuf *obuf;
    Eterm *hp, res;
    Sint qsize;
    Uint32 conn_id, get_size;
    Uint hsz = 0, data_sz;
    SysIOVec *iov;
    ErlDrvBinary **binv;
#ifdef DEBUG
    Eterm *hendp;
#endif

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        BIF_ERROR(BIF_P, BADARG);
    }

    if (dep->state == ERTS_DE_STATE_EXITING)
        goto return_none;

    ASSERT(dep->cid == BIF_P->common.id);

#if 0
    if (dep->finalized_out_queue.first) {
        obuf = dep->finalized_out_queue.first;
        dep->finalized_out_queue.first = obuf->next;
        if (!obuf->next)
            dep->finalized_out_queue.last = NULL;
    }
    else
#endif
    {
        if (!dep->tmp_out_queue.first) {
            ASSERT(!dep->tmp_out_queue.last);
            qsize = (Sint) erts_atomic_read_acqb(&dep->total_qsize);
            if (qsize > 0) {
                erts_mtx_lock(&dep->qlock);
                dep->tmp_out_queue.first = dep->out_queue.first;
                dep->tmp_out_queue.last = dep->out_queue.last;
                dep->out_queue.first = NULL;
                dep->out_queue.last = NULL;
                erts_mtx_unlock(&dep->qlock);
            }
        }

        if (!dep->tmp_out_queue.first) {
            ASSERT(!dep->tmp_out_queue.last);
        return_none:
            erts_de_runlock(dep);
            BIF_RET(am_none);
        }

        obuf = dep->tmp_out_queue.first;
        add_obuf_sizes(obuf, &obufsize, &ignore_obufsize);
        reds = erts_encode_ext_dist_header_finalize(obuf, dep, dep->dflags, reds);
        subtract_obuf_sizes(obuf, &obufsize, &ignore_obufsize);
        if (reds < 0) { /* finalize needs to be restarted... */
            erts_de_runlock(dep);
            if (obufsize) {
                erts_mtx_lock(&dep->qlock);
                update_qsizes(dep, NULL, NULL, -obufsize, -ignore_obufsize);
                erts_mtx_unlock(&dep->qlock);
            }
            ERTS_BIF_YIELD1(BIF_TRAP_EXPORT(BIF_dist_ctrl_get_data_1),
                            BIF_P, BIF_ARG_1);
        }

        dep->tmp_out_queue.first = obuf->next;
        if (!obuf->next)
            dep->tmp_out_queue.last = NULL;
    }

    erts_atomic64_inc_nob(&dep->out);

    erts_de_runlock(dep);

    vlen = obuf->eiov->vsize;
    data_sz = obuf->eiov->size;
    iov = obuf->eiov->iov;
    binv = obuf->eiov->binv;
    
#ifdef DEBUG
    {
        Uint dbg_sz;
        for (ix = 0, dbg_sz = 0; ix < vlen; ix++)
            dbg_sz += iov[ix].iov_len;
        ASSERT(data_sz == dbg_sz);
    }
#endif

    ASSERT(vlen >= 1);
    ASSERT(iov[0].iov_len == 0);
    ASSERT(!binv[0]);

    hsz = 2 /* cons */ + ERL_REFC_BITS_SIZE;
    hsz *= vlen - 1;

    get_size = dep->opts & ERTS_DIST_CTRL_OPT_GET_SIZE;
    if (get_size) {
        hsz += 3; /* 2 tuple */
        if (!IS_USMALL(0, data_sz))
            hsz += BIG_UINT_HEAP_SIZE;
    }

    hp = HAlloc(BIF_P, hsz);
#ifdef DEBUG
    hendp = hp + hsz;
#endif

    res = NIL;

    for (ix = vlen - 1; ix > 0; ix--) {
        Eterm bin_term;
        Binary *bin;

        ASSERT(binv[ix]);
        bin = ErlDrvBinary2Binary(binv[ix]);
        bin_term = erts_wrap_refc_bitstring(&MSO(BIF_P).first,
                                            &MSO(BIF_P).overhead,
                                            &hp,
                                            bin,
                                            iov[ix].iov_base,
                                            0,
                                            NBITS(iov[ix].iov_len));

        res = CONS(hp, bin_term, res);
        hp += 2;
    }

    add_obuf_sizes(obuf, &obufsize, &ignore_obufsize);

    erts_mtx_lock(&dep->qlock);

    update_qsizes(dep, NULL, &qsize, -obufsize, -ignore_obufsize);

    if (qsize >= erts_dist_buf_busy_limit/2
        || !(de_qflags_read(dep) & ERTS_DE_QFLG_BUSY)) {
        erts_mtx_unlock(&dep->qlock);
    }
    else {
        ErtsProcList *resume_procs = NULL;
        resume_procs = get_suspended_on_de(dep, ERTS_DE_QFLG_BUSY);
        erts_mtx_unlock(&dep->qlock);
        if (resume_procs) {
            int resumed = erts_resume_processes(resume_procs);
            reds -= resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
        }
    }

    if (get_size) {
        Eterm sz_term;
        if (IS_USMALL(0, data_sz))
            sz_term = make_small(data_sz);
        else {
            sz_term = uint_to_big(data_sz, hp);
            hp += BIG_UINT_HEAP_SIZE;
        }
        res = TUPLE2(hp, sz_term, res);
        hp += 3;
    }

    ASSERT(hendp == hp);

    free_dist_obuf(obuf, 0);

    BIF_RET2(res, (initial_reds - reds));
}

void
erts_dist_port_not_busy(Port *prt)
{
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_port_not_busy)) {
        DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);

        ERTS_ASSUME(dep);

        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", dep->sysname);
        DTRACE3(dist_port_not_busy, erts_this_node_sysname,
                port_str, remote_str);
    }
#endif
    erts_schedule_dist_command(prt, NULL);
}

static void kill_connection(DistEntry *dep)
{
    ERTS_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    ASSERT(dep->state == ERTS_DE_STATE_CONNECTED);

    dep->state = ERTS_DE_STATE_EXITING;
    erts_mtx_lock(&dep->qlock);
    ASSERT(!(de_qflags_read(dep) & ERTS_DE_QFLG_EXIT));
    de_qflags_read_set(dep, ERTS_DE_QFLG_EXIT);
    erts_mtx_unlock(&dep->qlock);

    if (is_internal_port(dep->cid))
        erts_schedule_dist_command(NULL, dep);
    else if (is_internal_pid(dep->cid))
        schedule_kill_dist_ctrl_proc(dep->cid);
}

void
erts_kill_dist_connection(DistEntry *dep, Uint32 conn_id)
{
#ifdef ERTS_DIST_MSG_DBG
    erts_fprintf(dbg_file, "INTR: kill dist conn to %T:%u\n",
                 dep->sysname, conn_id);
#endif
    erts_de_rwlock(dep);
    if (conn_id == dep->connection_id
        && dep->state == ERTS_DE_STATE_CONNECTED) {

        kill_connection(dep);
    }
    erts_de_rwunlock(dep);
}

struct print_to_data {
    fmtfn_t to;
    void *arg;
};

static int doit_print_monitor_info(ErtsMonitor *mon, void *vptdp, Sint reds)
{
    fmtfn_t to = ((struct print_to_data *) vptdp)->to;
    void *arg = ((struct print_to_data *) vptdp)->arg;
    ErtsMonitorDataExtended *mdep;

    ASSERT(mon->flags & ERTS_ML_FLG_EXTENDED);

    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);

    ASSERT(mdep->dist);

    if (erts_monitor_is_origin(mon)) {
	erts_print(to, arg, "Remotely monitored by: %T %T\n",
		   mon->other.item, mdep->md.u.target.other.item);
    }
    else {
	erts_print(to, arg, "Remote monitoring: %T ", mon->other.item);
        if (mon->flags & ERTS_ML_FLG_NAME)
	    erts_print(to, arg, "{%T, %T}\n", mdep->u.name, mdep->dist->nodename);
        else
	    erts_print(to, arg, "%T\n", mdep->md.origin.other.item);
    }
    return 1;
}    

static void print_monitor_info(fmtfn_t to, void *arg, DistEntry *dep)
{
    struct print_to_data ptd = {to, arg};
    if (dep->mld) {
        erts_monitor_list_foreach(dep->mld->monitors,
                                  doit_print_monitor_info,
                                  (void *) &ptd);
        erts_monitor_tree_foreach(dep->mld->orig_name_monitors,
                                  doit_print_monitor_info,
                                  (void *) &ptd);
    }
}

static int doit_print_link_info(ErtsLink *lnk, void *vptdp, Sint reds)
{
    struct print_to_data *ptdp = vptdp;
    ErtsLink *lnk2 = erts_link_to_other(lnk, NULL);
    ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);
    erts_print(ptdp->to, ptdp->arg, "Remote link: %T %T\n",
	       lnk2->other.item, lnk->other.item);
    return 1;
}

static void print_link_info(fmtfn_t to, void *arg, DistEntry *dep)
{
    struct print_to_data ptd = {to, arg};
    if (dep->mld)
        erts_link_list_foreach(dep->mld->links,
                               doit_print_link_info,
                               (void *) &ptd);
}

typedef struct {
    struct print_to_data ptd;
    Eterm sysname;
} PrintNodeLinkContext;
    
static int
info_dist_entry(fmtfn_t to, void *arg, DistEntry *dep, int visible, int connected)
{

  if (visible && connected) {
      erts_print(to, arg, "=visible_node:");
  } else if (connected) {
      erts_print(to, arg, "=hidden_node:");
  } else {
      erts_print(to, arg, "=not_connected:");
  }
  erts_print(to, arg, "%d\n", dist_entry_channel_no(dep));

  if(connected && is_nil(dep->cid)) {
    erts_print(to, arg,
	       "Error: Not connected node still registered as connected:%T\n",
	       dep->sysname);
    return 0;
  }

  if(!connected && is_not_nil(dep->cid)) {
    erts_print(to, arg,
	       "Error: Connected node not registered as connected:%T\n",
	       dep->sysname);
    return 0;
  }

  erts_print(to, arg, "Name: %T", dep->sysname);
  erts_print(to, arg, "\n");
  if (!connected && is_nil(dep->cid)) {
    if (dep->mld) {
      erts_print(to, arg, "Error: Got links/monitors to not connected node:%T\n",
		 dep->sysname);
    }
    return 0;
  }

  erts_print(to, arg, "Controller: %T\n", dep->cid, to);

  erts_print_node_info(to, arg, dep->sysname, NULL, NULL);
  print_monitor_info(to, arg, dep);
  print_link_info(to, arg, dep);

  return 0;
    
}
int distribution_info(fmtfn_t to, void *arg)	/* Called by break handler */
{
    DistEntry *dep;

    erts_print(to, arg, "=node:%T\n", erts_this_dist_entry->sysname);
 
    if (erts_this_node->sysname == am_Noname) {
	erts_print(to, arg, "=no_distribution\n");
	return(0);
    }

#if 0
    if (!erts_visible_dist_entries && !erts_hidden_dist_entries) 
      erts_print(to, arg, "Alive but not holding any connections \n");
#endif

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, arg, dep, 1, 1);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, arg, dep, 0, 1);
    }

    for (dep = erts_pending_dist_entries; dep; dep = dep->next) {
        info_dist_entry(to, arg, dep, 0, 0);
    }

    for (dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
        if (dep != erts_this_dist_entry) {
            info_dist_entry(to, arg, dep, 0, 0);
        }
    }

    return(0);
}

/****************************************************************************
  DISTRIBUTION BIFS:

            setnode/2     -- start distribution
            setnode/3     -- set node controller

            node/1        -- return objects node name
            node/0        -- return this node name
            nodes/0       -- return a list of all (non hidden) nodes
            is_alive      -- return true if distribution is running else false
	    monitor_node  -- turn on/off node monitoring

            node controller only:
            dist_link/2       -- link a remote process to a local
            dist_unlink/2     -- unlink a remote from a local
****************************************************************************/



/**********************************************************************
 ** Set the node name of current node fail if node already is set.
 ** setnode(name@host, Creation)
 ***********************************************************************/

BIF_RETTYPE setnode_2(BIF_ALIST_2)
{
    Process *net_kernel = NULL;
    Uint32 creation;
    int success;

    /* valid creation ? */
    if(!term_to_Uint32(BIF_ARG_2, &creation))
	goto error;

    /* valid node name ? */
    if (!is_node_name_atom(BIF_ARG_1))
	goto error;

    if (BIF_ARG_1 == am_Noname) /* can't use this name !! */
	goto error;
    if (erts_is_alive)     /* must not be alive! */
	goto error;

    /* Check that all trap functions are defined !! */
    if (dmonitor_node_trap->dispatch.addresses[0] == NULL) {
	goto error;
    }

    net_kernel = erts_whereis_process(BIF_P,
                                      ERTS_PROC_LOCK_MAIN,
				      am_net_kernel,
                                      0,
                                      ERTS_P2P_FLG_INC_REFC);
    if (!net_kernel)
	goto error;

#ifdef DEBUG
    erts_rwmtx_rlock(&erts_dist_table_rwmtx);
    ASSERT(!erts_visible_dist_entries && !erts_hidden_dist_entries);
    erts_rwmtx_runlock(&erts_dist_table_rwmtx);
#endif

    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    
    erts_thr_progress_block();

    success = (!ERTS_PROC_IS_EXITING(net_kernel)
               && !ERTS_PROC_GET_DIST_ENTRY(net_kernel));
    if (success) {
        /*
         * Ensure we don't use a nodename-creation pair with
         * external identifiers existing in the system.
         */
        while (!0) {
            ErlNode *nep;
            if (creation < 4)
                creation = 4;
            nep = erts_find_node(BIF_ARG_1, creation);
            if (!nep || erts_node_refc(nep) == 0)
                break;
            creation++;
        }
        inc_no_nodes();
        erts_set_this_node(BIF_ARG_1, (Uint32) creation);
        erts_this_dist_entry->creation = creation;
        erts_is_alive = 1;
        send_nodes_mon_msgs(NULL, am_nodeup, BIF_ARG_1, ~((Uint32) 0),
                            am_visible, NIL);
        erts_proc_lock(net_kernel, ERTS_PROC_LOCKS_ALL);

        /* By setting F_DISTRIBUTION on net_kernel,
         * erts_do_net_exits will be called when net_kernel is terminated !! */
        net_kernel->flags |= F_DISTRIBUTION;

        /*
         * Note erts_this_dist_entry is changed by erts_set_this_node(),
         * so we *need* to use the new one after erts_set_this_node()
         * is called.
         */
        erts_ref_dist_entry(erts_this_dist_entry);
        ERTS_PROC_SET_DIST_ENTRY(net_kernel, erts_this_dist_entry);
        erts_proc_unlock(net_kernel, ERTS_PROC_LOCKS_ALL);
    }
    
    erts_thr_progress_unblock();

    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    
    if (success) {
        erts_proc_dec_refc(net_kernel);
        BIF_RET(am_true);
    }

 error:
    if (net_kernel)
        erts_proc_dec_refc(net_kernel);
    BIF_ERROR(BIF_P, BADARG);
}

/*
 * erts_is_this_node_alive() returns the same result as erlang:is_alive()
 */
int
erts_is_this_node_alive(void)
{
    Eterm tmp_heap[3];
    Eterm dyn_name_key, dyn_name_value;

    /*
     * erts_is_alive is only non zero if a node name has been set. Not if
     * dynamic node name has been enabled.
     */
    if (erts_is_alive) {
        return !0;
    }

    /*
     * A persistent term with key '{erts_internal, dynamic_node_name}' has been
     * set if dynamic node name has been enabled.
     */
    dyn_name_key = TUPLE2(&tmp_heap[0], am_erts_internal, am_dynamic_node_name);
    dyn_name_value = erts_persistent_term_get(dyn_name_key);
    if (is_value(dyn_name_value) && dyn_name_value != am_false) {
        return !0;
    }

    return 0;
}

/*
 * erts_internal:create_dist_channel/4 is used by
 * erlang:setnode/3.
 */

typedef struct {
    DistEntry *dep;
    int de_locked;
    Uint64 dflags;
    Uint32 creation;
    Eterm setup_pid;
    Process *net_kernel;
} ErtsSetupConnDistCtrl;

static int
setup_connection_epiloge_rwunlock(Process *c_p, DistEntry *dep,
                                  Eterm ctrlr, Uint64 flags,
                                  Uint32 creation, Eterm setup_pid,
                                  Process *net_kernel);

static Eterm
setup_connection_distctrl(Process *c_p, void *arg,
                          int *redsp, ErlHeapFragment **bpp);

BIF_RETTYPE erts_internal_create_dist_channel_3(BIF_ALIST_3)
{
    BIF_RETTYPE ret;
    Uint64 flags;
    Uint32 creation;
    Eterm *hp, res_tag = THE_NON_VALUE, res = THE_NON_VALUE;
    DistEntry *dep = NULL;
    int de_locked = 0;
    Port *pp = NULL;
    int true_nk;
    Eterm *tpl;
    Process *net_kernel = erts_whereis_process(BIF_P, ERTS_PROC_LOCK_MAIN,
                                               am_net_kernel,
                                               ERTS_PROC_LOCK_STATUS,
                                               ERTS_P2P_FLG_INC_REFC);

    if (!net_kernel)
        goto badarg;

    true_nk = ERTS_PROC_GET_DIST_ENTRY(net_kernel) == erts_this_dist_entry;
    erts_proc_unlock(net_kernel, ERTS_PROC_LOCK_STATUS);
    if (!true_nk)
        goto badarg;
    
    /*
     * Check and pick out arguments
     */

    /* Node name... */
    if (!is_node_name_atom(BIF_ARG_1))
        goto badarg;

    /* Distribution controller... */
    if (!is_internal_port(BIF_ARG_2) && !is_internal_pid(BIF_ARG_2))
        goto badarg;

    if (!is_tuple_arity(BIF_ARG_3, 2))
        goto badarg;

    tpl = tuple_val(BIF_ARG_3);

    /* Dist flags... */
    if (!term_to_Uint64(tpl[1], &flags))
        goto badarg;

    /* Creation... */
    if (!term_to_Uint32(tpl[2], &creation))
        goto badarg;

    if (~flags & DFLAG_DIST_MANDATORY) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "%T", BIF_P->common.id);
	if (BIF_P->common.u.alive.reg)
	    erts_dsprintf(dsbufp, " (%T)", BIF_P->common.u.alive.reg->name);
	erts_dsprintf(dsbufp,
		      " attempted to enable connection to node %T "
		      "which does not support all mandatory capabilities.\n",
		      BIF_ARG_1);
	erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	goto badarg;
    }

    /*
     * ToDo: Should we not pass connection_id as well
     *       to make sure it's the right connection we commit.
     */

    /*
     * Arguments seem to be in order.
     */

    /* get dist_entry */
    dep = erts_find_or_insert_dist_entry(BIF_ARG_1);
    if (dep == erts_this_dist_entry)
	goto badarg;
    else if (!dep)
	goto system_limit; /* Should never happen!!! */

    if (is_internal_pid(BIF_ARG_2)) {
        erts_de_rwlock(dep);
        de_locked = 1;
        if (dep->pending_nodedown)
            goto suspend;

        if (BIF_P->common.id == BIF_ARG_2) {
            ErtsSetupConnDistCtrl scdc;

            scdc.dep = dep;
            scdc.de_locked = 1;
            scdc.dflags = flags;
            scdc.creation = creation;
            scdc.setup_pid = BIF_P->common.id;
            scdc.net_kernel = net_kernel;

            res = setup_connection_distctrl(BIF_P, &scdc, NULL, NULL);
            /* Dec of refc on net_kernel by setup_connection_distctrl() */
            net_kernel = NULL;
            de_locked = 0;
            BUMP_REDS(BIF_P, 5);
            dep = NULL;

            if (res == am_badarg)
                goto badarg;

            ASSERT(is_internal_magic_ref(res));
            res_tag = am_ok; /* Connection up */
        }
        else {
            ErtsSetupConnDistCtrl *scdcp;

            erts_de_rwunlock(dep);
            de_locked = 0;

            scdcp = erts_alloc(ERTS_ALC_T_SETUP_CONN_ARG,
                               sizeof(ErtsSetupConnDistCtrl));

            scdcp->dep = dep;
            scdcp->de_locked = 0;
            scdcp->dflags = flags;
            scdcp->creation = creation;
            scdcp->setup_pid = BIF_P->common.id;
            scdcp->net_kernel = net_kernel;

            res = erts_proc_sig_send_rpc_request(BIF_P,
                                                 BIF_ARG_2,
                                                 !0,
                                                 setup_connection_distctrl,
                                                 (void *) scdcp);
            if (is_non_value(res))
                goto badarg; /* Was not able to send signal... */

            /* Dec of refc on net_kernel by setup_connection_distctrl() */
            net_kernel = NULL;
            
            dep = NULL;

            ASSERT(is_internal_ordinary_ref(res));

            res_tag = am_message; /* Caller need to wait for dhandle in message */
        }
        hp = HAlloc(BIF_P, 3);
    }
    else {
        Uint32 conn_id;
        int set_res;

        pp = erts_id2port_sflgs(BIF_ARG_2,
                                BIF_P,
                                ERTS_PROC_LOCK_MAIN,
                                ERTS_PORT_SFLGS_INVALID_LOOKUP);
        erts_de_rwlock(dep);
        de_locked = 1;

        if (dep->state != ERTS_DE_STATE_PENDING)
            goto badarg;

        if (!pp || (erts_atomic32_read_nob(&pp->state)
                    & ERTS_PORT_SFLG_EXITING))
            goto badarg;

        if ((pp->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY) == 0)
            goto badarg;

        if (erts_prtsd_get(pp, ERTS_PRTSD_DIST_ENTRY) != NULL
            || is_not_nil(dep->cid))
            goto badarg;

        if(dep->pending_nodedown)
            goto suspend;

        erts_atomic32_read_bor_nob(&pp->state, ERTS_PORT_SFLG_DISTRIBUTION);

        erts_prtsd_set(pp, ERTS_PRTSD_DIST_ENTRY, dep);
        erts_prtsd_set(pp, ERTS_PRTSD_CONN_ID, (void*)(UWord)dep->connection_id);

        ASSERT(pp->drv_ptr->outputv || pp->drv_ptr->output);

        dep->send = (pp->drv_ptr->outputv
                     ? dist_port_commandv
                     : dist_port_command);
        ASSERT(dep->send);

        conn_id = dep->connection_id;
        set_res = setup_connection_epiloge_rwunlock(BIF_P, dep, BIF_ARG_2, flags,
                                                    creation, BIF_P->common.id,
                                                    net_kernel);
        /* Dec of refc on net_kernel by setup_connection_epiloge_rwunlock() */
        net_kernel = NULL;
        if (set_res == 0) {
            erts_atomic32_read_band_nob(&pp->state, ~ERTS_PORT_SFLG_DISTRIBUTION);
            erts_prtsd_set(pp, ERTS_PRTSD_DIST_ENTRY, NULL);
            erts_prtsd_set(pp, ERTS_PRTSD_CONN_ID, NULL);
            goto badarg;
        }
        de_locked = 0;

        /*
         * Dist-ports do not use the "busy port message queue" functionality,
         * but instead use "busy dist entry" functionality.
         */
        {
            ErlDrvSizeT disable = ERL_DRV_BUSY_MSGQ_DISABLED;
            erl_drv_busy_msgq_limits(ERTS_Port2ErlDrvPort(pp), &disable, NULL);
        }

        hp = HAlloc(BIF_P, 3 + ERTS_DHANDLE_SIZE);
        res = erts_build_dhandle(&hp, &BIF_P->off_heap, dep, conn_id);
        res_tag = am_ok; /* Connection up */
        dep = NULL; /* inc of refc transferred to port (dist_entry field) */
    }

    ASSERT(is_value(res) && is_value(res_tag));

    res = TUPLE2(hp, res_tag, res);

    ERTS_BIF_PREP_RET(ret, res);

 done:

    if (net_kernel)
        erts_proc_dec_refc(net_kernel);

    if (dep && dep != erts_this_dist_entry) {
        if (de_locked) {
            if (de_locked > 0)
                erts_de_rwunlock(dep);
            else
                erts_de_runlock(dep);
        }
	erts_deref_dist_entry(dep);
    }

    if (pp)
	erts_port_release(pp);

    return ret;

 suspend:
     ASSERT(de_locked);
     ASSERT(!dep->suspended_nodeup);
     dep->suspended_nodeup = BIF_P;
     erts_proc_inc_refc(BIF_P);
     erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
     ERTS_BIF_PREP_YIELD3(ret,
                          BIF_TRAP_EXPORT(BIF_erts_internal_create_dist_channel_3),
                          BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
     goto done;

 badarg:
    ERTS_BIF_PREP_RET(ret, am_badarg);
    goto done;

 system_limit:
    ERTS_BIF_PREP_RET(ret, am_system_limit);
    goto done;
}

static int
setup_connection_epiloge_rwunlock(Process *c_p, DistEntry *dep,
                                  Eterm ctrlr, Uint64 flags,
                                  Uint32 creation, Eterm setup_pid,
                                  Process *net_kernel)
{
    Eterm notify_proc = NIL;
    ErtsProcLocks nk_locks;
    int success = 0;

    /* Notify net_kernel about the new dist controller... */
    ASSERT(net_kernel);
    nk_locks = ERTS_PROC_LOCK_MSGQ;
    erts_proc_lock(net_kernel, nk_locks);
    if (!ERTS_PROC_IS_EXITING(net_kernel)
        && ERTS_PROC_GET_DIST_ENTRY(net_kernel) == erts_this_dist_entry) {
        Eterm *hp;
        ErlOffHeap *ohp;
        ErtsMessage *mp = erts_alloc_message_heap(net_kernel, &nk_locks,
                                                  5 /* 4-tuple */,
                                                  &hp, &ohp);
        Eterm msg = TUPLE4(hp, am_dist_ctrlr, ctrlr, dep->sysname, setup_pid);
        erts_queue_message(net_kernel, nk_locks, mp, msg, am_system);
        success = !0;
    }
    erts_proc_unlock(net_kernel, nk_locks);
    erts_proc_dec_refc(net_kernel);

    if (!success)
        return 0;
    
    dep->creation = creation;

    ASSERT(is_internal_port(ctrlr) || is_internal_pid(ctrlr));
    ASSERT(dep->state == ERTS_DE_STATE_PENDING);

    if (flags & DFLAG_DIST_HDR_ATOM_CACHE)
	create_cache(dep);

    erts_set_dist_entry_connected(dep, ctrlr, flags);

    notify_proc = NIL;
    if (erts_atomic_read_nob(&dep->total_qsize)) {
        if (is_internal_port(dep->cid)) {
            erts_schedule_dist_command(NULL, dep);
        }
        else {
            erts_aint32_t notify;
            ERTS_THR_READ_MEMORY_BARRIER;
            notify = erts_atomic32_read_nob(&dep->notify);
            if (notify) {
                notify = erts_atomic32_xchg_mb(&dep->notify,
                                               (erts_aint32_t) 0);
                if (notify) {
                    notify_proc = dep->cid;
                    ASSERT(is_internal_pid(notify_proc));
                }
            }
        }
    }

    erts_de_rwunlock(dep);

    if (is_internal_pid(notify_proc))
        notify_dist_data(c_p, notify_proc);

    inc_no_nodes();

    send_nodes_mon_msgs(c_p,
			am_nodeup,
			dep->sysname,
                        dep->connection_id,
			flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			NIL);

    return !0;
}

static Eterm
setup_connection_distctrl(Process *c_p, void *arg, int *redsp, ErlHeapFragment **bpp)
{
    ErtsSetupConnDistCtrl *scdcp = (ErtsSetupConnDistCtrl *) arg;
    DistEntry *dep = scdcp->dep;
    Eterm *hp;
    Uint32 conn_id;
    int dec_net_kernel_on_error = !0;

    if (redsp)
        *redsp = 1;

    if (ERTS_PROC_IS_EXITING(c_p))
        goto badarg;

    if (!scdcp->de_locked) {
        erts_de_rwlock(dep);
        scdcp->de_locked = !0;
    }

    if (dep->state != ERTS_DE_STATE_PENDING)
        goto badarg;

    conn_id = dep->connection_id;

    if (is_not_nil(dep->cid))
        goto badarg;

    if (ERTS_PROC_GET_DIST_ENTRY(c_p))
        goto badarg;
    
    erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    c_p->flags |= F_DISTRIBUTION;
    ERTS_PROC_SET_DIST_ENTRY(c_p, dep);
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    dep->send = NULL; /* Only for distr ports... */

    if (redsp)
        *redsp = 5;

    if (!setup_connection_epiloge_rwunlock(c_p, dep, c_p->common.id,
                                           scdcp->dflags, scdcp->creation,
                                           scdcp->setup_pid,
                                           scdcp->net_kernel)) {
        erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
        c_p->flags &= ~F_DISTRIBUTION;
        ERTS_PROC_SET_DIST_ENTRY(c_p, NULL);
        erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
        dec_net_kernel_on_error = 0; /* dec:ed in epilog... */
        goto badarg;
    }

    /* we take over previous inc in refc of dep */

    if (!bpp) /* called directly... */
        return erts_make_dhandle(c_p, dep, conn_id);

    erts_free(ERTS_ALC_T_SETUP_CONN_ARG, arg);

    *bpp = new_message_buffer(ERTS_DHANDLE_SIZE);
    hp = (*bpp)->mem;
    return erts_build_dhandle(&hp, &(*bpp)->off_heap, dep, conn_id);

badarg:

    if (dec_net_kernel_on_error)
        erts_proc_dec_refc(scdcp->net_kernel);

    if (bpp) /* not called directly */
        erts_free(ERTS_ALC_T_SETUP_CONN_ARG, arg);

    if (scdcp->de_locked)
        erts_de_rwunlock(dep);

    erts_deref_dist_entry(dep);

    return am_badarg;
}


BIF_RETTYPE erts_internal_get_dflags_0(BIF_ALIST_0)
{
    if (erts_dflags_test_remove) {
        /* For internal emulator tests only! */
        const Uint64 mask = ~erts_dflags_test_remove;
        Eterm *hp, **hpp = NULL;
        Uint sz = 0, *szp = &sz;
        Eterm res;
        while (1) {
            res = erts_bld_tuple(hpp, szp, 6,
                am_erts_dflags,
                erts_bld_uint64(hpp, szp, DFLAG_DIST_DEFAULT & mask),
                erts_bld_uint64(hpp, szp, DFLAG_DIST_MANDATORY & mask),
                erts_bld_uint64(hpp, szp, DFLAG_DIST_ADDABLE & mask),
                erts_bld_uint64(hpp, szp, DFLAG_DIST_REJECTABLE & mask),
                erts_bld_uint64(hpp, szp, DFLAG_DIST_STRICT_ORDER & mask));
            if (hpp) {
                ASSERT(is_value(res));
                return res;
            }
            hp = HAlloc(BIF_P, sz);
            hpp = &hp;
            szp = NULL;
        }
    }
    return ERTS_GLOBAL_LIT_DFLAGS_RECORD;
}

BIF_RETTYPE erts_internal_get_creation_0(BIF_ALIST_0)
{
    Eterm *hp;
    Uint hsz = 0;
    Uint32 cr = erts_this_dist_entry->creation;
    Eterm ret;

    if (cr == 0)
        ret = am_undefined;
    else {
        erts_bld_uint(NULL, &hsz, cr);
        hp = HAlloc(BIF_P, hsz);
        ret = erts_bld_uint(&hp, NULL, cr);
    }
    return ret;
}

BIF_RETTYPE erts_internal_new_connection_1(BIF_ALIST_1)
{
    DistEntry* dep;
    Uint32 conn_id;
    Eterm* hp;
    Eterm dhandle;

    if (!is_node_name_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    dep = erts_find_or_insert_dist_entry(BIF_ARG_1);

    if (dep == erts_this_dist_entry) {
        erts_deref_dist_entry(dep);
        BIF_ERROR(BIF_P, BADARG);
    }

    erts_de_rwlock(dep);

    switch (dep->state) {
    case ERTS_DE_STATE_CONNECTED:
    case ERTS_DE_STATE_EXITING:
    case ERTS_DE_STATE_PENDING:
	conn_id = dep->connection_id;
        break;
    case ERTS_DE_STATE_IDLE:
        erts_set_dist_entry_pending(dep);
	conn_id = dep->connection_id;
        break;
    default:
        erts_exit(ERTS_ABORT_EXIT, "Invalid dep->state (%d)\n", dep->state);
    }
    erts_de_rwunlock(dep);
    hp = HAlloc(BIF_P, ERTS_DHANDLE_SIZE);
    dhandle = erts_build_dhandle(&hp, &BIF_P->off_heap, dep, conn_id);
    erts_deref_dist_entry(dep);
    BIF_RET(dhandle);
}

Sint erts_abort_pending_connection_rwunlock(DistEntry* dep,
                                            int *was_connected_p)
{
    ERTS_LC_ASSERT(erts_lc_is_de_rwlocked(dep));

    if (was_connected_p)
        *was_connected_p = dep->state == ERTS_DE_STATE_CONNECTED;

    if (dep->state == ERTS_DE_STATE_PENDING) {
        ErtsAtomCache *cache;
        ErtsDistOutputBuf *obuf;
        ErtsProcList *resume_procs;
        Sint reds = 0;
        ErtsMonLnkDist *mld;

	ASSERT(is_nil(dep->cid));

        mld = dep->mld;
        dep->mld = NULL;

	cache = dep->cache;
	dep->cache = NULL;
	erts_mtx_lock(&dep->qlock);
        obuf = dep->out_queue.first;
        dep->out_queue.first = NULL;
        dep->out_queue.last = NULL;
        ASSERT(!dep->tmp_out_queue.first);
        ASSERT(!dep->finalized_out_queue.first);
        resume_procs = get_suspended_on_de(dep, ERTS_DE_QFLGS_ALL);
	erts_mtx_unlock(&dep->qlock);
        erts_atomic32_set_relb(&dep->notify, 0);
	erts_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
	dep->send = NULL;

        erts_set_dist_entry_not_connected(dep);
	erts_de_rwunlock(dep);

        schedule_con_monitor_link_seq_cleanup(
            NULL, mld, NULL, THE_NON_VALUE,
            THE_NON_VALUE, THE_NON_VALUE);

        if (resume_procs) {
            int resumed = erts_resume_processes(resume_procs);
            reds += resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
        }

	delete_cache(cache);
	free_de_out_queues(dep, obuf);
        return reds;
    }
    erts_de_rwunlock(dep);
    return 0;
}

static Sint abort_pending_connection(DistEntry *dep, Uint32 conn_id,
                                     int *was_connected_p)
{
    erts_de_rwlock(dep);
    if (dep->connection_id == conn_id)
        return erts_abort_pending_connection_rwunlock(dep, was_connected_p);
    erts_de_rwunlock(dep);
    if (was_connected_p)
        *was_connected_p = 0;
    return 0;
}

BIF_RETTYPE erts_internal_abort_pending_connection_2(BIF_ALIST_2)
{
    DistEntry* dep;
    Uint32 conn_id;
    Sint reds;
    int was_connected;

    if (is_not_atom(BIF_ARG_1))
        BIF_ERROR(BIF_P, BADARG);
    dep = erts_dhandle_to_dist_entry(BIF_ARG_2, &conn_id);
    if (!dep || dep != erts_find_dist_entry(BIF_ARG_1)
        || dep == erts_this_dist_entry) {
        BIF_ERROR(BIF_P, BADARG);
    }

    reds = abort_pending_connection(dep, conn_id, &was_connected);
    BUMP_REDS(BIF_P, reds);
    BIF_RET(was_connected ? am_false : am_true);
}

int erts_auto_connect(DistEntry* dep, Process *proc, ErtsProcLocks proc_locks)
{
    erts_de_rwlock(dep);
    if (dep->state != ERTS_DE_STATE_IDLE) {
        erts_de_rwunlock(dep);
    }
    else {
        Process* net_kernel;
        ErtsProcLocks nk_locks = ERTS_PROC_LOCK_MSGQ;
        Eterm *hp;
        ErlOffHeap *ohp;
        ErtsMessage *mp;
        Eterm msg, dhandle;
        Uint32 conn_id;

        erts_set_dist_entry_pending(dep);
        conn_id = dep->connection_id;
        erts_de_rwunlock(dep);

        net_kernel = erts_whereis_process(proc, proc_locks,
                                          am_net_kernel, nk_locks, 0);
        if (!net_kernel) {
            abort_pending_connection(dep, conn_id, NULL);
            return 0;
        }

        if (proc == net_kernel)
            nk_locks |= ERTS_PROC_LOCK_MAIN;

        /*
         * Send {auto_connect, Node, DHandle} to net_kernel
         */
        mp = erts_alloc_message_heap(net_kernel, &nk_locks,
                                     4 + ERTS_DHANDLE_SIZE,
                                     &hp, &ohp);
        dhandle = erts_build_dhandle(&hp, ohp, dep, conn_id);
        msg = TUPLE3(hp, am_auto_connect, dep->sysname, dhandle);
        ERL_MESSAGE_TOKEN(mp) = am_undefined;
        erts_queue_proc_message(proc, net_kernel, nk_locks, mp, msg);

        if (proc == net_kernel)
            nk_locks &= ~ERTS_PROC_LOCK_MAIN;

        erts_proc_unlock(net_kernel, nk_locks);
    }

    return 1;
}

static BIF_RETTYPE spawn_request_yield_3(BIF_ALIST_3)
{
    Binary* bin = erts_magic_ref2bin(BIF_ARG_1);
    ErtsDSigSendContext *ctx = (ErtsDSigSendContext*) ERTS_MAGIC_BIN_DATA(bin);
    Sint initial_reds = (Sint) (ERTS_BIF_REDS_LEFT(BIF_P) * TERM_TO_BINARY_LOOP_FACTOR);
    int code;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == erts_dsend_context_dtor);

    ctx->reds = initial_reds;
    code = erts_dsig_send(ctx);

    switch (code)  {
    case ERTS_DSIG_SEND_OK:
        erts_set_gc_state(BIF_P, 1);
        BIF_RET(BIF_ARG_2);
            
    case ERTS_DSIG_SEND_YIELD:
        erts_set_gc_state(BIF_P, 1);
        ERTS_BIF_YIELD_RETURN(BIF_P, BIF_ARG_2);

    case ERTS_DSIG_SEND_CONTINUE: {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP3(&spawn_request_yield_export, BIF_P,
                  BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    case ERTS_DSIG_SEND_TOO_LRG: {
        ErtsMonitor *mon;
        ErtsMonitorData *mdp;
        Eterm ref;
        
        erts_set_gc_state(BIF_P, 1);

        if (is_internal_ordinary_ref(BIF_ARG_2))
            ref = BIF_ARG_2;
        else {
            Eterm *tp;
            ASSERT(is_tuple_arity(BIF_ARG_2, 2));
            tp = tuple_val(BIF_ARG_2);
            ref = tp[1];
            ASSERT(is_internal_ordinary_ref(ref));
        }
        
        mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P), ref);
        ASSERT(mon);
        erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), mon);
        mdp = erts_monitor_to_data(mon);
        if (erts_monitor_dist_delete(&mdp->u.target))
            erts_monitor_release_both(mdp);
        else
            erts_monitor_release(mon);

        erts_send_local_spawn_reply(BIF_P, ERTS_PROC_LOCK_MAIN, NULL,
                                    BIF_ARG_3, ref, am_system_limit,
                                    am_undefined);
        BIF_RET(BIF_ARG_2);
    }
        
    default:
        ERTS_INTERNAL_ERROR("Invalid spawn request result");
        BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
    }
}

BIF_RETTYPE erts_internal_dist_spawn_request_4(BIF_ALIST_4)
{
    BIF_RETTYPE ret_val;
    Eterm node = BIF_ARG_1;
    Eterm mfa = BIF_ARG_2;
    Eterm opts = BIF_ARG_3;
    Eterm tag = am_spawn_reply;
    Eterm monitor_tag = THE_NON_VALUE;
    Eterm mod, func, alist, new_opts, error, ref,
        ok_result;
    Uint nargs, nopts, rm_opts, rebuild_opts, add_monitor, add_link;
    DistEntry *dep = NULL;
    Eterm list;
    ErtsDSigSendContext ctx;
    int code;
    Uint32 monitor_oflags = 0, monitor_opts_oflags = 0;

    add_monitor = 0;
    add_link = 0;
    ok_result = THE_NON_VALUE;
    
    if (!is_atom(node))
        goto badarg;
    dep = erts_find_or_insert_dist_entry(node);
    if (dep == erts_this_dist_entry)
        goto badarg;
    if (!is_tuple_arity(mfa, 3))
        goto badarg;
    else {
        Eterm *tp = tuple_val(mfa);
        mod = tp[1];
        func = tp[2];
        alist = tp[3];
        if (!is_atom(mod))
            goto badarg;
        if (!is_atom(func))
            goto badarg;
        nargs = 0;
        list = alist;
        while (is_list(list)) {
            Eterm *cp = list_val(list);
            list = CDR(cp);
            nargs++;
        }
        if (!is_nil(list))
            goto badarg;
    }
    
    new_opts = list = opts;
    nopts = 0;
    rm_opts = 0;
    rebuild_opts = 0;
    
    while (is_list(list)) {
        Eterm *cp = list_val(list);
        Eterm car = CAR(cp);
        list = CDR(cp);
        nopts++;
        switch (car) {
        case am_link:
            monitor_oflags |= ERTS_ML_FLG_SPAWN_LINK;
            break;
        case am_monitor:
            monitor_oflags |= ERTS_ML_FLG_SPAWN_MONITOR;
            break;
        default:
            if (is_tuple_arity(car, 2)) {
                Eterm *tp = tuple_val(car);
                switch (tp[1]) {
                    
                case am_reply_tag:
                    tag = tp[2];

                    if (0) {
                    case am_reply:
                        switch (tp[2]) {
                        case am_error_only:
                            monitor_oflags |= ERTS_ML_FLG_SPAWN_NO_SMSG;
                            monitor_oflags &= ~ERTS_ML_FLG_SPAWN_NO_EMSG;
                            break;
                        case am_success_only:
                            monitor_oflags &= ~ERTS_ML_FLG_SPAWN_NO_SMSG;
                            monitor_oflags |= ERTS_ML_FLG_SPAWN_NO_EMSG;
                            break;
                        case am_no:
                            monitor_oflags |= (ERTS_ML_FLG_SPAWN_NO_SMSG
                                               | ERTS_ML_FLG_SPAWN_NO_EMSG);
                            break;
                        case am_yes:
                            monitor_oflags &= ~(ERTS_ML_FLG_SPAWN_NO_SMSG
                                                | ERTS_ML_FLG_SPAWN_NO_EMSG);
                            break;
                        default:
                            if (BIF_ARG_4 != am_spawn_request)
                                goto badarg;
                            ok_result = ref = erts_make_ref(BIF_P);
                            goto badopt;
                        }
                    }

                    if (BIF_ARG_4 != am_spawn_request)
                        goto badarg;

                    if (0) {
                    case am_monitor:
                        monitor_opts_oflags = erts_monitor_opts(tp[2],
                                                                &monitor_tag);
                        if (monitor_opts_oflags == (Uint32) ~0) {
                            if (BIF_ARG_4 != am_spawn_request)
                                goto badarg;
                            ok_result = ref = erts_make_ref(BIF_P);
                            goto badopt;
                        }
                        monitor_opts_oflags |= ERTS_ML_FLG_SPAWN_MONITOR;
                        add_monitor = 1;
                    }

                    if (0) {
                        Uint32 link_opts_oflags;
                    case am_link:
                        link_opts_oflags = erts_link_opts(tp[2], NULL);
                        if (link_opts_oflags == (Uint32) ~0) {
                            if (BIF_ARG_4 != am_spawn_request)
                                goto badarg;
                            ok_result = ref = erts_make_ref(BIF_P);
                            goto badopt;
                        }
                        monitor_oflags |= ERTS_ML_FLG_SPAWN_LINK;
                        if (link_opts_oflags & ERTS_ML_FLG_PRIO_ML)
                            monitor_oflags |= ERTS_ML_FLG_SPAWN_LINK_PRIO;
                        add_link = 1;
                    }

                    rm_opts++;
                    new_opts = list;
                    rebuild_opts = nopts - rm_opts + add_monitor + add_link;
                    break;
                    
                default:
                    break;
                }
            }
            break;
        }
    }
    if (!is_nil(list))
        goto badarg;

    monitor_oflags |= monitor_opts_oflags;

    if (rm_opts) {
        /*
         * If no 'rebuild_opts', all options to drop were in
         * the head of the 'opts' list. 'new_opts' now contain
         * the tail of original option list without the dropped
         * options.
         */
        if (rebuild_opts) {
#ifdef DEBUG
            Eterm reusable_tail, *hp_start;
#endif
            Uint rm_cnt;
            Eterm *hp, *prev_cp;
            /*
             * Remove 'reply_tag' option in option list.
             * This options are mixed with other options.
             *
             * We build the list backwards and reuse tail
             * without options to remove, if such exist.
             */

            hp = HAlloc(BIF_P, 2*rebuild_opts);

#ifdef DEBUG
            hp_start = hp;
            reusable_tail = new_opts;
#endif

            hp += 2*(rebuild_opts - 1);
            new_opts = make_list(hp);
            prev_cp = NULL;
            list = opts;
            rm_cnt = 0;

            if (add_link) {
#ifdef DEBUG
                rebuild_opts--;
#endif
                CAR(hp) = am_link;
                prev_cp = hp;
                hp -= 2;
                CDR(prev_cp) = make_list(hp);
            }
            if (add_monitor) {
#ifdef DEBUG
                rebuild_opts--;
#endif
                CAR(hp) = am_monitor;
                prev_cp = hp;
                hp -= 2;
                CDR(prev_cp) = make_list(hp);
            }
            while (is_list(list)) {
                Eterm *cp = list_val(list);
                Eterm car = CAR(cp);
                list = CDR(cp);
                if (is_tuple_arity(car, 2)) {
                    Eterm *tp = tuple_val(car);
                    if (am_reply_tag == tp[1]
                        || am_reply == tp[1]
                        || am_monitor == tp[1]
                        || am_link == tp[1]) {
                        rm_cnt++;
                        /* skip option */
                        if (rm_cnt == rm_opts) {
                            ASSERT(prev_cp);
                            ASSERT(list == reusable_tail);
                            CDR(prev_cp) = list;
                            break; /* last option to skip */
                        }
                        continue;
                    }
                }
#ifdef DEBUG
                rebuild_opts--;
#endif
                CAR(hp) = car;
                prev_cp = hp;
                hp -= 2;
                CDR(prev_cp) = make_list(hp);
            }
            ASSERT(hp == hp_start - 2);
            ASSERT(rebuild_opts == 0);

        }
        
        opts = new_opts;
    }

    /* Arguments checked; do it... */
    if (monitor_oflags & ERTS_ML_STATE_ALIAS_MASK)
        ref = erts_make_pid_ref(BIF_P);
    else
        ref = erts_make_ref(BIF_P);
    if (BIF_ARG_4 == am_spawn_request)
        ok_result = ref;
    else {
        Eterm *hp = HAlloc(BIF_P, 3);
        Eterm spawns_monitor = ((monitor_oflags & ERTS_ML_FLG_SPAWN_MONITOR)
                      ? am_true : am_false);
        ASSERT(BIF_ARG_4 == am_spawn_opt);
        ok_result = TUPLE2(hp, ref, spawns_monitor);
    }

    code = erts_dsig_prepare(&ctx, dep,
                             BIF_P, ERTS_PROC_LOCK_MAIN,
                             ERTS_DSP_RLOCK, 0, 0, 1);
    switch (code) {
    case ERTS_DSIG_PREP_NOT_ALIVE:
    case ERTS_DSIG_PREP_NOT_CONNECTED:        
        goto noconnection;
        
    case ERTS_DSIG_PREP_CONNECTED:
        if (!(dep->dflags & DFLAG_SPAWN)) {
            erts_de_runlock(dep);
            goto notsup;
        }
        /* Fall through... */
    case ERTS_DSIG_PREP_PENDING: {
        int inserted;
        ErtsMonitorData *mdp;
        Eterm nargs_term, mfna, *hp;

        if (IS_USMALL(0, nargs)) {
            hp = HAlloc(BIF_P, 4);
	    nargs_term = make_small(nargs);
        }
        else {
            hp = HAlloc(BIF_P, 4+BIG_UINT_HEAP_SIZE);
	    nargs_term = uint_to_big(nargs, hp);
	    hp += BIG_UINT_HEAP_SIZE;
        }

        mfna = TUPLE3(hp, mod, func, nargs_term);

        mdp = erts_monitor_create(ERTS_MON_TYPE_DIST_PROC, ref,
                                  BIF_P->common.id, am_pending,
                                  tag, monitor_tag);
        mdp->origin.flags |= monitor_oflags;
            
        erts_monitor_tree_insert(&ERTS_P_MONITORS(BIF_P),
                                 &mdp->origin);
        inserted = erts_monitor_dist_insert(&mdp->u.target, dep->mld);
        ASSERT(inserted); (void)inserted;
        
        erts_de_runlock(dep);

        if (monitor_oflags & ERTS_ML_FLG_PRIO_ML)
            erts_proc_sig_prio_item_added(BIF_P, ERTS_PRIO_ITEM_TYPE_MONITOR);

        ctx.reds = (Sint) (ERTS_BIF_REDS_LEFT(BIF_P) * TERM_TO_BINARY_LOOP_FACTOR);

        code = dsig_send_spawn_request(&ctx, ref, BIF_P->common.id,
                                       BIF_P->group_leader, mfna,
                                       alist, opts);
        switch (code)  {
        case ERTS_DSIG_SEND_OK:
            ERTS_BIF_PREP_RET(ret_val, ok_result);
            break;
            
        case ERTS_DSIG_SEND_YIELD:
            ERTS_BIF_PREP_YIELD_RETURN(ret_val, BIF_P, ok_result);
            break;

        case ERTS_DSIG_SEND_CONTINUE: {
            Eterm ctx_term;
            /* Keep dist entry alive over trap... */
            ctx.deref_dep = 1;
            dep = NULL;

            erts_set_gc_state(BIF_P, 0);
            ctx_term = erts_dsend_export_trap_context(BIF_P, &ctx);
            BUMP_ALL_REDS(BIF_P);
            ERTS_BIF_PREP_TRAP3(ret_val, &spawn_request_yield_export,
                                BIF_P, ctx_term, ok_result, tag);
            break;
        }

        case ERTS_DSIG_SEND_TOO_LRG: {
            ErtsMonitor *mon;
            ErtsMonitorData *mdp;
            
            mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(BIF_P), ref);
            ASSERT(mon);
            erts_monitor_tree_delete(&ERTS_P_MONITORS(BIF_P), mon);
            mdp = erts_monitor_to_data(mon);
            if (erts_monitor_dist_delete(&mdp->u.target))
                erts_monitor_release_both(mdp);
            else
                erts_monitor_release(mon);
        
            goto system_limit;
        }
            
        default:
            ERTS_INTERNAL_ERROR("Invalid spawn request result");
            ERTS_BIF_PREP_RET(ret_val, am_internal_error);
        }
        break;
    }   
    default:
        ERTS_INTERNAL_ERROR("Invalid dsig prepare result");
        ERTS_BIF_PREP_RET(ret_val, am_internal_error);
        break;
    }
    
do_return:
    
    if (dep)
        erts_deref_dist_entry(dep);

    return ret_val;

badarg:
    ERTS_BIF_PREP_RET(ret_val, am_badarg);
    goto do_return;

system_limit:
    error = am_system_limit;
    goto send_error;
noconnection:
    error = am_noconnection;
    goto send_error;
notsup:
    error = am_notsup;
    goto send_error;
badopt:
    error = am_badopt;
    /* fall through... */
send_error:
    ASSERT(is_value(ok_result));
    if (!(monitor_oflags & ERTS_ML_FLG_SPAWN_NO_EMSG))
        erts_send_local_spawn_reply(BIF_P, ERTS_PROC_LOCK_MAIN, NULL,
                                    tag, ref, error, am_undefined);
    ERTS_BIF_PREP_RET(ret_val, ok_result);
    goto do_return;
}


/**********************************************************************/
/* node(Object) -> Node */

BIF_RETTYPE node_1(BIF_ALIST_1)
{
    /* NOTE: The JIT has its own implementation of this BIF. */
    if (is_not_node_container(BIF_ARG_1))
      BIF_ERROR(BIF_P, BADARG);
    BIF_RET(node_container_node_name(BIF_ARG_1));
}

/**********************************************************************/
/* node() -> Node */

BIF_RETTYPE node_0(BIF_ALIST_0)
{
    BIF_RET(erts_this_dist_entry->sysname);
}

/**********************************************************************/
/* nodes() -> [ Node ] */

static BIF_RETTYPE nodes(Process *c_p, Eterm node_types, Eterm options);

BIF_RETTYPE nodes_0(BIF_ALIST_0)
{
    return nodes(BIF_P, am_visible, THE_NON_VALUE);
}

BIF_RETTYPE nodes_1(BIF_ALIST_1)
{
    return nodes(BIF_P, BIF_ARG_1, THE_NON_VALUE);
}

BIF_RETTYPE nodes_2(BIF_ALIST_2)
{
    return nodes(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

typedef struct {
    Eterm name;
    Eterm type;
    Uint32 cid;
} ErtsNodeInfo;

static BIF_RETTYPE
nodes(Process *c_p, Eterm node_types, Eterm options)
{
    BIF_RETTYPE ret_val;
    ErtsNodeInfo *eni, *eni_start = NULL, *eni_end;
    Eterm result;
    Uint length;
    int not_connected = 0;
    int visible = 0;
    int hidden = 0;
    int this = 0;
    int node_type = 0;
    int connection_id = 0;
    int xinfo = 0;
    Eterm tmp_heap[2]; /* For one cons-cell */
    DistEntry *dep;
    Eterm arg_list;

    if (is_atom(node_types))
        arg_list = CONS(&tmp_heap[0], node_types, NIL);
    else
        arg_list = node_types;

    while (is_list(arg_list)) {
      switch(CAR(list_val(arg_list))) {
      case am_visible:   visible = 1;                                 break;
      case am_hidden:    hidden = 1;                                  break;
      case am_known:     visible = hidden = not_connected = this = 1; break;
      case am_this:      this = 1;                                    break;
      case am_connected: visible = hidden = 1;                        break;
      default:           goto badarg;                                 break;
      }
      arg_list = CDR(list_val(arg_list));
    }

    if (is_not_nil(arg_list)) {
	goto badarg;
    }

    if (is_value(options)) {
        if (is_not_map(options)) {
            goto badarg;
        }
        else {
            Sint no_opts = 0;
            const Eterm *conn_idp = erts_maps_get(am_connection_id, options);
            const Eterm *node_typep = erts_maps_get(am_node_type, options);
            if (conn_idp) {
                switch (*conn_idp) {
                case am_true: connection_id = !0; break;
                case am_false: connection_id = 0; break;
                default: goto badarg;
                }
                no_opts++;
            }
            if (node_typep) {
                switch (*node_typep) {
                case am_true: node_type = !0; break;
                case am_false: node_type = 0; break;
                default: goto badarg;
                }
                no_opts++;
            }
            if (no_opts != erts_map_size(options))
                goto badarg; /* got invalid options... */
            xinfo = !0;
        }
    }

    length = 0;

    erts_rwmtx_rlock(&erts_dist_table_rwmtx);

    ASSERT(erts_no_of_not_connected_dist_entries > 0);
    ASSERT(erts_no_of_hidden_dist_entries >= 0);
    ASSERT(erts_no_of_pending_dist_entries >= 0);
    ASSERT(erts_no_of_visible_dist_entries >= 0);
    if(not_connected)
      length += ((erts_no_of_not_connected_dist_entries - 1)
                 + erts_no_of_pending_dist_entries);
    if(hidden)
      length += erts_no_of_hidden_dist_entries;
    if(visible)
      length += erts_no_of_visible_dist_entries;
    if(this)
      length++;

    result = NIL;

    if (length == 0) {
	erts_rwmtx_runlock(&erts_dist_table_rwmtx);
        ERTS_BIF_PREP_RET(ret_val, NIL);
        return ret_val;
    }

    eni_start = eni = erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsNodeInfo)*length);

    if (this) {
        eni->name = erts_this_dist_entry->sysname;
        eni->type = am_this;
        eni->cid = ~((Uint32) 0);
        eni++;
    }

    if (visible) {
        for (dep = erts_visible_dist_entries; dep; dep = dep->next) {
            eni->name = dep->sysname;
            eni->type = am_visible;
            eni->cid = dep->connection_id;
            ASSERT(eni->cid >= 0);
            eni++;
        }
    }

    if (hidden) {
        for (dep = erts_hidden_dist_entries; dep; dep = dep->next) {
            eni->name = dep->sysname;
            eni->type = am_hidden;
            eni->cid = dep->connection_id;
            eni++;
        }
    }

    if (not_connected) {
        for (dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
            if (dep != erts_this_dist_entry) {
                eni->name = dep->sysname;
                eni->type = am_known;
                eni->cid =  ~((Uint32) 0);
                eni++;
            }
        }
        for (dep = erts_pending_dist_entries; dep; dep = dep->next) {
            eni->name = dep->sysname;
            eni->type = am_known;
            eni->cid =  ~((Uint32) 0);
            eni++;
        }
    }

    erts_rwmtx_runlock(&erts_dist_table_rwmtx);

    eni_end = eni;

    result = NIL;
    if (!xinfo) {
        Eterm *hp = HAlloc(c_p, 2*length);
        for (eni = eni_start; eni < eni_end; eni++) {
            result = CONS(hp, eni->name, result);
            hp += 2;
        }
    }
    else {
        Eterm ks[2], *hp;
        Uint map_size = 0, el_xtra, xtra;
        ErtsHeapFactory hfact;

        erts_factory_proc_init(&hfact, c_p);

        if (connection_id) {
            ks[map_size++] = am_connection_id;
        }
        if (node_type) {
            ks[map_size++] = am_node_type;
        }

        el_xtra = 3 + 2 + MAP_HEADER_FLATMAP_SZ + map_size;
        xtra = length*el_xtra;
        
        for (eni = eni_start; eni < eni_end; eni++) {
            Eterm vs[2], info_map, tuple;
            map_size = 0;
            if (connection_id) {
                Eterm cid;
                if (eni->cid == ~((Uint32) 0))
                    cid = am_undefined;
                else if (IS_USMALL(0, (Uint) eni->cid))
                    cid = make_small((Uint) eni->cid);
                else {
                    hp = erts_produce_heap(&hfact, BIG_UINT_HEAP_SIZE, xtra);
                    cid = uint_to_big((Uint) eni->cid, hp);
                }
                vs[map_size++] = cid;
            }
            if (node_type) {
                vs[map_size++] = eni->type;
            }

            info_map = erts_map_from_ks_and_vs(&hfact, ks, vs, map_size);
            ASSERT(is_value(info_map));

            hp = erts_produce_heap(&hfact, 3+2, xtra);

            tuple = TUPLE2(hp, eni->name, info_map);
            hp += 3;
            result = CONS(hp, tuple, result);
            xtra -= el_xtra;
        }

        erts_factory_close(&hfact);
    }

    erts_free(ERTS_ALC_T_TMP, (void *) eni_start);

    if (length > 10) {
        Uint reds = length / 10;
        BUMP_REDS(c_p, reds);
    }
    
    ERTS_BIF_PREP_RET(ret_val, result);
    return ret_val;

badarg:
    ERTS_BIF_PREP_ERROR(ret_val, c_p, BADARG);
    return ret_val;
}

/**********************************************************************/
/* erlang:monitor_node(Node, Bool, Options) -> Bool */

static BIF_RETTYPE
monitor_node(Process* p, Eterm Node, Eterm Bool, Eterm Options)
{
    BIF_RETTYPE ret;
    DistEntry *dep = NULL;
    Eterm l;
    int async_connect = 1;

    p->fvalue = am_badopt;
    for (l = Options; l != NIL && is_list(l); l = CDR(list_val(l))) {
	Eterm t = CAR(list_val(l));
	if (t == am_allow_passive_connect) {
	    /*
	     * Handle this horrible feature by falling back on old synchronous
	     * auto-connect (if needed)
	     */
	    async_connect = 0;
	} else {
	    BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
	}
    }
    if (l != NIL) {
	BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }
    if (l != NIL)
        goto badarg;

    if (is_not_atom(Node))
        goto badarg;

    if (Node != am_Noname && !erts_is_this_node_alive()) {
        ERTS_BIF_PREP_ERROR(ret, p, EXC_NOTALIVE);
        goto do_return;
    }

    switch (Bool) {

    case am_false: {
        ErtsMonitor *mon;
        /*
         * Before OTP-21, monitor_node(Node, false) triggered
         * auto-connect and a 'nodedown' message if that failed.
         * Now it's a simple no-op which feels more reasonable.
         */
        mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(p), Node);
        if (mon) {
            ErtsMonitorDataExtended *mdep;
            ASSERT(erts_monitor_is_origin(mon));

            mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);

            ASSERT((mdep->u.refc > 0));
            if (--mdep->u.refc == 0) {
                if (!mdep->uptr.node_monitors)
                    erts_monitor_tree_delete(&ERTS_P_MONITORS(p), mon);
                else {
                    ErtsMonitor *sub_mon;
                    ErtsMonitorDataExtended *sub_mdep;
                    sub_mon = erts_monitor_list_last(mdep->uptr.node_monitors);
                    erts_monitor_list_delete(&mdep->uptr.node_monitors, sub_mon);
                    sub_mon->flags &= ~ERTS_ML_FLG_IN_SUBTABLE;
                    sub_mdep = ((ErtsMonitorDataExtended *)
                                erts_monitor_to_data(sub_mon));
                    sub_mdep->uptr.node_monitors = mdep->uptr.node_monitors;
                    mdep->uptr.node_monitors = NULL;
                    erts_monitor_tree_replace(&ERTS_P_MONITORS(p), mon, sub_mon);
                }
                if (erts_monitor_dist_delete(&mdep->md.u.target))
                    erts_monitor_release_both((ErtsMonitorData *) mdep);
                else
                    erts_monitor_release(mon);
            }
        }
        break;
    }

    case am_true: {
        ErtsDSigSendContext ctx;
        ctx.node = Node;

        dep = erts_find_or_insert_dist_entry(Node);
        if (dep == erts_this_dist_entry)
            break;

        switch (erts_dsig_prepare(&ctx, dep, p,
                                  ERTS_PROC_LOCK_MAIN,
                                  ERTS_DSP_RLOCK, 0, 0, async_connect)) {
        case ERTS_DSIG_PREP_NOT_ALIVE:
	case ERTS_DSIG_PREP_NOT_CONNECTED:
	    /* Trap to either send 'nodedown' or do passive connection attempt */
            goto do_trap;
	case ERTS_DSIG_PREP_PENDING:
	    if (!async_connect) {
		/*
		 * Pending connection may fail, so we must trap
		 * to ensure passive connection attempt
		 */
		erts_de_runlock(dep);
		goto do_trap;
	    }
	    /*fall through*/
        case ERTS_DSIG_PREP_CONNECTED: {
            ErtsMonitor *mon;
            ErtsMonitorDataExtended *mdep;
            int created;

            mon = erts_monitor_tree_lookup_create(&ERTS_P_MONITORS(p),
                                                  &created,
                                                  ERTS_MON_TYPE_NODE,
                                                  p->common.id,
                                                  Node);
            mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
            if (created) {
                int inserted =
                    erts_monitor_dist_insert(&mdep->md.u.target, dep->mld);
                ASSERT(inserted); (void)inserted;
                ASSERT(mdep->dist->connection_id == dep->connection_id);
            }
            else if (mdep->dist->connection_id != dep->connection_id) {
                ErtsMonitorDataExtended *mdep2;
                ErtsMonitor *mon2;
                int inserted;
                mdep2 = ((ErtsMonitorDataExtended *)
                         erts_monitor_create(ERTS_MON_TYPE_NODE, NIL,
                                             p->common.id, Node, NIL,
                                             THE_NON_VALUE));
                mon2 = &mdep2->md.origin;
                inserted =
                    erts_monitor_dist_insert(&mdep->md.u.target, dep->mld);
                ASSERT(inserted); (void)inserted;
                ASSERT(mdep2->dist->connection_id == dep->connection_id);

                mdep2->uptr.node_monitors = mdep->uptr.node_monitors;
                mdep->uptr.node_monitors = NULL;
                erts_monitor_tree_replace(&ERTS_P_MONITORS(p), mon, mon2);
                erts_monitor_list_insert(&mdep2->uptr.node_monitors, mon);
                mon->flags |= ERTS_ML_FLG_IN_SUBTABLE;
                mdep = mdep2;
            }

            mdep->u.refc++;

            break;
        }

        default:
            ERTS_ASSERT(! "Invalid dsig prepare result");
        }

        erts_de_runlock(dep);

        break;
    }

    default:
        goto badarg;
    }

    ERTS_BIF_PREP_RET(ret, am_true);

do_return:

    if (dep)
        erts_deref_dist_entry(dep);

    return ret;

do_trap:
    ERTS_BIF_PREP_TRAP3(ret, dmonitor_node_trap, p, Node, Bool, Options);
    goto do_return;

badarg:
    ERTS_BIF_PREP_ERROR(ret, p, BADARG);
    goto do_return;
}

BIF_RETTYPE monitor_node_3(BIF_ALIST_3)
{
    BIF_RET(monitor_node(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3));
}


/* monitor_node(Node, Bool) -> Bool */

BIF_RETTYPE monitor_node_2(BIF_ALIST_2)
{
    BIF_RET(monitor_node(BIF_P, BIF_ARG_1, BIF_ARG_2, NIL));
}

BIF_RETTYPE net_kernel_dflag_unicode_io_1(BIF_ALIST_1)
{
    DistEntry *de;
    Uint64 f;
    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    de = pid_dist_entry(BIF_ARG_1);
    ASSERT(de != NULL);
    if (de == erts_this_dist_entry) {
	BIF_RET(am_true);
    }
    erts_de_rlock(de);
    f = de->dflags;
    erts_de_runlock(de);
    BIF_RET(((f & DFLAG_UNICODE_IO) ? am_true : am_false));
}
    
/*
 * The major part of the implementation of net_kernel:monitor_nodes/[1,2]
 * follows.
 *
 * Currently net_kernel:monitor_nodes/[1,2] calls process_flag/2 which in
 * turn calls erts_monitor_nodes(). If the process_flag() call fails (with
 * badarg), the code in net_kernel determines what type of error to return.
 * This in order to simplify the task of being backward compatible.
 */

#define ERTS_NODES_MON_OPT_TYPE_VISIBLE		(((Uint16) 1) << 0)
#define ERTS_NODES_MON_OPT_TYPE_HIDDEN		(((Uint16) 1) << 1)
#define ERTS_NODES_MON_OPT_DOWN_REASON		(((Uint16) 1) << 2)
#define ERTS_NODES_MON_OPT_INFO_MAP             (((Uint16) 1) << 3)
#define ERTS_NODES_MON_OPT_CONN_ID              (((Uint16) 1) << 4)

#define ERTS_NODES_MON_OPT_TYPES \
  (ERTS_NODES_MON_OPT_TYPE_VISIBLE|ERTS_NODES_MON_OPT_TYPE_HIDDEN)

static erts_mtx_t nodes_monitors_mtx;
static ErtsMonitor *nodes_monitors;
static Uint no_nodes_monitors;

/*
 * Nodes monitors are stored in a double linked list. 'nodes_monitors'
 * points to the beginning of the list and 'nodes_monitors_end' points
 * to the end of the list.
 *
 * There might be more than one entry per process in the list. If so,
 * they are located in sequence. The 'nodes_monitors' field of the
 * process struct refers to the first element in the sequence
 * corresponding to the process in question.
 */

static void
init_nodes_monitors(void)
{
    erts_mtx_init(&nodes_monitors_mtx, "nodes_monitors", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    nodes_monitors = NULL;
    no_nodes_monitors = 0;
}

Eterm
erts_monitor_nodes(Process *c_p, Eterm on, Eterm options)
{
    Eterm key, old_value;
    Uint opts = (Uint) ERTS_NODES_MON_OPT_INFO_MAP;

    ASSERT(c_p);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    if (on != am_true && on != am_false)
	return THE_NON_VALUE;

    if (is_nil(options)) {
        opts &= ~ERTS_NODES_MON_OPT_INFO_MAP;
    }
    else if (is_not_map(options)) {
        return THE_NON_VALUE;
    }
    else {
        Sint no_opts = 0;
        const Eterm *l = erts_maps_get(am_list, options);
        const Eterm *cid = erts_maps_get(am_connection_id, options);
        const Eterm *nt = erts_maps_get(am_node_type, options);
        const Eterm *nr = erts_maps_get(am_nodedown_reason, options);
        if (l) {
            if (*l == am_true) {
                opts &= ~ERTS_NODES_MON_OPT_INFO_MAP;
            }
            else {
                return THE_NON_VALUE;
            }
            no_opts++;
        }
        if (cid) {
            if (*cid == am_true) {
		opts |= ERTS_NODES_MON_OPT_CONN_ID;                
            }
            else if (*cid != am_false) {
                return THE_NON_VALUE;
            }
            no_opts++;
        }
        if (nt) {
            switch (*nt) {
            case am_visible:
                opts |= ERTS_NODES_MON_OPT_TYPE_VISIBLE;
                break;
            case am_hidden:
                opts |= ERTS_NODES_MON_OPT_TYPE_HIDDEN;
                break;
            case am_all:
                opts |= ERTS_NODES_MON_OPT_TYPES;
                break;
            default:
                return THE_NON_VALUE;
            }
            no_opts++;
        }
        if (nr) {
            if (*nr == am_true) {
		opts |= ERTS_NODES_MON_OPT_DOWN_REASON;
            }
            else if (*nr != am_false) {
                return THE_NON_VALUE;
            }
            no_opts++;
        }
        if (no_opts != erts_map_size(options))
            return THE_NON_VALUE; /* got invalid options... */
    }

    key = make_small(opts);

    if (on == am_true) {
        ErtsMonitorDataExtended *mdep;
        ErtsMonitor *omon;
        int created;
        omon = erts_monitor_tree_lookup_create(&ERTS_P_MONITORS(c_p),
                                               &created,
                                               ERTS_MON_TYPE_NODES,
                                               c_p->common.id,
                                               key);
        mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(omon);
        if (created) {
            erts_mtx_lock(&nodes_monitors_mtx);
            no_nodes_monitors++;
            erts_monitor_list_insert(&nodes_monitors, &mdep->md.u.target);
            erts_mtx_unlock(&nodes_monitors_mtx);
        }
        old_value = mdep->u.refc;
        mdep->u.refc++;
    }
    else {
        ErtsMonitorDataExtended *mdep;
        ErtsMonitor *omon;
        omon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), key);
        if (!omon)
            old_value = 0;
        else {
            mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(omon);
            old_value = mdep->u.refc;
            ASSERT(mdep->u.refc > 0);
            erts_mtx_lock(&nodes_monitors_mtx);
            ASSERT(no_nodes_monitors > 0);
            no_nodes_monitors--;
            ASSERT(erts_monitor_is_in_table(&mdep->md.u.target));
            erts_monitor_list_delete(&nodes_monitors, &mdep->md.u.target);
            erts_mtx_unlock(&nodes_monitors_mtx);
            erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), omon);
            erts_monitor_release_both((ErtsMonitorData *) mdep);
        }
    }

    return erts_make_integer(old_value, c_p);
}

void
erts_monitor_nodes_delete(ErtsMonitor *omon)
{
    ErtsMonitorData *mdp;

    ASSERT(ERTS_ML_GET_TYPE(omon) == ERTS_MON_TYPE_NODES);
    ASSERT(erts_monitor_is_origin(omon));

    mdp = erts_monitor_to_data(omon);

    erts_mtx_lock(&nodes_monitors_mtx);
    ASSERT(erts_monitor_is_in_table(&mdp->u.target));
    ASSERT(no_nodes_monitors > 0);
    no_nodes_monitors--;
    erts_monitor_list_delete(&nodes_monitors, &mdp->u.target);
    erts_mtx_unlock(&nodes_monitors_mtx);
    erts_monitor_release_both(mdp);
}

typedef struct {
    Eterm pid;
    Eterm options;
} ErtsNodesMonitorData;

typedef struct {
    ErtsNodesMonitorData *nmdp;
    Uint i;
} ErtsNodesMonitorContext;

static int
save_nodes_monitor(ErtsMonitor *mon, void *vctxt, Sint reds)
{
    ErtsNodesMonitorContext *ctxt = vctxt;
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);

    ASSERT(erts_monitor_is_target(mon));
    ASSERT(ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_NODES);

    ctxt->nmdp[ctxt->i].pid = mon->other.item;
    ctxt->nmdp[ctxt->i].options = mdp->origin.other.item;

    ctxt->i++;
    return 1;
}

#define ERTS_MON_NODES_MAX_INFO_LIST_SZ__(MAX_ELEMS)            \
    ((MAX_ELEMS)*(3 /* key/value 2-tuple */ + 2/* cons cell */) \
     + BIG_UINT_HEAP_SIZE /* connection id value */             \
     + 4 /* top 3-tuple */)
#define ERTS_MON_NODES_MAX_INFO_MAP_SZ__(MAX_ELEMS)             \
    ((MAX_ELEMS)*2 /* keys and values */                        \
     + 1 /* key tuple header */ + MAP_HEADER_FLATMAP_SZ /* 3 */ \
     + BIG_UINT_HEAP_SIZE /* connection id value */             \
     + 4 /* top 3-tuple */)
#define ERTS_MON_NODES_MAX_INFO_SZ__(MAX_ELEMS)                 \
    ((ERTS_MON_NODES_MAX_INFO_MAP_SZ__((MAX_ELEMS))             \
      > ERTS_MON_NODES_MAX_INFO_LIST_SZ__((MAX_ELEMS)))         \
     ? ERTS_MON_NODES_MAX_INFO_MAP_SZ__((MAX_ELEMS))            \
     : ERTS_MON_NODES_MAX_INFO_LIST_SZ__((MAX_ELEMS)))
        
static void
send_nodes_mon_msgs(Process *c_p, Eterm what, Eterm node,
                    Uint32 connection_id, Eterm type, Eterm reason)
{
    Uint opts;
    Uint i, no, reason_size;
    ErtsNodesMonitorData def_buf[100];
    ErtsNodesMonitorData *nmdp = &def_buf[0];
    ErtsNodesMonitorContext ctxt;

    ASSERT(is_immed(what));
    ASSERT(is_immed(node));
    ASSERT(is_immed(type));
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_monitor)) {
        DTRACE_CHARBUF(what_str, 12);
        DTRACE_CHARBUF(node_str, 64);
        DTRACE_CHARBUF(type_str, 12);
        DTRACE_CHARBUF(reason_str, 64);

        erts_snprintf(what_str, sizeof(DTRACE_CHARBUF_NAME(what_str)), "%T", what);
        erts_snprintf(node_str, sizeof(DTRACE_CHARBUF_NAME(node_str)), "%T", node);
        erts_snprintf(type_str, sizeof(DTRACE_CHARBUF_NAME(type_str)), "%T", type);
        erts_snprintf(reason_str, sizeof(DTRACE_CHARBUF_NAME(reason_str)), "%T", reason);
        DTRACE5(dist_monitor, erts_this_node_sysname,
                what_str, node_str, type_str, reason_str);
    }
#endif

    ctxt.i = 0;

    reason_size = is_immed(reason) ? 0 : size_object(reason);

    erts_mtx_lock(&nodes_monitors_mtx);
    if (no_nodes_monitors > sizeof(def_buf)/sizeof(def_buf[0]))
        nmdp = erts_alloc(ERTS_ALC_T_TMP,
                          no_nodes_monitors*sizeof(ErtsNodesMonitorData));
    ctxt.nmdp = nmdp;
    erts_monitor_list_foreach(nodes_monitors,
                              save_nodes_monitor,
                              (void *) &ctxt);
    
    ASSERT(ctxt.i == no_nodes_monitors);
    no = no_nodes_monitors;

    erts_mtx_unlock(&nodes_monitors_mtx);

    for (i = 0; i < no; i++) {
        ErtsHeapFactory hfact;
        Eterm tmp_heap[ERTS_MON_NODES_MAX_INFO_SZ__(3/* max info elements */)];
        Eterm *hp, msg;
        Uint hsz;

        ASSERT(is_small(nmdp[i].options));
        opts = (Uint) signed_val(nmdp[i].options);
	if (!opts) {
	    if (type != am_visible)
		continue;
	}
	else {
	    switch (type) {
	    case am_hidden:
		if (!(opts & ERTS_NODES_MON_OPT_TYPE_HIDDEN))
		    continue;
		break;
	    case am_visible:
		if ((opts & ERTS_NODES_MON_OPT_TYPES)
		    && !(opts & ERTS_NODES_MON_OPT_TYPE_VISIBLE))
		    continue;
		break;
	    default:
		erts_exit(ERTS_ABORT_EXIT, "Bad node type found\n");
	    }
	}

        /*
         * tmp_heap[] is sized so there will be room for everything
         * we need assuming no info, a two-tuple info list, or an info
         * flat map is generated. In case there would be a greater heap
         * need this will be taken care of by the heap factory...
         */
        erts_factory_tmp_init(&hfact,
                              &tmp_heap[0],
                              sizeof(tmp_heap)/sizeof(Uint),
                              ERTS_ALC_T_TMP);
        hsz = 0;

        if (!opts) {
            hp = erts_produce_heap(&hfact, 3, 0);
            msg = TUPLE2(hp, what, node);
        }
        else { /* Info list or map... */
            Eterm tup;
            Eterm info;

            if (opts & ERTS_NODES_MON_OPT_INFO_MAP) { /* Info map */
                Uint map_size = 0;
                Eterm ks[3], vs[3];

                if (opts & ERTS_NODES_MON_OPT_CONN_ID) {
                    Eterm cid;
                    if (connection_id == ~((Uint32) 0)) {
                        cid = am_undefined;
                    }
                    else if (IS_USMALL(0, (Uint) connection_id)) {
                        cid = make_small(connection_id);
                    }
                    else {
                        hp = erts_produce_heap(&hfact, BIG_UINT_HEAP_SIZE, 0);
                        cid = uint_to_big(connection_id, hp);
                    }
                    ks[map_size] = am_connection_id;
                    vs[map_size] = cid;
                    map_size++;
                }
                if (opts & (ERTS_NODES_MON_OPT_TYPE_VISIBLE
                            | ERTS_NODES_MON_OPT_TYPE_HIDDEN)) {
                    ks[map_size] = am_node_type;
                    vs[map_size] = type;
                    map_size++;
                }
                if (what == am_nodedown
                    && (opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
                    hsz += reason_size;
                    ks[map_size] = am_nodedown_reason;
                    vs[map_size] = reason;
                    map_size++;
                }

                info = erts_map_from_ks_and_vs(&hfact, ks, vs, map_size);
                ASSERT(is_value(info));
            }
            else { /* Info list */

                info = NIL;
                if (opts & (ERTS_NODES_MON_OPT_TYPE_VISIBLE
                            | ERTS_NODES_MON_OPT_TYPE_HIDDEN)) {
                    hp = erts_produce_heap(&hfact, 3 + 2, 0);
                    tup = TUPLE2(hp, am_node_type, type);
                    hp += 3;
                    info = CONS(hp, tup, info);
                }

                if (what == am_nodedown
                    && (opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
                    hp = erts_produce_heap(&hfact, 3 + 2, 0);
                    hsz += reason_size;
                    tup = TUPLE2(hp, am_nodedown_reason, reason);
                    hp += 3;
                    info = CONS(hp, tup, info);
                }

                if (opts & ERTS_NODES_MON_OPT_CONN_ID) {
                    Eterm cid;
                    if (connection_id == ~((Uint32) 0)) {
                        cid = am_undefined;
                    }
                    else if (IS_USMALL(0, (Uint) connection_id)) {
                        cid = make_small(connection_id);
                    }
                    else {
                        hp = erts_produce_heap(&hfact, BIG_UINT_HEAP_SIZE, 0);
                        cid = uint_to_big(connection_id, hp);
                    }
                    hp = erts_produce_heap(&hfact, 3 + 2, 0);
                    tup = TUPLE2(hp, am_connection_id, cid);
                    hp += 3;
                    info = CONS(hp, tup, info);
                }
            }
            
            hp = erts_produce_heap(&hfact, 4, 0);
            msg = TUPLE3(hp, what, node, info);
        }


        hsz += hfact.hp - hfact.hp_start;
        if (hfact.heap_frags) {
            ErlHeapFragment *bp;
            for (bp = hfact.heap_frags; bp; bp = bp->next)
                hsz += bp->used_size;
        }

        erts_proc_sig_send_monitor_nodes_msg(nmdp[i].options, nmdp[i].pid,
                                             msg, hsz);

        erts_factory_close(&hfact);
    }

    if (nmdp != &def_buf[0])
        erts_free(ERTS_ALC_T_TMP, nmdp);
}
                           

typedef struct {
    Eterm **hpp;
    Uint *szp;
    Eterm res;
} ErtsNodesMonitorInfoContext;


static int
nodes_monitor_info(ErtsMonitor *mon, void *vctxt, Sint reds)
{
    ErtsMonitorDataExtended *mdep;
    ErtsNodesMonitorInfoContext *ctxt = vctxt;
    Uint no, i, opts, *szp;
    Eterm **hpp, res;

    hpp = ctxt->hpp;
    szp = ctxt->szp;
    res = ctxt->res;

    ASSERT(erts_monitor_is_target(mon));
    ASSERT(ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_NODES);
    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
    no = mdep->u.refc;

    ASSERT(is_small(mdep->md.origin.other.item));
    opts = (Uint) signed_val(mdep->md.origin.other.item);

    for (i = 0; i < no; i++) {
        Eterm olist = NIL;
        if (opts & ERTS_NODES_MON_OPT_TYPES) {
            Eterm type;
            switch (opts & ERTS_NODES_MON_OPT_TYPES) {
            case ERTS_NODES_MON_OPT_TYPES:        type = am_all;     break;
            case ERTS_NODES_MON_OPT_TYPE_VISIBLE: type = am_visible; break;
            case ERTS_NODES_MON_OPT_TYPE_HIDDEN:  type = am_hidden;  break;
            default: erts_exit(ERTS_ABORT_EXIT, "Bad node type found\n");
            }
            olist = erts_bld_cons(hpp, szp, 
                                  erts_bld_tuple(hpp, szp, 2,
                                                 am_node_type,
                                                 type),
                                  olist);
        }
        if (opts & ERTS_NODES_MON_OPT_DOWN_REASON)
            olist = erts_bld_cons(hpp, szp, am_nodedown_reason, olist);
        res = erts_bld_cons(hpp, szp,
                            erts_bld_tuple(hpp, szp, 2,
                                           mon->other.item,
                                           olist),
                            res);
    }

    ctxt->hpp = hpp;
    ctxt->szp = szp;
    ctxt->res = res;
    return 1;
}

Eterm
erts_processes_monitoring_nodes(Process *c_p)
{
    /*
     * Note, this function is only used for debugging.
     */
    ErtsNodesMonitorInfoContext ctxt;
    Eterm *hp;
    Uint sz;
#ifdef DEBUG
    Eterm *hend;
#endif

    ASSERT(c_p);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    erts_thr_progress_block();
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

    erts_mtx_lock(&nodes_monitors_mtx);

    sz = 0;
    ctxt.szp = &sz;
    ctxt.hpp = NULL;
    ctxt.res = NIL;
    erts_monitor_list_foreach(nodes_monitors,
                              nodes_monitor_info,
                              &ctxt);

    hp = HAlloc(c_p, sz);
#ifdef DEBUG
    hend = hp + sz;
#endif
    ctxt.hpp = &hp;
    ctxt.szp = NULL;
    ctxt.res = NIL;
    erts_monitor_list_foreach(nodes_monitors,
                              nodes_monitor_info,
                              &ctxt);
    ASSERT(hp == hend);

    erts_mtx_unlock(&nodes_monitors_mtx);

    erts_thr_progress_unblock();

    return ctxt.res;
}

static void
print_suspended_on_de(fmtfn_t to, void *to_arg, DistEntry *dep)
{
    for (; dep; dep = dep->next) {
        ErtsProcList *curr = erts_proclist_peek_first(dep->suspended);
        while (curr) {
            if (!is_internal_pid(curr->u.pid))
                print_process_info(to, to_arg, curr->u.p, 0);
            curr = erts_proclist_peek_next(dep->suspended, curr);
        }
    }
}

void
erts_dist_print_procs_suspended_on_de(fmtfn_t to, void *to_arg) {
    print_suspended_on_de(to, to_arg, erts_hidden_dist_entries);
    print_suspended_on_de(to, to_arg, erts_visible_dist_entries);
}
