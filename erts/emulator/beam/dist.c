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
    ASSERT(0);
    return "UNKNOWN";
}

#endif

#if defined(VALGRIND)
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>

#  define PURIFY_MSG(msg)                                                    \
    VALGRIND_PRINTF("%s, line %d: %s", __FILE__, __LINE__, msg)
#else
#  define PURIFY_MSG(msg)
#endif

int erts_is_alive; /* System must be blocked on change */
int erts_dist_buf_busy_limit;


/* distribution trap functions */
Export* dmonitor_node_trap = NULL;

/* local variables */
static Export *dist_ctrl_put_data_trap;

/* forward declarations */

static void erts_schedule_dist_command(Port *, DistEntry *);
static int dsig_send_exit(ErtsDSigSendContext *ctx, Eterm ctl, Eterm msg);
static int dsig_send_ctl(ErtsDSigSendContext *ctx, Eterm ctl);
static void send_nodes_mon_msgs(Process *, Eterm, Eterm, Eterm, Eterm);
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
    qflgs = erts_atomic32_read_band_acqb(&dep->qflgs, ~unset_qflgs);
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
    if (erts_monitor_is_origin(mon))
        erts_proc_sig_send_demonitor(mon);
    else
        erts_proc_sig_send_monitor_down(mon, am_noconnection);
    return ERTS_MON_LNK_FIRE_REDS;
}

static int link_connection_down(ErtsLink *lnk, void *vdist, Sint reds)
{
    erts_proc_sig_send_link_exit(NULL, THE_NON_VALUE, lnk,
                                 am_noconnection, NIL);
    return ERTS_MON_LNK_FIRE_REDS;
}

typedef enum {
    ERTS_CML_CLEANUP_STATE_LINKS,
    ERTS_CML_CLEANUP_STATE_MONITORS,
    ERTS_CML_CLEANUP_STATE_ONAME_MONITORS,
    ERTS_CML_CLEANUP_STATE_SEQUENCES,
    ERTS_CML_CLEANUP_STATE_NODE_MONITORS
} ErtsConMonLnkSeqCleanupState;

typedef struct {
    ErtsConMonLnkSeqCleanupState state;
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
    case ERTS_CML_CLEANUP_STATE_MONITORS:
        reds = erts_monitor_list_foreach_delete_yielding(&dist->monitors,
                                                         monitor_connection_down,
                                                         NULL, &cmlcp->yield_state,
                                                         reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_ONAME_MONITORS;
    case ERTS_CML_CLEANUP_STATE_ONAME_MONITORS:
        reds = erts_monitor_tree_foreach_delete_yielding(&dist->orig_name_monitors,
                                                         monitor_connection_down,
                                                         NULL, &cmlcp->yield_state,
                                                         reds);
        if (reds <= 0)
            break;

        cmlcp->dist = NULL;
        erts_mon_link_dist_dec_refc(dist);

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_SEQUENCES;
    case ERTS_CML_CLEANUP_STATE_SEQUENCES:
        reds = erts_dist_seq_tree_foreach_delete_yielding(&cmlcp->seq,
                                                          &cmlcp->yield_state,
                                                          reds);
        if (reds <= 0)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_NODE_MONITORS;
    case ERTS_CML_CLEANUP_STATE_NODE_MONITORS:
        if (cmlcp->trigger_node_monitors) {
            send_nodes_mon_msgs(NULL,
                                am_nodedown,
                                cmlcp->nodename,
                                cmlcp->visability,
                                cmlcp->reason);
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
schedule_con_monitor_link_seq_cleanup(ErtsMonLnkDist *dist,
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
        if (!dist)
            cmlcp->state = ERTS_CML_CLEANUP_STATE_NODE_MONITORS;
        else {
            cmlcp->state = ERTS_CML_CLEANUP_STATE_LINKS;
            erts_mtx_lock(&dist->mtx);
            ASSERT(dist->alive);
            dist->alive = 0;
            erts_mtx_unlock(&dist->mtx);
        }

        cmlcp->seq = seq;

        cmlcp->trigger_node_monitors = is_value(nodename);
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
    return is_node_name((char*)atom_tab(i)->name, atom_tab(i)->len);
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
    send_nodes_mon_msgs(NULL, am_nodedown, nodename, am_visible, nodedown.reason);
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
    erts_proc_sig_send_exit(NULL, (Eterm) vpid, (Eterm) vpid,
                            am_kill, NIL, 0);
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
	Uint32 flags;

	erts_atomic_set_mb(&dep->dist_cmd_scheduled, 1);
	erts_de_rwlock(dep);

        if (is_internal_port(dep->cid)) {
            ERTS_LC_ASSERT(erts_lc_is_port_locked(erts_port_lookup_raw(dep->cid)));

            if (erts_port_task_is_scheduled(&dep->dist_cmd))
                erts_port_task_abort(&dep->dist_cmd);
        }

	if (dep->state == ERTS_DE_STATE_EXITING) {
	    ASSERT(erts_atomic32_read_nob(&dep->qflgs) & ERTS_DE_QFLG_EXIT);
	}
	else {
	    dep->state = ERTS_DE_STATE_EXITING;
	    erts_mtx_lock(&dep->qlock);
	    ASSERT(!(erts_atomic32_read_nob(&dep->qflgs) & ERTS_DE_QFLG_EXIT));
	    erts_atomic32_read_bor_relb(&dep->qflgs, ERTS_DE_QFLG_EXIT);
	    erts_mtx_unlock(&dep->qlock);
	}

        mld = dep->mld;
        dep->mld = NULL;

        sequences = dep->sequences;
        dep->sequences = NULL;

	nodename = dep->sysname;
	flags = dep->flags;

        erts_atomic_set_nob(&dep->input_handler, (erts_aint_t) NIL);
        cache = dep->cache;
        dep->cache = NULL;

        erts_mtx_lock(&dep->qlock);

        erts_atomic64_set_nob(&dep->in, 0);
        erts_atomic64_set_nob(&dep->out, 0);

        obuf = clear_de_out_queues(dep);
        suspendees = get_suspended_on_de(dep, ERTS_DE_QFLGS_ALL);

        erts_mtx_unlock(&dep->qlock);
        erts_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
        dep->send = NULL;

	erts_set_dist_entry_not_connected(dep);

	erts_de_rwunlock(dep);

        schedule_con_monitor_link_seq_cleanup(mld,
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
        if (dep->transcode_ctx)
            transcode_free_ctx(dep);
    }

    dec_no_nodes();

    return 1;
}

static Export*
trap_function(Eterm func, int arity)
{
    return erts_export_put(am_erlang, func, arity);
}

/*
 * Sync with dist_util.erl:
 *
 * -record(erts_dflags,
 *         {default, mandatory, addable, rejectable, strict_order}).
 */
static Eterm erts_dflags_record;

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
    {
        Eterm* hp = erts_alloc(ERTS_ALC_T_LITERAL, (1+6)*sizeof(Eterm));
        erts_dflags_record = TUPLE6(hp, am_erts_dflags,
                                    make_small(DFLAG_DIST_DEFAULT),
                                    make_small(DFLAG_DIST_MANDATORY),
                                    make_small(DFLAG_DIST_ADDABLE),
                                    make_small(DFLAG_DIST_REJECTABLE),
                                    make_small(DFLAG_DIST_STRICT_ORDER));
        erts_set_literal_tag(&erts_dflags_record, hp, (1+6));
    }
}

#define ErtsDistOutputBuf2Binary(OB) OB->bin

static ERTS_INLINE ErtsDistOutputBuf *
alloc_dist_obuf(Uint size, Uint headers)
{
    Uint obuf_size = sizeof(ErtsDistOutputBuf)*(headers);
    ErtsDistOutputBuf *obuf;
    Binary *bin;
    byte *extp;
    int i;

    bin = erts_bin_drv_alloc(obuf_size + size);
    erts_refc_add(&bin->intern.refc, headers - 1, 1);

    obuf = (ErtsDistOutputBuf *)&bin->orig_bytes[0];
    extp = (byte *)&bin->orig_bytes[obuf_size];

    for (i = 0; i < headers; i++) {
        obuf[i].bin = bin;
        obuf[i].extp = extp;
#ifdef DEBUG
        obuf[i].dbg_pattern = ERTS_DIST_OUTPUT_BUF_DBG_PATTERN;
        obuf[i].ext_startp = extp;
        obuf[i].alloc_endp = &extp[size];
        ASSERT(bin == ErtsDistOutputBuf2Binary(obuf));
#endif
    }
    return obuf;
}

static ERTS_INLINE void
free_dist_obuf(ErtsDistOutputBuf *obuf)
{
    Binary *bin = ErtsDistOutputBuf2Binary(obuf);
    ASSERT(obuf->dbg_pattern == ERTS_DIST_OUTPUT_BUF_DBG_PATTERN);
    erts_bin_release(bin);
}

static ERTS_INLINE Sint
size_obuf(ErtsDistOutputBuf *obuf)
{
    return sizeof(ErtsDistOutputBuf) + (obuf->ext_endp - obuf->ext_start)
        + (obuf->hdr_endp - obuf->hdrp);
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
    Sint obufsize = 0;

    while (obuf) {
	ErtsDistOutputBuf *fobuf;
	fobuf = obuf;
	obuf = obuf->next;
	obufsize += size_obuf(fobuf);
	free_dist_obuf(fobuf);
    }

    if (obufsize) {
	erts_mtx_lock(&dep->qlock);
        ASSERT(erts_atomic_read_nob(&dep->qsize) >= obufsize);
        erts_atomic_add_nob(&dep->qsize,
                            (erts_aint_t) -obufsize);
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
            free_dist_obuf(&ctx->obuf[i]);
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
erts_dsig_send_unlink(ErtsDSigSendContext *ctx, Eterm local, Eterm remote)
{
    Eterm ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_UNLINK), local, remote);
    return dsig_send_ctl(ctx, ctl);
}


/* A local process that's being monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason} */
int
erts_dsig_send_m_exit(ErtsDSigSendContext *ctx, Eterm watcher, Eterm watched,
                      Eterm ref, Eterm reason)
{
    Eterm ctl, msg;

    if (~ctx->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_MONITOR_P_EXIT (see dsig_send_monitor)
         */
        return ERTS_DSIG_SEND_OK;
    }

    if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD) {
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

    if (~ctx->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
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

    if (~ctx->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
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

    if (ctx->flags & DFLAG_BIG_SEQTRACE_LABELS) {
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
erts_dsig_send_msg(ErtsDSigSendContext* ctx, Eterm remote, Eterm message)
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
    DTRACE_CHARBUF(receiver_name, 64);
#endif

    if (have_seqtrace(SEQ_TRACE_TOKEN(sender))) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote, sender);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *receiver_name = '\0';
    if (DTRACE_ENABLED(message_send) || DTRACE_ENABLED(message_send_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", ctx->dep->sysname);
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
                      "%T", sender->common.id);
        erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
                      "%T", remote);
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

        if (ctx->flags & DFLAG_SEND_SENDER) {
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

    DTRACE6(message_send, sender_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    DTRACE7(message_send_remote, sender_name, node_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    ctx->ctl = ctl;
    ctx->msg = message;
    res = erts_dsig_send(ctx);
    return res;
}

int
erts_dsig_send_reg_msg(ErtsDSigSendContext* ctx, Eterm remote_name,
                       Eterm full_to, Eterm message)
{
    Eterm ctl;
    Eterm token = NIL;
    Process *sender = ctx->c_p;
#ifdef USE_VM_PROBES
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    Uint32 msize = 0;
    DTRACE_CHARBUF(node_name, 64);
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(receiver_name, 128);
#endif

    if (have_seqtrace(SEQ_TRACE_TOKEN(sender))) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, full_to, sender);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *receiver_name = '\0';
    if (DTRACE_ENABLED(message_send) || DTRACE_ENABLED(message_send_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", ctx->dep->sysname);
        erts_snprintf(sender_name, sizeof(DTRACE_CHARBUF_NAME(sender_name)),
                      "%T", sender->common.id);
        erts_snprintf(receiver_name, sizeof(DTRACE_CHARBUF_NAME(receiver_name)),
                      "{%T,%s}", remote_name, node_name);
        msize = size_object(message);
        if (have_seqtrace(token)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(token);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
        }
    }
#endif

    if (token != NIL && can_send_seqtrace_token(ctx, token))
	ctl = TUPLE5(&ctx->ctl_heap[0], make_small(DOP_REG_SEND_TT),
		     sender->common.id, am_Empty, remote_name, token);
    else
	ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_REG_SEND),
		     sender->common.id, am_Empty, remote_name);

    DTRACE6(message_send, sender_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    DTRACE7(message_send_remote, sender_name, node_name, receiver_name,
            msize, tok_label, tok_lastcnt, tok_serial);
    ctx->ctl = ctl;
    ctx->msg = message;
    return erts_dsig_send(ctx);
}

/* local has died, deliver the exit signal to remote */
int
erts_dsig_send_exit_tt(ErtsDSigSendContext *ctx, Eterm local, Eterm remote, 
		       Eterm reason, Eterm token)
{
    Eterm ctl, msg = THE_NON_VALUE;
#ifdef USE_VM_PROBES
    Process *sender = ctx->c_p;
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    DTRACE_CHARBUF(node_name, 64);
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(remote_name, 128);
    DTRACE_CHARBUF(reason_str, 128);
#endif

    if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD)
        msg = reason;

    if (have_seqtrace(token)) {
	seq_trace_update_send(ctx->c_p);
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
        if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD) {
            ctl = TUPLE4(&ctx->ctl_heap[0],
                         make_small(DOP_PAYLOAD_EXIT_TT), local, remote, token);
        } else
            ctl = TUPLE5(&ctx->ctl_heap[0],
                         make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
        if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD)
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
    Eterm ctl, msg = ctx->dep->flags & DFLAG_EXIT_PAYLOAD ? reason : THE_NON_VALUE;

    if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD) {
        ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_PAYLOAD_EXIT), local, remote);
        msg = reason;
    } else {
        ctl = TUPLE4(&ctx->ctl_heap[0], make_small(DOP_EXIT), local, remote, reason);
        msg = THE_NON_VALUE;
    }
    return dsig_send_exit(ctx, ctl, msg);
}

int
erts_dsig_send_exit2(ErtsDSigSendContext *ctx, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl, msg;

    if (ctx->dep->flags & DFLAG_EXIT_PAYLOAD) {
        ctl = TUPLE3(&ctx->ctl_heap[0],
                     make_small(DOP_PAYLOAD_EXIT2), local, remote);
        msg = reason;
    } else {
        ctl = TUPLE4(&ctx->ctl_heap[0],
                     make_small(DOP_EXIT2), local, remote, reason);
        msg = THE_NON_VALUE;
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
		     byte *buf,
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
            PURIFY_MSG("data error");
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
            if (dist_seq_rbt_lookup_insert(&dep->sequences, seq) != NULL) {
                free_message_buffer(&seq->hfrag);
                goto data_error;
            }

            erts_make_dist_ext_copy(&ede, erts_get_dist_ext(&seq->hfrag));

            if (ede.data->frag_id > 1) {
                seq->cnt--;
                return 0;
            }
        }

        /* fall through, the first fragment in the sequence was the last fragment */
    case ERTS_PREP_DIST_EXT_FRAG_CONT: {
        DistSeqNode *seq = dist_seq_rbt_lookup(dep->sequences, ede.data->seq_id);

        if (!seq)
            goto data_error;

        /* If we did a fall-though we already did this */
        if (res == ERTS_PREP_DIST_EXT_FRAG_CONT)
            erts_dist_ext_frag(&ede_data, erts_get_dist_ext(&seq->hfrag));

        /* Verify that the fragments have arrived in the correct order */
        if (seq->cnt != ede.data->frag_id)
            goto data_error;

        seq->cnt--;

        /* Check if this was the last fragment */
        if (ede.data->frag_id > 1)
            return 0;

        /* Last fragment arrived, time to dispatch the signal */
        dist_seq_rbt_delete(&dep->sequences, seq);
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
	PURIFY_MSG("data error");
	goto decode_error;
    }

    /* Fill the unused part of the hfrag with a bignum header */
    if (ede_hfrag && ede_hfrag->mem + ede_hfrag->used_size > factory.hp) {
        Uint slot = factory.hp - ede_hfrag->mem;
        ede_hfrag->mem[slot] = make_pos_bignum_header(ede_hfrag->used_size - slot - 1);
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
            ErtsLinkData *ldp = erts_link_create(ERTS_LNK_TYPE_DIST_PROC,
                                                 from, to);
            ASSERT(ldp->a.other.item == to);
            ASSERT(eq(ldp->b.other.item, from));
            code = erts_link_dist_insert(&ldp->a, dep->mld);
            ASSERT(code);

            if (erts_proc_sig_send_link(NULL, to, &ldp->b))
                break; /* done */

            /* Failed to send signal; cleanup and reply noproc... */

            code = erts_link_dist_delete(&ldp->a);
            ASSERT(code);
            erts_link_release_both(ldp);
        }

        code = erts_dsig_prepare(&ctx, dep, NULL, 0, ERTS_DSP_NO_LOCK, 1, 1, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED) {
            code = erts_dsig_send_exit(&ctx, to, from, am_noproc);
            ASSERT(code == ERTS_DSIG_SEND_OK);
        }

	break;
    }

    case DOP_UNLINK: {
	if (tuple_arity != 3) {
	    goto invalid_message;
	}
	from = tuple[2];
	to = tuple[3];
	if (is_not_external_pid(from))
	    goto invalid_message;
        if (dep != external_pid_dist_entry(from))
	    goto invalid_message;

        if (is_external_pid(to)
            && erts_this_dist_entry == external_pid_dist_entry(from))
            break;

        if (is_not_internal_pid(to))
            goto invalid_message;

        erts_proc_sig_send_dist_unlink(dep, from, to);
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
                                      ref, watcher, pid, name);

            code = erts_monitor_dist_insert(&mdp->origin, dep->mld);
            ASSERT(code); (void)code;

            if (erts_proc_sig_send_monitor(&mdp->target, pid))
                break; /* done */

            /* Failed to send to local proc; cleanup reply noproc... */

            code = erts_monitor_dist_delete(&mdp->origin);
            ASSERT(code); (void)code;
            erts_monitor_release_both(mdp);

        }

        code = erts_dsig_prepare(&ctx, dep, NULL, 0, ERTS_DSP_NO_LOCK, 1, 1, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED) {
            code = erts_dsig_send_m_exit(&ctx, watcher, watched, ref, am_noproc);
            ASSERT(code == ERTS_DSIG_SEND_OK);
        }

        break;
    }

    case DOP_DEMONITOR_P:
	/* A remote node informs us that a local pid in no longer monitored
	   We get {DOP_DEMONITOR_P, Remote pid, Local pid or name, ref},
	   We need only the ref of course */

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

        if (is_internal_pid(watched))
            erts_proc_sig_send_dist_demonitor(watched, ref);
        else if (is_external_pid(watched)
                 && external_pid_dist_entry(watched) == erts_this_dist_entry) {
            /* old incarnation; ignore it */
            ;
        }
        else if (is_atom(watched)) {
            ErtsMonLnkDist *mld = dep->mld;
            ErtsMonitor *mon;

            erts_mtx_lock(&mld->mtx);

            mon = erts_monitor_tree_lookup(mld->orig_name_monitors, ref);
            if (mon)
                erts_monitor_tree_delete(&mld->orig_name_monitors, mon);

            erts_mtx_unlock(&mld->mtx);

            if (mon)
                erts_proc_sig_send_demonitor(mon);
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
               ? (is_pid(tuple[2]) && (dep->flags & DFLAG_SEND_SENDER))
               : tuple[2] == am_Empty);

#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(edep, "MSG", buf, orig_len);
#endif
	to = tuple[3];
	if (is_not_pid(to)) {
	    goto invalid_message;
	}
	rp = erts_proc_lookup(to);

	if (rp) {
	    ErtsProcLocks locks = 0;

	    erts_queue_dist_message(rp, locks, edep, ede_hfrag, token, am_Empty);
	    if (locks)
		erts_proc_unlock(rp, locks);
        } else if (ede_hfrag != NULL) {
            erts_free_dist_ext_copy(erts_get_dist_ext(ede_hfrag));
            free_message_buffer(ede_hfrag);
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
                break;
            goto invalid_message;
        }

        if (!erts_proc_lookup(watcher)) {
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

        erts_proc_sig_send_dist_exit(dep, from, to, edep, ede_hfrag, reason, token);
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
    PURIFY_MSG("data error");
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
    int res;

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

    if (no_suspend) {
	if (erts_atomic32_read_acqb(&dep->qflgs) & ERTS_DE_QFLG_BUSY) {
	    res = ERTS_DSIG_PREP_WOULD_SUSPEND;
	    goto fail;
	}
    }

    ctx->c_p = proc;
    ctx->dep = dep;
    ctx->deref_dep = 0;
    ctx->cid = dep->cid;
    ctx->connection_id = dep->connection_id;
    ctx->no_suspend = no_suspend;
    ctx->no_trap = no_trap;
    ctx->flags = dep->flags;
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
        ASSERT(dep);
	id = prt->common.id;
    }
    else {
	ASSERT(dist_entry);
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
	    ctx->flags = ctx->flags;
	    ctx->c_p = ctx->c_p;

	    if (!ctx->c_p) {
		ctx->no_trap = 1;
		ctx->no_suspend = 1;
            }

	    ERTS_LC_ASSERT(!ctx->c_p
			       || (ERTS_PROC_LOCK_MAIN
				   == erts_proc_lc_my_proc_locks(ctx->c_p)));

	    if (!erts_is_alive)
		return ERTS_DSIG_SEND_OK;

	    if (ctx->flags & DFLAG_DIST_HDR_ATOM_CACHE) {
		ctx->acmp = erts_get_atom_cache_map(ctx->c_p);
		ctx->max_finalize_prepend = 0;
	    }
	    else {
		ctx->acmp = NULL;
		ctx->max_finalize_prepend = 3;
	    }

    #ifdef ERTS_DIST_MSG_DBG
            erts_fprintf(dbg_file, "SEND: CTL: %s: %.80T\n",
                         erts_dop_to_string(unsigned_val(tuple_val(ctx->ctl)[1])),
                         ctx->ctl);
            if (is_value(ctx->msg))
                erts_fprintf(dbg_file, "    MSG: %.160T\n", ctx->msg);
    #endif

	    ctx->data_size = ctx->max_finalize_prepend;
	    erts_reset_atom_cache_map(ctx->acmp);

	    switch (erts_encode_dist_ext_size(ctx->ctl, ctx->flags,
                                              ctx->acmp, &ctx->data_size)) {
            case ERTS_EXT_SZ_OK:
                break;
            case ERTS_EXT_SZ_SYSTEM_LIMIT:
                retval = ERTS_DSIG_SEND_TOO_LRG;
                goto done;
            case ERTS_EXT_SZ_YIELD:
                ERTS_INTERNAL_ERROR("Unexpected yield result");
                break;
            }
	    if (is_non_value(ctx->msg)) {
                ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
                break;
            }
            ctx->u.sc.wstack.wstart = NULL;
            ctx->u.sc.flags = ctx->flags;
            ctx->u.sc.level = 0;

            ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_SIZE;
	case ERTS_DSIG_SEND_PHASE_MSG_SIZE: {
            ErtsExtSzRes sz_res;
            sz_res = (!ctx->no_trap
                      ? erts_encode_dist_ext_size_ctx(ctx->msg,
                                                      ctx,
                                                      &ctx->data_size)
                      : erts_encode_dist_ext_size(ctx->msg,
                                                  ctx->flags,
                                                  ctx->acmp,
                                                  &ctx->data_size));
            switch (sz_res) {
            case ERTS_EXT_SZ_OK:
                break;
            case ERTS_EXT_SZ_SYSTEM_LIMIT:
                retval = ERTS_DSIG_SEND_TOO_LRG;
                goto done;
            case ERTS_EXT_SZ_YIELD:
                if (ctx->no_trap)
                    ERTS_INTERNAL_ERROR("Unexpected yield result");
                retval = ERTS_DSIG_SEND_CONTINUE;
                goto done;
            }

	    ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
        }
	case ERTS_DSIG_SEND_PHASE_ALLOC:
	    erts_finalize_atom_cache_map(ctx->acmp, ctx->flags);

            if (ctx->flags & DFLAG_FRAGMENTS && is_value(ctx->msg) && is_not_immed(ctx->msg)) {
                /* Calculate the max number of fragments that are needed */
                ASSERT(is_pid(ctx->from) &&
                       "from has to be a pid because it is used as sequence id");
                ctx->fragments = ctx->data_size / ERTS_DIST_FRAGMENT_SIZE + 1;
            } else
                ctx->fragments = 1;

	    ctx->dhdr_ext_size = erts_encode_ext_dist_header_size(ctx->acmp, ctx->fragments);

	    ctx->obuf = alloc_dist_obuf(
                ctx->dhdr_ext_size + ctx->data_size +
                (ctx->fragments-1) * ERTS_DIST_FRAGMENT_HEADER_SIZE,
                ctx->fragments);
            ctx->obuf->ext_start = &ctx->obuf->extp[0];
	    ctx->obuf->ext_endp = &ctx->obuf->extp[0] + ctx->max_finalize_prepend
                + ctx->dhdr_ext_size;

	    /* Encode internal version of dist header */
	    ctx->obuf->extp = erts_encode_ext_dist_header_setup(
                ctx->obuf->ext_endp, ctx->acmp, ctx->fragments, ctx->from);
	    /* Encode control message */
	    erts_encode_dist_ext(ctx->ctl, &ctx->obuf->ext_endp, ctx->flags, ctx->acmp, NULL, NULL);

            ctx->obuf->hdrp = NULL;
            ctx->obuf->hdr_endp = NULL;

	    if (is_non_value(ctx->msg)) {
                ctx->obuf->msg_start = NULL;
                ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
                break;
            }
            ctx->u.ec.flags = ctx->flags;
            ctx->u.ec.hopefull_flags = 0;
            ctx->u.ec.level = 0;
            ctx->u.ec.wstack.wstart = NULL;
            ctx->obuf->msg_start = ctx->obuf->ext_endp;

            ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_ENCODE;
        case ERTS_DSIG_SEND_PHASE_MSG_ENCODE:
            if (!ctx->no_trap) {
                if (erts_encode_dist_ext(ctx->msg, &ctx->obuf->ext_endp, ctx->flags,
                                         ctx->acmp, &ctx->u.ec, &ctx->reds)) {
                    retval = ERTS_DSIG_SEND_CONTINUE;
                    goto done;
                }
            } else {
                erts_encode_dist_ext(ctx->msg, &ctx->obuf->ext_endp, ctx->flags,
                                     ctx->acmp, NULL, NULL);
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
	case ERTS_DSIG_SEND_PHASE_FIN: {

	    ASSERT(ctx->obuf->extp < ctx->obuf->ext_endp);
	    ASSERT(ctx->obuf->ext_startp <= ctx->obuf->extp - ctx->max_finalize_prepend);
	    ASSERT(ctx->obuf->ext_endp <= (byte*)ctx->obuf->ext_startp + ctx->data_size + ctx->dhdr_ext_size);

	    ctx->data_size = ctx->obuf->ext_endp - ctx->obuf->extp;

            ctx->obuf->hopefull_flags = ctx->u.ec.hopefull_flags;

            if (ctx->fragments > 1) {
                int fin_fragments;
                int i;
                byte *msg = ctx->obuf->msg_start,
                    *msg_end = ctx->obuf->ext_endp,
                    *hdrp = msg_end;

                ASSERT((ctx->obuf->hopefull_flags & ctx->flags) == ctx->obuf->hopefull_flags);
                ASSERT(get_int64(ctx->obuf->extp + 1 + 1 + 8) == ctx->fragments);

                /* Now that encoding is done we know how large the term will
                   be so we adjust the number of fragments to send. Note that
                   this can mean that only 1 fragment is sent. */
                fin_fragments = (ctx->obuf->ext_endp - ctx->obuf->msg_start + ERTS_DIST_FRAGMENT_SIZE-1) /
                    ERTS_DIST_FRAGMENT_SIZE - 1;

                /* Update the frag_id in the DIST_FRAG_HEADER */
                put_int64(fin_fragments+1, ctx->obuf->extp + 1 + 1 + 8);

                if (fin_fragments > 0)
                    msg += ERTS_DIST_FRAGMENT_SIZE;
                else
                    msg = msg_end;
                ctx->obuf->next = &ctx->obuf[1];
                ctx->obuf->ext_endp = msg;

                /* Loop through all fragments, updating the output buffers
                   to be correct and also writing the DIST_FRAG_CONT header. */
                for (i = 1; i < fin_fragments + 1; i++) {
                    ctx->obuf[i].hopefull_flags = 0;
                    ctx->obuf[i].extp = msg;
                    ctx->obuf[i].ext_start = msg;
                    if (msg + ERTS_DIST_FRAGMENT_SIZE > msg_end)
                        ctx->obuf[i].ext_endp = msg_end;
                    else {
                        msg += ERTS_DIST_FRAGMENT_SIZE;
                        ctx->obuf[i].ext_endp = msg;
                    }
                    ASSERT(ctx->obuf[i].ext_endp > ctx->obuf[i].extp);
                    ctx->obuf[i].hdrp = erts_encode_ext_dist_header_fragment(
                        &hdrp, fin_fragments - i + 1, ctx->from);
                    ctx->obuf[i].hdr_endp = hdrp;
                    ctx->obuf[i].next = &ctx->obuf[i+1];
                }
                /* If the initial fragment calculation was incorrect we free the
                   remaining output buffers. */
                for (; i < ctx->fragments; i++) {
                    free_dist_obuf(&ctx->obuf[i]);
                }
                if (!ctx->no_trap && !ctx->no_suspend)
                    ctx->reds -= ctx->fragments;
                ctx->fragments = fin_fragments + 1;
            }

            ctx->phase = ERTS_DSIG_SEND_PHASE_SEND;

            if (ctx->reds <= 0) {
                retval = ERTS_DSIG_SEND_CONTINUE;
                goto done;
            }
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
                    free_dist_obuf(&ctx->obuf[i]);
                ctx->fragments = 0;
	    }
	    else {
                Sint qsize = erts_atomic_read_nob(&dep->qsize);
                erts_aint32_t qflgs;
		ErtsProcList *plp = NULL;
                Eterm notify_proc = NIL;
                Sint obsz;
                int fragments;

                /* Calculate how many fragments to send. This depends on
                   the available space in the distr queue and the amount
                   of remaining reductions. */
                for (fragments = 0, obsz = 0;
                     fragments < ctx->fragments &&
                         ((ctx->reds > 0 && (qsize + obsz) < erts_dist_buf_busy_limit) ||
                          ctx->no_trap || ctx->no_suspend);
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
		qsize = erts_atomic_add_read_nob(&dep->qsize, (erts_aint_t) obsz);
                ASSERT(qsize >= obsz);
                qflgs = erts_atomic32_read_nob(&dep->qflgs);
		if (!(qflgs & ERTS_DE_QFLG_BUSY) && qsize >= erts_dist_buf_busy_limit) {
		    erts_atomic32_read_bor_relb(&dep->qflgs, ERTS_DE_QFLG_BUSY);
                    qflgs |= ERTS_DE_QFLG_BUSY;
                }
                if (qsize == obsz && (qflgs & ERTS_DE_QFLG_REQ_INFO)) {
                    /* Previously empty queue and info requested... */
                    qflgs = erts_atomic32_read_band_mb(&dep->qflgs,
                                                       ~ERTS_DE_QFLG_REQ_INFO);
                    if (qflgs & ERTS_DE_QFLG_REQ_INFO) {
                        notify_proc = dep->cid;
                        ASSERT(is_internal_pid(notify_proc));
                    }
                    /* else: requester will send itself the message... */
                    qflgs &= ~ERTS_DE_QFLG_REQ_INFO;
                }
		if (!ctx->no_suspend && (qflgs & ERTS_DE_QFLG_BUSY)) {
		    erts_mtx_unlock(&dep->qlock);

		    plp = erts_proclist_create(ctx->c_p);
		    erts_suspend(ctx->c_p, ERTS_PROC_LOCK_MAIN, NULL);
		    suspended = 1;
		    erts_mtx_lock(&dep->qlock);
		}

                if (fragments > 1) {
                    if (!ctx->obuf->hdrp) {
                        ASSERT(get_int64(ctx->obuf->extp + 10) == ctx->fragments);
                    } else {
                        ASSERT(get_int64(ctx->obuf->hdrp + 10) == ctx->fragments);
                    }
                }

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

		if (!ctx->no_suspend) {
                    qflgs = erts_atomic32_read_nob(&dep->qflgs);
		    if (!(qflgs & ERTS_DE_QFLG_BUSY)) {
			if (suspended)
			    resume = 1; /* was busy when we started, but isn't now */
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
		    else {
			/* Enqueue suspended process on dist entry */
			ASSERT(plp);
			erts_proclist_store_last(&dep->suspended, plp);
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
                    retval = ERTS_DSIG_SEND_CONTINUE;
                    if (!resume && erts_system_monitor_flags.busy_dist_port)
                        monitor_generic(ctx->c_p, am_busy_dist_port, cid);
                    goto done;
                }
	    }
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
		if (!resume && erts_system_monitor_flags.busy_dist_port)
		    monitor_generic(ctx->c_p, am_busy_dist_port, cid);
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
    int fpe_was_unmasked;
    ErlDrvSizeT size;
    char *bufp;

    ERTS_CHK_NO_PROC_LOCKS;
    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (!obuf) {
        size = 0;
        bufp = NULL;
    }
    else {
        size = obuf->ext_endp - obuf->extp;
        bufp = (char*) obuf->extp;
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
    fpe_was_unmasked = erts_block_fpe();
    (*prt->drv_ptr->output)((ErlDrvData) prt->drv_data, bufp, size);
    erts_unblock_fpe(fpe_was_unmasked);
    return size;
}

static Uint
dist_port_commandv(Port *prt, ErtsDistOutputBuf *obuf)
{
    int fpe_was_unmasked;
    ErlDrvSizeT size = 0;
    SysIOVec iov[3];
    ErlDrvBinary* bv[3];
    ErlIOVec eiov;

    ERTS_CHK_NO_PROC_LOCKS;
    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    iov[0].iov_base = NULL;
    iov[0].iov_len = 0;
    bv[0] = NULL;

    if (!obuf) {
        size = 0;
        eiov.vsize = 1;
    }
    else {
        int i = 1;
        eiov.vsize = 2;

        if (obuf->hdrp) {
            eiov.vsize = 3;
            iov[i].iov_base = obuf->hdrp;
            iov[i].iov_len = obuf->hdr_endp - obuf->hdrp;
            size += iov[i].iov_len;
            bv[i] = Binary2ErlDrvBinary(ErtsDistOutputBuf2Binary(obuf));
#ifdef ERTS_RAW_DIST_MSG_DBG
            erts_fprintf(dbg_file, "SEND: ");
            bw(iov[i].iov_base, iov[i].iov_len);
#endif
            i++;

        }

        iov[i].iov_base = obuf->extp;
        iov[i].iov_len = obuf->ext_endp - obuf->extp;
#ifdef ERTS_RAW_DIST_MSG_DBG
            erts_fprintf(dbg_file, "SEND: ");
            bw(iov[i].iov_base, iov[i].iov_len);
#endif
        size += iov[i].iov_len;
        bv[i] = Binary2ErlDrvBinary(ErtsDistOutputBuf2Binary(obuf));
    }

    eiov.size = size;
    eiov.iov = iov;
    eiov.binv = bv;

    if (size > (Uint) INT_MAX)
	erts_exit(ERTS_DUMP_EXIT,
		 "Absurdly large distribution output data buffer "
		 "(%beu bytes) passed.\n",
		 size);

    ASSERT(prt->drv_ptr->outputv);

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_outputv)) {
        DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
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
    fpe_was_unmasked = erts_block_fpe();
    (*prt->drv_ptr->outputv)((ErlDrvData) prt->drv_data, &eiov);
    erts_unblock_fpe(fpe_was_unmasked);

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

int
erts_dist_command(Port *prt, int initial_reds)
{
    Sint reds = initial_reds - ERTS_PORT_REDS_DIST_CMD_START;
    enum dist_entry_state state;
    Uint32 flags;
    Sint qsize, obufsize = 0;
    ErtsDistOutputQueue oq, foq;
    DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
    Uint (*send)(Port *prt, ErtsDistOutputBuf *obuf);
    erts_aint32_t sched_flags;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_LC_ASSERT(erts_lc_is_port_locked(prt));

    erts_atomic_set_mb(&dep->dist_cmd_scheduled, 0);

    erts_de_rlock(dep);
    flags = dep->flags;
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
    oq.first = dep->out_queue.first;
    oq.last = dep->out_queue.last;
    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    erts_mtx_unlock(&dep->qlock);

    foq.first = dep->finalized_out_queue.first;
    foq.last = dep->finalized_out_queue.last;
    dep->finalized_out_queue.first = NULL;
    dep->finalized_out_queue.last = NULL;

#ifdef DEBUG
    {
        Uint sz = 0;
        ErtsDistOutputBuf *curr = oq.first;
        while (curr) {
            sz += size_obuf(curr);
            curr = curr->next;
        }
        curr = foq.first;
        while (curr) {
            sz += size_obuf(curr);
            curr = curr->next;
        }
        ASSERT(sz <= erts_atomic_read_nob(&dep->qsize));
    }
#endif

    sched_flags = erts_atomic32_read_nob(&prt->sched.flags);

    if (reds < 0)
	goto preempted;

    if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT) && foq.first) {
	int preempt = 0;
	do {
            Uint size;
            ErtsDistOutputBuf *fob;
            size = (*send)(prt, foq.first);
            erts_atomic64_inc_nob(&dep->out);
            esdp->io.out += (Uint64) size;
	    reds -= ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = foq.first;
	    obufsize += size_obuf(fob);
	    foq.first = foq.first->next;
	    free_dist_obuf(fob);
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
            ErtsDistOutputBuf *last_finalized = NULL;
	finalize_only:
	    ob = oq.first;
	    ASSERT(ob);
	    do {
                obufsize += size_obuf(ob);
		reds = erts_encode_ext_dist_header_finalize(ob, dep, flags, reds);
                obufsize -= size_obuf(ob);
                if (reds < 0)
                    break;
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
            obufsize += size_obuf(oq.first);
            reds = erts_encode_ext_dist_header_finalize(oq.first, dep, flags, reds);
            obufsize -= size_obuf(oq.first);
            if (reds < 0) {
                preempt = 1;
                break;
            }
	    ASSERT(oq.first->bin->orig_bytes <= (char*)oq.first->extp
                   && oq.first->extp <= oq.first->ext_endp);
	    size = (*send)(prt, oq.first);
            erts_atomic64_inc_nob(&dep->out);
	    esdp->io.out += (Uint64) size;
	    reds -= ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = oq.first;
	    obufsize += size_obuf(fob);
	    oq.first = oq.first->next;
	    free_dist_obuf(fob);
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
	 * and we havn't got too muched queued on dist entry, set
	 * dist entry in a non-busy state and resume suspended
	 * processes.
	 */
	erts_mtx_lock(&dep->qlock);
        de_busy = !!(erts_atomic32_read_nob(&dep->qflgs) & ERTS_DE_QFLG_BUSY);
        qsize = (Sint) erts_atomic_add_read_nob(&dep->qsize,
                                                (erts_aint_t) -obufsize);
	ASSERT(qsize >= 0);
	obufsize = 0;
	if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT)
	    && de_busy && qsize < erts_dist_buf_busy_limit) {
	    ErtsProcList *suspendees;
	    int resumed;
	    suspendees = get_suspended_on_de(dep, ERTS_DE_QFLG_BUSY);
	    erts_mtx_unlock(&dep->qlock);

	    resumed = erts_resume_processes(suspendees);
	    reds -= resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
	}
	else
	    erts_mtx_unlock(&dep->qlock);
    }

    ASSERT(!oq.first && !oq.last);

 done:

    if (obufsize != 0) {
	ASSERT(obufsize > 0);
	erts_mtx_lock(&dep->qlock);
#ifdef DEBUG
        qsize = (Sint) erts_atomic_add_read_nob(&dep->qsize,
                                                (erts_aint_t) -obufsize);
	ASSERT(qsize >= 0);
#else
        erts_atomic_add_nob(&dep->qsize, (erts_aint_t) -obufsize);
#endif
	erts_mtx_unlock(&dep->qlock);
    }

    ASSERT(!!foq.first == !!foq.last);
    ASSERT(!dep->finalized_out_queue.first);
    ASSERT(!dep->finalized_out_queue.last);

    if (foq.first) {
	dep->finalized_out_queue.first = foq.first;
	dep->finalized_out_queue.last = foq.last;
    }

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
	    obufsize += size_obuf(fob);
	    free_dist_obuf(fob);
	}

	foq.first = NULL;
	foq.last = NULL;
    }
    else {
	if (oq.first) {
	    /*
	     * Unhandle buffers need to be put back first
	     * in out_queue.
	     */
	    erts_mtx_lock(&dep->qlock);
	    erts_atomic_add_nob(&dep->qsize, -obufsize);
	    obufsize = 0;
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
    erts_aint32_t qflgs;
    erts_aint_t qsize;
    Eterm receiver = NIL;
    Uint32 conn_id;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
        BIF_ERROR(BIF_P, BADARG);

    /*
     * Caller is the only one that can consume from this queue
     * and the only one that can set the req-info flag...
     */

    erts_de_rlock(dep);

    if (dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        BIF_ERROR(BIF_P, BADARG);
    }

    ASSERT(dep->cid == BIF_P->common.id);

    qflgs = erts_atomic32_read_acqb(&dep->qflgs);

    if (!(qflgs & ERTS_DE_QFLG_REQ_INFO)) {
        qsize = erts_atomic_read_acqb(&dep->qsize);
        ASSERT(qsize >= 0);
        if (qsize > 0)
            receiver = BIF_P->common.id; /* Notify ourselves... */
        else { /* Empty queue; set req-info flag... */
            qflgs = erts_atomic32_read_bor_mb(&dep->qflgs,
                                                  ERTS_DE_QFLG_REQ_INFO);
            qsize = erts_atomic_read_acqb(&dep->qsize);
            ASSERT(qsize >= 0);
            if (qsize > 0) {
                qflgs = erts_atomic32_read_band_mb(&dep->qflgs,
                                                       ~ERTS_DE_QFLG_REQ_INFO);
                if (qflgs & ERTS_DE_QFLG_REQ_INFO)
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
    ErlDrvSizeT size;
    Eterm input_handler;
    Uint32 conn_id;
    Binary *bin = NULL;

    if (is_binary(BIF_ARG_2))
        size = binary_size(BIF_ARG_2);
    else if (is_nil(BIF_ARG_2))
        size = 0;
    else if (is_list(BIF_ARG_2))
        BIF_TRAP2(dist_ctrl_put_data_trap,
                  BIF_P, BIF_ARG_1, BIF_ARG_2);
    else
        BIF_ERROR(BIF_P, BADARG);

    dep = erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id);
    if (!dep)
        BIF_ERROR(BIF_P, BADARG);

    input_handler = (Eterm) erts_atomic_read_nob(&dep->input_handler);

    if (input_handler != BIF_P->common.id)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    erts_atomic64_inc_nob(&dep->in);

    if (size != 0) {
        byte *data, *temp_alloc = NULL;

        if (binary_bitoffset(BIF_ARG_2))
            data = (byte *) erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc);
        else {
            Eterm real_bin;
            ProcBin *proc_bin;
            Uint offset, bitoffs, bitsize;

            ERTS_GET_REAL_BIN(BIF_ARG_2, real_bin, offset, bitoffs, bitsize);
            ASSERT(bitoffs == 0);
            data = binary_bytes(real_bin) + offset;
            proc_bin = (ProcBin *)binary_val(real_bin);
            if (proc_bin->thing_word == HEADER_PROC_BIN)
                bin = proc_bin->val;
        }

        if (!data)
            BIF_ERROR(BIF_P, BADARG);

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
    pend = (Sint64) erts_atomic_read_nob(&dep->qsize);

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
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    Uint32 conn_id;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1, &conn_id) != dep)
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
    Sint reds = initial_reds, obufsize = 0;
    ErtsDistOutputBuf *obuf;
    Eterm *hp, res;
    ProcBin *pb;
    erts_aint_t qsize;
    Uint32 conn_id, get_size;
    Uint hsz = 0, bin_sz;

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
            ASSERT(!dep->transcode_ctx);
            qsize = erts_atomic_read_acqb(&dep->qsize);
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
        obufsize += size_obuf(obuf);
        reds = erts_encode_ext_dist_header_finalize(obuf, dep, dep->flags, reds);
        obufsize -= size_obuf(obuf);
        if (reds < 0) {
            erts_de_runlock(dep);
            if (obufsize)
                erts_atomic_add_nob(&dep->qsize, (erts_aint_t) -obufsize);
            ERTS_BIF_YIELD1(bif_export[BIF_dist_ctrl_get_data_1],
                            BIF_P, BIF_ARG_1);
        }

        dep->tmp_out_queue.first = obuf->next;
        if (!obuf->next)
            dep->tmp_out_queue.last = NULL;
    }

    erts_atomic64_inc_nob(&dep->out);

    erts_de_runlock(dep);

    bin_sz = obuf->ext_endp - obuf->extp + obuf->hdr_endp - obuf->hdrp;

    get_size = dep->opts & ERTS_DIST_CTRL_OPT_GET_SIZE;
    if (get_size) {
        hsz += 3; /* 2 tuple */
        if (!IS_USMALL(0, bin_sz))
            hsz += BIG_UINT_HEAP_SIZE;
    }

    if (!obuf->hdrp) {
        hp =  HAlloc(BIF_P, PROC_BIN_SIZE + hsz);
        pb = (ProcBin *) (char *) hp;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = obuf->ext_endp - obuf->extp;
        pb->next = MSO(BIF_P).first;
        MSO(BIF_P).first = (struct erl_off_heap_header*) pb;
        pb->val = ErtsDistOutputBuf2Binary(obuf);
        pb->bytes = (byte*) obuf->extp;
        pb->flags = 0;
        res = make_binary(pb);
        hp += PROC_BIN_SIZE;
    } else {
        hp =  HAlloc(BIF_P, PROC_BIN_SIZE * 2 + 4 + hsz);
        pb = (ProcBin *) (char *) hp;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = obuf->ext_endp - obuf->extp;
        pb->next = MSO(BIF_P).first;
        MSO(BIF_P).first = (struct erl_off_heap_header*) pb;
        pb->val = ErtsDistOutputBuf2Binary(obuf);
        pb->bytes = (byte*) obuf->extp;
        pb->flags = 0;
        hp += PROC_BIN_SIZE;

        res = CONS(hp, make_binary(pb), NIL);
        hp += 2;

        pb = (ProcBin *) (char *) hp;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = obuf->hdr_endp - obuf->hdrp;
        pb->next = MSO(BIF_P).first;
        MSO(BIF_P).first = (struct erl_off_heap_header*) pb;
        pb->val = ErtsDistOutputBuf2Binary(obuf);
        erts_refc_inc(&pb->val->intern.refc, 1);
        pb->bytes = (byte*) obuf->hdrp;
        pb->flags = 0;
        hp += PROC_BIN_SIZE;
        res = CONS(hp, make_binary(pb), res);
        hp += 2;
    }

    obufsize += size_obuf(obuf);

    qsize = erts_atomic_add_read_nob(&dep->qsize, (erts_aint_t) -obufsize);

    ASSERT(qsize >= 0);

    if (qsize < erts_dist_buf_busy_limit/2
        && (erts_atomic32_read_acqb(&dep->qflgs) & ERTS_DE_QFLG_BUSY)) {
        ErtsProcList *resume_procs = NULL;
        erts_mtx_lock(&dep->qlock);
        resume_procs = get_suspended_on_de(dep, ERTS_DE_QFLG_BUSY);
        erts_mtx_unlock(&dep->qlock);
        if (resume_procs) {
            int resumed = erts_resume_processes(resume_procs);
            reds -= resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
        }
    }

    if (get_size) {
        Eterm sz_term;
        if (IS_USMALL(0, bin_sz))
            sz_term = make_small(bin_sz);
        else {
            sz_term = uint_to_big(bin_sz, hp);
            hp += BIG_UINT_HEAP_SIZE;
        }
        res = TUPLE2(hp, sz_term, res);
    }

    BIF_RET2(res, (initial_reds - reds));
}

void
erts_dist_port_not_busy(Port *prt)
{
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_port_not_busy)) {
        DistEntry *dep = (DistEntry*) erts_prtsd_get(prt, ERTS_PRTSD_DIST_ENTRY);
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
    ASSERT(!(erts_atomic32_read_nob(&dep->qflgs) & ERTS_DE_QFLG_EXIT));
    erts_atomic32_read_bor_nob(&dep->qflgs, ERTS_DE_QFLG_EXIT);
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
		   mon->other.item, mdep->md.target.other.item);
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
    Uint creation;
    int success;

    /* valid creation ? */
    if(!term_to_Uint(BIF_ARG_2, &creation))
	goto error;
    if(creation > 3)
	goto error;

    /* valid node name ? */
    if (!is_node_name_atom(BIF_ARG_1))
	goto error;

    if (BIF_ARG_1 == am_Noname) /* cant use this name !! */
	goto error;
    if (erts_is_alive)     /* must not be alive! */
	goto error;

    /* Check that all trap functions are defined !! */
    if (dmonitor_node_trap->addressv[0] == NULL) {
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
               & !ERTS_PROC_GET_DIST_ENTRY(net_kernel));
    if (success) {
        inc_no_nodes();
        erts_set_this_node(BIF_ARG_1, (Uint32) creation);
        erts_is_alive = 1;
        send_nodes_mon_msgs(NULL, am_nodeup, BIF_ARG_1, am_visible, NIL);
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
 * erts_internal:create_dist_channel/4 is used by
 * erlang:setnode/3.
 */

typedef struct {
    DistEntry *dep;
    Uint flags;
    Uint version;
    Eterm setup_pid;
    Process *net_kernel;
} ErtsSetupConnDistCtrl;

static int
setup_connection_epiloge_rwunlock(Process *c_p, DistEntry *dep,
                                  Eterm ctrlr, Uint flags,
                                  Uint version, Eterm setup_pid,
                                  Process *net_kernel);

static Eterm
setup_connection_distctrl(Process *c_p, void *arg,
                          int *redsp, ErlHeapFragment **bpp);

BIF_RETTYPE erts_internal_create_dist_channel_4(BIF_ALIST_4)
{
    BIF_RETTYPE ret;
    Uint flags;
    Uint version;
    Eterm *hp, res_tag = THE_NON_VALUE, res = THE_NON_VALUE;
    DistEntry *dep = NULL;
    int de_locked = 0;
    Port *pp = NULL;
    int true_nk;
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

    /* Dist flags... */
    if (!is_small(BIF_ARG_3))
        goto badarg;
    flags = unsigned_val(BIF_ARG_3);

    /* Version... */
    if (!is_small(BIF_ARG_4))
        goto badarg;
    version = unsigned_val(BIF_ARG_4);

    if (version == 0)
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
        if (BIF_P->common.id == BIF_ARG_2) {
            ErtsSetupConnDistCtrl scdc;

            scdc.dep = dep;
            scdc.flags = flags;
            scdc.version = version;
            scdc.setup_pid = BIF_P->common.id;
            scdc.net_kernel = net_kernel;

            res = setup_connection_distctrl(BIF_P, &scdc, NULL, NULL);
            /* Dec of refc on net_kernel by setup_connection_distctrl() */
            net_kernel = NULL;
            BUMP_REDS(BIF_P, 5);
            dep = NULL;

            if (res == am_badarg)
                goto badarg;

            ASSERT(is_internal_magic_ref(res));
            res_tag = am_ok; /* Connection up */
        }
        else {
            ErtsSetupConnDistCtrl *scdcp;

            scdcp = erts_alloc(ERTS_ALC_T_SETUP_CONN_ARG,
                               sizeof(ErtsSetupConnDistCtrl));

            scdcp->dep = dep;
            scdcp->flags = flags;
            scdcp->version = version;
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

        erts_atomic32_read_bor_nob(&pp->state, ERTS_PORT_SFLG_DISTRIBUTION);

        erts_prtsd_set(pp, ERTS_PRTSD_DIST_ENTRY, dep);
        erts_prtsd_set(pp, ERTS_PRTSD_CONN_ID, (void*)(UWord)dep->connection_id);

        ASSERT(pp->drv_ptr->outputv || pp->drv_ptr->output);

        dep->send = (pp->drv_ptr->outputv
                     ? dist_port_commandv
                     : dist_port_command);
        ASSERT(dep->send);

        /*
         * Dist-ports do not use the "busy port message queue" functionality, but
         * instead use "busy dist entry" functionality.
        */
        {
            ErlDrvSizeT disable = ERL_DRV_BUSY_MSGQ_DISABLED;
            erl_drv_busy_msgq_limits(ERTS_Port2ErlDrvPort(pp), &disable, NULL);
        }

        conn_id = dep->connection_id;
        set_res = setup_connection_epiloge_rwunlock(BIF_P, dep, BIF_ARG_2, flags,
                                                    version, BIF_P->common.id,
                                                    net_kernel);
        /* Dec of refc on net_kernel by setup_connection_epiloge_rwunlock() */
        net_kernel = NULL;
        if (set_res == 0)
            goto badarg;
        de_locked = 0;

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

 badarg:
    ERTS_BIF_PREP_RET(ret, am_badarg);
    goto done;

 system_limit:
    ERTS_BIF_PREP_RET(ret, am_system_limit);
    goto done;
}

static int
setup_connection_epiloge_rwunlock(Process *c_p, DistEntry *dep,
                                  Eterm ctrlr, Uint flags,
                                  Uint version, Eterm setup_pid,
                                  Process *net_kernel)
{
    Eterm notify_proc = NIL;
    erts_aint32_t qflgs;
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
    
    dep->version = version;
    dep->creation = 0;

    ASSERT(is_internal_port(ctrlr) || is_internal_pid(ctrlr));
    ASSERT(dep->state == ERTS_DE_STATE_PENDING);

    if (flags & DFLAG_DIST_HDR_ATOM_CACHE)
	create_cache(dep);

    erts_set_dist_entry_connected(dep, ctrlr, flags);

    notify_proc = NIL;
    if (erts_atomic_read_nob(&dep->qsize)) {
        if (is_internal_port(dep->cid)) {
            erts_schedule_dist_command(NULL, dep);
        }
        else {
            qflgs = erts_atomic32_read_nob(&dep->qflgs);
            if (qflgs & ERTS_DE_QFLG_REQ_INFO) {
                qflgs = erts_atomic32_read_band_mb(&dep->qflgs,
                                                   ~ERTS_DE_QFLG_REQ_INFO);
                if (qflgs & ERTS_DE_QFLG_REQ_INFO) {
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
			flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			NIL);

    return !0;
}

static Eterm
setup_connection_distctrl(Process *c_p, void *arg, int *redsp, ErlHeapFragment **bpp)
{
    ErtsSetupConnDistCtrl *scdcp = (ErtsSetupConnDistCtrl *) arg;
    DistEntry *dep = scdcp->dep;
    int dep_locked = 0;
    Eterm *hp;
    Uint32 conn_id;
    int dec_net_kernel_on_error = !0;

    if (redsp)
        *redsp = 1;

    if (ERTS_PROC_IS_EXITING(c_p))
        goto badarg;

    erts_de_rwlock(dep);
    dep_locked = !0;

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
                                           scdcp->flags, scdcp->version,
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

    if (dep_locked)
        erts_de_rwunlock(dep);

    erts_deref_dist_entry(dep);

    return am_badarg;
}


BIF_RETTYPE erts_internal_get_dflags_0(BIF_ALIST_0)
{
    return erts_dflags_record;
}

BIF_RETTYPE erts_internal_new_connection_1(BIF_ALIST_1)
{
    DistEntry* dep;
    Uint32 conn_id;
    Eterm* hp;
    Eterm dhandle;

    if (is_not_atom(BIF_ARG_1)) {
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
	erts_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
	dep->send = NULL;

        erts_set_dist_entry_not_connected(dep);
	erts_de_rwunlock(dep);

        schedule_con_monitor_link_seq_cleanup(
            mld, NULL, THE_NON_VALUE,
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
        erts_proc_unlock(net_kernel, nk_locks);
    }

    return 1;
}

/**********************************************************************/
/* node(Object) -> Node */

BIF_RETTYPE node_1(BIF_ALIST_1)
{ 
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

#if 0 /* Done in erlang.erl instead. */
BIF_RETTYPE nodes_0(BIF_ALIST_0)
{
  return nodes_1(BIF_P, am_visible);
}
#endif


BIF_RETTYPE nodes_1(BIF_ALIST_1)
{
    Eterm result;
    int length;
    Eterm* hp;
    int not_connected = 0;
    int visible = 0;
    int hidden = 0;
    int this = 0;
    DeclareTmpHeap(buf,2,BIF_P); /* For one cons-cell */
    DistEntry *dep;
    Eterm arg_list = BIF_ARG_1;
#ifdef DEBUG
    Eterm* endp;
#endif

    UseTmpHeap(2,BIF_P);

    if (is_atom(BIF_ARG_1))
      arg_list = CONS(buf, BIF_ARG_1, NIL);

    while (is_list(arg_list)) {
      switch(CAR(list_val(arg_list))) {
      case am_visible:   visible = 1;                                 break;
      case am_hidden:    hidden = 1;                                  break;
      case am_known:     visible = hidden = not_connected = this = 1; break;
      case am_this:      this = 1;                                    break;
      case am_connected: visible = hidden = 1;                        break;
      default:           goto error;                                  break;
      }
      arg_list = CDR(list_val(arg_list));
    }

    if (is_not_nil(arg_list)) {
	goto error;
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
	goto done;
    }

    hp = HAlloc(BIF_P, 2*length);

#ifdef DEBUG
    endp = hp + length*2;
#endif
    if(not_connected) {
      for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
          if (dep != erts_this_dist_entry) {
            result = CONS(hp, dep->sysname, result);
            hp += 2;
          }
        }
      for(dep = erts_pending_dist_entries; dep; dep = dep->next) {
          result = CONS(hp, dep->sysname, result);
          hp += 2;
      }
    }
    if(hidden)
      for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	result = CONS(hp, dep->sysname, result);
	hp += 2;
      }
    if(visible)
      for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
	result = CONS(hp, dep->sysname, result);
	hp += 2;
      }
    if(this) {
	result = CONS(hp, erts_this_dist_entry->sysname, result);
	hp += 2;
    }
    ASSERT(endp == hp);
    erts_rwmtx_runlock(&erts_dist_table_rwmtx);

done:
    UnUseTmpHeap(2,BIF_P);
    BIF_RET(result);

error:
    UnUseTmpHeap(2,BIF_P);
    BIF_ERROR(BIF_P,BADARG);
}

/**********************************************************************/
/* is_alive() -> Bool */

BIF_RETTYPE is_alive_0(BIF_ALIST_0)
{
    Eterm res = erts_is_alive ? am_true : am_false;
    BIF_RET(res);
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

    for (l = Options; l != NIL && is_list(l); l = CDR(list_val(l))) {
	Eterm t = CAR(list_val(l));
	if (t == am_allow_passive_connect) {
	    /*
	     * Handle this horrible feature by falling back on old synchronous
	     * auto-connect (if needed)
	     */
	    async_connect = 0;
	} else {
	    BIF_ERROR(p, BADARG);
	}
    }
    if (l != NIL) {
	BIF_ERROR(p, BADARG);
    }
    if (l != NIL)
        goto badarg;

    if (is_not_atom(Node))
        goto badarg;

    if (erts_this_node->sysname == am_Noname && Node != am_Noname)
	goto badarg;

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
                if (erts_monitor_dist_delete(&mdep->md.target))
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
                    erts_monitor_dist_insert(&mdep->md.target, dep->mld);
                ASSERT(inserted); (void)inserted;
                ASSERT(mdep->dist->connection_id == dep->connection_id);
            }
            else if (mdep->dist->connection_id != dep->connection_id) {
                ErtsMonitorDataExtended *mdep2;
                ErtsMonitor *mon2;
                int inserted;
                mdep2 = ((ErtsMonitorDataExtended *)
                         erts_monitor_create(ERTS_MON_TYPE_NODE, NIL,
                                             p->common.id, Node, NIL));
                mon2 = &mdep2->md.origin;
                inserted =
                    erts_monitor_dist_insert(&mdep->md.target, dep->mld);
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
    Uint32 f;
    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    de = pid_dist_entry(BIF_ARG_1);
    ASSERT(de != NULL);
    if (de == erts_this_dist_entry) {
	BIF_RET(am_true);
    }
    erts_de_rlock(de);
    f = de->flags;
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
erts_monitor_nodes(Process *c_p, Eterm on, Eterm olist)
{
    Eterm key, old_value, opts_list = olist;
    Uint opts = (Uint) 0;

    ASSERT(c_p);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    if (on != am_true && on != am_false)
	return THE_NON_VALUE;

    if (is_not_nil(opts_list)) {
	int all = 0, visible = 0, hidden = 0;

	while (is_list(opts_list)) {
	    Eterm *cp = list_val(opts_list);
	    Eterm opt = CAR(cp);
	    opts_list = CDR(cp);
	    if (opt == am_nodedown_reason)
		opts |= ERTS_NODES_MON_OPT_DOWN_REASON;
	    else if (is_tuple(opt)) {
		Eterm* tp = tuple_val(opt);
		if (arityval(tp[0]) != 2)
		    return THE_NON_VALUE;
		switch (tp[1]) {
		case am_node_type:
		    switch (tp[2]) {
		    case am_visible:
			if (hidden || all)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPE_VISIBLE;
			visible = 1;
			break;
		    case am_hidden:
			if (visible || all)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPE_HIDDEN;
			hidden = 1;
			break;
		    case am_all:
			if (visible || hidden)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPES;
			all = 1;
			break;
		    default:
			return THE_NON_VALUE;
		    }
		    break;
		default:
		    return THE_NON_VALUE;
		}
	    }
	    else {
		return THE_NON_VALUE;
	    }
	}

	if (is_not_nil(opts_list))
	    return THE_NON_VALUE;
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
            erts_monitor_list_insert(&nodes_monitors, &mdep->md.target);
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
            ASSERT(erts_monitor_is_in_table(&mdep->md.target));
            erts_monitor_list_delete(&nodes_monitors, &mdep->md.target);
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

    ASSERT(omon->type == ERTS_MON_TYPE_NODES);
    ASSERT(erts_monitor_is_origin(omon));

    mdp = erts_monitor_to_data(omon);

    erts_mtx_lock(&nodes_monitors_mtx);
    ASSERT(erts_monitor_is_in_table(&mdp->target));
    ASSERT(no_nodes_monitors > 0);
    no_nodes_monitors--;
    erts_monitor_list_delete(&nodes_monitors, &mdp->target);
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
    ASSERT(mon->type == ERTS_MON_TYPE_NODES);

    ctxt->nmdp[ctxt->i].pid = mon->other.item;
    ctxt->nmdp[ctxt->i].options = mdp->origin.other.item;

    ctxt->i++;
    return 1;
}

static void
send_nodes_mon_msgs(Process *c_p, Eterm what, Eterm node, Eterm type, Eterm reason)
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
        Eterm tmp_heap[3+2+3+2+4 /* max need */];
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

        hsz = 0;
        hp = &tmp_heap[0];

        if (!opts) {
            msg = TUPLE2(hp, what, node);
            hp += 3;
        }
        else {
            Eterm tup;
            Eterm info = NIL;

            if (opts & (ERTS_NODES_MON_OPT_TYPE_VISIBLE
                        | ERTS_NODES_MON_OPT_TYPE_HIDDEN)) {

                tup = TUPLE2(hp, am_node_type, type);
                hp += 3;
                info = CONS(hp, tup, info);
                hp += 2;
            }

            if (what == am_nodedown
                && (opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
                hsz += reason_size;
                tup = TUPLE2(hp, am_nodedown_reason, reason);
                hp += 3;
                info = CONS(hp, tup, info);
                hp += 2;
            }

            msg = TUPLE3(hp, what, node, info);
            hp += 4;
        }

        ASSERT(hp - &tmp_heap[0] <= sizeof(tmp_heap)/sizeof(tmp_heap[0]));

        hsz += hp - &tmp_heap[0];

        erts_proc_sig_send_persistent_monitor_msg(ERTS_MON_TYPE_NODES,
                                                  nmdp[i].options,
                                                  am_system,
                                                  nmdp[i].pid,
                                                  msg,
                                                  hsz);
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
    ASSERT(mon->type == ERTS_MON_TYPE_NODES);
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

    erts_mtx_lock(&nodes_monitors_mtx);

    sz = 0;
    ctxt.szp = &sz;
    ctxt.hpp = NULL;

    while (1) {
        ctxt.res = NIL;

        erts_monitor_list_foreach(nodes_monitors,
                                  nodes_monitor_info,
                                  (void *) &ctxt);

        if (ctxt.hpp)
            break;

	hp = HAlloc(c_p, sz);
#ifdef DEBUG
	hend = hp + sz;
#endif
	ctxt.hpp = &hp;
	ctxt.szp = NULL;
    }

    ASSERT(hp == hend);

    erts_mtx_unlock(&nodes_monitors_mtx);

    erts_thr_progress_unblock();
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

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
