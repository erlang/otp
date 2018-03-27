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
 * which go on the line
 */
#if 0
#define ERTS_DIST_MSG_DBG
#endif
#if 0
#define ERTS_RAW_DIST_MSG_DBG
#endif

#if defined(ERTS_DIST_MSG_DBG) || defined(ERTS_RAW_DIST_MSG_DBG)
static void bw(byte *buf, ErlDrvSizeT sz)
{
    bin_write(ERTS_PRINT_STDERR, NULL, buf, sz);
}
#endif

#ifdef ERTS_DIST_MSG_DBG
static void
dist_msg_dbg(ErtsDistExternal *edep, char *what, byte *buf, int sz)
{
    ErtsHeapFactory factory;
    DeclareTmpHeapNoproc(ctl_default,DIST_CTL_DEFAULT_SIZE);
    Eterm* ctl = ctl_default;
    byte *extp = edep->extp;
    Eterm msg;
    Sint ctl_len;
    Sint size = ctl_len = erts_decode_dist_ext_size(edep);
    if (size < 0) {
	erts_fprintf(stderr,
		     "DIST MSG DEBUG: erts_decode_dist_ext_size(%s) failed:\n",
		     what);
	bw(buf, sz);
    }
    else {
	ErlHeapFragment *mbuf = new_message_buffer(size);
	erts_factory_static_init(&factory, ctl, ctl_len, &mbuf->off_heap);
	msg = erts_decode_dist_ext(&factory, edep);
	if (is_value(msg))
	    erts_fprintf(stderr, "    %s: %T\n", what, msg);
	else {
	    erts_fprintf(stderr,
			 "DIST MSG DEBUG: erts_decode_dist_ext(%s) failed:\n",
			 what);
	    bw(buf, sz);
	}
	free_message_buffer(mbuf);
	edep->extp = extp;
    }
}

#endif



int erts_is_alive; /* System must be blocked on change */
int erts_dist_buf_busy_limit;


/* distribution trap functions */
Export* dmonitor_node_trap = NULL;

/* local variables */
static Export *dist_ctrl_put_data_trap;

/* forward declarations */

static void clear_dist_entry(DistEntry*);
static int dsig_send_ctl(ErtsDSigData* dsdp, Eterm ctl, int force_busy);
static void send_nodes_mon_msgs(Process *, Eterm, Eterm, Eterm, Eterm);
static void init_nodes_monitors(void);
static Sint abort_connection(DistEntry* dep, Uint32 conn_id);

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

#define ERTS_MON_LNK_FIRE_LIMIT 100

static void monitor_connection_down(ErtsMonitor *mon, void *unused)
{
    if (erts_monitor_is_origin(mon))
        erts_proc_sig_send_demonitor(mon);
    else
        erts_proc_sig_send_monitor_down(mon, am_noconnection);
}

static void link_connection_down(ErtsLink *lnk, void *vdist)
{
    erts_proc_sig_send_link_exit(NULL, THE_NON_VALUE, lnk,
                                 am_noconnection, NIL);
}

typedef enum {
    ERTS_CML_CLEANUP_STATE_LINKS,
    ERTS_CML_CLEANUP_STATE_MONITORS,
    ERTS_CML_CLEANUP_STATE_ONAME_MONITORS,
    ERTS_CML_CLEANUP_STATE_NODE_MONITORS
} ErtsConMonLnkCleaupState;

typedef struct {
    ErtsConMonLnkCleaupState state;
    ErtsMonLnkDist *dist;
    void *yield_state;
    int trigger_node_monitors;
    Eterm nodename;
    Eterm visability;
    Eterm reason;
    ErlOffHeap oh;
    Eterm heap[1];
} ErtsConMonLnkCleanup;

static void
con_monitor_link_cleanup(void *vcmlcp)
{
    ErtsConMonLnkCleanup *cmlcp = vcmlcp;
    ErtsMonLnkDist *dist = cmlcp->dist;
    ErtsSchedulerData *esdp;
    int yield;

    switch (cmlcp->state) {
    case ERTS_CML_CLEANUP_STATE_LINKS:
        yield = erts_link_list_foreach_delete_yielding(&dist->links,
                                                       link_connection_down,
                                                       NULL, &cmlcp->yield_state,
                                                       ERTS_MON_LNK_FIRE_LIMIT);
        if (yield)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_MONITORS;
    case ERTS_CML_CLEANUP_STATE_MONITORS:
        yield = erts_monitor_list_foreach_delete_yielding(&dist->monitors,
                                                          monitor_connection_down,
                                                          NULL, &cmlcp->yield_state,
                                                          ERTS_MON_LNK_FIRE_LIMIT);
        if (yield)
            break;

        ASSERT(!cmlcp->yield_state);
        cmlcp->state = ERTS_CML_CLEANUP_STATE_ONAME_MONITORS;
    case ERTS_CML_CLEANUP_STATE_ONAME_MONITORS:
        yield = erts_monitor_tree_foreach_delete_yielding(&dist->orig_name_monitors,
                                                          monitor_connection_down,
                                                          NULL, &cmlcp->yield_state,
                                                          ERTS_MON_LNK_FIRE_LIMIT/2);
        if (yield)
            break;

        cmlcp->dist = NULL;
        erts_mon_link_dist_dec_refc(dist);

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
                                con_monitor_link_cleanup,
                                (void *) cmlcp);
}

static void
schedule_con_monitor_link_cleanup(ErtsMonLnkDist *dist,
                                  Eterm nodename,
                                  Eterm visability,
                                  Eterm reason)
{
    if (dist || is_value(nodename)) {
        ErtsSchedulerData *esdp;
        ErtsConMonLnkCleanup *cmlcp;
        Uint rsz, size;

        size = sizeof(ErtsConMonLnkCleanup);

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
                                    con_monitor_link_cleanup,
                                    (void *) cmlcp);
    }
}

/*
** A full node name constists of a "n@h"
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
                abort_connection(pending[i], pending[i]->connection_id);
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
	Uint32 flags;

	erts_atomic_set_mb(&dep->dist_cmd_scheduled, 1);
	erts_de_rwlock(dep);

        if (is_internal_port(dep->cid)) {
            ERTS_LC_ASSERT(erts_lc_is_port_locked(erts_port_lookup_raw(dep->cid)));

            if (erts_port_task_is_scheduled(&dep->dist_cmd))
                erts_port_task_abort(&dep->dist_cmd);
        }

	if (dep->state == ERTS_DE_STATE_EXITING) {
#ifdef DEBUG
	    ASSERT(erts_atomic32_read_nob(&dep->qflgs) & ERTS_DE_QFLG_EXIT);
#endif
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

	nodename = dep->sysname;
	flags = dep->flags;

	erts_set_dist_entry_not_connected(dep);

	erts_de_rwunlock(dep);

        schedule_con_monitor_link_cleanup(mld,
                                          nodename,
                                          (flags & DFLAG_PUBLISHED
                                           ? am_visible
                                           : am_hidden),
                                          (reason == am_normal
                                           ? am_connection_closed
                                           : reason));

	clear_dist_entry(dep);
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

#define ErtsDistOutputBuf2Binary(OB) \
  ((Binary *) (((char *) (OB)) - offsetof(Binary, orig_bytes)))

static ERTS_INLINE ErtsDistOutputBuf *
alloc_dist_obuf(Uint size)
{
    ErtsDistOutputBuf *obuf;
    Uint obuf_size = sizeof(ErtsDistOutputBuf)+sizeof(byte)*(size-1);
    Binary *bin = erts_bin_drv_alloc(obuf_size);
    obuf = (ErtsDistOutputBuf *) &bin->orig_bytes[0];
#ifdef DEBUG
    obuf->dbg_pattern = ERTS_DIST_OUTPUT_BUF_DBG_PATTERN;
    obuf->alloc_endp = obuf->data + size;
    ASSERT(bin == ErtsDistOutputBuf2Binary(obuf));
#endif
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
    Binary *bin = ErtsDistOutputBuf2Binary(obuf);
    return bin->orig_size;
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

static void clear_dist_entry(DistEntry *dep)
{
    ErtsAtomCache *cache;
    ErtsProcList *suspendees;
    ErtsDistOutputBuf *obuf;

    erts_de_rwlock(dep);
    erts_atomic_set_nob(&dep->input_handler,
                        (erts_aint_t) NIL);
    cache = dep->cache;
    dep->cache = NULL;

    erts_mtx_lock(&dep->qlock);

    erts_atomic64_set_nob(&dep->in, 0);
    erts_atomic64_set_nob(&dep->out, 0);

    obuf = clear_de_out_queues(dep);
    dep->state = ERTS_DE_STATE_IDLE;
    suspendees = get_suspended_on_de(dep, ERTS_DE_QFLGS_ALL);

    erts_mtx_unlock(&dep->qlock);
    erts_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
    dep->send = NULL;
    erts_de_rwunlock(dep);

    erts_resume_processes(suspendees);

    delete_cache(cache);

    free_de_out_queues(dep, obuf);
    if (dep->transcode_ctx)
        transcode_free_ctx(dep);
}

int erts_dsend_context_dtor(Binary* ctx_bin)
{
    ErtsSendContext* ctx = ERTS_MAGIC_BIN_DATA(ctx_bin);
    switch (ctx->dss.phase) {
    case ERTS_DSIG_SEND_PHASE_MSG_SIZE:
	DESTROY_SAVED_WSTACK(&ctx->dss.u.sc.wstack);
	break;
    case ERTS_DSIG_SEND_PHASE_MSG_ENCODE:
	DESTROY_SAVED_WSTACK(&ctx->dss.u.ec.wstack);
	break;
    default:;
    }
    if (ctx->dss.phase >= ERTS_DSIG_SEND_PHASE_ALLOC && ctx->dss.obuf) {
	free_dist_obuf(ctx->dss.obuf);
    }
    if (ctx->deref_dep)
	erts_deref_dist_entry(ctx->dep);

    return 1;
}

Eterm erts_dsend_export_trap_context(Process* p, ErtsSendContext* ctx)
{
    struct exported_ctx {
	ErtsSendContext ctx;
	ErtsAtomCacheMap acm;
    };
    Binary* ctx_bin = erts_create_magic_binary(sizeof(struct exported_ctx),
					       erts_dsend_context_dtor);
    struct exported_ctx* dst = ERTS_MAGIC_BIN_DATA(ctx_bin);
    Eterm* hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);

    sys_memcpy(&dst->ctx, ctx, sizeof(ErtsSendContext));
    ASSERT(ctx->dss.ctl == make_tuple(ctx->ctl_heap));
    dst->ctx.dss.ctl = make_tuple(dst->ctx.ctl_heap);
    if (ctx->dss.acmp) {
	sys_memcpy(&dst->acm, ctx->dss.acmp, sizeof(ErtsAtomCacheMap));
	dst->ctx.dss.acmp = &dst->acm;
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
erts_dsig_send_link(ErtsDSigData *dsdp, Eterm local, Eterm remote)
{
    DeclareTmpHeapNoproc(ctl_heap,4);
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_LINK), local, remote);
    int res;
    UseTmpHeapNoproc(4);

    res = dsig_send_ctl(dsdp, ctl, 0);
    UnUseTmpHeapNoproc(4);
    return res;
}

int
erts_dsig_send_unlink(ErtsDSigData *dsdp, Eterm local, Eterm remote)
{
    DeclareTmpHeapNoproc(ctl_heap,4);
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_UNLINK), local, remote);
    int res;

    UseTmpHeapNoproc(4);
    res = dsig_send_ctl(dsdp, ctl, 0);
    UnUseTmpHeapNoproc(4);
    return res;
}


/* A local process that's being monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason} */
int
erts_dsig_send_m_exit(ErtsDSigData *dsdp, Eterm watcher, Eterm watched, 
			  Eterm ref, Eterm reason)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,6);
    int res;

    if (~dsdp->dep->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_MONITOR_P_EXIT (see dsig_send_monitor)
         */
        return ERTS_DSIG_SEND_OK;
    }

    UseTmpHeapNoproc(6);

    ctl = TUPLE5(&ctl_heap[0], make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

    res = dsig_send_ctl(dsdp, ctl, 1);
    UnUseTmpHeapNoproc(6);
    return res;
}

/* We want to monitor a process (named or unnamed) on another node, we send:
   {DOP_MONITOR_P, Local pid, Remote pid or name, Ref}, which is exactly what's
   needed on the other side... */
int
erts_dsig_send_monitor(ErtsDSigData *dsdp, Eterm watcher, Eterm watched,
		       Eterm ref)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,5);
    int res;

    if (~dsdp->dep->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_MONITOR_P.
         * Just avoid sending it and by doing that reduce this monitor
         * to only supervise the connection. This will work for simple c-nodes
         * with a 1-to-1 relation between "Erlang process" and OS-process.
         */
        return ERTS_DSIG_SEND_OK;
    }

    UseTmpHeapNoproc(5);
    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    res = dsig_send_ctl(dsdp, ctl, 0);
    UnUseTmpHeapNoproc(5);
    return res;
}

/* A local process monitoring a remote one wants to stop monitoring, either 
   because of a demonitor bif call or because the local process died. We send
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref} */
int
erts_dsig_send_demonitor(ErtsDSigData *dsdp, Eterm watcher,
			 Eterm watched, Eterm ref, int force)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,5);
    int res;

    if (~dsdp->dep->flags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME)) {
        /*
         * Receiver does not support DOP_DEMONITOR_P (see dsig_send_monitor)
         */
        return ERTS_DSIG_SEND_OK;
    }

    UseTmpHeapNoproc(5);
    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    res = dsig_send_ctl(dsdp, ctl, force);
    UnUseTmpHeapNoproc(5);
    return res;
}

static int can_send_seqtrace_token(ErtsSendContext* ctx, Eterm token) {
    Eterm label;

    if (ctx->dep->flags & DFLAG_BIG_SEQTRACE_LABELS) {
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
erts_dsig_send_msg(Eterm remote, Eterm message, ErtsSendContext* ctx)
{
    Eterm ctl;
    Eterm token = NIL;
    Process *sender = ctx->dsd.proc;
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
                      "%T", ctx->dsd.dep->sysname);
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

        if (ctx->dep->flags & DFLAG_SEND_SENDER) {
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
    ctx->dss.ctl = ctl;
    ctx->dss.msg = message;
    ctx->dss.force_busy = 0;
    res = erts_dsig_send(&ctx->dsd, &ctx->dss);
    return res;
}

int
erts_dsig_send_reg_msg(Eterm remote_name, Eterm message,
		       ErtsSendContext* ctx)
{
    Eterm ctl;
    Eterm token = NIL;
    Process *sender = ctx->dsd.proc;
    int res;
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
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote_name, sender);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *receiver_name = '\0';
    if (DTRACE_ENABLED(message_send) || DTRACE_ENABLED(message_send_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", ctx->dsd.dep->sysname);
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
    ctx->dss.ctl = ctl;
    ctx->dss.msg = message;
    ctx->dss.force_busy = 0;
    res = erts_dsig_send(&ctx->dsd, &ctx->dss);
    return res;
}

/* local has died, deliver the exit signal to remote */
int
erts_dsig_send_exit_tt(ErtsDSigData *dsdp, Eterm local, Eterm remote, 
		       Eterm reason, Eterm token)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,6);
    int res;
#ifdef USE_VM_PROBES
    Process *sender = dsdp->proc;
    Sint tok_label = 0;
    Sint tok_lastcnt = 0;
    Sint tok_serial = 0;
    DTRACE_CHARBUF(node_name, 64);
    DTRACE_CHARBUF(sender_name, 64);
    DTRACE_CHARBUF(remote_name, 128);
    DTRACE_CHARBUF(reason_str, 128);
#endif

    UseTmpHeapNoproc(6);
    if (have_seqtrace(token)) {
	seq_trace_update_send(dsdp->proc);
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
	ctl = TUPLE5(&ctl_heap[0],
		     make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
	ctl = TUPLE4(&ctl_heap[0], make_small(DOP_EXIT), local, remote, reason);
    }
#ifdef USE_VM_PROBES
    *node_name = *sender_name = *remote_name = '\0';
    if (DTRACE_ENABLED(process_exit_signal_remote)) {
        erts_snprintf(node_name, sizeof(DTRACE_CHARBUF_NAME(node_name)),
                      "%T", dsdp->dep->sysname);
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
    /* forced, i.e ignore busy */
    res = dsig_send_ctl(dsdp, ctl, 1);
    UnUseTmpHeapNoproc(6);
    return res;
}

int
erts_dsig_send_exit(ErtsDSigData *dsdp, Eterm local, Eterm remote, Eterm reason)
{
    DeclareTmpHeapNoproc(ctl_heap,5);
    int res;
    Eterm ctl;

    UseTmpHeapNoproc(5);
    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_EXIT), local, remote, reason);
    /* forced, i.e ignore busy */
    res =  dsig_send_ctl(dsdp, ctl, 1);
    UnUseTmpHeapNoproc(5);
    return res;
}

int
erts_dsig_send_exit2(ErtsDSigData *dsdp, Eterm local, Eterm remote, Eterm reason)
{
    DeclareTmpHeapNoproc(ctl_heap,5);
    int res;
    Eterm ctl;

    UseTmpHeapNoproc(5);
    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_EXIT2), local, remote, reason);

    res = dsig_send_ctl(dsdp, ctl, 0);
    UnUseTmpHeapNoproc(5);
    return res;
}


int
erts_dsig_send_group_leader(ErtsDSigData *dsdp, Eterm leader, Eterm remote)
{
    DeclareTmpHeapNoproc(ctl_heap,4);
    int res;
    Eterm ctl;

    UseTmpHeapNoproc(4);
    ctl = TUPLE3(&ctl_heap[0],
		 make_small(DOP_GROUP_LEADER), leader, remote);

    res = dsig_send_ctl(dsdp, ctl, 0);
    UnUseTmpHeapNoproc(4);
    return res;
}

#if defined(PURIFY)
#  define PURIFY_MSG(msg) \
    purify_printf("%s, line %d: %s", __FILE__, __LINE__, msg)
#elif defined(VALGRIND)
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>

#ifndef HAVE_VALGRIND_PRINTF_XML
#define VALGRIND_PRINTF_XML VALGRIND_PRINTF
#endif

#  define PURIFY_MSG(msg)                                                    \
    do {								     \
	char buf__[1]; size_t bufsz__ = sizeof(buf__);			     \
	if (erts_sys_explicit_8bit_getenv("VALGRIND_LOG_XML", buf__, &bufsz__) >= 0) { \
	    VALGRIND_PRINTF_XML("<erlang_error_log>"			     \
			    "%s, line %d: %s</erlang_error_log>\n",	     \
			    __FILE__, __LINE__, msg);			     \
	} else {							     \
	    VALGRIND_PRINTF("%s, line %d: %s", __FILE__, __LINE__, msg);     \
	}								     \
    } while (0)
#else
#  define PURIFY_MSG(msg)
#endif

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
		     byte *hbuf,
		     ErlDrvSizeT hlen,
		     byte *buf,
		     ErlDrvSizeT len)
{
    ErtsDistExternal ede;
    byte *t;
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
    Eterm* hp;
    Sint type;
    Eterm token;
    Eterm token_size;
    Uint tuple_arity;
    int res;
    Uint32 connection_id;
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
    erts_fprintf(stderr, "<< ");
    bw(buf, len);
#endif

    if (dep->flags & DFLAG_DIST_HDR_ATOM_CACHE)
	t = buf;
    else {
	/* Skip PASS_THROUGH */
	t = buf+1;
	len--;
    }

    res = erts_prepare_dist_ext(&ede, t, len, dep, dep->cache, &connection_id);

    switch (res) {
    case ERTS_PREP_DIST_EXT_CLOSED:
        return 0; /* Connection not alive; ignore signal... */
    case ERTS_PREP_DIST_EXT_FAILED:
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(stderr, "DIST MSG DEBUG: erts_prepare_dist_ext() failed:\n");
	bw(buf, orig_len);
#endif
        goto data_error;
    case ERTS_PREP_DIST_EXT_SUCCESS:
	ctl_len = erts_decode_dist_ext_size(&ede);
        if (ctl_len < 0) {
#ifdef ERTS_DIST_MSG_DBG
            erts_fprintf(stderr, "DIST MSG DEBUG: erts_decode_dist_ext_size(CTL) failed:\n");
            bw(buf, orig_len);
#endif
            PURIFY_MSG("data error");
            goto data_error;
        }
        break;
    default:
        ERTS_INTERNAL_ERROR("Unexpected result from erts_prepare_dist_ext()");
        break;
    }

    if (ctl_len > DIST_CTL_DEFAULT_SIZE) {
	ctl = erts_alloc(ERTS_ALC_T_DCTRL_BUF, ctl_len * sizeof(Eterm));
    }
    hp = ctl;

    erts_factory_tmp_init(&factory, ctl, ctl_len, ERTS_ALC_T_DCTRL_BUF);
    arg = erts_decode_dist_ext(&factory, &ede);
    if (is_non_value(arg)) {
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(stderr, "DIST MSG DEBUG: erts_decode_dist_ext(CTL) failed:\n");
	bw(buf, orig_len);
#endif
	PURIFY_MSG("data error");
	goto decode_error;
    }
    ctl_len = t - buf;

#ifdef ERTS_DIST_MSG_DBG
    erts_fprintf(stderr, "<<%s CTL: %T\n", len != orig_len ? "P" : " ", arg);
#endif

    if (is_not_tuple(arg) || 
	(tuple = tuple_val(arg), (tuple_arity = arityval(*tuple)) < 1) ||
	is_not_small(tuple[1])) {
 	goto invalid_message;
    }

    token_size = 0;
    token = NIL;

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK: {
        ErtsDSigData dsd;
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
#ifdef DEBUG
            code =
#endif
                erts_link_dist_insert(&ldp->a, dep->mld);
            ASSERT(code);

            if (erts_proc_sig_send_link(NULL, to, &ldp->b))
                break; /* done */

            /* Failed to send signal; cleanup and reply noproc... */

#ifdef DEBUG
            code =
#endif
                erts_link_dist_delete(&ldp->a);
            ASSERT(code);
            erts_link_release_both(ldp);
        }

        code = erts_dsig_prepare(&dsd, dep, NULL, 0, ERTS_DSP_NO_LOCK, 0, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED) {
            code = erts_dsig_send_exit(&dsd, to, from, am_noproc);
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
        ErtsDSigData dsd;
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

#ifdef DEBUG
            code =
#endif
                erts_monitor_dist_insert(&mdp->origin, dep->mld);
            ASSERT(code);

            if (erts_proc_sig_send_monitor(&mdp->target, pid))
                break; /* done */

            /* Failed to send to local proc; cleanup reply noproc... */

#ifdef DEBUG
            code =
#endif
                erts_monitor_dist_delete(&mdp->origin);
            ASSERT(code);
            erts_monitor_release_both(mdp);

        }

        code = erts_dsig_prepare(&dsd, dep, NULL, 0, ERTS_DSP_NO_LOCK, 0, 0);
        if (code == ERTS_DSIG_PREP_CONNECTED) {
            code = erts_dsig_send_m_exit(&dsd, watcher, watched, ref,
                                         am_noproc);
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

	token_size = size_object(tuple[5]);
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
	dist_msg_dbg(&ede, "MSG", buf, orig_len);
#endif

	from = tuple[2];
	to = tuple[4];
	if (is_not_pid(from) || is_not_atom(to)){
	    goto invalid_message;
	}
	rp = erts_whereis_process(NULL, 0, to, 0, 0);
	if (rp) {
	    Uint xsize = (type == DOP_REG_SEND
			  ? 0
			  : ERTS_HEAP_FRAG_SIZE(token_size));
	    ErtsProcLocks locks = 0;
	    ErtsDistExternal *ede_copy;

	    ede_copy = erts_make_dist_ext_copy(&ede, xsize);
	    if (type == DOP_REG_SEND) {
		token = NIL;
	    } else {
		ErlHeapFragment *heap_frag;
		ErlOffHeap *ohp;
		ASSERT(xsize);
		heap_frag = erts_dist_ext_trailer(ede_copy);
		ERTS_INIT_HEAP_FRAG(heap_frag, token_size, token_size);
		hp = heap_frag->mem;
		ohp = &heap_frag->off_heap;
		token = tuple[5];
		token = copy_struct(token, token_size, &hp, ohp);
	    }

	    erts_queue_dist_message(rp, locks, ede_copy, token, from);
	    if (locks)
		erts_proc_unlock(rp, locks);
	}
	break;

    case DOP_SEND_SENDER_TT: {
        Uint xsize;
    case DOP_SEND_TT:

	if (tuple_arity != 4) {
	    goto invalid_message;
	}

	token = tuple[4];
	token_size = size_object(token);
        xsize = ERTS_HEAP_FRAG_SIZE(token_size);
        goto send_common;

    case DOP_SEND_SENDER:
    case DOP_SEND:

        token = NIL;
        xsize = 0;
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
	dist_msg_dbg(&ede, "MSG", buf, orig_len);
#endif
	to = tuple[3];
	if (is_not_pid(to)) {
	    goto invalid_message;
	}
	rp = erts_proc_lookup(to);
	if (rp) {
	    ErtsProcLocks locks = 0;
	    ErtsDistExternal *ede_copy;

	    ede_copy = erts_make_dist_ext_copy(&ede, xsize);
	    if (is_not_nil(token)) {
		ErlHeapFragment *heap_frag;
		ErlOffHeap *ohp;
		ASSERT(xsize);
		heap_frag = erts_dist_ext_trailer(ede_copy);
		ERTS_INIT_HEAP_FRAG(heap_frag, token_size, token_size);
		hp = heap_frag->mem;
		ohp = &heap_frag->off_heap;
		token = copy_struct(token, token_size, &hp, ohp);
	    }

	    erts_queue_dist_message(rp, locks, ede_copy, token, am_Empty);
	    if (locks)
		erts_proc_unlock(rp, locks);
	}
	break;
    }

    case DOP_MONITOR_P_EXIT: {
	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */
	   
	if (tuple_arity != 5) {
	    goto invalid_message;
	}

	watched = tuple[2];  /* remote proc or name which died */
	watcher = tuple[3];
	ref     = tuple[4];
	reason  = tuple[5];

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

        erts_proc_sig_send_dist_monitor_down(dep, ref, watched,
                                             watcher, reason);
	break;
    }

    case DOP_EXIT_TT:
    case DOP_EXIT: {
	/* 'from', which 'to' is linked to, died */
	if (type == DOP_EXIT) {
	    if (tuple_arity != 4) {
		goto invalid_message;
	    }
	    
	    from = tuple[2];
	    to = tuple[3];
	    reason = tuple[4];
	    token = NIL;
	} else {
	    if (tuple_arity != 5) {
		goto invalid_message;
	    }
	    from = tuple[2];
	    to = tuple[3];
	    token = tuple[4];
	    reason = tuple[5];
	}
	if (is_not_external_pid(from)
            || dep != external_pid_dist_entry(from)
            || is_not_internal_pid(to)) {
	    goto invalid_message;
	}

        erts_proc_sig_send_dist_link_exit(dep,
                                          from, to,
                                          reason, token);
	break;
    }
    case DOP_EXIT2_TT:
    case DOP_EXIT2:
	/* 'from' is send an exit signal to 'to' */
	if (type == DOP_EXIT2) {
	    if (tuple_arity != 4) {
		goto invalid_message;
	    }
	    from = tuple[2];
	    to = tuple[3];
	    reason = tuple[4];
	    token = NIL;
	} else {
	    if (tuple_arity != 5) {
		goto invalid_message;
	    }
	    from = tuple[2];
	    to = tuple[3];
	    token = tuple[4];
	    reason = tuple[5];
	}
	if (is_not_pid(from) || is_not_internal_pid(to)) {
	    goto invalid_message;
	}

        erts_proc_sig_send_exit(NULL, from, to, reason, token, 0);
	break;

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

    erts_factory_close(&factory);
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
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
    erts_factory_close(&factory);
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
data_error:
    UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
    erts_kill_dist_connection(dep, connection_id);
    ERTS_CHK_NO_PROC_LOCKS;
    return -1;
}

static int dsig_send_ctl(ErtsDSigData* dsdp, Eterm ctl, int force_busy)
{
    struct erts_dsig_send_context ctx;
    int ret;
    ctx.ctl = ctl;
    ctx.msg = THE_NON_VALUE;
    ctx.force_busy = force_busy;
    ctx.phase = ERTS_DSIG_SEND_PHASE_INIT;
#ifdef DEBUG
    ctx.reds = 1; /* provoke assert below (no reduction count without msg) */
#endif
    ret = erts_dsig_send(dsdp, &ctx);
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
erts_dsig_send(ErtsDSigData *dsdp, struct erts_dsig_send_context* ctx)
{
    int retval;
    Sint initial_reds = ctx->reds;
    Eterm cid;

    while (1) {
	switch (ctx->phase) {
	case ERTS_DSIG_SEND_PHASE_INIT:
	    ctx->flags = dsdp->dep->flags;
	    ctx->c_p = dsdp->proc;

	    if (!ctx->c_p || dsdp->no_suspend)
		ctx->force_busy = 1;

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
            erts_fprintf(stderr, ">> CTL: %T\n", ctx->ctl);
            if (is_value(ctx->msg))
                erts_fprintf(stderr, "    MSG: %T\n", ctx->msg);
    #endif

	    ctx->data_size = ctx->max_finalize_prepend;
	    erts_reset_atom_cache_map(ctx->acmp);
	    erts_encode_dist_ext_size(ctx->ctl, ctx->flags, ctx->acmp, &ctx->data_size);

	    if (is_non_value(ctx->msg)) {
                ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
                break;
            }
            ctx->u.sc.wstack.wstart = NULL;
            ctx->u.sc.flags = ctx->flags;
            ctx->u.sc.level = 0;

            ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_SIZE;
	case ERTS_DSIG_SEND_PHASE_MSG_SIZE:
	    if (erts_encode_dist_ext_size_int(ctx->msg, ctx, &ctx->data_size)) {
		retval = ERTS_DSIG_SEND_CONTINUE;
		goto done;
	    }

	    ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
	case ERTS_DSIG_SEND_PHASE_ALLOC:
	    erts_finalize_atom_cache_map(ctx->acmp, ctx->flags);

	    ctx->dhdr_ext_size = erts_encode_ext_dist_header_size(ctx->acmp);
	    ctx->data_size += ctx->dhdr_ext_size;

	    ctx->obuf = alloc_dist_obuf(ctx->data_size);
	    ctx->obuf->ext_endp = &ctx->obuf->data[0] + ctx->max_finalize_prepend + ctx->dhdr_ext_size;

	    /* Encode internal version of dist header */
	    ctx->obuf->extp = erts_encode_ext_dist_header_setup(ctx->obuf->ext_endp, ctx->acmp);
	    /* Encode control message */
	    erts_encode_dist_ext(ctx->ctl, &ctx->obuf->ext_endp, ctx->flags, ctx->acmp, NULL, NULL);
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
	    if (erts_encode_dist_ext(ctx->msg, &ctx->obuf->ext_endp, ctx->flags, ctx->acmp, &ctx->u.ec, &ctx->reds)) {
		retval = ERTS_DSIG_SEND_CONTINUE;
		goto done;
	    }

	    ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
	case ERTS_DSIG_SEND_PHASE_FIN: {
	    DistEntry *dep = dsdp->dep;
	    int suspended = 0;
	    int resume = 0;

	    ASSERT(ctx->obuf->extp < ctx->obuf->ext_endp);
	    ASSERT(&ctx->obuf->data[0] <= ctx->obuf->extp - ctx->max_finalize_prepend);
	    ASSERT(ctx->obuf->ext_endp <= &ctx->obuf->data[0] + ctx->data_size);

	    ctx->data_size = ctx->obuf->ext_endp - ctx->obuf->extp;

            ctx->obuf->hopefull_flags = ctx->u.ec.hopefull_flags;
	    /*
	     * Signal encoded; now verify that the connection still exists,
	     * and if so enqueue the signal and schedule it for send.
	     */
	    ctx->obuf->next = NULL;
	    erts_de_rlock(dep);
	    cid = dep->cid;
	    if (dep->state == ERTS_DE_STATE_EXITING
                || dep->state == ERTS_DE_STATE_IDLE
                || dep->connection_id != dsdp->connection_id) {
		/* Not the same connection as when we started; drop message... */
		erts_de_runlock(dep);
		free_dist_obuf(ctx->obuf);
	    }
	    else {
                Sint qsize;
                erts_aint32_t qflgs;
		ErtsProcList *plp = NULL;
                Eterm notify_proc = NIL;
                Sint obsz = size_obuf(ctx->obuf);

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
		if (!ctx->force_busy && (qflgs & ERTS_DE_QFLG_BUSY)) {
		    erts_mtx_unlock(&dep->qlock);

		    plp = erts_proclist_create(ctx->c_p);
		    erts_suspend(ctx->c_p, ERTS_PROC_LOCK_MAIN, NULL);
		    suspended = 1;
		    erts_mtx_lock(&dep->qlock);
		}

		/* Enqueue obuf on dist entry */
		if (dep->out_queue.last)
		    dep->out_queue.last->next = ctx->obuf;
		else
		    dep->out_queue.first = ctx->obuf;
		dep->out_queue.last = ctx->obuf;

		if (!ctx->force_busy) {
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
        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", prt->dist_entry->sysname);
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
    ErlDrvSizeT size;
    SysIOVec iov[2];
    ErlDrvBinary* bv[2];
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
        size = obuf->ext_endp - obuf->extp;
        eiov.vsize = 2;

        iov[1].iov_base = obuf->extp;
        iov[1].iov_len = size;
        bv[1] = Binary2ErlDrvBinary(ErtsDistOutputBuf2Binary(obuf));
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
        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", prt->dist_entry->sysname);
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
    DistEntry *dep = prt->dist_entry;
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
#ifdef ERTS_RAW_DIST_MSG_DBG
            erts_fprintf(stderr, ">> ");
            bw(foq.first->extp, size);
#endif
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
		reds = erts_encode_ext_dist_header_finalize(ob, dep, flags, reds);
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
            reds = erts_encode_ext_dist_header_finalize(oq.first, dep, flags, reds);
            if (reds < 0) {
                preempt = 1;
                break;
            }
	    ASSERT(&oq.first->data[0] <= oq.first->extp
		   && oq.first->extp <= oq.first->ext_endp);
	    size = (*send)(prt, oq.first);
            erts_atomic64_inc_nob(&dep->out);
	    esdp->io.out += (Uint64) size;
#ifdef ERTS_RAW_DIST_MSG_DBG
            erts_fprintf(stderr, ">> ");
            bw(oq.first->extp, size);
#endif
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

#if 0

int
dist_data_finalize(Process *c_p, int reds_limit)
{
    int reds = 5;
    DistEntry *dep = ;
    ErtsDistOutputQueue oq, foq;
    ErtsDistOutputBuf *ob;
    int preempt;


    erts_mtx_lock(&dep->qlock);
    flags = dep->flags;
    oq.first = dep->out_queue.first;
    oq.last = dep->out_queue.last;
    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    erts_mtx_unlock(&dep->qlock);

    if (!oq.first) {
        ASSERT(!oq.last);
        oq.first = dep->tmp_out_queue.first;
        oq.last = dep->tmp_out_queue.last;
    }
    else {
        ErtsDistOutputBuf *f, *l;
        ASSERT(oq.last);
        if (dep->tmp_out_queue.last) {
            dep->tmp_out_queue.last->next = oq.first;
            oq.first = dep->tmp_out_queue.first;
        }
    }

    if (!oq.first) {
        /* Nothing to do... */
        ASSERT(!oq.last);
        return reds;
    }

    foq.first = dep->finalized_out_queue.first;
    foq.last = dep->finalized_out_queue.last;

    preempt = 0;
    ob = oq.first;
    ASSERT(ob);

    do {
        ob->extp = erts_encode_ext_dist_header_finalize(ob->extp,
                                                        dep->cache,
                                                        flags);
        if (!(flags & DFLAG_DIST_HDR_ATOM_CACHE))
            *--ob->extp = PASS_THROUGH; /* Old node; 'pass through'
                                           needed */
        ASSERT(&ob->data[0] <= ob->extp && ob->extp < ob->ext_endp);
        reds += ERTS_PORT_REDS_DIST_CMD_FINALIZE;
        preempt = reds > reds_limit;
        if (preempt)
            break;
        ob = ob->next;
    } while (ob);
    /*
     * At least one buffer was finalized; if we got preempted,
     * ob points to the last buffer that we finalized.
     */
    if (foq.last)
        foq.last->next = oq.first;
    else
        foq.first = oq.first;
    if (!preempt) {
        /* All buffers finalized */
        foq.last = oq.last;
        oq.first = oq.last = NULL;
    }
    else {
        /* Not all buffers finalized; split oq. */
        foq.last = ob;
        oq.first = ob->next;
        if (oq.first)
            ob->next = NULL;
        else
            oq.last = NULL;
    }

    dep->finalized_out_queue.first = foq.first;
    dep->finalized_out_queue.last = foq.last;
    dep->tmp_out_queue.first = oq.first;
    dep->tmp_out_queue.last = oq.last;

    return reds;
}

#endif

BIF_RETTYPE
dist_ctrl_get_data_notification_1(BIF_ALIST_1)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    erts_aint32_t qflgs;
    erts_aint_t qsize;
    Eterm receiver = NIL;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1) != dep)
        BIF_ERROR(BIF_P, BADARG);

    /*
     * Caller is the only one that can consume from this queue
     * and the only one that can set the req-info flag...
     */

    erts_de_rlock(dep);

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

    if (is_binary(BIF_ARG_2))
        size = binary_size(BIF_ARG_2);
    else if (is_nil(BIF_ARG_2))
        size = 0;
    else if (is_list(BIF_ARG_2))
        BIF_TRAP2(dist_ctrl_put_data_trap,
                  BIF_P, BIF_ARG_1, BIF_ARG_2);
    else
        BIF_ERROR(BIF_P, BADARG);

    dep = erts_dhandle_to_dist_entry(BIF_ARG_1);
    if (!dep)
        BIF_ERROR(BIF_P, BADARG);

    input_handler = (Eterm) erts_atomic_read_nob(&dep->input_handler);

    if (input_handler != BIF_P->common.id)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    erts_atomic64_inc_nob(&dep->in);

    if (size != 0) {
        byte *data, *temp_alloc = NULL;

        data = (byte *) erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc);
        if (!data)
            BIF_ERROR(BIF_P, BADARG);

        erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);

        (void) erts_net_message(NULL, dep, NULL, 0, data, size);
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
dist_get_stat_1(BIF_ALIST_1)
{
    Sint64 read, write, pend;
    Eterm res, *hp, **hpp;
    Uint sz, *szp;
    DistEntry *dep = erts_dhandle_to_dist_entry(BIF_ARG_1);

    if (!dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

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
                             pend ? am_true : am_false);
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

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1) != dep)
        BIF_ERROR(BIF_P, BADARG);

    if (is_not_internal_pid(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);

    erts_atomic_set_nob(&dep->input_handler,
                        (erts_aint_t) BIF_ARG_2);

    BIF_RET(am_ok);
}

BIF_RETTYPE
dist_ctrl_get_data_1(BIF_ALIST_1)
{
    DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(BIF_P);
    const Sint initial_reds = ERTS_BIF_REDS_LEFT(BIF_P);
    Sint reds = initial_reds;
    ErtsDistOutputBuf *obuf;
    Eterm *hp;
    ProcBin *pb;
    erts_aint_t qsize;

    if (!dep)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (erts_dhandle_to_dist_entry(BIF_ARG_1) != dep)
        BIF_ERROR(BIF_P, BADARG);

    erts_de_rlock(dep);

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
        reds = erts_encode_ext_dist_header_finalize(obuf, dep, dep->flags, reds);
        if (reds < 0) {
            erts_de_runlock(dep);
            ERTS_BIF_YIELD1(bif_export[BIF_dist_ctrl_get_data_1],
                            BIF_P, BIF_ARG_1);
        }

        dep->tmp_out_queue.first = obuf->next;
        if (!obuf->next)
            dep->tmp_out_queue.last = NULL;
    }

    erts_atomic64_inc_nob(&dep->out);

    erts_de_runlock(dep);

    hp =  HAlloc(BIF_P, PROC_BIN_SIZE);
    pb = (ProcBin *) (char *) hp;
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = obuf->ext_endp - obuf->extp;
    pb->next = MSO(BIF_P).first;
    MSO(BIF_P).first = (struct erl_off_heap_header*) pb;
    pb->val = ErtsDistOutputBuf2Binary(obuf);
    pb->bytes = (byte*) obuf->extp;
    pb->flags = 0;

    qsize = erts_atomic_add_read_nob(&dep->qsize, -size_obuf(obuf));
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

    BIF_RET2(make_binary(pb), (initial_reds - reds));
}

void
erts_dist_port_not_busy(Port *prt)
{
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(dist_port_not_busy)) {
        DTRACE_CHARBUF(port_str, 64);
        DTRACE_CHARBUF(remote_str, 64);

        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)),
                      "%T", prt->common.id);
        erts_snprintf(remote_str, sizeof(DTRACE_CHARBUF_NAME(remote_str)),
                      "%T", prt->dist_entry->sysname);
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
erts_kill_dist_connection(DistEntry *dep, Uint32 connection_id)
{
    erts_de_rwlock(dep);
    if (connection_id == dep->connection_id
        && dep->state == ERTS_DE_STATE_CONNECTED) {

        kill_connection(dep);
    }
    erts_de_rwunlock(dep);
}

struct print_to_data {
    fmtfn_t to;
    void *arg;
};

static void doit_print_monitor_info(ErtsMonitor *mon, void *vptdp)
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

static void doit_print_link_info(ErtsLink *lnk, void *vptdp)
{
    struct print_to_data *ptdp = vptdp;
    ErtsLink *lnk2 = erts_link_to_other(lnk, NULL);
    erts_print(ptdp->to, ptdp->arg, "Remote link: %T %T\n",
	       lnk2->other.item, lnk->other.item);
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
    Process *net_kernel;
    Uint creation;

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
                                      ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS,
                                      0);
    if (!net_kernel || ERTS_PROC_GET_DIST_ENTRY(net_kernel))
	goto error;

    /* By setting F_DISTRIBUTION on net_kernel,
     * erts_do_net_exits will be called when net_kernel is terminated !! */
    net_kernel->flags |= F_DISTRIBUTION;

    erts_proc_unlock(net_kernel,
                     (ERTS_PROC_LOCK_STATUS
                      | ((net_kernel != BIF_P)
                         ? ERTS_PROC_LOCK_MAIN
                         : 0)));

#ifdef DEBUG
    erts_rwmtx_rlock(&erts_dist_table_rwmtx);
    ASSERT(!erts_visible_dist_entries && !erts_hidden_dist_entries);
    erts_rwmtx_runlock(&erts_dist_table_rwmtx);
#endif

    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_thr_progress_block();
    inc_no_nodes();
    erts_set_this_node(BIF_ARG_1, (Uint32) creation);
    erts_is_alive = 1;
    send_nodes_mon_msgs(NULL, am_nodeup, BIF_ARG_1, am_visible, NIL);
    erts_thr_progress_unblock();
    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    /*
     * Note erts_this_dist_entry is changed by erts_set_this_node(),
     * so we *need* to use the new one after erts_set_this_node()
     * is called.
     */
    erts_ref_dist_entry(erts_this_dist_entry);
    ERTS_PROC_SET_DIST_ENTRY(net_kernel, erts_this_dist_entry);

    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************
 ** Allocate a dist entry, set node name install the connection handler
 ** setnode_3({name@host, Creation}, Cid, {Type, Version, Initial, IC, OC})
 ** Type = flag field, where the flags are specified in dist.h
 ** Version = distribution version, >= 1
 ** IC = in_cookie (ignored)
 ** OC = out_cookie (ignored)
 **
 ** Note that in distribution protocols above 1, the Initial parameter
 ** is always NIL and the cookies are always the atom '', cookies are not
 ** sent in the distribution messages but are only used in 
 ** the handshake.
 **
 ***********************************************************************/

BIF_RETTYPE setnode_3(BIF_ALIST_3)
{
    BIF_RETTYPE ret;
    Uint flags;
    unsigned long version;
    Eterm ic, oc;
    Eterm *tp;
    DistEntry *dep = NULL;
    ErtsProcLocks proc_unlock = 0;
    Process *proc;
    Port *pp = NULL;
    Eterm notify_proc;
    erts_aint32_t qflgs;

    /*
     * Check and pick out arguments
     */

    if (!is_node_name_atom(BIF_ARG_1) ||
	!(is_internal_port(BIF_ARG_2)
          || is_internal_pid(BIF_ARG_2))
	|| (erts_this_node->sysname == am_Noname)) {
	goto badarg;
    }

    if (!is_tuple(BIF_ARG_3))
	goto badarg;
    tp = tuple_val(BIF_ARG_3);
    if (*tp++ != make_arityval(4))
	goto badarg;
    if (!is_small(*tp))
	goto badarg;
    flags = unsigned_val(*tp++);
    if (!is_small(*tp) || (version = unsigned_val(*tp)) == 0)
	goto badarg;
    ic = *(++tp);
    oc = *(++tp);
    if (!is_atom(ic) || !is_atom(oc))
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
            proc_unlock = 0;
            proc = BIF_P;
        }
        else {
            proc_unlock = ERTS_PROC_LOCK_MAIN;
            proc = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
                                             BIF_ARG_2, proc_unlock);
        }
        erts_de_rwlock(dep);

        if (!proc)
            goto badarg;
        else if (proc == ERTS_PROC_LOCK_BUSY) {
            proc_unlock = 0;
            goto yield;
        }

        erts_proc_lock(proc, ERTS_PROC_LOCK_STATUS);
        proc_unlock |= ERTS_PROC_LOCK_STATUS;

        if (ERTS_PROC_GET_DIST_ENTRY(proc)) {
            if (dep == ERTS_PROC_GET_DIST_ENTRY(proc)
                && (proc->flags & F_DISTRIBUTION)
                && dep->cid == BIF_ARG_2) {
                ERTS_BIF_PREP_RET(ret, erts_make_dhandle(BIF_P, dep));
                goto done;
            }
            goto badarg;
        }

        if (dep->state == ERTS_DE_STATE_EXITING) {
            /* Suspend on dist entry waiting for the exit to finish */
            ErtsProcList *plp = erts_proclist_create(BIF_P);
            plp->next = NULL;
            erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
            erts_mtx_lock(&dep->qlock);
            erts_proclist_store_last(&dep->suspended, plp);
            erts_mtx_unlock(&dep->qlock);
            goto yield;
        }
        if (dep->state != ERTS_DE_STATE_PENDING) {
            if (dep->state == ERTS_DE_STATE_IDLE)
                erts_set_dist_entry_pending(dep);
            else
                goto badarg;
        }

        if (is_not_nil(dep->cid))
            goto badarg;

        proc->flags |= F_DISTRIBUTION;
        ERTS_PROC_SET_DIST_ENTRY(proc, dep);

        proc_unlock &= ~ERTS_PROC_LOCK_STATUS;
        erts_proc_unlock(proc, ERTS_PROC_LOCK_STATUS);

        dep->send = NULL; /* Only for distr ports... */

    }
    else {

        pp = erts_id2port_sflgs(BIF_ARG_2,
                                BIF_P,
                                ERTS_PROC_LOCK_MAIN,
                                ERTS_PORT_SFLGS_INVALID_LOOKUP);
        erts_de_rwlock(dep);

        if (!pp || (erts_atomic32_read_nob(&pp->state)
                    & ERTS_PORT_SFLG_EXITING))
            goto badarg;

        if ((pp->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY) == 0)
            goto badarg;

        if (dep->cid == BIF_ARG_2 && pp->dist_entry == dep) {
            ERTS_BIF_PREP_RET(ret, erts_make_dhandle(BIF_P, dep));
            goto done; /* Already set */
        }

        if (dep->state == ERTS_DE_STATE_EXITING) {
            /* Suspend on dist entry waiting for the exit to finish */
            ErtsProcList *plp = erts_proclist_create(BIF_P);
            plp->next = NULL;
            erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
            erts_mtx_lock(&dep->qlock);
            erts_proclist_store_last(&dep->suspended, plp);
            erts_mtx_unlock(&dep->qlock);
            goto yield;
        }
        if (dep->state != ERTS_DE_STATE_PENDING) {
            if (dep->state == ERTS_DE_STATE_IDLE)
                erts_set_dist_entry_pending(dep);
            else
                goto badarg;
        }

        if (pp->dist_entry || is_not_nil(dep->cid))
            goto badarg;

        erts_atomic32_read_bor_nob(&pp->state, ERTS_PORT_SFLG_DISTRIBUTION);

        pp->dist_entry = dep;

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

    }

    dep->version = version;
    dep->creation = 0;

#ifdef DEBUG
    ASSERT(erts_atomic_read_nob(&dep->qsize) == 0 
           || (dep->state == ERTS_DE_STATE_PENDING));
#endif

    if (flags & DFLAG_DIST_HDR_ATOM_CACHE)
	create_cache(dep);

    erts_set_dist_entry_connected(dep, BIF_ARG_2, flags);

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
        notify_dist_data(BIF_P, notify_proc);

    ERTS_BIF_PREP_RET(ret, erts_make_dhandle(BIF_P, dep));

    dep = NULL; /* inc of refc transferred to port (dist_entry field) */

    inc_no_nodes();

    send_nodes_mon_msgs(BIF_P,
			am_nodeup,
			BIF_ARG_1,
			flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			NIL);
 done:

    if (dep && dep != erts_this_dist_entry) {
	erts_de_rwunlock(dep);
	erts_deref_dist_entry(dep);
    }

    if (pp)
	erts_port_release(pp);

    if (proc_unlock)
        erts_proc_unlock(proc, proc_unlock);

    return ret;

 yield:
    ERTS_BIF_PREP_YIELD3(ret, bif_export[BIF_setnode_3], BIF_P,
			 BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    goto done;

 badarg:
    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    goto done;

 system_limit:
    ERTS_BIF_PREP_ERROR(ret, BIF_P, SYSTEM_LIMIT);
    goto done;
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
    case ERTS_DE_STATE_PENDING:
    case ERTS_DE_STATE_CONNECTED:
	conn_id = dep->connection_id;
        break;
    case ERTS_DE_STATE_IDLE:
        erts_set_dist_entry_pending(dep);
	conn_id = dep->connection_id;
        break;
    case ERTS_DE_STATE_EXITING:
        conn_id = (dep->connection_id + 1) & ERTS_DIST_CON_ID_MASK;
        break;
    default:
        erts_exit(ERTS_ABORT_EXIT, "Invalid dep->state (%d)\n", dep->state);
    }
    erts_de_rwunlock(dep);
    hp = HAlloc(BIF_P, 3 + ERTS_MAGIC_REF_THING_SIZE);
    dhandle = erts_build_dhandle(&hp, &BIF_P->off_heap, dep);
    erts_deref_dist_entry(dep);
    BIF_RET(TUPLE2(hp, make_small(conn_id), dhandle));
}

static Sint abort_connection(DistEntry* dep, Uint32 conn_id)
{
    erts_de_rwlock(dep);

    if (dep->connection_id != conn_id)
        ;
    else if (dep->state == ERTS_DE_STATE_CONNECTED) {
        kill_connection(dep);
    }
    else if (dep->state == ERTS_DE_STATE_PENDING) {
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

        schedule_con_monitor_link_cleanup(mld, THE_NON_VALUE,
                                          THE_NON_VALUE, THE_NON_VALUE);

        if (resume_procs) {
            int resumed = erts_resume_processes(resume_procs);
            reds += resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
        }

	delete_cache(cache);
	free_de_out_queues(dep, obuf);

        /*
         * We wait to make DistEntry idle and accept new connection attempts
         * until all is cleared and deallocated. This to get some back pressure
         * against repeated failing connection attempts saturating all CPUs
         * with cleanup jobs.
         */
        erts_de_rwlock(dep);
        ASSERT(dep->state == ERTS_DE_STATE_EXITING);
        dep->state = ERTS_DE_STATE_IDLE;
        erts_de_rwunlock(dep);
        return reds;
    }
    erts_de_rwunlock(dep);
    return 0;
}

BIF_RETTYPE erts_internal_abort_connection_2(BIF_ALIST_2)
{
    DistEntry* dep;
    Eterm* tp;

    if (is_not_atom(BIF_ARG_1) || is_not_tuple_arity(BIF_ARG_2, 2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    tp = tuple_val(BIF_ARG_2);
    dep = erts_dhandle_to_dist_entry(tp[2]);
    if (is_not_small(tp[1]) || dep != erts_find_dist_entry(BIF_ARG_1)
        || dep == erts_this_dist_entry) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (dep) {
        Sint reds = abort_connection(dep, unsigned_val(tp[1]));
        BUMP_REDS(BIF_P, reds);
    }
    BIF_RET(am_true);
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
            abort_connection(dep, conn_id);
            return 0;
        }

        /*
         * Send {auto_connect, Node, ConnId, DHandle} to net_kernel
         */
        mp = erts_alloc_message_heap(net_kernel, &nk_locks,
                                     5 + ERTS_MAGIC_REF_THING_SIZE,
                                     &hp, &ohp);
        dhandle = erts_build_dhandle(&hp, ohp, dep);
        msg = TUPLE4(hp, am_auto_connect, dep->sysname, make_small(conn_id),
                     dhandle);
        erts_queue_message(net_kernel, nk_locks, mp, msg, proc->common.id);
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
        ErtsDSigData dsd;
        dsd.node = Node;

        dep = erts_find_or_insert_dist_entry(Node);
        if (dep == erts_this_dist_entry)
            break;

        switch (erts_dsig_prepare(&dsd, dep, p,
                                  ERTS_PROC_LOCK_MAIN,
                                  ERTS_DSP_RLOCK, 0, async_connect)) {
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
#ifdef DEBUG
                int inserted =
#endif
                    erts_monitor_dist_insert(&mdep->md.target, dep->mld);
                ASSERT(inserted);
                ASSERT(mdep->dist->connection_id == dep->connection_id);
            }
            else if (mdep->dist->connection_id != dep->connection_id) {
                ErtsMonitorDataExtended *mdep2;
                ErtsMonitor *mon2;
#ifdef DEBUG
                int inserted;
#endif
                mdep2 = ((ErtsMonitorDataExtended *)
                         erts_monitor_create(ERTS_MON_TYPE_NODE, NIL,
                                             p->common.id, Node, NIL));
                mon2 = &mdep2->md.origin;
#ifdef DEBUG
                inserted =
#endif
                    erts_monitor_dist_insert(&mdep->md.target, dep->mld);
                ASSERT(inserted);
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

static void
save_nodes_monitor(ErtsMonitor *mon, void *vctxt)
{
    ErtsNodesMonitorContext *ctxt = vctxt;
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);

    ASSERT(erts_monitor_is_target(mon));
    ASSERT(mon->type == ERTS_MON_TYPE_NODES);

    ctxt->nmdp[ctxt->i].pid = mon->other.item;
    ctxt->nmdp[ctxt->i].options = mdp->origin.other.item;

    ctxt->i++;
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


static void
nodes_monitor_info(ErtsMonitor *mon, void *vctxt)
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
