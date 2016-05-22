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

/*
 * distribution of erlang messages to other nodes.
 */


/* define this to get a lot of debug output */
/* #define ERTS_DIST_MSG_DBG */

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



#define PASS_THROUGH 'p'        /* This code should go */

int erts_is_alive; /* System must be blocked on change */
int erts_dist_buf_busy_limit;


/* distribution trap functions */
Export* dsend2_trap = NULL;
Export* dsend3_trap = NULL;
/*Export* dsend_nosuspend_trap = NULL;*/
Export* dlink_trap = NULL;
Export* dunlink_trap = NULL;
Export* dmonitor_node_trap = NULL;
Export* dgroup_leader_trap = NULL;
Export* dexit_trap = NULL;
Export* dmonitor_p_trap = NULL;

/* local variables */


/* forward declarations */

static void clear_dist_entry(DistEntry*);
static int dsig_send_ctl(ErtsDSigData* dsdp, Eterm ctl, int force_busy);
static void send_nodes_mon_msgs(Process *, Eterm, Eterm, Eterm, Eterm);
static void init_nodes_monitors(void);

static erts_smp_atomic_t no_caches;
static erts_smp_atomic_t no_nodes;

struct {
    Eterm reason;
    ErlHeapFragment *bp;
} nodedown;


static void
delete_cache(ErtsAtomCache *cache)
{
    if (cache) {
	erts_free(ERTS_ALC_T_DCACHE, (void *) cache);
	ASSERT(erts_smp_atomic_read_nob(&no_caches) > 0);
	erts_smp_atomic_dec_nob(&no_caches);
    }
}


static void
create_cache(DistEntry *dep)
{
    int i;
    ErtsAtomCache *cp;

    ERTS_SMP_LC_ASSERT(
	is_internal_port(dep->cid)
	&& erts_lc_is_port_locked(erts_port_lookup_raw(dep->cid)));
    ASSERT(!dep->cache);

    dep->cache = cp = (ErtsAtomCache*) erts_alloc(ERTS_ALC_T_DCACHE,
						  sizeof(ErtsAtomCache));
    erts_smp_atomic_inc_nob(&no_caches);
    for (i = 0; i < sizeof(cp->in_arr)/sizeof(cp->in_arr[0]); i++) {
	cp->in_arr[i] = THE_NON_VALUE;
	cp->out_arr[i] = THE_NON_VALUE;
    }
}

Uint erts_dist_cache_size(void)
{
    return (Uint) erts_smp_atomic_read_mb(&no_caches)*sizeof(ErtsAtomCache);
}

static ErtsProcList *
get_suspended_on_de(DistEntry *dep, Uint32 unset_qflgs)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&dep->qlock));
    dep->qflgs &= ~unset_qflgs;
    if (dep->qflgs & ERTS_DE_QFLG_EXIT) {
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

typedef struct {
    DistEntry *dep;
    Eterm *lhp;
} NetExitsContext;

/* 
** This function is called when a distribution 
** port or process terminates
*/
static void doit_monitor_net_exits(ErtsMonitor *mon, void *vnecp)
{
    Process *rp;
    ErtsMonitor *rmon;
    DistEntry *dep = ((NetExitsContext *) vnecp)->dep;
    ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;

    rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);
    if (!rp)
	goto done;

    if (mon->type == MON_ORIGIN) {
	/* local pid is beeing monitored */
	rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
	/* ASSERT(rmon != NULL); nope, can happen during process exit */
	if (rmon != NULL) {
	    erts_destroy_monitor(rmon);
	}
    } else {
	DeclareTmpHeapNoproc(lhp,3);
	Eterm watched;
	UseTmpHeapNoproc(3);
	ASSERT(mon->type == MON_TARGET);
	rmon = erts_remove_monitor(&ERTS_P_MONITORS(rp), mon->ref);
	/* ASSERT(rmon != NULL); can happen during process exit */
	if (rmon != NULL) {
	    ASSERT(is_atom(rmon->name) || is_nil(rmon->name));
	    watched = (is_atom(rmon->name)
		       ? TUPLE2(lhp, rmon->name, dep->sysname)
		       : rmon->pid);
#ifdef ERTS_SMP
	    rp_locks |= ERTS_PROC_LOCKS_MSG_SEND;
	    erts_smp_proc_lock(rp, ERTS_PROC_LOCKS_MSG_SEND);
#endif
	    erts_queue_monitor_message(rp, &rp_locks, mon->ref, am_process, 
				       watched, am_noconnection);
	    erts_destroy_monitor(rmon);
	}
	UnUseTmpHeapNoproc(3);
    }
    erts_smp_proc_unlock(rp, rp_locks);
 done:
    erts_destroy_monitor(mon);
}
	
typedef struct {
    NetExitsContext *necp;
    ErtsLink *lnk;
} LinkNetExitsContext;

/* 
** This is the function actually doing the job of sending exit messages
** for links in a dist entry upon net_exit (the node goes down), NB,
** only process links, not node monitors are handled here, 
** they reside in a separate tree....
*/
static void doit_link_net_exits_sub(ErtsLink *sublnk, void *vlnecp)
{
    ErtsLink *lnk = ((LinkNetExitsContext *) vlnecp)->lnk; /* the local pid */
    ErtsLink *rlnk;
    Process *rp;

    ASSERT(lnk->type == LINK_PID);
    if (is_internal_pid(lnk->pid)) {
	int xres;
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;

	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (!rp) {
	    goto done;
	}

	rlnk = erts_remove_link(&ERTS_P_LINKS(rp), sublnk->pid);
	xres = erts_send_exit_signal(NULL,
				     sublnk->pid,
				     rp,
				     &rp_locks,
				     am_noconnection,
				     NIL,
				     NULL,
				     0);

	if (rlnk) {
	    erts_destroy_link(rlnk);
	    if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
		/* We didn't exit the process and it is traced */
		trace_proc(NULL, 0, rp, am_getting_unlinked, sublnk->pid);
	    }
	}
	erts_smp_proc_unlock(rp, rp_locks);
    }
 done:
    erts_destroy_link(sublnk);

}
    




/* 
** This function is called when a distribution 
** port or process terminates, once for each link on the high level, 
** it in turn traverses the link subtree for the specific link node...
*/
static void doit_link_net_exits(ErtsLink *lnk, void *vnecp)
{
    LinkNetExitsContext lnec = {(NetExitsContext *) vnecp, lnk};
    ASSERT(lnk->type == LINK_PID);
    erts_sweep_links(ERTS_LINK_ROOT(lnk), &doit_link_net_exits_sub, (void *) &lnec);
#ifdef DEBUG
    ERTS_LINK_ROOT(lnk) = NULL;
#endif
    erts_destroy_link(lnk);
}


static void doit_node_link_net_exits(ErtsLink *lnk, void *vnecp)
{
    DistEntry *dep = ((NetExitsContext *) vnecp)->dep;
    Eterm name = dep->sysname;
    Process *rp;
    ErtsLink *rlnk;
    Uint i,n;
    ASSERT(lnk->type == LINK_NODE);
    if (is_internal_pid(lnk->pid)) {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;
	ErlOffHeap *ohp;
	rp = erts_proc_lookup(lnk->pid);
	if (!rp)
	    goto done;
	erts_smp_proc_lock(rp, rp_locks);
	rlnk = erts_remove_link(&ERTS_P_LINKS(rp), name);
	if (rlnk != NULL) {
	    ASSERT(is_atom(rlnk->pid) && (rlnk->type == LINK_NODE));
	    erts_destroy_link(rlnk);
	}
	n = ERTS_LINK_REFC(lnk);
	for (i = 0; i < n; ++i) {
	    Eterm tup;
	    Eterm *hp;
	    ErtsMessage *msgp;

	    msgp = erts_alloc_message_heap(rp, &rp_locks,
					   3, &hp, &ohp);
	    tup = TUPLE2(hp, am_nodedown, name);
	    erts_queue_message(rp, rp_locks, msgp, tup, am_system);
	}
	erts_smp_proc_unlock(rp, rp_locks);
    }
 done:
    erts_destroy_link(lnk);
}

static void
set_node_not_alive(void *unused)
{
    ErlHeapFragment *bp;
    Eterm nodename = erts_this_dist_entry->sysname;

    ASSERT(erts_smp_atomic_read_nob(&no_nodes) == 0);

    erts_smp_thr_progress_block();
    erts_set_this_node(am_Noname, 0);
    erts_is_alive = 0;
    send_nodes_mon_msgs(NULL, am_nodedown, nodename, am_visible, nodedown.reason);
    nodedown.reason = NIL;
    bp = nodedown.bp;
    nodedown.bp = NULL;
    erts_smp_thr_progress_unblock();
    if (bp)
	free_message_buffer(bp);
}

static ERTS_INLINE void
dec_no_nodes(void)
{
    erts_aint_t no = erts_smp_atomic_dec_read_mb(&no_nodes);
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
    erts_aint_t no = erts_smp_atomic_read_nob(&no_nodes);
    ASSERT(erts_is_alive ? no > 0 : no == 0);
#endif
    erts_smp_atomic_inc_mb(&no_nodes);
}
	
/*
 * proc is currently running or exiting process.
 */
int erts_do_net_exits(DistEntry *dep, Eterm reason)
{
    Eterm nodename;

    if (dep == erts_this_dist_entry) {  /* Net kernel has died (clean up!!) */
	DistEntry *tdep;
	int no_dist_port = 0;
	Eterm nd_reason = (reason == am_no_network
			   ? am_no_network
			   : am_net_kernel_terminated);
	erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);

	for (tdep = erts_hidden_dist_entries; tdep; tdep = tdep->next)
	    no_dist_port++;
	for (tdep = erts_visible_dist_entries; tdep; tdep = tdep->next)
	    no_dist_port++;

	/* KILL all port controllers */
	if (no_dist_port == 0)
	    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
	else {
	    Eterm def_buf[128];
	    int i = 0;
	    Eterm *dist_port;

	    if (no_dist_port <= sizeof(def_buf)/sizeof(def_buf[0]))
		dist_port = &def_buf[0];
	    else
		dist_port = erts_alloc(ERTS_ALC_T_TMP,
				       sizeof(Eterm)*no_dist_port);
	    for (tdep = erts_hidden_dist_entries; tdep; tdep = tdep->next) {
		ASSERT(is_internal_port(tdep->cid));
		dist_port[i++] = tdep->cid;
	    }
	    for (tdep = erts_visible_dist_entries; tdep; tdep = tdep->next) {
		ASSERT(is_internal_port(tdep->cid));
		dist_port[i++] = tdep->cid;
	    }
	    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);

	    for (i = 0; i < no_dist_port; i++) {
		Port *prt = erts_port_lookup(dist_port[i],
					     ERTS_PORT_SFLGS_INVALID_LOOKUP);
		if (!prt)
		    continue;
		ASSERT(erts_atomic32_read_nob(&prt->state)
		       & ERTS_PORT_SFLG_DISTRIBUTION);

		erts_port_exit(NULL, ERTS_PORT_SIG_FLG_FORCE_SCHED,
			       prt, dist_port[i], nd_reason, NULL);
	    }

	    if (dist_port != &def_buf[0])
		erts_free(ERTS_ALC_T_TMP, dist_port);
	}

	/*
	 * When last dist port exits, node will be taken
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
    else { /* Call from distribution port */
	NetExitsContext nec = {dep};
	ErtsLink *nlinks;
	ErtsLink *node_links;
	ErtsMonitor *monitors;
	Uint32 flags;

	erts_smp_atomic_set_mb(&dep->dist_cmd_scheduled, 1);
	erts_smp_de_rwlock(dep);

	ERTS_SMP_LC_ASSERT(is_internal_port(dep->cid)
			   && erts_lc_is_port_locked(erts_port_lookup_raw(dep->cid)));

	if (erts_port_task_is_scheduled(&dep->dist_cmd))
	    erts_port_task_abort(&dep->dist_cmd);

	if (dep->status & ERTS_DE_SFLG_EXITING) {
#ifdef DEBUG
	    erts_smp_mtx_lock(&dep->qlock);
	    ASSERT(dep->qflgs & ERTS_DE_QFLG_EXIT);
	    erts_smp_mtx_unlock(&dep->qlock);
#endif
	}
	else {
	    dep->status |= ERTS_DE_SFLG_EXITING;
	    erts_smp_mtx_lock(&dep->qlock);
	    ASSERT(!(dep->qflgs & ERTS_DE_QFLG_EXIT));
	    dep->qflgs |= ERTS_DE_QFLG_EXIT;
	    erts_smp_mtx_unlock(&dep->qlock);
	}

	erts_smp_de_links_lock(dep);
	monitors	= dep->monitors;
        nlinks		= dep->nlinks;
	node_links	= dep->node_links;
	dep->monitors	= NULL;
        dep->nlinks	= NULL;
	dep->node_links	= NULL;
	erts_smp_de_links_unlock(dep);

	nodename = dep->sysname;
	flags = dep->flags;

	erts_set_dist_entry_not_connected(dep);

	erts_smp_de_rwunlock(dep);

	erts_sweep_monitors(monitors, &doit_monitor_net_exits, (void *) &nec);
	erts_sweep_links(nlinks, &doit_link_net_exits, (void *) &nec);
	erts_sweep_links(node_links, &doit_node_link_net_exits, (void *) &nec);

	send_nodes_mon_msgs(NULL,
			    am_nodedown,
			    nodename,
			    flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			    reason == am_normal ? am_connection_closed : reason);

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

void init_dist(void)
{
    init_nodes_monitors();

    nodedown.reason = NIL;
    nodedown.bp = NULL;

    erts_smp_atomic_init_nob(&no_nodes, 0);
    erts_smp_atomic_init_nob(&no_caches, 0);

    /* Lookup/Install all references to trap functions */
    dsend2_trap = trap_function(am_dsend,2);
    dsend3_trap = trap_function(am_dsend,3);
    /*    dsend_nosuspend_trap = trap_function(am_dsend_nosuspend,2);*/
    dlink_trap = trap_function(am_dlink,1);
    dunlink_trap = trap_function(am_dunlink,1);
    dmonitor_node_trap = trap_function(am_dmonitor_node,3);
    dgroup_leader_trap = trap_function(am_dgroup_leader,2);
    dexit_trap = trap_function(am_dexit, 2);
    dmonitor_p_trap = trap_function(am_dmonitor_p, 2);
}

#define ErtsDistOutputBuf2Binary(OB) \
  ((Binary *) (((char *) (OB)) - offsetof(Binary, orig_bytes)))

static ERTS_INLINE ErtsDistOutputBuf *
alloc_dist_obuf(Uint size)
{
    ErtsDistOutputBuf *obuf;
    Uint obuf_size = sizeof(ErtsDistOutputBuf)+sizeof(byte)*(size-1);
    Binary *bin = erts_bin_drv_alloc(obuf_size);
    erts_refc_init(&bin->refc, 1);
    obuf = (ErtsDistOutputBuf *) &bin->orig_bytes[0];
#ifdef DEBUG
    obuf->dbg_pattern = ERTS_DIST_OUTPUT_BUF_DBG_PATTERN;
    ASSERT(bin == ErtsDistOutputBuf2Binary(obuf));
#endif
    return obuf;
}

static ERTS_INLINE void
free_dist_obuf(ErtsDistOutputBuf *obuf)
{
    Binary *bin = ErtsDistOutputBuf2Binary(obuf);
    ASSERT(obuf->dbg_pattern == ERTS_DIST_OUTPUT_BUF_DBG_PATTERN);
    if (erts_refc_dectest(&bin->refc, 0) == 0)
	erts_bin_free(bin);
}

static ERTS_INLINE Sint
size_obuf(ErtsDistOutputBuf *obuf)
{
    Binary *bin = ErtsDistOutputBuf2Binary(obuf);
    return bin->orig_size;
}

static void clear_dist_entry(DistEntry *dep)
{
    Sint obufsize = 0;
    ErtsAtomCache *cache;
    ErtsProcList *suspendees;
    ErtsDistOutputBuf *obuf;

    erts_smp_de_rwlock(dep);
    cache = dep->cache;
    dep->cache = NULL;

#ifdef DEBUG
    erts_smp_de_links_lock(dep);
    ASSERT(!dep->nlinks);
    ASSERT(!dep->node_links);
    ASSERT(!dep->monitors);
    erts_smp_de_links_unlock(dep);
#endif

    erts_smp_mtx_lock(&dep->qlock);

    if (!dep->out_queue.last)
	obuf = dep->finalized_out_queue.first;
    else {
	dep->out_queue.last->next = dep->finalized_out_queue.first;
	obuf = dep->out_queue.first;
    }

    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    dep->finalized_out_queue.first = NULL;
    dep->finalized_out_queue.last = NULL;
    dep->status = 0;
    suspendees = get_suspended_on_de(dep, ERTS_DE_QFLGS_ALL);

    erts_smp_mtx_unlock(&dep->qlock);
    erts_smp_atomic_set_nob(&dep->dist_cmd_scheduled, 0);
    dep->send = NULL;
    erts_smp_de_rwunlock(dep);

    erts_resume_processes(suspendees);

    delete_cache(cache);

    while (obuf) {
	ErtsDistOutputBuf *fobuf;
	fobuf = obuf;
	obuf = obuf->next;
	obufsize += size_obuf(fobuf);
	free_dist_obuf(fobuf);
    }

    if (obufsize) {
	erts_smp_mtx_lock(&dep->qlock);
	ASSERT(dep->qsize >= obufsize);
	dep->qsize -= obufsize;
	erts_smp_mtx_unlock(&dep->qlock);
    }
}

void erts_dsend_context_dtor(Binary* ctx_bin)
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
    if (ctx->dep_to_deref)
	erts_deref_dist_entry(ctx->dep_to_deref);
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
    Eterm* hp = HAlloc(p, PROC_BIN_SIZE);

    sys_memcpy(&dst->ctx, ctx, sizeof(ErtsSendContext));
    ASSERT(ctx->dss.ctl == make_tuple(ctx->ctl_heap));
    dst->ctx.dss.ctl = make_tuple(dst->ctx.ctl_heap);
    if (ctx->dss.acmp) {
	sys_memcpy(&dst->acm, ctx->dss.acmp, sizeof(ErtsAtomCacheMap));
	dst->ctx.dss.acmp = &dst->acm;
    }
    return erts_mk_magic_binary_term(&hp, &MSO(p), ctx_bin);
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


/* A local process that's beeing monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason},
   which is rather sad as only the ref is needed, no pid's... */
int
erts_dsig_send_m_exit(ErtsDSigData *dsdp, Eterm watcher, Eterm watched, 
			  Eterm ref, Eterm reason)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,6);
    int res;

    UseTmpHeapNoproc(6);

    ctl = TUPLE5(&ctl_heap[0], make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

#ifdef DEBUG
    erts_smp_de_links_lock(dsdp->dep);
    ASSERT(!erts_lookup_monitor(dsdp->dep->monitors, ref));
    erts_smp_de_links_unlock(dsdp->dep);
#endif

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
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref}, which is once again
   rather redundant as only the ref will be needed on the other side... */
int
erts_dsig_send_demonitor(ErtsDSigData *dsdp, Eterm watcher,
			 Eterm watched, Eterm ref, int force)
{
    Eterm ctl;
    DeclareTmpHeapNoproc(ctl_heap,5);
    int res;

    UseTmpHeapNoproc(5);
    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    res = dsig_send_ctl(dsdp, ctl, force);
    UnUseTmpHeapNoproc(5);
    return res;
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
            tok_label = signed_val(SEQ_TRACE_T_LABEL(token));
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
        }
    }
#endif

    if (token != NIL)
	ctl = TUPLE4(&ctx->ctl_heap[0],
		     make_small(DOP_SEND_TT), am_Empty, remote, token);
    else
	ctl = TUPLE3(&ctx->ctl_heap[0], make_small(DOP_SEND), am_Empty, remote);
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
            tok_label = signed_val(SEQ_TRACE_T_LABEL(token));
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token));
        }
    }
#endif

    if (token != NIL)
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
            tok_label = signed_val(SEQ_TRACE_T_LABEL(token));
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
	if (erts_sys_getenv_raw("VALGRIND_LOG_XML", buf__, &bufsz__) >= 0) { \
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
    ErtsMonitor *mon;
    ErtsLink *lnk;
    Uint tuple_arity;
    int res;
#ifdef ERTS_DIST_MSG_DBG
    ErlDrvSizeT orig_len = len;
#endif

    UseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (!erts_is_alive) {
	UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
	return 0;
    }
    if (hlen != 0)
	goto data_error;
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

    if (len == 0) {
	PURIFY_MSG("data error");
	goto data_error;
    }

    res = erts_prepare_dist_ext(&ede, t, len, dep, dep->cache);

    if (res >= 0)
	res = ctl_len = erts_decode_dist_ext_size(&ede);
    else {
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(stderr, "DIST MSG DEBUG: erts_prepare_dist_ext() failed:\n");
	bw(buf, orig_len);
#endif
	ctl_len = 0;
    }

    if (res < 0) {
#ifdef ERTS_DIST_MSG_DBG
	erts_fprintf(stderr, "DIST MSG DEBUG: erts_decode_dist_ext_size(CTL) failed:\n");
	bw(buf, orig_len);
#endif
	PURIFY_MSG("data error");
	goto data_error;
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

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK:
	if (tuple_arity != 3) {
	    goto invalid_message;
	}
	from = tuple[2];
	to   = tuple[3];  /* local proc to link to */

	if (is_not_pid(from) || is_not_pid(to)) {
	    goto invalid_message;
	}

	rp = erts_pid2proc_opt(NULL, 0,
			       to, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (!rp) {
	    /* This is tricky (we MUST force a distributed send) */
	    ErtsDSigData dsd;
	    int code;
	    code = erts_dsig_prepare(&dsd, dep, NULL, ERTS_DSP_NO_LOCK, 0);
	    if (code == ERTS_DSIG_PREP_CONNECTED) {
		code = erts_dsig_send_exit(&dsd, to, from, am_noproc);
		ASSERT(code == ERTS_DSIG_SEND_OK);
	    }
	    break;
	}

	erts_smp_de_links_lock(dep);
	res = erts_add_link(&ERTS_P_LINKS(rp), LINK_PID, from);

	if (res < 0) {
	    /* It was already there! Lets skip the rest... */
	    erts_smp_de_links_unlock(dep);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	    break;
	}
	lnk = erts_add_or_lookup_link(&(dep->nlinks), LINK_PID, rp->common.id);
	erts_add_link(&(ERTS_LINK_ROOT(lnk)), LINK_PID, from);
	erts_smp_de_links_unlock(dep);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(NULL, 0, rp, am_getting_linked, from);

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	break;

    case DOP_UNLINK: {
	ErtsDistLinkData dld;
	if (tuple_arity != 3) {
	    goto invalid_message;
	}
	from = tuple[2];
	to = tuple[3];
	if (is_not_pid(from) || is_not_pid(to)) {
	    goto invalid_message;
	}
	
	rp = erts_pid2proc_opt(NULL, 0,
			       to, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (!rp)
	    break;

	lnk = erts_remove_link(&ERTS_P_LINKS(rp), from);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && lnk != NULL) {
	    trace_proc(NULL, 0, rp, am_getting_unlinked, from);
	}

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

	erts_remove_dist_link(&dld, to, from, dep);
	erts_destroy_dist_link(&dld);
	if (lnk)
	    erts_destroy_link(lnk);
	break;
    }
    
    case DOP_MONITOR_P: {
	/* A remote process wants to monitor us, we get:
	   {DOP_MONITOR_P, Remote pid, local pid or name, ref} */
	Eterm name;
	
	if (tuple_arity != 4) {
	    goto invalid_message;
	}

	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

	if (is_not_ref(ref)) {
	    goto invalid_message;
	}

	if (is_atom(watched)) {
	    name = watched;
	    rp = erts_whereis_process(NULL, 0,
				      watched, ERTS_PROC_LOCK_LINK,
				      ERTS_P2P_FLG_ALLOW_OTHER_X);
	}
	else {
	    name = NIL;
	    rp = erts_pid2proc_opt(NULL, 0,
				   watched, ERTS_PROC_LOCK_LINK,
				   ERTS_P2P_FLG_ALLOW_OTHER_X);
	}

	if (!rp) {
	    ErtsDSigData dsd;
	    int code;
	    code = erts_dsig_prepare(&dsd, dep, NULL, ERTS_DSP_NO_LOCK, 0);
	    if (code == ERTS_DSIG_PREP_CONNECTED) {
		code = erts_dsig_send_m_exit(&dsd, watcher, watched, ref,
					     am_noproc);
		ASSERT(code == ERTS_DSIG_SEND_OK);
	    }
	}
	else {
	    if (is_atom(watched))
		watched = rp->common.id;
	    erts_smp_de_links_lock(dep);
	    erts_add_monitor(&(dep->monitors), MON_ORIGIN, ref, watched, name);
	    erts_add_monitor(&ERTS_P_MONITORS(rp), MON_TARGET, ref, watcher, name);
	    erts_smp_de_links_unlock(dep);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
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
	/* watcher = tuple[2]; */
	/* watched = tuple[3]; May be an atom in case of monitor name */
	ref = tuple[4];

	if(is_not_ref(ref)) {
	    goto invalid_message;
	}

	erts_smp_de_links_lock(dep);
	mon = erts_remove_monitor(&(dep->monitors),ref);
	erts_smp_de_links_unlock(dep);
	/* ASSERT(mon != NULL); can happen in case of broken dist message */
	if (mon == NULL) {
	    break;
	}
	watched = mon->pid;
	erts_destroy_monitor(mon);
	rp = erts_pid2proc_opt(NULL, 0,
			       watched, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (!rp) {
	    break;
	}
	mon = erts_remove_monitor(&ERTS_P_MONITORS(rp), ref);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	ASSERT(mon != NULL);
	if (mon == NULL) {
	    break;
	}
	erts_destroy_monitor(mon);
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
		erts_smp_proc_unlock(rp, locks);
	}
	break;

    case DOP_SEND_TT:
	if (tuple_arity != 4) {
	    goto invalid_message;
	}
	
	token_size = size_object(tuple[4]);
	/* Fall through ... */
    case DOP_SEND:
	/*
	 * There is intentionally no testing of the cookie (it is always '')
	 * from R9B and onwards.
	 */
#ifdef ERTS_DIST_MSG_DBG
	dist_msg_dbg(&ede, "MSG", buf, orig_len);
#endif
	if (type != DOP_SEND_TT && tuple_arity != 3) {
	    goto invalid_message;
	}
	to = tuple[3];
	if (is_not_pid(to)) {
	    goto invalid_message;
	}
	rp = erts_proc_lookup(to);
	if (rp) {
	    Uint xsize = type == DOP_SEND ? 0 : ERTS_HEAP_FRAG_SIZE(token_size);
	    ErtsProcLocks locks = 0;
	    ErtsDistExternal *ede_copy;

	    ede_copy = erts_make_dist_ext_copy(&ede, xsize);
	    if (type == DOP_SEND) {
		token = NIL;
	    } else {
		ErlHeapFragment *heap_frag;
		ErlOffHeap *ohp;
		ASSERT(xsize);
		heap_frag = erts_dist_ext_trailer(ede_copy);
		ERTS_INIT_HEAP_FRAG(heap_frag, token_size, token_size);
		hp = heap_frag->mem;
		ohp = &heap_frag->off_heap;
		token = tuple[4];
		token = copy_struct(token, token_size, &hp, ohp);
	    }

	    erts_queue_dist_message(rp, locks, ede_copy, token, tuple[2]);
	    if (locks)
		erts_smp_proc_unlock(rp, locks);
	}
	break;

    case DOP_MONITOR_P_EXIT: {
	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */
	   

	DeclareTmpHeapNoproc(lhp,3);
	Eterm sysname;
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_MSG_SEND|ERTS_PROC_LOCK_LINK;

	if (tuple_arity != 5) {
	    goto invalid_message;
	}

	/* watched = tuple[2]; */  /* remote proc which died */
	/* watcher = tuple[3]; */
	ref     = tuple[4];
	reason  = tuple[5];

	if(is_not_ref(ref)) {
	    goto invalid_message;
	}

	erts_smp_de_links_lock(dep);
	sysname = dep->sysname;
	mon = erts_remove_monitor(&(dep->monitors), ref);
	/*
	 * If demonitor was performed at the same time as the
	 * monitored process exits, monitoring side will have
	 * removed info about monitor. In this case, do nothing
	 * and everything will be as it should.
	 */
	erts_smp_de_links_unlock(dep);
	if (mon == NULL) {
	    break;
	}
	rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);

	erts_destroy_monitor(mon);
	if (rp == NULL) {
	    break;
	}

	mon = erts_remove_monitor(&ERTS_P_MONITORS(rp), ref);

	if (mon == NULL) {
	    erts_smp_proc_unlock(rp, rp_locks);
	    break;
	}
	UseTmpHeapNoproc(3);
	
	watched = (is_not_nil(mon->name)
		   ? TUPLE2(&lhp[0], mon->name, sysname)
		   : mon->pid);
	
	erts_queue_monitor_message(rp, &rp_locks,
				   ref, am_process, watched, reason);
	erts_smp_proc_unlock(rp, rp_locks);
	erts_destroy_monitor(mon);
	UnUseTmpHeapNoproc(3);
	break;
    }

    case DOP_EXIT_TT:
    case DOP_EXIT: {
	ErtsDistLinkData dld;
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
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
	if (is_not_pid(from) || is_not_internal_pid(to)) {
	    goto invalid_message;
	}

	rp = erts_pid2proc(NULL, 0, to, rp_locks);
	if (!rp)
	    lnk = NULL;
	else {
	    lnk = erts_remove_link(&ERTS_P_LINKS(rp), from);

	    /* If lnk == NULL, we have unlinked on this side, i.e.
	     * ignore exit.
	     */
	    if (lnk) {
		int xres;
#if 0
		/* Arndt: Maybe it should never be 'kill', but it can be,
		   namely when a linked process does exit(kill). Until we know
		   whether that is incorrect and what should happen instead,
		   we leave the assertion out. */
		ASSERT(reason != am_kill); /* should never be kill (killed) */
#endif
		xres = erts_send_exit_signal(NULL,
					     from,
					     rp,
					     &rp_locks, 
					     reason,
					     token,
					     NULL,
					     ERTS_XSIG_FLG_IGN_KILL);
		if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
		    /* We didn't exit the process and it is traced */
                    if (rp_locks & ERTS_PROC_LOCKS_XSIG_SEND) {
                        erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_XSIG_SEND);
                        rp_locks &= ~ERTS_PROC_LOCKS_XSIG_SEND;
                    }
		    trace_proc(NULL, 0, rp, am_getting_unlinked, from);
		}
	    }
	    erts_smp_proc_unlock(rp, rp_locks);
	}
	erts_remove_dist_link(&dld, to, from, dep);
	if (lnk)
	    erts_destroy_link(lnk);
	erts_destroy_dist_link(&dld);
	break;
    }
    case DOP_EXIT2_TT:
    case DOP_EXIT2: {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
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
	rp = erts_pid2proc(NULL, 0, to, rp_locks);
	if (rp) {
	    (void) erts_send_exit_signal(NULL,
					 from,
					 rp,
					 &rp_locks,
					 reason,
					 token,
					 NULL,
					 0);
	    erts_smp_proc_unlock(rp, rp_locks);
	}
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

	rp = erts_pid2proc(NULL, 0, to, ERTS_PROC_LOCK_MAIN);
	if (!rp)
	    break;
	rp->group_leader = STORE_NC_IN_PROC(rp, from);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
	break;

    default: 
	goto invalid_message;
    }

    erts_factory_close(&factory);
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
    UnUseTmpHeapNoproc(DIST_CTL_DEFAULT_SIZE);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
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
    erts_deliver_port_exit(prt, dep->cid, am_killed, 0, 1);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
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

	    ERTS_SMP_LC_ASSERT(!ctx->c_p
			       || (ERTS_PROC_LOCK_MAIN
				   == erts_proc_lc_my_proc_locks(ctx->c_p)));

	    if (!erts_is_alive)
		return ERTS_DSIG_SEND_OK;

	    if (ctx->flags & DFLAG_DIST_HDR_ATOM_CACHE) {
		ctx->acmp = erts_get_atom_cache_map(ctx->c_p);
		ctx->pass_through_size = 0;
	    }
	    else {
		ctx->acmp = NULL;
		ctx->pass_through_size = 1;
	    }

    #ifdef ERTS_DIST_MSG_DBG
	    erts_fprintf(stderr, ">>%s CTL: %T\n", ctx->pass_through_size ? "P" : " ", ctx->ctl);
        if (is_value(ctx->msg))
            erts_fprintf(stderr, "    MSG: %T\n", ctx->msg);
    #endif

	    ctx->data_size = ctx->pass_through_size;
	    erts_reset_atom_cache_map(ctx->acmp);
	    erts_encode_dist_ext_size(ctx->ctl, ctx->flags, ctx->acmp, &ctx->data_size);

	    if (is_value(ctx->msg)) {
		ctx->u.sc.wstack.wstart = NULL;
		ctx->u.sc.flags = ctx->flags;
		ctx->u.sc.level = 0;
		ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_SIZE;
	    } else {
		ctx->phase = ERTS_DSIG_SEND_PHASE_ALLOC;
	    }
	    break;

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
	    ctx->obuf->ext_endp = &ctx->obuf->data[0] + ctx->pass_through_size + ctx->dhdr_ext_size;

	    /* Encode internal version of dist header */
	    ctx->obuf->extp = erts_encode_ext_dist_header_setup(ctx->obuf->ext_endp, ctx->acmp);
	    /* Encode control message */
	    erts_encode_dist_ext(ctx->ctl, &ctx->obuf->ext_endp, ctx->flags, ctx->acmp, NULL, NULL);
	    if (is_value(ctx->msg)) {
		ctx->u.ec.flags = ctx->flags;
		ctx->u.ec.level = 0;
		ctx->u.ec.wstack.wstart = NULL;
		ctx->phase = ERTS_DSIG_SEND_PHASE_MSG_ENCODE;
	    } else {
		ctx->phase = ERTS_DSIG_SEND_PHASE_FIN;
	    }
	    break;

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
	    ASSERT(&ctx->obuf->data[0] <= ctx->obuf->extp - ctx->pass_through_size);
	    ASSERT(ctx->obuf->ext_endp <= &ctx->obuf->data[0] + ctx->data_size);

	    ctx->data_size = ctx->obuf->ext_endp - ctx->obuf->extp;

	    /*
	     * Signal encoded; now verify that the connection still exists,
	     * and if so enqueue the signal and schedule it for send.
	     */
	    ctx->obuf->next = NULL;
	    erts_smp_de_rlock(dep);
	    cid = dep->cid;
	    if (cid != dsdp->cid
		|| dep->connection_id != dsdp->connection_id
		|| dep->status & ERTS_DE_SFLG_EXITING) {
		/* Not the same connection as when we started; drop message... */
		erts_smp_de_runlock(dep);
		free_dist_obuf(ctx->obuf);
	    }
	    else {
		ErtsProcList *plp = NULL;
		erts_smp_mtx_lock(&dep->qlock);
		dep->qsize += size_obuf(ctx->obuf);
		if (dep->qsize >= erts_dist_buf_busy_limit)
		    dep->qflgs |= ERTS_DE_QFLG_BUSY;
		if (!ctx->force_busy && (dep->qflgs & ERTS_DE_QFLG_BUSY)) {
		    erts_smp_mtx_unlock(&dep->qlock);

		    plp = erts_proclist_create(ctx->c_p);
		    erts_suspend(ctx->c_p, ERTS_PROC_LOCK_MAIN, NULL);
		    suspended = 1;
		    erts_smp_mtx_lock(&dep->qlock);
		}

		/* Enqueue obuf on dist entry */
		if (dep->out_queue.last)
		    dep->out_queue.last->next = ctx->obuf;
		else
		    dep->out_queue.first = ctx->obuf;
		dep->out_queue.last = ctx->obuf;

		if (!ctx->force_busy) {
		    if (!(dep->qflgs & ERTS_DE_QFLG_BUSY)) {
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

		erts_smp_mtx_unlock(&dep->qlock);
		erts_schedule_dist_command(NULL, dep);
		erts_smp_de_runlock(dep);

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
    Uint size = obuf->ext_endp - obuf->extp;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (size > (Uint) INT_MAX)
	erts_exit(ERTS_DUMP_EXIT,
		 "Absurdly large distribution output data buffer "
		 "(%beu bytes) passed.\n",
		 size);

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
    (*prt->drv_ptr->output)((ErlDrvData) prt->drv_data,
			    (char*) obuf->extp,
			    (int) size);
    erts_unblock_fpe(fpe_was_unmasked);
    return size;
}

static Uint
dist_port_commandv(Port *prt, ErtsDistOutputBuf *obuf)
{
    int fpe_was_unmasked;
    Uint size = obuf->ext_endp - obuf->extp;
    SysIOVec iov[2];
    ErlDrvBinary* bv[2];
    ErlIOVec eiov;

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (size > (Uint) INT_MAX)
	erts_exit(ERTS_DUMP_EXIT,
		 "Absurdly large distribution output data buffer "
		 "(%beu bytes) passed.\n",
		 size);

    iov[0].iov_base = NULL;
    iov[0].iov_len = 0;
    bv[0] = NULL;

    iov[1].iov_base = obuf->extp;
    iov[1].iov_len = size;
    bv[1] = Binary2ErlDrvBinary(ErtsDistOutputBuf2Binary(obuf));

    eiov.vsize = 2;
    eiov.size = size;
    eiov.iov = iov;
    eiov.binv = bv;

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
#define ERTS_PORT_REDS_DIST_CMD_FINALIZE 3
#define ERTS_PORT_REDS_DIST_CMD_EXIT 200
#define ERTS_PORT_REDS_DIST_CMD_RESUMED 5
#define ERTS_PORT_REDS_DIST_CMD_DATA(SZ) \
  ((SZ) < (1 << 10) \
   ? ((Sint) 1) \
   : ((((Sint) (SZ)) >> 10) & ((Sint) ERTS_PORT_REDS_MASK__)))

int
erts_dist_command(Port *prt, int reds_limit)
{
    Sint reds = ERTS_PORT_REDS_DIST_CMD_START;
    Uint32 status;
    Uint32 flags;
    Sint obufsize = 0;
    ErtsDistOutputQueue oq, foq;
    DistEntry *dep = prt->dist_entry;
    Uint (*send)(Port *prt, ErtsDistOutputBuf *obuf);
    erts_aint32_t sched_flags;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    erts_refc_inc(&dep->refc, 1); /* Otherwise dist_entry might be
				     removed if port command fails */

    erts_smp_atomic_set_mb(&dep->dist_cmd_scheduled, 0);

    erts_smp_de_rlock(dep);
    flags = dep->flags;
    status = dep->status;
    send = dep->send;
    erts_smp_de_runlock(dep);

    if (status & ERTS_DE_SFLG_EXITING) {
	erts_deliver_port_exit(prt, prt->common.id, am_killed, 0, 1);
	erts_deref_dist_entry(dep);
	return reds + ERTS_PORT_REDS_DIST_CMD_EXIT;
    }

    ASSERT(send);

    /*
     * We need to remove both out queues from the
     * dist entry while passing it to port command;
     * otherwise, port command will free the buffers
     * in the queues on failure and we'll end up with
     * a mess.
     */

    erts_smp_mtx_lock(&dep->qlock);
    oq.first = dep->out_queue.first;
    oq.last = dep->out_queue.last;
    dep->out_queue.first = NULL;
    dep->out_queue.last = NULL;
    erts_smp_mtx_unlock(&dep->qlock);

    foq.first = dep->finalized_out_queue.first;
    foq.last = dep->finalized_out_queue.last;
    dep->finalized_out_queue.first = NULL;
    dep->finalized_out_queue.last = NULL;

    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);

    if (reds > reds_limit)
	goto preempted;

    if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT) && foq.first) {
	int preempt = 0;
	do {
	    Uint size;
	    ErtsDistOutputBuf *fob;

	    size = (*send)(prt, foq.first);
	    esdp->io.out += (Uint64) size;
#ifdef ERTS_RAW_DIST_MSG_DBG
	    erts_fprintf(stderr, ">> ");
	    bw(foq.first->extp, size);
#endif
	    reds += ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = foq.first;
	    obufsize += size_obuf(fob);
	    foq.first = foq.first->next;
	    free_dist_obuf(fob);
	    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
	    preempt = reds > reds_limit || (sched_flags & ERTS_PTS_FLG_EXIT);
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
	    int preempt;
	finalize_only:
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
	    if (preempt)
		goto preempted;
	}
    }
    else {
	int preempt = 0;
	while (oq.first && !preempt) {
	    ErtsDistOutputBuf *fob;
	    Uint size;
	    oq.first->extp
		= erts_encode_ext_dist_header_finalize(oq.first->extp,
						       dep->cache,
						       flags);
	    reds += ERTS_PORT_REDS_DIST_CMD_FINALIZE;
	    if (!(flags & DFLAG_DIST_HDR_ATOM_CACHE))
		*--oq.first->extp = PASS_THROUGH; /* Old node; 'pass through'
						     needed */
	    ASSERT(&oq.first->data[0] <= oq.first->extp
		   && oq.first->extp < oq.first->ext_endp);
	    size = (*send)(prt, oq.first);
	    esdp->io.out += (Uint64) size;
#ifdef ERTS_RAW_DIST_MSG_DBG
	    erts_fprintf(stderr, ">> ");
	    bw(oq.first->extp, size);
#endif
	    reds += ERTS_PORT_REDS_DIST_CMD_DATA(size);
	    fob = oq.first;
	    obufsize += size_obuf(fob);
	    oq.first = oq.first->next;
	    free_dist_obuf(fob);
	    sched_flags = erts_smp_atomic32_read_nob(&prt->sched.flags);
	    preempt = reds > reds_limit || (sched_flags & ERTS_PTS_FLG_EXIT);
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
	erts_smp_mtx_lock(&dep->qlock);
	ASSERT(dep->qsize >= obufsize);
	dep->qsize -= obufsize;
	obufsize = 0;
	if (!(sched_flags & ERTS_PTS_FLG_BUSY_PORT)
	    && (dep->qflgs & ERTS_DE_QFLG_BUSY)
	    && dep->qsize < erts_dist_buf_busy_limit) {
	    ErtsProcList *suspendees;
	    int resumed;
	    suspendees = get_suspended_on_de(dep, ERTS_DE_QFLG_BUSY);
	    erts_smp_mtx_unlock(&dep->qlock);

	    resumed = erts_resume_processes(suspendees);
	    reds += resumed*ERTS_PORT_REDS_DIST_CMD_RESUMED;
	}
	else
	    erts_smp_mtx_unlock(&dep->qlock);
    }

    ASSERT(!oq.first && !oq.last);

 done:

    if (obufsize != 0) {
	ASSERT(obufsize > 0);
	erts_smp_mtx_lock(&dep->qlock);
	ASSERT(dep->qsize >= obufsize);
	dep->qsize -= obufsize;
	erts_smp_mtx_unlock(&dep->qlock);
    }

    ASSERT(foq.first || !foq.last);
    ASSERT(!foq.first || foq.last);
    ASSERT(!dep->finalized_out_queue.first);
    ASSERT(!dep->finalized_out_queue.last);

    if (foq.first) {
	dep->finalized_out_queue.first = foq.first;
	dep->finalized_out_queue.last = foq.last;
    }

     /* Avoid wrapping reduction counter... */
    if (reds > INT_MAX/2)
	reds = INT_MAX/2;

    erts_deref_dist_entry(dep);

    return reds;

 preempted:
    /*
     * Here we assume that state has been read
     * since last call to driver.
     */

    ASSERT(oq.first || !oq.last);
    ASSERT(!oq.first || oq.last);

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

#ifdef DEBUG
	erts_smp_mtx_lock(&dep->qlock);
	ASSERT(dep->qsize == obufsize);
	erts_smp_mtx_unlock(&dep->qlock);
#endif
    }
    else {
	if (oq.first) {
	    /*
	     * Unhandle buffers need to be put back first
	     * in out_queue.
	     */
	    erts_smp_mtx_lock(&dep->qlock);
	    dep->qsize -= obufsize;
	    obufsize = 0;
	    oq.last->next = dep->out_queue.first;
	    dep->out_queue.first = oq.first;
	    if (!dep->out_queue.last)
		dep->out_queue.last = oq.last;
	    erts_smp_mtx_unlock(&dep->qlock);
	}

	erts_schedule_dist_command(prt, NULL);
    }
    goto done;
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

void
erts_kill_dist_connection(DistEntry *dep, Uint32 connection_id)
{
    erts_smp_de_rwlock(dep);
    if (is_internal_port(dep->cid)
	&& connection_id == dep->connection_id
	&& !(dep->status & ERTS_DE_SFLG_EXITING)) {

	dep->status |= ERTS_DE_SFLG_EXITING;

	erts_smp_mtx_lock(&dep->qlock);
	ASSERT(!(dep->qflgs & ERTS_DE_QFLG_EXIT));
	dep->qflgs |= ERTS_DE_QFLG_EXIT;
	erts_smp_mtx_unlock(&dep->qlock);

	erts_schedule_dist_command(NULL, dep);
    }
    erts_smp_de_rwunlock(dep);
}

struct print_to_data {
    int to;
    void *arg;
};

static void doit_print_monitor_info(ErtsMonitor *mon, void *vptdp)
{
    int to = ((struct print_to_data *) vptdp)->to;
    void *arg = ((struct print_to_data *) vptdp)->arg;
    Process *rp;
    ErtsMonitor *rmon;
    rp = erts_proc_lookup(mon->pid);
    if (!rp || (rmon = erts_lookup_monitor(ERTS_P_MONITORS(rp), mon->ref)) == NULL) {
	erts_print(to, arg, "Warning, stray monitor for: %T\n", mon->pid);
    } else if (mon->type == MON_ORIGIN) {
	/* Local pid is being monitored */
	erts_print(to, arg, "Remotely monitored by: %T %T\n",
		   mon->pid, rmon->pid);
    } else {
	erts_print(to, arg, "Remote monitoring: %T ", mon->pid);
	if (is_not_atom(rmon->pid))
	    erts_print(to, arg, "%T\n", rmon->pid);
	else
	    erts_print(to, arg, "{%T, %T}\n",
		       rmon->name,
		       rmon->pid); /* which in this case is the 
				      remote system name... */
    }
}    

static void print_monitor_info(int to, void *arg, ErtsMonitor *mon)
{
    struct print_to_data ptd = {to, arg};
    erts_doforall_monitors(mon,&doit_print_monitor_info,&ptd);
}

typedef struct {
    struct print_to_data *ptdp;
    Eterm from;
} PrintLinkContext;

static void doit_print_link_info2(ErtsLink *lnk, void *vpplc)
{
    PrintLinkContext *pplc = (PrintLinkContext *) vpplc;
    erts_print(pplc->ptdp->to, pplc->ptdp->arg, "Remote link: %T %T\n",
	       pplc->from, lnk->pid);
}

static void doit_print_link_info(ErtsLink *lnk, void *vptdp)
{
    if (is_internal_pid(lnk->pid) && erts_proc_lookup(lnk->pid)) {
	PrintLinkContext plc = {(struct print_to_data *) vptdp, lnk->pid};
	erts_doforall_links(ERTS_LINK_ROOT(lnk), &doit_print_link_info2, &plc);
    } 
}

static void print_link_info(int to, void *arg, ErtsLink *lnk)
{
    struct print_to_data ptd = {to, arg};
    erts_doforall_links(lnk, &doit_print_link_info, (void *) &ptd);
}

typedef struct {
    struct print_to_data ptd;
    Eterm sysname;
} PrintNodeLinkContext;
    

static void doit_print_nodelink_info(ErtsLink *lnk, void *vpcontext)
{
    PrintNodeLinkContext *pcontext = vpcontext;

    if (is_internal_pid(lnk->pid) && erts_proc_lookup(lnk->pid))
	erts_print(pcontext->ptd.to, pcontext->ptd.arg,
		   "Remote monitoring: %T %T\n", lnk->pid, pcontext->sysname);
}

static void print_nodelink_info(int to, void *arg, ErtsLink *lnk, Eterm sysname)
{
    PrintNodeLinkContext context = {{to, arg}, sysname};
    erts_doforall_links(lnk, &doit_print_nodelink_info, &context);
}


static int
info_dist_entry(int to, void *arg, DistEntry *dep, int visible, int connected)
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
#ifdef DEBUG
  erts_print(to, arg, " (refc=%d)", erts_refc_read(&dep->refc, 0));
#endif
  erts_print(to, arg, "\n");
  if (!connected && is_nil(dep->cid)) {
    if (dep->nlinks) {
      erts_print(to, arg, "Error: Got links to not connected node:%T\n",
		 dep->sysname);
    }
    return 0;
  }

  erts_print(to, arg, "Controller: %T\n", dep->cid, to);

  erts_print_node_info(to, arg, dep->sysname, NULL, NULL);
  print_monitor_info(to, arg, dep->monitors);
  print_link_info(to, arg, dep->nlinks);
  print_nodelink_info(to, arg, dep->node_links, dep->sysname);

  return 0;
    
}
int distribution_info(int to, void *arg)	/* Called by break handler */
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
            dist_exit/3       -- send exit signals from remote to local process
            dist_link/2       -- link a remote process to a local
            dist_unlink/2     -- unlink a remote from a local
****************************************************************************/



/**********************************************************************
 ** Set the node name of current node fail if node already is set.
 ** setnode(name@host, Creation)
 ** loads functions pointer to trap_functions from module erlang.
 **    erlang:dsend/2
 **    erlang:dlink/1
 **    erlang:dunlink/1
 **    erlang:dmonitor_node/3
 **    erlang:dgroup_leader/2
 **    erlang:dexit/2
 **  -- are these needed ?
 **    dexit/1
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
    if (dsend2_trap->addressv[0] == NULL ||
	dsend3_trap->addressv[0] == NULL ||
	/*	dsend_nosuspend_trap->address == NULL ||*/
	dlink_trap->addressv[0] == NULL ||
	dunlink_trap->addressv[0] == NULL ||
	dmonitor_node_trap->addressv[0] == NULL ||
	dgroup_leader_trap->addressv[0] == NULL ||
	dmonitor_p_trap->addressv[0] == NULL ||
	dexit_trap->addressv[0] == NULL) {
	goto error;
    }

    net_kernel = erts_whereis_process(BIF_P, ERTS_PROC_LOCK_MAIN,
				      am_net_kernel, ERTS_PROC_LOCK_MAIN, 0);
    if (!net_kernel)
	goto error;

    /* By setting F_DISTRIBUTION on net_kernel,
     * do_net_exist will be called when net_kernel is terminated !! */
    net_kernel->flags |= F_DISTRIBUTION;

    if (net_kernel != BIF_P)
	erts_smp_proc_unlock(net_kernel, ERTS_PROC_LOCK_MAIN);

#ifdef DEBUG
    erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);
    ASSERT(!erts_visible_dist_entries && !erts_hidden_dist_entries);
    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
#endif

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();
    inc_no_nodes();
    erts_set_this_node(BIF_ARG_1, (Uint32) creation);
    erts_is_alive = 1;
    send_nodes_mon_msgs(NULL, am_nodeup, BIF_ARG_1, am_visible, NIL);
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

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
    Port *pp = NULL;

    /* Prepare for success */
    ERTS_BIF_PREP_RET(ret, am_true);

    /*
     * Check and pick out arguments
     */

    if (!is_node_name_atom(BIF_ARG_1) ||
	is_not_internal_port(BIF_ARG_2) ||
	(erts_this_node->sysname == am_Noname)) {
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

    /* DFLAG_EXTENDED_REFERENCES is compulsory from R9 and forward */
    if (!(DFLAG_EXTENDED_REFERENCES & flags)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "%T", BIF_P->common.id);
	if (BIF_P->common.u.alive.reg)
	    erts_dsprintf(dsbufp, " (%T)", BIF_P->common.u.alive.reg->name);
	erts_dsprintf(dsbufp,
		      " attempted to enable connection to node %T "
		      "which is not able to handle extended references.\n",
		      BIF_ARG_1);
	erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	goto badarg;
    }

    /*
     * Arguments seem to be in order.
     */

    /* get dist_entry */
    dep = erts_find_or_insert_dist_entry(BIF_ARG_1);
    if (dep == erts_this_dist_entry)
	goto badarg;
    else if (!dep)
	goto system_limit; /* Should never happen!!! */

    pp = erts_id2port_sflgs(BIF_ARG_2,
			    BIF_P,
			    ERTS_PROC_LOCK_MAIN,
			    ERTS_PORT_SFLGS_INVALID_LOOKUP);
    erts_smp_de_rwlock(dep);

    if (!pp || (erts_atomic32_read_nob(&pp->state)
		& ERTS_PORT_SFLG_EXITING))
	goto badarg;

    if ((pp->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY) == 0)
	goto badarg;

    if (dep->cid == BIF_ARG_2 && pp->dist_entry == dep)
	goto done; /* Already set */

    if (dep->status & ERTS_DE_SFLG_EXITING) {
	/* Suspend on dist entry waiting for the exit to finish */
	ErtsProcList *plp = erts_proclist_create(BIF_P);
	plp->next = NULL;
	erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
	erts_smp_mtx_lock(&dep->qlock);
	erts_proclist_store_last(&dep->suspended, plp);
	erts_smp_mtx_unlock(&dep->qlock);
	goto yield;
    }

    ASSERT(!(dep->status & ERTS_DE_SFLG_EXITING));

    if (pp->dist_entry || is_not_nil(dep->cid))
	goto badarg;

    erts_atomic32_read_bor_nob(&pp->state, ERTS_PORT_SFLG_DISTRIBUTION);

    /*
     * Dist-ports do not use the "busy port message queue" functionality, but
     * instead use "busy dist entry" functionality.
     */
    {
	ErlDrvSizeT disable = ERL_DRV_BUSY_MSGQ_DISABLED;
	erl_drv_busy_msgq_limits(ERTS_Port2ErlDrvPort(pp), &disable, NULL);
    }

    pp->dist_entry = dep;

    dep->version = version;
    dep->creation = 0;

    ASSERT(pp->drv_ptr->outputv || pp->drv_ptr->output);

#if 1
    dep->send = (pp->drv_ptr->outputv
		 ? dist_port_commandv
		 : dist_port_command);
#else
    dep->send = dist_port_command;
#endif
    ASSERT(dep->send);

#ifdef DEBUG
    erts_smp_mtx_lock(&dep->qlock);
    ASSERT(dep->qsize == 0);
    erts_smp_mtx_unlock(&dep->qlock);
#endif

    erts_set_dist_entry_connected(dep, BIF_ARG_2, flags);

    if (flags & DFLAG_DIST_HDR_ATOM_CACHE)
	create_cache(dep);

    erts_smp_de_rwunlock(dep);
    dep = NULL; /* inc of refc transferred to port (dist_entry field) */

    inc_no_nodes();

    send_nodes_mon_msgs(BIF_P,
			am_nodeup,
			BIF_ARG_1,
			flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			NIL);
 done:

    if (dep && dep != erts_this_dist_entry) {
	erts_smp_de_rwunlock(dep);
	erts_deref_dist_entry(dep);
    }

    if (pp)
	erts_port_release(pp);

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


/**********************************************************************/
/* dist_exit(Local, Term, Remote) -> Bool */

BIF_RETTYPE dist_exit_3(BIF_ALIST_3)
{
    Eterm local;
    Eterm remote;
    DistEntry *rdep;

    local = BIF_ARG_1;
    remote = BIF_ARG_3;

    /* Check that remote is a remote process */
    if (is_not_external_pid(remote))
	goto error;

    rdep = external_dist_entry(remote);
    
    if(rdep == erts_this_dist_entry)
	goto error;

    /* Check that local is local */
    if (is_internal_pid(local)) {
	Process *lp;
	ErtsProcLocks lp_locks;
	if (BIF_P->common.id == local) {
	    lp_locks = ERTS_PROC_LOCKS_ALL;
	    lp = BIF_P;
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
	}
	else {
	    lp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	    lp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
			       local, lp_locks);
	    if (!lp) {
		BIF_RET(am_true); /* ignore */
	    }
	}
	
	(void) erts_send_exit_signal(BIF_P,
				     remote,
				     lp,
				     &lp_locks,
				     BIF_ARG_2,
				     NIL,
				     NULL,
				     0);
#ifdef ERTS_SMP
	if (lp == BIF_P)
	    lp_locks &= ~ERTS_PROC_LOCK_MAIN;
#endif
	erts_smp_proc_unlock(lp, lp_locks);
	if (lp == BIF_P) {
	    erts_aint32_t state = erts_smp_atomic32_read_acqb(&BIF_P->state);
	    /*
	     * We may have exited current process and may have to take action.
	     */
	    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT)) {
#ifdef ERTS_SMP
		if (state & ERTS_PSFLG_PENDING_EXIT)
		    erts_handle_pending_exit(BIF_P, ERTS_PROC_LOCK_MAIN);
#endif
		ERTS_BIF_EXITED(BIF_P);
	    }
	}
    }
    else if (is_external_pid(local)
	     && external_dist_entry(local) == erts_this_dist_entry) {
	BIF_RET(am_true); /* ignore */
    }
    else
	goto error;
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
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

    erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);

    ASSERT(erts_no_of_not_connected_dist_entries > 0);
    ASSERT(erts_no_of_hidden_dist_entries >= 0);
    ASSERT(erts_no_of_visible_dist_entries >= 0);
    if(not_connected)
      length += (erts_no_of_not_connected_dist_entries - 1);
    if(hidden)
      length += erts_no_of_hidden_dist_entries;
    if(visible)
      length += erts_no_of_visible_dist_entries;
    if(this)
      length++;

    result = NIL;

    if (length == 0) {
	erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
	goto done;
    }

    hp = HAlloc(BIF_P, 2*length);

#ifdef DEBUG
    endp = hp + length*2;
#endif
    if(not_connected)
      for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
          if (dep != erts_this_dist_entry) {
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
    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);

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
    DistEntry *dep;
    ErtsLink *lnk;
    Eterm l;

    for (l = Options; l != NIL && is_list(l); l = CDR(list_val(l))) {
	Eterm t = CAR(list_val(l));
	/* allow_passive_connect the only available option right now */
	if (t != am_allow_passive_connect) {
	    BIF_ERROR(p, BADARG);
	}
    }
    if (l != NIL) {
	BIF_ERROR(p, BADARG);
    }

    if (is_not_atom(Node) ||
	((Bool != am_true) && (Bool != am_false)) ||
	((erts_this_node->sysname == am_Noname)
	 && (Node != erts_this_node->sysname))) {
	BIF_ERROR(p, BADARG);
    }
    dep = erts_sysname_to_connected_dist_entry(Node);
    if (!dep) {
    do_trap:
	BIF_TRAP3(dmonitor_node_trap, p, Node, Bool, Options);
    }
    if (dep == erts_this_dist_entry)
	goto done;

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_LINK);
    erts_smp_de_rlock(dep);
    if (ERTS_DE_IS_NOT_CONNECTED(dep)) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
	erts_smp_de_runlock(dep);
	goto do_trap;
    }
    erts_smp_de_links_lock(dep);
    erts_smp_de_runlock(dep);

    if (Bool == am_true) {
	ASSERT(dep->cid != NIL);
	lnk = erts_add_or_lookup_link(&(dep->node_links), LINK_NODE, 
				      p->common.id);
	++ERTS_LINK_REFC(lnk);
	lnk = erts_add_or_lookup_link(&ERTS_P_LINKS(p), LINK_NODE, Node);
	++ERTS_LINK_REFC(lnk);
    }
    else  {
	lnk = erts_lookup_link(dep->node_links, p->common.id);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_REFC(lnk)) == 0) {
		erts_destroy_link(erts_remove_link(&(dep->node_links), 
						   p->common.id));
	    }
	}
	lnk = erts_lookup_link(ERTS_P_LINKS(p), Node);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_REFC(lnk)) == 0) {
		erts_destroy_link(erts_remove_link(&ERTS_P_LINKS(p),
						   Node));
	    }
	}
    }

    erts_smp_de_links_unlock(dep);
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);

 done:
    erts_deref_dist_entry(dep);
    BIF_RET(am_true);
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
    erts_smp_de_rlock(de);
    f = de->flags;
    erts_smp_de_runlock(de);
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

typedef struct ErtsNodesMonitor_ ErtsNodesMonitor;
struct ErtsNodesMonitor_ {
    ErtsNodesMonitor *prev;
    ErtsNodesMonitor *next;
    Process *proc;
    Uint16 opts;
    Uint16 no;
};

static erts_smp_mtx_t nodes_monitors_mtx;
static ErtsNodesMonitor *nodes_monitors;
static ErtsNodesMonitor *nodes_monitors_end;

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
    erts_smp_mtx_init(&nodes_monitors_mtx, "nodes_monitors");
    nodes_monitors = NULL;
    nodes_monitors_end = NULL;
}

static ERTS_INLINE Uint
nodes_mon_msg_sz(ErtsNodesMonitor *nmp, Eterm what, Eterm reason)
{
    Uint sz;
    if (!nmp->opts) {
	sz = 3;
    }
    else {
	sz = 0;

	if (nmp->opts & ERTS_NODES_MON_OPT_TYPES)
	    sz += 2 + 3;

	if (what == am_nodedown
	    && (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
	    if (is_not_immed(reason))
		sz += size_object(reason);
	    sz += 2 + 3;
	}

	sz += 4;
    }
    return sz;
}

static ERTS_INLINE void
send_nodes_mon_msg(Process *rp,
		   ErtsProcLocks *rp_locksp,
		   ErtsNodesMonitor *nmp,
		   Eterm node,
		   Eterm what,
		   Eterm type,
		   Eterm reason,
		   Uint sz)
{
    Eterm msg;
    Eterm *hp;
    ErtsMessage *mp;
    ErlOffHeap *ohp;
#ifdef DEBUG
    Eterm *hend;
#endif

    mp = erts_alloc_message_heap(rp, rp_locksp, sz, &hp, &ohp);
#ifdef DEBUG
    hend = hp + sz;
#endif

    if (!nmp->opts) {
	msg = TUPLE2(hp, what, node);
#ifdef DEBUG
	hp += 3;
#endif
    }
    else {
	Eterm tup;
	Eterm info = NIL;

	if (nmp->opts & (ERTS_NODES_MON_OPT_TYPE_VISIBLE
			 | ERTS_NODES_MON_OPT_TYPE_HIDDEN)) {

	    tup = TUPLE2(hp, am_node_type, type);
	    hp += 3;
	    info = CONS(hp, tup, info);
	    hp += 2;
	}

	if (what == am_nodedown
	    && (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
	    Eterm rsn_cpy;
	    
	    if (is_immed(reason))
		rsn_cpy = reason;
	    else {
		Eterm rsn_sz = size_object(reason);
		rsn_cpy = copy_struct(reason, rsn_sz, &hp, ohp);
	    }

	    tup = TUPLE2(hp, am_nodedown_reason, rsn_cpy);
	    hp += 3;
	    info = CONS(hp, tup, info);
	    hp += 2;
	}

	msg = TUPLE3(hp, what, node, info);
#ifdef DEBUG
	hp += 4;
#endif
    }

    ASSERT(hend == hp);
    erts_queue_message(rp, *rp_locksp, mp, msg, am_system);
}

static void
send_nodes_mon_msgs(Process *c_p, Eterm what, Eterm node, Eterm type, Eterm reason)
{
    ErtsNodesMonitor *nmp;
    ErtsProcLocks rp_locks = 0; /* Init to shut up false warning */
    Process *rp = NULL;

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

    ERTS_SMP_LC_ASSERT(!c_p
		       || (erts_proc_lc_my_proc_locks(c_p)
			   == ERTS_PROC_LOCK_MAIN));
    erts_smp_mtx_lock(&nodes_monitors_mtx);

    for (nmp = nodes_monitors; nmp; nmp = nmp->next) {
	int i;
	Uint16 no;
	Uint sz;

	ASSERT(nmp->proc != NULL);

	if (!nmp->opts) {
	    if (type != am_visible)
		continue;
	}
	else {
	    switch (type) {
	    case am_hidden:
		if (!(nmp->opts & ERTS_NODES_MON_OPT_TYPE_HIDDEN))
		    continue;
		break;
	    case am_visible:
		if ((nmp->opts & ERTS_NODES_MON_OPT_TYPES)
		    && !(nmp->opts & ERTS_NODES_MON_OPT_TYPE_VISIBLE))
		    continue;
		break;
	    default:
		erts_exit(ERTS_ABORT_EXIT, "Bad node type found\n");
	    }
	}

	if (rp != nmp->proc) {
	    if (rp) {
		if (rp == c_p)
		    rp_locks &= ~ERTS_PROC_LOCK_MAIN;
		erts_smp_proc_unlock(rp, rp_locks);
	    }

	    rp = nmp->proc;
	    rp_locks = 0;
	    if (rp == c_p)
		rp_locks |= ERTS_PROC_LOCK_MAIN;
	}

	ASSERT(rp);

	sz = nodes_mon_msg_sz(nmp, what, reason);

	for (i = 0, no = nmp->no; i < no; i++)
	    send_nodes_mon_msg(rp,
			       &rp_locks,
			       nmp,
			       node,
			       what,
			       type,
			       reason,
			       sz);
    }

    if (rp) {
	if (rp == c_p)
	    rp_locks &= ~ERTS_PROC_LOCK_MAIN;
	erts_smp_proc_unlock(rp, rp_locks);
    }

    erts_smp_mtx_unlock(&nodes_monitors_mtx);
}

static Eterm
insert_nodes_monitor(Process *c_p, Uint32 opts)
{
    Uint16 no = 1;
    Eterm res = am_false;
    ErtsNodesMonitor *xnmp, *nmp;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&nodes_monitors_mtx));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) & ERTS_PROC_LOCK_MAIN);

    xnmp = c_p->nodes_monitors;
    if (xnmp) {
	ASSERT(!xnmp->prev || xnmp->prev->proc != c_p);

	while (1) {
	    ASSERT(xnmp->proc == c_p);
	    if (xnmp->opts == opts)
		break;
	    if (!xnmp->next || xnmp->next->proc != c_p)
		break;
	    xnmp = xnmp->next;
	}
	ASSERT(xnmp);
	ASSERT(xnmp->proc == c_p);
	ASSERT(xnmp->opts == opts
	       || !xnmp->next
	       || xnmp->next->proc != c_p);

	if (xnmp->opts != opts)
	    goto alloc_new;
	else {
	    res = am_true;
	    no = xnmp->no++;
	    if (!xnmp->no) {
		/*
		 * 'no' wrapped; transfer all prevous monitors to new
		 * element (which will be the next element in the list)
		 * and set this to one...
		 */
		xnmp->no = 1;
		goto alloc_new;
	    }
	}
    }
    else {
    alloc_new:
	nmp = erts_alloc(ERTS_ALC_T_NODES_MON, sizeof(ErtsNodesMonitor));
	nmp->proc = c_p;
	nmp->opts = opts;
	nmp->no = no;

	if (xnmp) {
	    ASSERT(nodes_monitors);
	    ASSERT(c_p->nodes_monitors);
	    nmp->next = xnmp->next;
	    nmp->prev = xnmp;
	    xnmp->next = nmp;
	    if (nmp->next) {
		ASSERT(nodes_monitors_end != xnmp);
		ASSERT(nmp->next->prev == xnmp);
		nmp->next->prev = nmp;
	    }
	    else {
		ASSERT(nodes_monitors_end == xnmp);
		nodes_monitors_end = nmp;
	    }
	}
	else {
	    ASSERT(!c_p->nodes_monitors);
	    c_p->nodes_monitors = nmp;
	    nmp->next = NULL;
	    nmp->prev = nodes_monitors_end;
	    if (nodes_monitors_end) {
		ASSERT(nodes_monitors);
		nodes_monitors_end->next = nmp;
	    }
	    else {
		ASSERT(!nodes_monitors);
		nodes_monitors = nmp;
	    }
	    nodes_monitors_end = nmp;
	}
    }
    return res;
}

static Eterm
remove_nodes_monitors(Process *c_p, Uint32 opts, int all)
{
    Eterm res = am_false;
    ErtsNodesMonitor *nmp;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&nodes_monitors_mtx));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) & ERTS_PROC_LOCK_MAIN);

    nmp = c_p->nodes_monitors;
    ASSERT(!nmp || !nmp->prev || nmp->prev->proc != c_p);

    while (nmp && nmp->proc == c_p) {
	if (!all && nmp->opts != opts)
	    nmp = nmp->next;
	else { /* if (all || nmp->opts == opts) */
	    ErtsNodesMonitor *free_nmp;
	    res = am_true;
	    if (nmp->prev) {
		ASSERT(nodes_monitors != nmp);
		nmp->prev->next = nmp->next;
	    }
	    else {
		ASSERT(nodes_monitors == nmp);
		nodes_monitors = nmp->next;
	    }
	    if (nmp->next) {
		ASSERT(nodes_monitors_end != nmp);
		nmp->next->prev = nmp->prev;
	    }
	    else {
		ASSERT(nodes_monitors_end == nmp);
		nodes_monitors_end = nmp->prev;
	    }
	    free_nmp = nmp;
	    nmp = nmp->next;
	    if (c_p->nodes_monitors == free_nmp)
		c_p->nodes_monitors = nmp && nmp->proc == c_p ? nmp : NULL;
	    erts_free(ERTS_ALC_T_NODES_MON, free_nmp);
	}
    }
    
    ASSERT(!all || !c_p->nodes_monitors);
    return res;
}

void
erts_delete_nodes_monitors(Process *c_p, ErtsProcLocks locks)
{
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
    if (c_p) {
	ErtsProcLocks might_unlock = locks & ~ERTS_PROC_LOCK_MAIN;
	if (might_unlock)
	    erts_proc_lc_might_unlock(c_p, might_unlock);
    }
#endif
    if (erts_smp_mtx_trylock(&nodes_monitors_mtx) == EBUSY) {
	ErtsProcLocks unlock_locks = locks & ~ERTS_PROC_LOCK_MAIN;
	if (c_p && unlock_locks)
	    erts_smp_proc_unlock(c_p, unlock_locks);
	erts_smp_mtx_lock(&nodes_monitors_mtx);
	if (c_p && unlock_locks)
	    erts_smp_proc_lock(c_p, unlock_locks);
    }
    remove_nodes_monitors(c_p, 0, 1);
    erts_smp_mtx_unlock(&nodes_monitors_mtx);
}

Eterm
erts_monitor_nodes(Process *c_p, Eterm on, Eterm olist)
{
    Eterm res;
    Eterm opts_list = olist;
    Uint16 opts = (Uint16) 0;

    ASSERT(c_p);
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

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

    erts_smp_mtx_lock(&nodes_monitors_mtx);

    if (on == am_true)
	res = insert_nodes_monitor(c_p, opts);
    else
	res = remove_nodes_monitors(c_p, opts, 0);

    erts_smp_mtx_unlock(&nodes_monitors_mtx);

    return res;
}

/*
 * Note, this function is only used for debuging.
 */

Eterm
erts_processes_monitoring_nodes(Process *c_p)
{
    ErtsNodesMonitor *nmp;
    Eterm res;
    Eterm *hp;
    Eterm **hpp;
    Uint sz;
    Uint *szp;
#ifdef DEBUG
    Eterm *hend;
#endif

    ASSERT(c_p);
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);
    erts_smp_mtx_lock(&nodes_monitors_mtx);

    sz = 0;
    szp = &sz;
    hpp = NULL;

 bld_result:
    res = NIL;

    for (nmp = nodes_monitors_end; nmp; nmp = nmp->prev) {
	Uint16 i;
	for (i = 0; i < nmp->no; i++) {
	    Eterm olist = NIL;
	    if (nmp->opts & ERTS_NODES_MON_OPT_TYPES) {
		Eterm type;
		switch (nmp->opts & ERTS_NODES_MON_OPT_TYPES) {
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
	    if (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)
		olist = erts_bld_cons(hpp, szp, am_nodedown_reason, olist);
	    res = erts_bld_cons(hpp, szp,
				erts_bld_tuple(hpp, szp, 2,
					       nmp->proc->common.id,
					       olist),
				res);
	}
    }

    if (!hpp) {
	hp = HAlloc(c_p, sz);
#ifdef DEBUG
	hend = hp + sz;
#endif
	hpp = &hp;
	szp = NULL;
	goto bld_result;
    }

    ASSERT(hp == hend);

    erts_smp_mtx_unlock(&nodes_monitors_mtx);

    return res;
}
