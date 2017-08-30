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

#ifndef __DIST_H__
#define __DIST_H__

#include "erl_process.h"
#include "erl_node_tables.h"
#include "zlib.h"

#define DFLAG_PUBLISHED           0x01
#define DFLAG_ATOM_CACHE          0x02
#define DFLAG_EXTENDED_REFERENCES 0x04
#define DFLAG_DIST_MONITOR        0x08
#define DFLAG_FUN_TAGS            0x10
#define DFLAG_DIST_MONITOR_NAME   0x20
#define DFLAG_HIDDEN_ATOM_CACHE   0x40
#define DFLAG_NEW_FUN_TAGS        0x80
#define DFLAG_EXTENDED_PIDS_PORTS 0x100
#define DFLAG_EXPORT_PTR_TAG      0x200
#define DFLAG_BIT_BINARIES        0x400
#define DFLAG_NEW_FLOATS          0x800
#define DFLAG_UNICODE_IO          0x1000
#define DFLAG_DIST_HDR_ATOM_CACHE 0x2000
#define DFLAG_SMALL_ATOM_TAGS     0x4000
#define DFLAG_INTERNAL_TAGS       0x8000
#define DFLAG_UTF8_ATOMS          0x10000
#define DFLAG_MAP_TAG             0x20000
#define DFLAG_BIG_CREATION        0x40000

/* All flags that should be enabled when term_to_binary/1 is used. */
#define TERM_TO_BINARY_DFLAGS (DFLAG_EXTENDED_REFERENCES	\
			       | DFLAG_NEW_FUN_TAGS		\
			       | DFLAG_NEW_FLOATS		\
			       | DFLAG_EXTENDED_PIDS_PORTS	\
			       | DFLAG_EXPORT_PTR_TAG		\
			       | DFLAG_BIT_BINARIES             \
			       | DFLAG_MAP_TAG                  \
                               | DFLAG_BIG_CREATION)

/* opcodes used in distribution messages */
#define DOP_LINK		1
#define DOP_SEND		2
#define DOP_EXIT		3
#define DOP_UNLINK		4
/* Ancient DOP_NODE_LINK (5) was here, can be reused */
#define DOP_REG_SEND		6
#define DOP_GROUP_LEADER	7
#define DOP_EXIT2		8

#define DOP_SEND_TT		12
#define DOP_EXIT_TT		13
#define DOP_REG_SEND_TT		16
#define DOP_EXIT2_TT		18

#define DOP_MONITOR_P		19
#define DOP_DEMONITOR_P		20
#define DOP_MONITOR_P_EXIT	21

/* distribution trap functions */
extern Export* dsend2_trap;
extern Export* dsend3_trap;
extern Export* dlink_trap;
extern Export* dunlink_trap;
extern Export* dmonitor_node_trap;
extern Export* dgroup_leader_trap;
extern Export* dexit_trap;
extern Export* dmonitor_p_trap;

typedef enum {
    ERTS_DSP_NO_LOCK,
    ERTS_DSP_RLOCK,
    ERTS_DSP_RWLOCK
} ErtsDSigPrepLock;


typedef struct {
    Process *proc;
    DistEntry *dep;
    Eterm cid;
    Eterm connection_id;
    int no_suspend;
} ErtsDSigData;

#define ERTS_DE_IS_NOT_CONNECTED(DEP) \
  (ERTS_SMP_LC_ASSERT(erts_lc_rwmtx_is_rlocked(&(DEP)->rwmtx) \
		      || erts_lc_rwmtx_is_rwlocked(&(DEP)->rwmtx)), \
   (is_nil((DEP)->cid) || ((DEP)->status & ERTS_DE_SFLG_EXITING)))

#define ERTS_DE_IS_CONNECTED(DEP) \
  (!ERTS_DE_IS_NOT_CONNECTED((DEP)))

#define ERTS_DE_BUSY_LIMIT (1024*1024)
extern int erts_dist_buf_busy_limit;
extern int erts_is_alive;

/*
 * erts_dsig_prepare() prepares a send of a distributed signal.
 * One of the values defined below are returned. If the returned
 * value is another than ERTS_DSIG_PREP_CONNECTED, the
 * distributed signal cannot be sent before apropriate actions
 * have been taken. Apropriate actions would typically be setting
 * up the connection.
 */

/* Connected; signal can be sent. */
#define ERTS_DSIG_PREP_CONNECTED	0
/* Not connected; connection needs to be set up. */
#define ERTS_DSIG_PREP_NOT_CONNECTED	1
/* Caller would be suspended on send operation. */
#define ERTS_DSIG_PREP_WOULD_SUSPEND	2
/* System not alive (distributed) */
#define ERTS_DSIG_PREP_NOT_ALIVE	3

ERTS_GLB_INLINE int erts_dsig_prepare(ErtsDSigData *,
				      DistEntry *,
				      Process *,
				      ErtsDSigPrepLock,
				      int);

ERTS_GLB_INLINE
void erts_schedule_dist_command(Port *, DistEntry *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int 
erts_dsig_prepare(ErtsDSigData *dsdp,
		  DistEntry *dep,
		  Process *proc,
		  ErtsDSigPrepLock dspl,
		  int no_suspend)
{
    int failure;
    if (!erts_is_alive)
	return ERTS_DSIG_PREP_NOT_ALIVE;
    if (!dep)
	return ERTS_DSIG_PREP_NOT_CONNECTED;
    if (dspl == ERTS_DSP_RWLOCK)
	erts_smp_de_rwlock(dep);
    else
	erts_smp_de_rlock(dep);
    if (ERTS_DE_IS_NOT_CONNECTED(dep)) {
	failure = ERTS_DSIG_PREP_NOT_CONNECTED;
	goto fail;
    }
    if (no_suspend) {
	failure = ERTS_DSIG_PREP_CONNECTED;
	erts_smp_mtx_lock(&dep->qlock);
	if (dep->qflgs & ERTS_DE_QFLG_BUSY)
	    failure = ERTS_DSIG_PREP_WOULD_SUSPEND;
	erts_smp_mtx_unlock(&dep->qlock);
	if (failure == ERTS_DSIG_PREP_WOULD_SUSPEND)
	    goto fail;
    }
    dsdp->proc = proc;
    dsdp->dep = dep;
    dsdp->cid = dep->cid;
    dsdp->connection_id = dep->connection_id;
    dsdp->no_suspend = no_suspend;
    if (dspl == ERTS_DSP_NO_LOCK)
	erts_smp_de_runlock(dep);
    return ERTS_DSIG_PREP_CONNECTED;

 fail:
    if (dspl == ERTS_DSP_RWLOCK)
	erts_smp_de_rwunlock(dep);
    else
	erts_smp_de_runlock(dep);
    return failure;

}

ERTS_GLB_INLINE
void erts_schedule_dist_command(Port *prt, DistEntry *dist_entry)
{
    DistEntry *dep;
    Eterm id;

    if (prt) {
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));
	ASSERT((erts_atomic32_read_nob(&prt->state)
		& ERTS_PORT_SFLGS_DEAD) == 0);
	ASSERT(prt->dist_entry);

	dep = prt->dist_entry;
	id = prt->common.id;
    }
    else {
	ASSERT(dist_entry);
	ERTS_SMP_LC_ASSERT(erts_lc_rwmtx_is_rlocked(&dist_entry->rwmtx)
			   || erts_lc_rwmtx_is_rwlocked(&dist_entry->rwmtx));
	ASSERT(is_internal_port(dist_entry->cid));

 	dep = dist_entry;
	id = dep->cid;
    }

    if (!erts_smp_atomic_xchg_mb(&dep->dist_cmd_scheduled, 1))
	erts_port_task_schedule(id, &dep->dist_cmd, ERTS_PORT_TASK_DIST_CMD);
}

#endif

typedef struct {
    ErtsLink *d_lnk;
    ErtsLink *d_sub_lnk;
} ErtsDistLinkData;

ERTS_GLB_INLINE void erts_remove_dist_link(ErtsDistLinkData *,
					   Eterm,
					   Eterm,
					   DistEntry *);
ERTS_GLB_INLINE int erts_was_dist_link_removed(ErtsDistLinkData *);
ERTS_GLB_INLINE void erts_destroy_dist_link(ErtsDistLinkData *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_remove_dist_link(ErtsDistLinkData *dldp,
		      Eterm lid,
		      Eterm rid,
		      DistEntry *dep)
{
    erts_smp_de_links_lock(dep);
    dldp->d_lnk = erts_lookup_link(dep->nlinks, lid);
    if (!dldp->d_lnk)
	dldp->d_sub_lnk = NULL;
    else {
	dldp->d_sub_lnk = erts_remove_link(&ERTS_LINK_ROOT(dldp->d_lnk), rid);
	dldp->d_lnk = (ERTS_LINK_ROOT(dldp->d_lnk)
		       ? NULL
		       : erts_remove_link(&dep->nlinks, lid));
    }
    erts_smp_de_links_unlock(dep);
}

ERTS_GLB_INLINE int
erts_was_dist_link_removed(ErtsDistLinkData *dldp)
{
    return dldp->d_sub_lnk != NULL;
}

ERTS_GLB_INLINE void
erts_destroy_dist_link(ErtsDistLinkData *dldp)
{
    if (dldp->d_lnk)
	erts_destroy_link(dldp->d_lnk);
    if (dldp->d_sub_lnk)
	erts_destroy_link(dldp->d_sub_lnk);	
}

#endif



/* Define for testing */
/* #define EXTREME_TTB_TRAPPING 1 */

#ifndef EXTREME_TTB_TRAPPING
#define TERM_TO_BINARY_LOOP_FACTOR 32
#else
#define TERM_TO_BINARY_LOOP_FACTOR 1
#endif

typedef enum { TTBSize, TTBEncode, TTBCompress } TTBState;
typedef struct TTBSizeContext_ {
    Uint flags;
    int level;
    Uint result;
    Eterm obj;
    ErtsWStack wstack;
} TTBSizeContext;

typedef struct TTBEncodeContext_ {
    Uint flags;
    int level;
    byte* ep;
    Eterm obj;
    ErtsWStack wstack;
    Binary *result_bin;
} TTBEncodeContext;

typedef struct {
    Uint real_size;
    Uint dest_len;
    byte *dbytes;
    Binary *result_bin;
    Binary *destination_bin;
    z_stream stream;
} TTBCompressContext;

typedef struct {
    int alive;
    TTBState state;
    union {
	TTBSizeContext sc;
	TTBEncodeContext ec;
	TTBCompressContext cc;
    } s;
} TTBContext;

enum erts_dsig_send_phase {
    ERTS_DSIG_SEND_PHASE_INIT,
    ERTS_DSIG_SEND_PHASE_MSG_SIZE,
    ERTS_DSIG_SEND_PHASE_ALLOC,
    ERTS_DSIG_SEND_PHASE_MSG_ENCODE,
    ERTS_DSIG_SEND_PHASE_FIN
};

struct erts_dsig_send_context {
    enum erts_dsig_send_phase phase;
    Sint reds;

    Eterm ctl;
    Eterm msg;
    int force_busy;
    Uint32 pass_through_size;
    Uint data_size, dhdr_ext_size;
    ErtsAtomCacheMap *acmp;
    ErtsDistOutputBuf *obuf;
    Uint32 flags;
    Process *c_p;
    union {
	TTBSizeContext sc;
	TTBEncodeContext ec;
    }u;
};

typedef struct {
    int suspend;

    Eterm ctl_heap[6];
    ErtsDSigData dsd;
    DistEntry* dep_to_deref;
    struct erts_dsig_send_context dss;

    Eterm return_term;
}ErtsSendContext;


/*
 * erts_dsig_send_* return values.
 */
#define ERTS_DSIG_SEND_OK	0
#define ERTS_DSIG_SEND_YIELD	1
#define ERTS_DSIG_SEND_CONTINUE 2

extern int erts_dsig_send_link(ErtsDSigData *, Eterm, Eterm);
extern int erts_dsig_send_msg(Eterm, Eterm, ErtsSendContext*);
extern int erts_dsig_send_exit_tt(ErtsDSigData *, Eterm, Eterm, Eterm, Eterm);
extern int erts_dsig_send_unlink(ErtsDSigData *, Eterm, Eterm);
extern int erts_dsig_send_reg_msg(Eterm, Eterm, ErtsSendContext*);
extern int erts_dsig_send_group_leader(ErtsDSigData *, Eterm, Eterm);
extern int erts_dsig_send_exit(ErtsDSigData *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_exit2(ErtsDSigData *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_demonitor(ErtsDSigData *, Eterm, Eterm, Eterm, int);
extern int erts_dsig_send_monitor(ErtsDSigData *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_m_exit(ErtsDSigData *, Eterm, Eterm, Eterm, Eterm);

extern int erts_dsig_send(ErtsDSigData *dsdp, struct erts_dsig_send_context* ctx);
extern void erts_dsend_context_dtor(Binary*);
extern Eterm erts_dsend_export_trap_context(Process* p, ErtsSendContext* ctx);

extern int erts_dist_command(Port *prt, int reds);
extern void erts_dist_port_not_busy(Port *prt);
extern void erts_kill_dist_connection(DistEntry *dep, Uint32);

extern Uint erts_dist_cache_size(void);


#endif
