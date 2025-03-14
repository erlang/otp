/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
 * Description:	Process signal queue implementation.
 *
 * Author: 	Rickard Green
 */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "dist.h"
#include "erl_process.h"
#include "erl_port_task.h"
#include "erl_trace.h"
#include "beam_bp.h"
#include "erl_binary.h"
#include "big.h"
#include "erl_gc.h"
#include "bif.h"
#include "erl_bif_unique.h"
#include "erl_proc_sig_queue.h"
#include "erl_check_io.h"
#include "erl_global_literals.h"
#include "erl_map.h"
#include "dtrace-wrapper.h"

#define ERTS_SIG_REDS_CNT_FACTOR 4
#define ERTS_PROC_SIG_TRACE_COUNT_LIMIT 200

#define ERTS_SIG_IS_GEN_EXIT(sig)                                       \
    (ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag) == ERTS_SIG_Q_TYPE_GEN_EXIT)
#define ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig)                              \
    (ASSERT(ERTS_SIG_IS_GEN_EXIT(sig)),is_non_value(get_exit_signal_data(sig)->reason))


#define ERTS_SIG_LNK_X_FLAG_NORMAL_KILLS        (((Uint32) 1) << 0)
#define ERTS_SIG_LNK_X_FLAG_CONNECTION_LOST     (((Uint32) 1) << 1)

#define ERTS_SIG_ALTACT_SIG_X_PRIO              (1 << 0)
#define ERTS_SIG_ALTACT_SIG_X_TOKEN             (1 << 1)
#define ERTS_SIG_ALTACT_SIG_X_ALIAS             (1 << 2)

#define ERTS_PROC_SIG_ADJ_MSGQ_SCAN_FACTOR \
    (ERTS_CLA_SCAN_WORDS_PER_RED / ERTS_SIG_REDS_CNT_FACTOR)
#define ERTS_PROC_SIG_ADJ_MSGQ_COPY_FACTOR  \
    (ERTS_PROC_SIG_ADJ_MSGQ_SCAN_FACTOR / 8)
#define ERTS_PROC_SIG_ADJ_MSGQ_MSGS_FACTOR \
    25

#ifdef USE_VM_PROBES
#  define ERTS_CLEAR_SEQ_TOKEN_VALUE(MP)                                \
    ((ERL_MESSAGE_DT_UTAG((MP)) != NIL) ? am_have_dt_utag : NIL)
#else
#  define ERTS_CLEAR_SEQ_TOKEN_VALUE(MP) NIL
#endif
#define ERTS_CLEAR_SEQ_TOKEN(MP)                                        \
    ERL_MESSAGE_TOKEN((MP)) = ERTS_CLEAR_SEQ_TOKEN_VALUE((MP))

Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler);
Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler_high);
Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler_max);

void
erts_proc_sig_queue_init(void)
{
    ERTS_CT_ASSERT(ERTS_SIG_Q_OP_MASK > ERTS_SIG_Q_OP_MAX);
    ERTS_CT_ASSERT(ERTS_SIG_Q_TYPE_MASK >= ERTS_SIG_Q_TYPE_MAX);
}

typedef struct {
    int active;
    int procs;
    struct {
        int active;
#if defined(USE_VM_PROBES)
        int vm_probes;
        char receiver_name[DTRACE_TERM_BUF_SIZE];
#endif
        int receive_trace;
        int bp_ix;
        ErtsMessage **next;
    } messages;
} ErtsSigRecvTracing;

typedef struct {
    Eterm message;
    Eterm from;
    Eterm reason;
    union {
        Eterm ref;
        struct {
            Uint32 flags;
            /*
             * connection_id is only set when the
             * ERTS_SIG_LNK_X_FLAG_CONNECTION_LOST
             * flag has been set...
             */
            Uint32 connection_id;
        } link;
    } u;
} ErtsExitSignalData;

typedef struct {
    ErtsExitSignalData xsigd;
    Eterm token;
} ErtsSeqTokenExitSignalData;

typedef struct {
    Eterm message;
    Eterm key;
} ErtsPersistMonMsg;

typedef struct {
    ErtsNonMsgSignal common;
    Eterm nodename;
    Uint32 connection_id;
    Eterm local; /* internal pid (immediate) */
    Eterm remote; /* external pid (heap for it follow) */
    Uint64 id;
    Eterm heap[EXTERNAL_PID_HEAP_SIZE];
} ErtsSigDistUnlinkOp;

typedef struct {
    Eterm message;
    Eterm ref;
    Eterm result;
    ErtsLink *link;
    Eterm *patch_point;
} ErtsDistSpawnReplySigData;

typedef struct {
    ErtsDistSpawnReplySigData data;
    Eterm token;
} ErtsDistSeqTokenSpawnReplySigData;

typedef struct {
    ErtsNonMsgSignal common;
    Uint flags_on;
    Uint flags_off;
    Eterm tracer;
    ErtsTraceSession *session;
} ErtsSigTraceInfo;

#define ERTS_SIG_GL_FLG_ACTIVE          (((erts_aint_t) 1) << 0)
#define ERTS_SIG_GL_FLG_RECEIVER        (((erts_aint_t) 1) << 1)
#define ERTS_SIG_GL_FLG_SENDER          (((erts_aint_t) 1) << 2)

typedef struct {
    ErtsNonMsgSignal common;
    erts_atomic_t flags;
    Eterm group_leader;
    Eterm reply_to;
    Eterm ref;
    ErlOffHeap oh;
    Eterm heap[1];
} ErtsSigGroupLeader;

typedef struct {
    Eterm message;
    Eterm requester;
} ErtsIsAliveRequest;

typedef struct {
    Eterm message;
    Eterm requester;
    int async;
} ErtsSyncSuspendRequest;

typedef struct {
    ErtsMonitorSuspend *mon;
    ErtsMessage *sync;
} ErtsProcSigPendingSuspend;

typedef struct {
    ErtsNonMsgSignal common;
    Eterm requester;
    Eterm ref;
    ErtsORefThing oref_thing;
    Uint reserve_size;
    Uint len;
    int flags;
    int *item_ix;
    Eterm *item_extra;
    ErlHeapFragment *extra_hfrag;
} ErtsProcessInfoSig;

typedef struct {
    ErtsNonMsgSignal common;
    Eterm requester;
    Eterm (*func)(Process *, void *, int *, ErlHeapFragment **);
    void *arg;
    Eterm ref;
    ErtsORefThing oref_thing;
} ErtsProcSigRPC;

typedef struct {
    ErtsRecvMarker next;
    ErtsRecvMarker last;
} ErtsYieldAdjMsgQ;

typedef struct {
    ErtsYieldAdjMsgQ *yield;
    Eterm requester;
    Eterm request_id;
} ErtsCLAData;

typedef struct {
    ErtsYieldAdjMsgQ *yield;
} ErtsAdjOffHeapMsgQData;

typedef struct {
    ErtsMessage *first;
    ErtsMessage **last;
} ErtsSavedNMSignals;

#define ERTS_PRIO_Q_MARK_END            0
#define ERTS_PRIO_Q_MARK_CONT           1

#define ERTS_PRIO_Q_MARK_IX_MIN         ERTS_PRIO_Q_MARK_END
#define ERTS_PRIO_Q_MARK_IX_MAX         ERTS_PRIO_Q_MARK_CONT

typedef struct {
    Uint32 saved_save_info;
    Sint refc;
#ifdef DEBUG
    Sint alias;
    Sint link;
    Sint monitor;
#endif
    ErtsRecvMarker marker[ERTS_PRIO_Q_MARK_IX_MAX
                          - ERTS_PRIO_Q_MARK_IX_MIN
                          + 1];
} ErtsPrioQInfo;

static void wake_handle_signals(Process *proc);

static int handle_msg_tracing(Process *c_p,
                              ErtsSigRecvTracing *tracing,
                              ErtsMessage ***next_nm_sig);
static int handle_trace_change_state(Process *c_p,
                                     ErtsSigRecvTracing *tracing,
                                     Uint16 type,
                                     ErtsMessage *sig,
                                     ErtsMessage ***next_nm_sig);
static void getting_unlinked(Process *c_p, Eterm unlinker);
static void getting_linked(Process *c_p, Eterm linker);
static void linking(Process *c_p, Eterm to);

static void group_leader_reply(Process *c_p, Eterm to,
                               Eterm ref, int success);
static int stretch_limit(Process *c_p, ErtsSigRecvTracing *tp,
                         int abs_lim, int *limp, int save_in_msgq);
static int
handle_cla(Process *c_p,
           ErtsMessage *sig,
           ErtsMessage ***next_nm_sig,
           int exiting,
           int limit,
           ErtsSavedNMSignals *saved_nm_sigs);

static int
handle_move_msgq_off_heap(Process *c_p,
			  ErtsMessage *sig,
			  ErtsMessage ***next_nm_sig,
			  int exiting,
                          int limit,
                          ErtsSavedNMSignals *saved_nm_sigs);
static void
send_cla_reply(Process *c_p, ErtsMessage *sig, Eterm to,
               Eterm req_id, Eterm result);
static void handle_missing_spawn_reply(Process *c_p, ErtsMonitor *omon);

static Uint proc_sig_queue_flush_buffer(Process* proc,
                                        Uint buffer_index,
                                        ErtsSignalInQueueBufferArray* buffers);
static void proc_sig_queue_flush_buffers(Process *proc,
                                         ErtsSignalInQueueBufferArray *buffers);
static void proc_sig_queue_lock_buffer(ErtsSignalInQueueBuffer* slot);
static void proc_sig_queue_unlock_buffer(ErtsSignalInQueueBuffer* slot);
static void handle_message_enqueued_tracing(Process *c_p,
                                            ErtsSigRecvTracing *tracing,
                                            ErtsMessage *msg);
static int
insert_prepared_prio_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                         ErtsMessage *sig, Eterm message, Eterm token,
                         ErtsMessage ***next_nm_sig);
static int
insert_prepared_prio_msg_attached(Process *c_p, ErtsSigRecvTracing *tracing,
                                  ErtsMessage *sig, void *attached,
                                  Eterm message, Eterm token,
                                  ErtsMessage ***next_nm_sig);

static ERTS_INLINE ErtsPrioQInfo *
get_prio_queue_info(Process *c_p)
{
    ErtsPrioQInfo *pq_info = ERTS_PROC_GET_PRIO_Q_INFO(c_p);
    ASSERT(pq_info);
    ASSERT(c_p->sig_qs.flags & (FS_PRIO_MQ|FS_PRIO_MQ_PENDING_RM));
    return pq_info;
}

#ifdef ERTS_PROC_SIG_HARD_DEBUG
#define ERTS_PROC_SIG_HDBG_PRIV_CHKQ(P, T, NMN)                 \
    do {                                                        \
        ErtsMessage **nm_next__ = *(NMN);                       \
        ErtsMessage **nm_last__ = (P)->sig_qs.nmsigs.last;      \
        if (!nm_next__ || !*nm_next__) {                        \
            nm_next__ = NULL;                                   \
            nm_last__ = NULL;                                   \
        }                                                       \
        proc_sig_hdbg_check_queue((P),                          \
                                  1,                            \
                                  &(P)->sig_qs.cont,            \
                                  (P)->sig_qs.cont_last,        \
                                  nm_next__,                    \
                                  nm_last__,                    \
                                  (T),                          \
                                  NULL,                         \
                                  0,                            \
                                  0);                           \
    } while (0);
static Sint
proc_sig_hdbg_check_queue(Process *c_p,
                          int privq,
                          ErtsMessage **sig_next,
                          ErtsMessage **sig_last,
                          ErtsMessage **sig_nm_next,
                          ErtsMessage **sig_nm_last,
                          ErtsSigRecvTracing *tracing,
                          int *found_saved_last_p,
                          erts_aint32_t nmsig_psflg,
                          erts_aint32_t msig_psflg);
#else
#define ERTS_PROC_SIG_HDBG_PRIV_CHKQ(P, T, NMN)
#endif

static void
save_delayed_nm_signal(ErtsSavedNMSignals *saved_sigs, ErtsMessage *sig)
{
    ErtsSignal *nm_sig = (ErtsSignal *) sig;
    /*
     * All saved signals will be restored at the front of the
     * middle queue, so no message signals will precede any of
     * them...
     */
    nm_sig->nm_sig.mlenoffs = 0;
    nm_sig->common.next = NULL;
    nm_sig->common.specific.next = NULL;
    if (!saved_sigs->first) {
        ASSERT(!saved_sigs->last);
        saved_sigs->first = sig;
        saved_sigs->last = &saved_sigs->first;
    }
    else {
        ErtsSignal *last;
        ASSERT(saved_sigs->last);
        last = (ErtsSignal *) *saved_sigs->last;
        last->common.next = sig;
        last->common.specific.next = &last->common.next;
        saved_sigs->last = &last->common.next;
    }
}

static erts_aint32_t
restore_delayed_nm_signals(Process *c_p, ErtsSavedNMSignals *saved_sigs)
{
    erts_aint32_t state;
    ErtsSignal *lsig;

    ASSERT(saved_sigs->first && saved_sigs->last);

    lsig = (ErtsSignal *) *saved_sigs->last;
    if (!c_p->sig_qs.cont) {
        ASSERT(!c_p->sig_qs.nmsigs.next);
        ASSERT(!c_p->sig_qs.nmsigs.last);
        if (saved_sigs->last == &saved_sigs->first) 
            c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
        else
            c_p->sig_qs.nmsigs.last = saved_sigs->last;
        c_p->sig_qs.cont_last = &lsig->common.next;
    }
    else {
        lsig->common.next = c_p->sig_qs.cont;
        if (c_p->sig_qs.nmsigs.next) {
            ASSERT(c_p->sig_qs.nmsigs.last);
            if (c_p->sig_qs.nmsigs.next == &c_p->sig_qs.cont)
                lsig->common.specific.next = &lsig->common.next;
            else
                lsig->common.specific.next = c_p->sig_qs.nmsigs.next;
            if (c_p->sig_qs.nmsigs.last == &c_p->sig_qs.cont)
                c_p->sig_qs.nmsigs.last = &lsig->common.next;
        }
        else {
            ASSERT(!c_p->sig_qs.nmsigs.last);
            if (saved_sigs->last == &saved_sigs->first) 
                c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
            else
                c_p->sig_qs.nmsigs.last = saved_sigs->last;
            if (c_p->sig_qs.cont_last == &c_p->sig_qs.cont)
                c_p->sig_qs.cont_last = &lsig->common.next;
        }
    }
    
    c_p->sig_qs.cont = saved_sigs->first;
    c_p->sig_qs.nmsigs.next = &c_p->sig_qs.cont;

    state = erts_atomic32_read_bor_nob(&c_p->state,
                                       ERTS_PSFLG_SIG_Q);
    state |= ERTS_PSFLG_SIG_Q;
    return state;
}

typedef struct {
    ErtsNonMsgSignal common;
    Eterm ref;
    Eterm heap[1];
} ErtsSigDistProcDemonitor;

static void
destroy_dist_proc_demonitor(ErtsSigDistProcDemonitor *dmon)
{
    Eterm ref = dmon->ref;
    if (is_external(ref)) {
        ExternalThing *etp = external_thing_ptr(ref);
        erts_deref_node_entry(etp->node, ref);
    }
    erts_free(ERTS_ALC_T_DIST_DEMONITOR, dmon);
}

static ERTS_INLINE ErtsSigDistUnlinkOp *
make_sig_dist_unlink_op(int op, Eterm nodename, Uint32 conn_id,
                        Eterm local, Eterm remote, Uint64 id)
{
    Eterm *hp;
    ErlOffHeap oh = {0};
    ErtsSigDistUnlinkOp *sdulnk = erts_alloc(ERTS_ALC_T_SIG_DATA,
                                             sizeof(ErtsSigDistUnlinkOp));
    ASSERT(is_internal_pid(local));
    ASSERT(is_external_pid(remote));

    hp = &sdulnk->heap[0];

    sdulnk->common.tag = ERTS_PROC_SIG_MAKE_TAG(op,
                                                ERTS_SIG_Q_TYPE_DIST_LINK,
                                                0);
    sdulnk->nodename = nodename;
    sdulnk->connection_id = conn_id;
    sdulnk->local = local;
    sdulnk->remote = STORE_NC(&hp, &oh, remote);
    sdulnk->id = id;
 
    ASSERT(&sdulnk->heap[0] < hp);
    ASSERT(hp <= &sdulnk->heap[0] + sizeof(sdulnk->heap)/sizeof(sdulnk->heap[0]));
    ASSERT(boxed_val(sdulnk->remote) == &sdulnk->heap[0]);

    return sdulnk;
}

static ERTS_INLINE void
destroy_sig_dist_unlink_op(ErtsSigDistUnlinkOp *sdulnk)
{
    ASSERT(is_external_pid(sdulnk->remote));
    ASSERT(boxed_val(sdulnk->remote) == &sdulnk->heap[0]);
    erts_deref_node_entry(((ExternalThing *) &sdulnk->heap[0])->node,
                          make_boxed(&sdulnk->heap[0]));
    erts_free(ERTS_ALC_T_SIG_DATA, sdulnk);
}

static ERTS_INLINE ErtsExitSignalData *
get_exit_signal_data(ErtsMessage *xsig)
{
    ASSERT(ERTS_SIG_IS_NON_MSG(xsig));
    ASSERT((ERTS_PROC_SIG_OP(((ErtsSignal *) xsig)->common.tag)
            == ERTS_SIG_Q_OP_EXIT)
           || (ERTS_PROC_SIG_OP(((ErtsSignal *) xsig)->common.tag)
                == ERTS_SIG_Q_OP_EXIT_LINKED)
           || (ERTS_PROC_SIG_OP(((ErtsSignal *) xsig)->common.tag)
               == ERTS_SIG_Q_OP_MONITOR_DOWN));
    ASSERT(xsig->hfrag.alloc_size > xsig->hfrag.used_size);
    ASSERT((xsig->hfrag.alloc_size - xsig->hfrag.used_size)*sizeof(UWord)
           >= sizeof(ErtsExitSignalData));
    return (ErtsExitSignalData *) (char *) (&xsig->hfrag.mem[0]
                                            + xsig->hfrag.used_size);
}

static ERTS_INLINE void
clear_seq_token_gen_exit(ErtsMessage *sig)
{
    Uint tag = ((ErtsSignal *) sig)->common.tag;
    ASSERT(ERTS_SIG_Q_OP_EXIT == ERTS_PROC_SIG_OP(tag)
           || ERTS_SIG_Q_OP_EXIT_LINKED == ERTS_PROC_SIG_OP(tag)
           || ERTS_SIG_Q_OP_MONITOR_DOWN == ERTS_PROC_SIG_OP(tag));
    if (ERTS_PROC_SIG_XTRA(tag)) {
        ErtsSeqTokenExitSignalData *datap
            = (ErtsSeqTokenExitSignalData *) get_exit_signal_data(sig);
        datap->token = ERTS_CLEAR_SEQ_TOKEN_VALUE(sig);
    }
}

static ERTS_INLINE ErtsDistSpawnReplySigData *
get_dist_spawn_reply_data(ErtsMessage *sig)
{
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    ASSERT(sig->hfrag.alloc_size > sig->hfrag.used_size);
    ASSERT((sig->hfrag.alloc_size - sig->hfrag.used_size)*sizeof(UWord)
           >= sizeof(ErtsDistSpawnReplySigData));
    return (ErtsDistSpawnReplySigData *) (char *) (&sig->hfrag.mem[0]
                                                   + sig->hfrag.used_size);
}

static ERTS_INLINE void
clear_seq_token_spawn_reply(ErtsMessage *sig)
{
    Uint tag = ((ErtsSignal *) sig)->common.tag;
    ASSERT(ERTS_SIG_Q_OP_DIST_SPAWN_REPLY == ERTS_PROC_SIG_OP(tag));
    if (ERTS_PROC_SIG_XTRA(tag)) {
        ErtsDistSeqTokenSpawnReplySigData *datap
            = (ErtsDistSeqTokenSpawnReplySigData *) get_dist_spawn_reply_data(sig);
        datap->token = ERTS_CLEAR_SEQ_TOKEN_VALUE(sig);
    }
}

static ERTS_INLINE ErtsCLAData *
get_cla_data(ErtsMessage *sig)
{
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    ASSERT(ERTS_PROC_SIG_OP(((ErtsSignal *) sig)->common.tag)
           == ERTS_SIG_Q_OP_ADJ_MSGQ);
    ASSERT(ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag)
           == ERTS_SIG_Q_TYPE_CLA);
    return (ErtsCLAData *) (char *) (&sig->hfrag.mem[0]
                                     + sig->hfrag.used_size);
}

static ERTS_INLINE ErtsAdjOffHeapMsgQData *
get_move_msgq_off_heap_data(ErtsMessage *sig)
{
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    ASSERT(ERTS_PROC_SIG_OP(((ErtsSignal *) sig)->common.tag)
           == ERTS_SIG_Q_OP_ADJ_MSGQ);
    ASSERT(ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag)
           == ERTS_SIG_Q_TYPE_OFF_HEAP);
    return (ErtsAdjOffHeapMsgQData *) (char *) (&sig->hfrag.mem[0]
                                                + sig->hfrag.used_size);
}

static ERTS_INLINE void
destroy_trace_info(ErtsSigTraceInfo *ti)
{
    if (is_value(ti->tracer))
        erts_tracer_update(&ti->tracer, NIL);
    if(ti->session)
        erts_deref_trace_session(ti->session);

    erts_free(ERTS_ALC_T_SIG_DATA, ti);
}

static void
destroy_sig_group_leader(ErtsSigGroupLeader *sgl)
{
    erts_cleanup_offheap(&sgl->oh);
    erts_free(ERTS_ALC_T_SIG_DATA, sgl);
}

static void
destroy_process_info_sig(ErtsProcessInfoSig *pis)
{
    if (pis->extra_hfrag)
        erts_cleanup_offheap(&pis->extra_hfrag->off_heap);
    erts_free(ERTS_ALC_T_SIG_DATA, pis);
}

static ERTS_INLINE void
sig_enqueue_trace(ErtsPTabElementCommon *sender, Eterm from,
                  ErtsMessage **sigp, int op, Process *rp,
                  ErtsMessage ***last_next)
{
    Process *c_p;

    if (sender == NULL || !is_internal_pid(from)) {
        return;
    }

    c_p = ErtsContainerStruct(sender, Process, common);

    switch (op) {
    case ERTS_SIG_Q_OP_LINK:
        if (c_p
            && ((ERTS_IS_P_TRACED_FL(c_p, F_TRACE_SOL | F_TRACE_SOL1)))) {
            ErtsSigTraceInfo *ti;
            Eterm tag;
            ErtsTracerRef *ref;
            ((ErtsSignal *) *sigp)->nm_sig.mlenoffs = 0; /* directly following
                                                            trace info signal...
                                                         */
            erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

            for (ref = c_p->common.tracee.first_ref; ref; ref = ref->next) {
                if (ref->flags & (F_TRACE_SOL | F_TRACE_SOL1)) {
                    /*
                    * Set on link enabled.
                    *
                    * Prepend a trace-change-state signal before the
                    * link signal...
                    */
                    tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_TRACE_CHANGE_STATE,
                                                ERTS_SIG_Q_TYPE_ADJUST_TRACE_INFO,
                                                0);

                    ti = erts_alloc(ERTS_ALC_T_SIG_DATA, sizeof(ErtsSigTraceInfo));
                    ti->common.next = *sigp;
                    ti->common.specific.next = &ti->common.next;
                    ti->common.tag = tag;
                    ti->common.mlenoffs = 0; /* Need to zero this since it may be
                                                preceeded by another non-message
                                                signal... */
                    ti->flags_on = ref->flags & TRACEE_FLAGS;
                    ti->session = ref->session;
                    erts_ref_trace_session(ref->session);
                    if (!(ti->flags_on & F_TRACE_SOL1)) {
                        ti->flags_off = 0;
                    } else {
                        ti->flags_off = F_TRACE_SOL1|F_TRACE_SOL;
                        ref->flags &= ~(F_TRACE_SOL1|F_TRACE_SOL);
                    }
                    ti->tracer = erts_tracer_nil;
                    if(ref->tracer != NIL)
                        erts_tracer_update(&ti->tracer, ref->tracer);

                    *sigp = (ErtsMessage *) ti;

                    if (!*last_next || *last_next == sigp) {
                        *last_next = &ti->common.next;
                    }
                }
            }
            ERTS_P_ALL_TRACE_FLAGS(c_p) = erts_sum_all_trace_flags(&c_p->common);

            erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
        }

        break;
#ifdef USE_VM_PROBES
    case ERTS_SIG_Q_OP_EXIT:
    case ERTS_SIG_Q_OP_EXIT_LINKED:
        if (DTRACE_ENABLED(process_exit_signal)) {
            ErtsMessage* sig = *sigp;
            Uint16 type = ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag);
            Eterm reason, from;
            ErtsExitSignalData *xsigd;

            ASSERT(type == ERTS_SIG_Q_TYPE_GEN_EXIT);
            (void)type;

            xsigd = get_exit_signal_data(sig);
            reason = xsigd->reason;
            from = xsigd->from;

            if (is_pid(from)) {

                DTRACE_CHARBUF(sender_str, DTRACE_TERM_BUF_SIZE);
                DTRACE_CHARBUF(receiver_str, DTRACE_TERM_BUF_SIZE);
                DTRACE_CHARBUF(reason_buf, DTRACE_TERM_BUF_SIZE);

                if (reason == am_kill) {
                    reason = am_killed;
                }

                dtrace_pid_str(from, sender_str);
                dtrace_proc_str(rp, receiver_str);
                erts_snprintf(reason_buf, sizeof(DTRACE_CHARBUF_NAME(reason_buf)) - 1, "%T", reason);
                DTRACE3(process_exit_signal, sender_str, receiver_str, reason_buf);
            }
        }
        break;
#endif
    default:
        break;
    }
}

static void
sig_enqueue_trace_cleanup(ErtsMessage *first, ErtsSignal *sig)
{
    ErtsMessage *tmp;

    /* The usual case; no tracing signals... */
    if (sig == (ErtsSignal *) first) {
        ASSERT(sig->common.next == NULL);
        return;
    }

    /* Got trace signals to clean up... */

    tmp = first;

    while (tmp) {
        ErtsMessage *tmp_free = tmp;
        tmp = tmp->next;
        if (sig != (ErtsSignal *) tmp_free) {
            switch (ERTS_PROC_SIG_OP(((ErtsSignal *) tmp_free)->common.tag)) {
            case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
                destroy_trace_info((ErtsSigTraceInfo *) tmp_free);
                break;
            case ERTS_SIG_Q_OP_MONITOR:
                break; /* ignore flushed pending signal */
            default:
                ERTS_INTERNAL_ERROR("Unexpected signal op");
                break;
            }
        }
    }
}

#ifdef DEBUG
static int dbg_count_nmsigs(ErtsMessage *first)
{
    ErtsMessage *sig;
    int cnt = 0;

    for (sig = first; sig; sig = sig->next) {
        if (ERTS_SIG_IS_NON_MSG(sig))
            ++cnt;
    }
    return cnt;
}
#endif

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_BUFFERS
static int dbg_count_all(ErtsMessage *first)
{
    ErtsMessage *sig;
    int cnt = 0;

    for (sig = first; sig; sig = sig->next) {
            ++cnt;
    }
    return cnt;
}

static int dbg_check_non_msg(ErtsSignalInQueue* q)
{
    ErtsMessage** m = q->nmsigs.next;
    int cnt = 0;
    ErtsMessage** prev_m = NULL;
    while (m != NULL) {
        ERTS_ASSERT(ERTS_SIG_IS_NON_MSG(*m));
        cnt++;
        prev_m = m;
        m = ((ErtsSignal *) (*m))->common.specific.next;
    }
    if (cnt > 0) {
        ERTS_ASSERT(prev_m == q->nmsigs.last);
    }
    return cnt;
}
#endif /* ERTS_PROC_SIG_HARD_DEBUG_SIGQ_BUFFERS */


/**
 * @brief Enqueue a sequence of signals on an *in* signal queue.
 *
 * The *only* valid scenarios:
 * * One or more message signals and no non-message signals.
 * * One non-message signal followed by one or more message signals
 * * One or more non-message signals and no message signals.
 *
 * Appart from next pointers between the signals in the sequence also:
 * * next pointer pointers between non-message signals must have been
 *   correctly set up.
 * * all non leading non-message signals should have 'mlenoffs' set to zero.
 *
 *
 * @param is_to_buffer       Non-zero if not enqueue on processes in signal
 *                           queue.
 * @param rp[in]             Process to which the in signal queue belong.
 * @param first[in]          Pointer to the first signal in signal sequence.
 * @param last[in]           Pointer to the next pointer of the last signal
 *                           in the sequence. This next pointer should equal
 *                           NULL.
 * @param last_next[in]      Pointer to the next pointer which points to the
 *                           last signal in the sequence if more than one
 *                           non-message signal is in the sequence. NULL if
 *                           no or only one non-message signal in the seqence.
 * @param state[in]          State of rp upon call. This state *must* have
 *                           been while holding the msgq lock or buffer lock
 *                           prior to calling this function.
 * @param dest_queue         In signal queue to enqueue on. This might
 *                           be the in signal queue of rp or an in signal
 *                           queue buffer of rp. The in signal queue has to
 *                           be apropriately locked when calling
 *                           enqueue_signals() by the caller.
 *
 * @return                   Possibly changed state of rp.
 */
static ERTS_INLINE erts_aint32_t
enqueue_signals(int is_to_buffer, Process *rp, ErtsMessage *first,
                ErtsMessage **last, ErtsMessage **last_next,
                Uint num_msgs,
                erts_aint32_t state,
                ErtsSignalInQueue* dest_queue)
{
    ErtsMessage **this;
    erts_aint32_t set_flags;
    int nmsig = ERTS_SIG_IS_NON_MSG(first);
    int flush_buffers = (!is_to_buffer) && (state & ERTS_PSFLG_OFF_HEAP_MSGQ);

    ASSERT(!!is_to_buffer == (dest_queue != &rp->sig_inq));

#if defined(ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN) && 0
    /* verify that the signal sequence fulfills the requirements... */
    Sint no_nmsg_sigs = 0, no_msg_sigs = 0;
    int last_next_found = 0, last_nmsig_found = 0;
    ErtsMessage *sig;
    ErtsNonMsgSignal *nmsig = NULL;


    for (sig = first; sig; sig = sig->next) {
        if (ERTS_SIG_IS_MSG(sig)) {
            no_msg_sigs++;
        }
        else {
            if (no_nmsg_sigs++ == 0) {
                ERTS_ASSERT(!nmsig);
                nmsig = (ErtsNonMsgSignal *) sig;
            }
            else {
                ERTS_ASSERT(((ErtsNonMsgSignal *) sig)->mlenoffs == 0);
            }
            ERTS_ASSERT(!last_nmsig_found);
            ERTS_ASSERT(no_msg_sigs == 0);
            ERTS_ASSERT(nmsig == (ErtsNonMsgSignal *) sig);
            if (last_next && last_next == nmsig->specific.next) {
                ERTS_ASSERT(!last_next_found);
                last_next_found = !0;
            }
            else if (last_next_found) {
                ERTS_ASSERT(last_next);
                ERTS_ASSERT(*last_next == sig);
                ERTS_ASSERT(!sig->next);
                ERTS_ASSERT(!nmsig->specific.next);
            }
            if (!nmsig->specific.next) {
                last_nmsig_found = !0;
                nmsig = NULL;
            }
            else {
                ERTS_ASSERT(nmsig->specific.next == &sig->next);
                nmsig = (ErtsNonMsgSignal *) *nmsig->specific.next;
            }
        }
    }

    ERTS_ASSERT(!nmsig);
    ERTS_ASSERT(no_msg_sigs == 0
                || no_nmsg_sigs == 0
                || no_nmsg_sigs == 1);
    ERTS_ASSERT(no_nmsg_sigs <= 1
                || (no_msg_sigs == 0 && last_next && last_next_found));
    ERTS_ASSERT(no_nmsg_sigs > 1
                || (!last_next && !last_next_found));
    ERTS_ASSERT(no_nmsg_sigs == 0 || last_nmsig_found);
    ERTS_ASSERT(num_msgs == no_msg_sigs);

#endif /* DEBUG */

    if (flush_buffers) {
        erts_proc_sig_queue_flush_buffers(rp);
    }


    ERTS_HDBG_INQ_LEN(dest_queue);

    this = dest_queue->last;

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(rp, dest_queue);

    ASSERT(!*this);
    *this = first;
    dest_queue->last = last;

    set_flags = num_msgs ? ERTS_PSFLG_MSG_SIG_IN_Q : 0;

    if (!dest_queue->nmsigs.next) {
        ASSERT(!dest_queue->nmsigs.last);
        if (nmsig) {
            dest_queue->nmsigs.next = this;
            set_flags |= ERTS_PSFLG_NMSG_SIG_IN_Q;
            ((ErtsNonMsgSignal *) first)->mlenoffs = dest_queue->mlenoffs;
            dest_queue->mlenoffs = 0;
        }
    }
    else {
        ErtsSignal *sig;
        ASSERT(dest_queue->nmsigs.last);

        sig = (ErtsSignal *) *dest_queue->nmsigs.last;

        ASSERT(sig && !sig->common.specific.next);
        if (nmsig) {
            sig->common.specific.next = this;
            ((ErtsNonMsgSignal *) first)->mlenoffs = dest_queue->mlenoffs;
            dest_queue->mlenoffs = 0;
        }
    }

    if ((state & set_flags) != set_flags)
        state = erts_atomic32_read_bor_nob(&rp->state, set_flags);

#ifdef DEBUG
    if (!is_to_buffer) {
        erts_aint32_t a = erts_atomic32_read_nob(&rp->state);
        erts_aint32_t e = 0;
        if (num_msgs)
            e |= ERTS_PSFLG_MSG_SIG_IN_Q;
        if (nmsig)
            e |= ERTS_PSFLG_NMSG_SIG_IN_Q;
        ASSERT((a & e) == e);
    }
#endif

    if (last_next) {
        ASSERT(dbg_count_nmsigs(first) >= 2);
        dest_queue->nmsigs.last = last_next;
    }
    else if (nmsig) {
        ASSERT(dbg_count_nmsigs(first) == 1);
        dest_queue->nmsigs.last = this;
    }
    else
        ASSERT(dbg_count_nmsigs(first) == 0);

    dest_queue->mlenoffs += num_msgs;

    ERTS_HDBG_INQ_LEN(dest_queue);

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(rp, dest_queue);

    return state;
}

erts_aint32_t erts_enqueue_signals(Process *rp, ErtsMessage *first,
                                   ErtsMessage **last, Uint num_msgs,
                                   erts_aint32_t in_state)
{
    return enqueue_signals(0, rp, first, last, NULL, num_msgs, in_state,
                           &rp->sig_inq);
}

static ERTS_INLINE void
notify_dirty_signal_handler(Eterm pid,
                            erts_aint32_t state,
                            erts_aint32_t prio)
{
    Eterm *hp;
    ErtsMessage *mp;
    Process *sig_handler;

    ASSERT(state & (ERTS_PSFLGS_DIRTY_WORK|ERTS_PSFLG_DIRTY_RUNNING));

    if (prio < 0)
        prio = (int) ERTS_PSFLGS_GET_USR_PRIO(state);

    switch (prio) {
    case PRIORITY_MAX:
        sig_handler = erts_dirty_process_signal_handler_max;
        break;
    case PRIORITY_HIGH:
        sig_handler = erts_dirty_process_signal_handler_high;
        break;
    default:
        sig_handler = erts_dirty_process_signal_handler;
        break;
    }

    /* Make sure signals are handled... */
    mp = erts_alloc_message(0, &hp);
    erts_queue_message(sig_handler, 0, mp, pid, am_system);
}

typedef struct {
    Eterm pid;
    erts_aint32_t prio;
} ErtsDirtySignalHandlerNotification;

static void
delayed_notify_dirty_signal_handler(void *vdshnp)
{
    ErtsDirtySignalHandlerNotification *dshnp
        = (ErtsDirtySignalHandlerNotification *) vdshnp;
    Process *proc;

    ASSERT(dshnp);

    proc = erts_proc_lookup(dshnp->pid);
    if (proc) {
        erts_aint32_t state = erts_atomic32_read_acqb(&proc->state);
        /*
         * Notify the dirty signal handler if it is still scheduled
         * or running dirty and still have signals to handle...
         */
        if (ERTS_PROC_NEED_DIRTY_SIG_HANDLING(state)) {
            notify_dirty_signal_handler(dshnp->pid, state, dshnp->prio);
        }
    }
    erts_free(ERTS_ALC_T_DSIG_HNDL_NTFY, vdshnp);
}

void
erts_ensure_dirty_proc_signals_handled(Process *proc,
                                       erts_aint32_t state,
                                       erts_aint32_t prio,
                                       ErtsProcLocks locks)
{
    /*
     * All minor locks need to be accurately reported...
     */
    ERTS_LC_ASSERT((locks & ERTS_PROC_LOCKS_ALL_MINOR)
                   == (erts_proc_lc_my_proc_locks(proc)
                       & ERTS_PROC_LOCKS_ALL_MINOR));

    if (!(locks & ERTS_PROC_LOCKS_ALL_MINOR)) {
        notify_dirty_signal_handler(proc->common.id, state, prio);
    }
    else {
        /*
         * We need to schedule the notification since we cannot
         * safely acquire the msgq lock on the dirty signal
         * handler process while other minor process locks are
         * held...
         */
        ErtsSchedulerData *esdp = erts_get_scheduler_data();
        int tid = esdp && esdp->type == ERTS_SCHED_NORMAL ? (int) esdp->no : 1;
        ErtsDirtySignalHandlerNotification *dshnp =
            (ErtsDirtySignalHandlerNotification *)
            erts_alloc(ERTS_ALC_T_DSIG_HNDL_NTFY,
                       sizeof(ErtsDirtySignalHandlerNotification));
        dshnp->pid = proc->common.id;
        dshnp->prio = prio;
        erts_schedule_misc_aux_work(tid,
                                    delayed_notify_dirty_signal_handler,
                                    (void *) dshnp);
    }
}

static int
proc_queue_signal(ErtsPTabElementCommon *sender, Eterm from, Eterm pid,
                  ErtsSignal *sig, int force_flush, int op)
{
    int res;
    Process *rp;
    ErtsMessage *first, *last, **last_next, **sigp;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int is_normal_sched = !!esdp && esdp->type == ERTS_SCHED_NORMAL;
    erts_aint32_t state;
    ErtsSignal *pend_sig;

    ASSERT(sender == NULL || sender->id == from);

    /* Tracing requires sender for local procs and ports. The assertions below
     * will not catch errors after time-of-death, but ought to find most
     * problems. */
    ASSERT(sender != NULL || op == ERTS_SIG_Q_OP_FLUSH ||
           (is_normal_sched && esdp->pending_signal.sig == sig) ||
           (!(is_internal_pid(from) &&
              erts_proc_lookup(from) != NULL) &&
            !(is_internal_port(from) &&
              erts_port_lookup(from, ERTS_PORT_SFLGS_INVALID_LOOKUP) != NULL)));

    ASSERT(is_value(from) && is_internal_pid(pid));
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));

    if (is_normal_sched) {
        pend_sig = esdp->pending_signal.sig;
        if (op == ERTS_SIG_Q_OP_MONITOR
            && ERTS_ML_GET_TYPE((ErtsMonitor *) sig) == ERTS_MON_TYPE_PROC) {

            if (!pend_sig) {
                esdp->pending_signal.sig = sig;
                esdp->pending_signal.to = pid;
#ifdef DEBUG
                esdp->pending_signal.dbg_from = esdp->current_process;
#endif
                return 1;
            }
            ASSERT(esdp->pending_signal.dbg_from == esdp->current_process ||
                   esdp->pending_signal.dbg_from == esdp->free_process);
            if (pend_sig != sig) {
                /* Switch them and send previously pending signal instead */
                Eterm pend_to = esdp->pending_signal.to;

                esdp->pending_signal.sig = sig;
                esdp->pending_signal.to = pid;

                sig = pend_sig;
                pid = pend_to;
            }
            else {
                /* Caller wants to flush pending signal */
                ASSERT(pid == esdp->pending_signal.to);
                esdp->pending_signal.sig = NULL;
                esdp->pending_signal.to = THE_NON_VALUE;
#ifdef DEBUG
                esdp->pending_signal.dbg_from = NULL;
#endif
                pend_sig = NULL;
            }

            rp = erts_proc_lookup_raw(pid);
            if (!rp) {
                erts_proc_sig_send_monitor_down(sender, from,
                                                (ErtsMonitor*)sig,
                                                am_noproc);
                return 1;
            }
        }
        else if (pend_sig && pid == esdp->pending_signal.to) {
            /* Flush pending signal to maintain signal order */
            esdp->pending_signal.sig = NULL;
            esdp->pending_signal.to = THE_NON_VALUE;

            rp = erts_proc_lookup_raw(pid);
            if (!rp) {
                erts_proc_sig_send_monitor_down(sender, from,
                                                (ErtsMonitor*)pend_sig,
                                                am_noproc);
                return 0;
            }

            /* Prepend pending signal */
            sig->nm_sig.mlenoffs = 0; /* directly preceeded by pend_sig... */
            pend_sig->common.next = (ErtsMessage*) sig;
            pend_sig->common.specific.next = &pend_sig->common.next;
            first = (ErtsMessage*) pend_sig;
            last = (ErtsMessage*) sig;
            sigp = last_next = &pend_sig->common.next;
            goto first_last_done; 
        }
        else {
            pend_sig = NULL;
            rp = erts_proc_lookup_raw(pid);
            if (!rp) {
                return 0;
            }
        }
    }
    else {
        rp = erts_proc_lookup_raw_inc_refc(pid);
        if (!rp) {
            return 0;
        }
        pend_sig = NULL;
    }

    first = last = (ErtsMessage *) sig;
    last_next = NULL;
    sigp = &first;

first_last_done:

    if ((void *) sender == (void *) rp)
	(void) erts_atomic32_read_bor_nob(&((Process *) sender)->xstate,
					  ERTS_PXSFLG_MAYBE_SELF_SIGS);

    sig->common.specific.next = NULL;

    /* may add signals before sig */
    sig_enqueue_trace(sender, from, sigp, op, rp, &last_next);

    last->next = NULL;

    if (!force_flush &&
        erts_proc_sig_queue_try_enqueue_to_buffer(from, rp, 0, first,
                                                  &last->next, last_next, 0)) {
        if (!is_normal_sched) {
            erts_proc_dec_refc(rp);
        }

        return 1;
    }

    erts_proc_sig_queue_lock(rp);

    state = erts_atomic32_read_nob(&rp->state);

    if (force_flush) {
        erts_proc_sig_queue_flush_buffers(rp);
    } else {
        erts_proc_sig_queue_maybe_install_buffers(rp, state);
    }

    if (ERTS_PSFLG_FREE & state) {
        res = 0;
    } else {
        state = enqueue_signals(0, rp, first, &last->next,
                                last_next, 0, state,
                                &rp->sig_inq);
        res = !0;
    }

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);

    if (res == 0) {
        sig_enqueue_trace_cleanup(first, sig);
        if (pend_sig) {
            erts_proc_sig_send_monitor_down(sender, from,
                                            (ErtsMonitor*)pend_sig, am_noproc);
            if (sig == pend_sig) {
                /* We did a switch, callers signal is now pending (still ok) */
                ASSERT(esdp->pending_signal.sig);
                res = 1;
            }
        }
    } else {
        erts_proc_notify_new_sig(rp, state, 0);
    }

    if (!is_normal_sched) {
        erts_proc_dec_refc(rp);
    }

    return res;
}

void erts_proc_sig_send_pending(Process *c_p, ErtsSchedulerData* esdp)
{
    ErtsSignal *sig = esdp->pending_signal.sig;
    Eterm to = esdp->pending_signal.to;
    int op;

    ASSERT(esdp && esdp->type == ERTS_SCHED_NORMAL);
    ASSERT(c_p && c_p == esdp->pending_signal.dbg_from);
    ASSERT(sig);
    ASSERT(is_internal_pid(to));

    op = ERTS_SIG_Q_OP_MONITOR;
    ASSERT(op == ERTS_PROC_SIG_OP(sig->common.tag));

    if (!proc_queue_signal(&c_p->common, c_p->common.id, to, sig, 0, op)) {
        ErtsMonitor* mon = (ErtsMonitor*)sig;
        erts_proc_sig_send_monitor_down(NULL, to, mon, am_noproc);
    }
}

static int
maybe_elevate_sig_handling_prio(Process *c_p, int prio, Eterm other)
{
    /*
     * returns:
     *  > 0 -> elevated prio; process alive or exiting
     *  < 0 -> no elevation needed; process alive or exiting
     *    0 -> process terminated (free)
     */
    int res;
    Process *rp;

    rp = erts_proc_lookup_raw(other);
    if (!rp)
        res = 0;
    else {
        erts_aint32_t state, min_prio, other_prio;
        res = -1;
        if (prio >= 0)
            min_prio = prio;
        else if (!c_p)
            return res;
        else {
            /* inherit from caller... */
            state = erts_atomic32_read_nob(&c_p->state);
            min_prio = ERTS_PSFLGS_GET_USR_PRIO(state);
        }

        ASSERT(PRIORITY_MAX <= min_prio && min_prio <= PRIORITY_LOW);

        state = erts_atomic32_read_nob(&rp->state);
        other_prio = ERTS_PSFLGS_GET_USR_PRIO(state);

        if (other_prio > min_prio) {
            /* Others prio is lower than min prio; elevate it... */
            res = !!erts_sig_prio(other, min_prio);
            if (res) {
                /* ensure handled if dirty executing... */
                state = erts_atomic32_read_nob(&rp->state);
                if (ERTS_PROC_NEED_DIRTY_SIG_HANDLING(state))
                    erts_ensure_dirty_proc_signals_handled(rp, state,
                                                           min_prio, 0);
            }
        }
    }
    return res;
}

typedef struct {
    Eterm pid;
    int nmsig;
    int msig;
} ErtsSchedSignalNotify;

static void
sched_sig_notify(void *vssnp);

void
erts_proc_sig_fetch__(Process *proc,
                      ErtsSignalInQueueBufferArray *buffers,
                      int need_unget_buffers)
{
    const erts_aint32_t clear_flags = (ERTS_PSFLG_MSG_SIG_IN_Q
                                       | ERTS_PSFLG_NMSG_SIG_IN_Q);
    erts_aint32_t set_flags = 0;
#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
    int not_exiting = !ERTS_PROC_IS_EXITING(proc);
    if (not_exiting) {
        ERTS_HDBG_PRIVQ_LEN(proc);
        ERTS_HDBG_INQ_LEN(&proc->sig_inq);
    }
#endif

    if (buffers)
        proc_sig_queue_flush_buffers(proc, buffers);
    if (!proc->sig_inq.first) {
        /*
         * 'clear_flags' may be set even though in-queue is empty and
         * if so needs to be cleared...
         */
        if (!(clear_flags & erts_atomic32_read_nob(&proc->state))) {
            if (buffers)
                goto unget_buffers_return;
            return;
        }
        /*
         * This can only happen when buffers are used. However, they may
         * recently have been used but just been uninstalled, so we must be
         * prepared for this scenario also without buffers installed...
         */
    }
    else {

        ASSERT(proc->sig_qs.mq_len >= 0);
        ASSERT(proc->sig_qs.mlenoffs >= 0);

        if (!proc->sig_inq.nmsigs.next) {
            ASSERT(!proc->sig_inq.nmsigs.last);
            ASSERT(proc->sig_inq.mlenoffs > 0);

            if (!proc->sig_qs.cont && !ERTS_MSG_RECV_TRACED(proc)) {
                *proc->sig_qs.last = proc->sig_inq.first;
                proc->sig_qs.last = proc->sig_inq.last;
                ASSERT(proc->sig_qs.mlenoffs == 0);
                proc->sig_qs.mq_len += proc->sig_inq.mlenoffs;
                erts_chk_sys_mon_long_msgq_on(proc);
            }
            else {
                *proc->sig_qs.cont_last = proc->sig_inq.first;
                proc->sig_qs.cont_last = proc->sig_inq.last;
                proc->sig_qs.mlenoffs += proc->sig_inq.mlenoffs;
                set_flags = ERTS_PSFLG_SIG_Q;
            }
        }
        else {
            ErtsNonMsgSignal *nmsig =
                (ErtsNonMsgSignal *) *proc->sig_inq.nmsigs.next;
            ASSERT(nmsig);

            ASSERT(proc->sig_inq.mlenoffs >= 0);
            ASSERT(nmsig->mlenoffs >= 0);

            nmsig->mlenoffs += proc->sig_qs.mlenoffs;
            proc->sig_qs.mlenoffs = proc->sig_inq.mlenoffs;

            ASSERT(proc->sig_inq.nmsigs.last);
            if (!proc->sig_qs.nmsigs.last) {
                ASSERT(!proc->sig_qs.nmsigs.next);
                if (proc->sig_inq.nmsigs.next == &proc->sig_inq.first)
                    proc->sig_qs.nmsigs.next = proc->sig_qs.cont_last;
                else
                    proc->sig_qs.nmsigs.next = proc->sig_inq.nmsigs.next;

                set_flags = ERTS_PSFLG_SIG_Q;
            }
            else {
                ErtsSignal *sig;
                ASSERT(proc->sig_qs.nmsigs.next);
                sig = ((ErtsSignal *) *proc->sig_qs.nmsigs.last);
                ASSERT(ERTS_SIG_IS_NON_MSG(sig));
                ASSERT(!sig->common.specific.next);
                if (proc->sig_inq.nmsigs.next == &proc->sig_inq.first)
                    sig->common.specific.next = proc->sig_qs.cont_last;
                else
                    sig->common.specific.next = proc->sig_inq.nmsigs.next;
            }
            if (proc->sig_inq.nmsigs.last == &proc->sig_inq.first)
                proc->sig_qs.nmsigs.last = proc->sig_qs.cont_last;
            else
                proc->sig_qs.nmsigs.last = proc->sig_inq.nmsigs.last;
            proc->sig_inq.nmsigs.next = NULL;
            proc->sig_inq.nmsigs.last = NULL;

            *proc->sig_qs.cont_last = proc->sig_inq.first;
            proc->sig_qs.cont_last = proc->sig_inq.last;
        }

        proc->sig_inq.first = NULL;
        proc->sig_inq.last = &proc->sig_inq.first;
        proc->sig_inq.mlenoffs = 0;
    }

    ASSERT((set_flags & clear_flags) == 0);

    if (!buffers) {
        (void) (!set_flags
                ? erts_atomic32_read_band_nob(&proc->state,
                                              ~clear_flags)
                : erts_atomic32_read_bset_nob(&proc->state,
                                              set_flags | clear_flags,
                                              set_flags));
    }
    else {
        (void) (!set_flags
                ? erts_atomic32_read_band_acqb(&proc->state,
                                               ~clear_flags)
                : erts_atomic32_read_bset_acqb(&proc->state,
                                               set_flags | clear_flags,
                                               set_flags));
        if (erts_atomic64_read_acqb(&buffers->nonempty_slots)) {
            set_flags = 0;
            /*
             * We raced with a signal being inserted into a buffer;
             * need to adjust accordingly. Note that 'nonmsgs_in_slots'
             * or 'msgs_in_slots' may be set even though corresponding
             * signals aren't present in the buffers. This will however
             * sort itself out when no such signals appears in a
             * future call to erts_proc_sig_fetch().
             */
            if (erts_atomic32_read_nob(&buffers->nonmsgs_in_slots))
                set_flags |= ERTS_PSFLG_NMSG_SIG_IN_Q;
            if (erts_atomic32_read_nob(&buffers->msgs_in_slots))
                set_flags |= ERTS_PSFLG_MSG_SIG_IN_Q;
            if (set_flags) {
                erts_aint32_t oflgs;
                oflgs = erts_atomic32_read_bor_relb(&proc->state, set_flags);
                if ((oflgs & (ERTS_PSFLG_NMSG_SIG_IN_Q
                              | ERTS_PSFLG_MSG_SIG_IN_Q)) != set_flags) {
                    int msig = 0, nmsig = 0;
                    /*
                     * We did set at least one of the flags; check if we may
                     * need to set corresponding active flag(s)...
                     */
                    if ((!!(set_flags & ERTS_PSFLG_NMSG_SIG_IN_Q))
                        & (!(oflgs & (ERTS_PSFLG_NMSG_SIG_IN_Q
                                      | ERTS_PSFLG_ACTIVE_SYS)))) {
                        /* We set nmsig-in-q flag and active-sys missing... */
                        nmsig = !0;
                    }
                    if ((!!(set_flags & ERTS_PSFLG_MSG_SIG_IN_Q))
                        & (!(oflgs & (ERTS_PSFLG_MSG_SIG_IN_Q
                                      | ERTS_PSFLG_ACTIVE)))) {
                        /* We set msig-in-q flag and active missing... */
                        msig = !0;
                    }
                    if (msig | nmsig) {
                        /*
                         * We don't know exactly what locks we got, so
                         * we need to schedule the notification...
                         */
                        ErtsSchedulerData *esdp = erts_get_scheduler_data();
                        int tid = (esdp && esdp->type == ERTS_SCHED_NORMAL
                                   ? (int) esdp->no
                                   : 1);
                        ErtsSchedSignalNotify *ssnp =
                            (ErtsSchedSignalNotify *)
                            erts_alloc(ERTS_ALC_T_SCHD_SIG_NTFY,
                                       sizeof(ErtsSchedSignalNotify));
                        ssnp->nmsig = nmsig;
                        ssnp->msig = msig;
                        ssnp->pid = proc->common.id;
                        erts_schedule_misc_aux_work(tid,
                                                    sched_sig_notify,
                                                    (void *) ssnp);
                    }
                }
            }
            /* else:
             *       Another thread is currently operating on a buffer and
             *       will soon set appropriate.
             */
        }
    unget_buffers_return:
        erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);
    }

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
    if (not_exiting) {
        ERTS_HDBG_PRIVQ_LEN(proc);
    }
#endif

}

static void
sched_sig_notify(void *vssnp)
{
    ErtsSchedSignalNotify *ssnp = (ErtsSchedSignalNotify *) vssnp;
    Process *proc = erts_proc_lookup(ssnp->pid);
    if (proc) {
        erts_aint32_t state = erts_atomic32_read_acqb(&proc->state);
        int nmsig = ssnp->nmsig;
        int msig = ssnp->msig;
        ASSERT(nmsig || msig);
        if ((!!nmsig) & ((!(state & (ERTS_PSFLG_SIG_Q
                                     | ERTS_PSFLG_NMSG_SIG_IN_Q)))
                         | (!!(state & ERTS_PSFLG_ACTIVE_SYS)))) {
            /*
             * Either already handled or someone else set the active-sys
             * flag...
             */
            nmsig = 0;
        }
        if ((!!msig) & (!!(state & ERTS_PSFLG_ACTIVE))) {
            /*
             * Someone else set the active flag (we cannot determine if it
             * has been handled or not by looking at the state flag)...
             */
            msig = 0;
        }
        if (msig|nmsig) {
            if (!nmsig) {
                erts_proc_notify_new_message(proc, 0);
            }
            else {
                erts_aint32_t extra = msig ? ERTS_PSFLG_ACTIVE : 0;
                erts_proc_notify_new_sig(proc, state, extra);
            }
        }
    }
    erts_free(ERTS_ALC_T_SCHD_SIG_NTFY, vssnp);
}

void
erts_proc_sig_destroy_unlink_op(ErtsSigUnlinkOp *sulnk)
{
    erts_free(ERTS_ALC_T_SIG_DATA, sulnk);
}

static ERTS_INLINE ErtsDistExternal * 
get_external_non_msg_signal(ErtsMessage *sig)
{
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));

    if (ERTS_SIG_IS_DIST_ALTACT_MSG(sig)) {
        Eterm tag = ((ErtsSignal *) (sig))->common.tag;
        ErlHeapFragment *hfrag;
        switch (ERTS_PROC_SIG_TYPE(tag)) {
        case ERTS_SIG_Q_TYPE_DIST:
            hfrag = &sig->hfrag;
            break;
        case ERTS_SIG_Q_TYPE_DIST_FRAG:
            hfrag = sig->hfrag.next;
            break;
        default:
            ERTS_INTERNAL_ERROR("Invalid dist altact message type");
            return NULL;
        }
        return erts_get_dist_ext(hfrag);
    }

    if (ERTS_SIG_IS_GEN_EXIT(sig) && ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig)) {
        ErtsExitSignalData *xsigd = get_exit_signal_data(sig);
        ASSERT(ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag)
               == ERTS_SIG_Q_TYPE_GEN_EXIT);
        ASSERT(is_non_value(xsigd->reason));
        if (sig->hfrag.next == NULL) {
            int extra = ERTS_PROC_SIG_XTRA(((ErtsSignal *) sig)->common.tag);
            char *ptr = (char *) xsigd;
            ptr += ((extra & ERTS_SIG_ALTACT_SIG_X_TOKEN)
                    ? sizeof(ErtsSeqTokenExitSignalData)
                    : sizeof(ErtsExitSignalData));
            return (ErtsDistExternal *) ptr;
        }
        return erts_get_dist_ext(sig->hfrag.next);
    }

    return NULL;
}

ErtsDistExternal *
erts_proc_sig_get_external(ErtsMessage *msgp)
{
    if (ERTS_SIG_IS_EXTERNAL_MSG(msgp))
        return erts_get_dist_ext(msgp->data.heap_frag);
    if (ERTS_SIG_IS_NON_MSG(msgp))
        return get_external_non_msg_signal(msgp);
    return NULL;
}

static void do_seq_trace_output(Eterm to_pid, Eterm to, Eterm token, Eterm msg);

static void
send_gen_exit_signal(ErtsPTabElementCommon *sender, Eterm from_tag,
                     Eterm from, Eterm from_type, Eterm to,
                     Sint16 op, Eterm reason, ErtsDistExternal *dist_ext,
                     ErlHeapFragment *dist_ext_hfrag,
                     Eterm ref, Eterm token, int normal_kills,
                     Uint32 conn_lost, Uint32 conn_id, int prio)
{
    ErtsExitSignalData *xsigd;
    Eterm *hp, *start_hp, s_reason, s_ref, s_message, s_token, s_from, pid;
    ErtsMessage *mp;
    ErlHeapFragment *hfrag;
    ErlOffHeap *ohp;
    Uint hsz = 0, from_sz, reason_sz, ref_sz, token_sz, dist_ext_sz = 0;
    int seq_trace, has_token, extra = 0;
    Process *c_p;
#ifdef USE_VM_PROBES
    Eterm s_utag, utag;
    Uint utag_sz;
#endif

    if (is_internal_pid(to)) {
        pid = to;
    }
    else {
        ASSERT(is_ref(to));
        ASSERT(is_nil(ref));
        pid = erts_get_pid_of_ref(to);
        if (!is_internal_pid(pid))
            return;
        ref = to;
        extra |= ERTS_SIG_ALTACT_SIG_X_ALIAS;
    }

    if (prio)
        extra |= ERTS_SIG_ALTACT_SIG_X_PRIO;

    if (sender && is_internal_pid(from)) {
        c_p = ErtsContainerStruct(sender, Process, common);
    } else {
        c_p = NULL;
    }

    ASSERT((is_value(reason) && dist_ext == NULL) ||
           (is_non_value(reason) && dist_ext != NULL));

    ASSERT(is_immed(from_tag));
    ASSERT(from_type == am_process || from_type == am_port);

#ifdef USE_VM_PROBES
    utag_sz = 0;
    utag = NIL;
    if (c_p && token != NIL && (DT_UTAG_FLAGS(c_p) & DT_UTAG_SPREADING)) {
        utag_sz = size_object(DT_UTAG(c_p));
        utag = DT_UTAG(c_p);
    }
    else if (token == am_have_dt_utag) {
        token = NIL;
    }
    hsz += utag_sz;
#endif

    has_token = !is_nil(token);
    if (has_token) {
        hsz += sizeof(ErtsSeqTokenExitSignalData)/sizeof(Eterm);
        token_sz = size_object(token);
        hsz += token_sz;
        extra |= ERTS_SIG_ALTACT_SIG_X_TOKEN;
    }
    else {
        hsz += sizeof(ErtsExitSignalData)/sizeof(Eterm);
        token_sz = 0;
    }

    seq_trace = c_p && have_seqtrace(token);
    if (seq_trace) {
        seq_trace_update_serial(c_p);
    }

    from_sz = size_object(from);
    hsz += from_sz;

    ref_sz = size_object(ref);
    hsz += ref_sz;

    reason_sz = 0; /* Set to silence gcc warning */

    /* The reason was part of the control message,
       just use copy it into the xsigd */
    if (is_value(reason)) {
        reason_sz = size_object(reason);
        hsz += reason_sz;

        switch (op) {
        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED: {
            /* {'EXIT', From, Reason} */
            hsz += 4; /* 3-tuple */
            break;
        }
        case ERTS_SIG_Q_OP_MONITOR_DOWN: {
            /* {'DOWN', Ref, process | port, From, Reason} */
            hsz += 6; /* 5-tuple */
            break;
        }
        default:
            ERTS_INTERNAL_ERROR("Invalid exit signal op");
            break;
        }
    } else if (dist_ext != NULL && dist_ext_hfrag == NULL) {
        /* The message was not fragmented so we need to create space
           for a single dist_ext element */
        dist_ext_sz = erts_dist_ext_size(dist_ext) / sizeof(Eterm);
        hsz += dist_ext_sz;
    }

    /*
     * Allocate message combined with heap fragment...
     */
    mp = erts_alloc_message(hsz, &hp);
    hfrag = &mp->hfrag;
    mp->next = NULL;
    ohp = &hfrag->off_heap;
    start_hp = hp;

    s_token = is_immed(token) ? token : copy_struct(token, token_sz, &hp, ohp);
    s_from = is_immed(from) ? from : copy_struct(from, from_sz, &hp, ohp);
    s_ref = copy_struct(ref, ref_sz, &hp, ohp);

    if (is_value(reason)) {
        s_reason = copy_struct(reason, reason_sz, &hp, ohp);

        switch (op) {
        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
            /* {'EXIT', From, Reason} */
            s_message = TUPLE3(hp, am_EXIT, s_from, s_reason);
            hp += 4;
            break;
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            /* {'DOWN', Ref, process | port, From, Reason} */
            s_message = TUPLE5(hp, am_DOWN, s_ref, from_type, s_from, s_reason);
            hp += 6;
            break;
        default:
            /* This cannot happen, used to silence gcc warning */
            s_message = THE_NON_VALUE;
            break;
        }
    } else {
        s_message = THE_NON_VALUE;
        s_reason = THE_NON_VALUE;
    }

#ifdef USE_VM_PROBES
    s_utag = (is_immed(utag)
              ? utag
              : copy_struct(utag, utag_sz, &hp, ohp));
    ERL_MESSAGE_DT_UTAG(mp) = s_utag;
#endif
    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(op,
                                                  ERTS_SIG_Q_TYPE_GEN_EXIT,
                                                  extra);
    ERL_MESSAGE_FROM(mp) = from_tag; /* immediate... */

    hfrag->used_size = hp - start_hp;

    xsigd = (ErtsExitSignalData *) hp;

    xsigd->message = s_message;
    xsigd->from = s_from;
    xsigd->reason = s_reason;
    if (has_token)
        ((ErtsSeqTokenExitSignalData *) xsigd)->token = s_token;
    hfrag->next = dist_ext_hfrag;

    if (is_not_nil(s_ref)) {
        ASSERT(is_ref(s_ref));
        xsigd->u.ref = s_ref;
    }
    else {
        xsigd->u.link.flags = 0;
        if (normal_kills)
            xsigd->u.link.flags |= ERTS_SIG_LNK_X_FLAG_NORMAL_KILLS;
        if (conn_lost)
            xsigd->u.link.flags |= ERTS_SIG_LNK_X_FLAG_CONNECTION_LOST;
        xsigd->u.link.connection_id = conn_id;
    }

    hp += (has_token
           ? sizeof(ErtsSeqTokenExitSignalData)/sizeof(Eterm)
           : sizeof(ErtsExitSignalData)/sizeof(Eterm));

    if (dist_ext != NULL && dist_ext_hfrag == NULL && is_non_value(reason)) {
        erts_make_dist_ext_copy(dist_ext, (ErtsDistExternal *) hp);
        hp += dist_ext_sz;
    }

    ASSERT(hp == mp->hfrag.mem + mp->hfrag.alloc_size);

    if (seq_trace) {
        do_seq_trace_output(pid, to, s_token, s_message);
    }

    {
        /* Ensure that we're ordered relative to the sender process if one
         * exists, and not `from` as it may be a name instead of a pid. */
        Eterm order_by = sender ? sender->id : from;

        if (!proc_queue_signal(sender, order_by, pid, (ErtsSignal *)mp,
                               !(is_pid(order_by) || is_port(order_by)), op)) {
            mp->next = NULL;
            erts_cleanup_messages(mp);
        }
    }
}

static void
do_seq_trace_output(Eterm to_pid, Eterm to, Eterm token, Eterm msg)
{
    /*
     * We could do this when enqueuing the signal and avoid some
     * locking. However, the enqueuing code would then always
     * have the penalty of this seq-tracing code which we do not
     * want...
     */
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int is_normal_sched = !!esdp && esdp->type == ERTS_SCHED_NORMAL;
    Process *rp;

    if (is_normal_sched)
        rp = erts_proc_lookup_raw(to_pid);
    else
        rp = erts_proc_lookup_raw_inc_refc(to_pid);

    if (rp) {
        erts_proc_lock(rp, ERTS_PROC_LOCK_MSGQ);

        if (!ERTS_PROC_IS_EXITING(rp))
            seq_trace_output(token, msg, SEQ_TRACE_SEND, to, rp);

        erts_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);

        if (!is_normal_sched)
            erts_proc_dec_refc(rp);
    }
}

static ERTS_INLINE int
get_altact_msg_data(ErtsMessage *sig, void **attachedp,
                    Eterm *msgp, Eterm *fromp,
                    int *priop, Eterm *senderp,
                    Eterm *aliasp, Eterm *tokenp)
{
    Eterm tag = ((ErtsSignal *) sig)->common.tag;
    int type = ERTS_PROC_SIG_TYPE(tag);
    int extra = (int) ERTS_PROC_SIG_XTRA(tag);
    int prio;
    Eterm from, msg, alias, token, sender;
    void *attached;

    prio = !!(extra & ERTS_SIG_ALTACT_SIG_X_PRIO);

    if (type == ERTS_SIG_Q_TYPE_DIST || type == ERTS_SIG_Q_TYPE_DIST_FRAG) {
        int ix = 0;
        ASSERT(sig->hfrag.alloc_size
               >= (!!(extra & ERTS_SIG_ALTACT_SIG_X_PRIO)
                   + !!(extra & ERTS_SIG_ALTACT_SIG_X_ALIAS)
                   + !!(extra & ERTS_SIG_ALTACT_SIG_X_TOKEN)));
        from = ERL_MESSAGE_FROM(sig);
        msg = THE_NON_VALUE;
        sender = ((extra & ERTS_SIG_ALTACT_SIG_X_PRIO)
                  ? sig->hfrag.mem[ix++] /* pid */
                  : from); /* node name */
        alias = ((extra & ERTS_SIG_ALTACT_SIG_X_ALIAS)
                 ? sig->hfrag.mem[ix++]
                 : THE_NON_VALUE);
        token = ((extra & ERTS_SIG_ALTACT_SIG_X_TOKEN)
                 ? sig->hfrag.mem[ix++]
                 : NIL);
        attached = ERTS_MSG_COMBINED_HFRAG;
    }
    else {
        Eterm *tp;
        int ix;

        ASSERT(is_tuple(ERL_MESSAGE_FROM(sig)));

        tp = tuple_val(ERL_MESSAGE_FROM(sig));

        ix = 0;
        from = tp[++ix];
        msg = tp[++ix];
        sender = from;
        alias = (extra & ERTS_SIG_ALTACT_SIG_X_ALIAS) ? tp[++ix] : THE_NON_VALUE;
        token = (extra & ERTS_SIG_ALTACT_SIG_X_TOKEN) ? tp[++ix] : NIL;
        switch (type) {
        case ERTS_SIG_Q_TYPE_HEAP:
            attached = NULL;
            break;
        case ERTS_SIG_Q_TYPE_OFF_HEAP:
            attached = ERTS_MSG_COMBINED_HFRAG;
            break;
        case ERTS_SIG_Q_TYPE_HEAP_FRAG: {
            Uint low, high;
            low = unsigned_val(tp[++ix]);
            high = unsigned_val(tp[++ix]);
#ifdef ARCH_64
            ASSERT((((Uint) 1) << 32) > low);
            ASSERT((((Uint) 1) << 32) > high);
            attached = (void *) ((((Uint) high) << 32) | ((Uint) low));
#else /* ARCH_32 */
            ASSERT((((Uint) 1) << 16) > low);
            ASSERT((((Uint) 1) << 16) > high);
            attached = (void *) ((((Uint) high) << 16) | ((Uint) low));
#endif
            ASSERT(attached != NULL);
            break;
        }
        default:
            attached = NULL;
            ERTS_INTERNAL_ERROR("Invalid altact message type");
        }
        ASSERT(arityval(tp[0]) == ix);
    }

    if (fromp)
        *fromp = from;
    if (senderp)
        *senderp = sender;
    if (msgp)
        *msgp = msg;
    if (aliasp)
        *aliasp = alias;
    if (attachedp)
        *attachedp = attached;
    if (tokenp)
        *tokenp = token;
    if (priop)
        *priop = prio;

    return type;
}

static ERTS_INLINE void
clear_seq_token_altact_msg(ErtsMessage *sig)
{
    Uint tag = ((ErtsSignal *) sig)->common.tag;
    int extra = (int) ERTS_PROC_SIG_XTRA(tag);
    ASSERT(ERTS_SIG_Q_OP_ALTACT_MSG == ERTS_PROC_SIG_OP(tag));
    if (extra & ERTS_SIG_ALTACT_SIG_X_TOKEN) {
        int ix;
        switch (ERTS_PROC_SIG_TYPE(tag)) {
        case ERTS_SIG_Q_TYPE_DIST:
            ix = (extra & ERTS_SIG_ALTACT_SIG_X_ALIAS) ? 1 : 0;
            sig->hfrag.mem[ix] = ERTS_CLEAR_SEQ_TOKEN_VALUE(sig);
            break;
        case ERTS_SIG_Q_TYPE_HEAP:
        case ERTS_SIG_Q_TYPE_OFF_HEAP:
        case ERTS_SIG_Q_TYPE_HEAP_FRAG: {
            Eterm *tp = tuple_val(ERL_MESSAGE_FROM(sig));
            ix = (extra & ERTS_SIG_ALTACT_SIG_X_ALIAS) ? 4 : 3;
            ASSERT(arityval(tp[0]) >= ix);
            tp[ix] = ERTS_CLEAR_SEQ_TOKEN_VALUE(sig);
            break;
        }
        default:
            ASSERT(0);
            break;
        }
    }
}

void
erts_proc_sig_cleanup_non_msg_signal(ErtsMessage *sig)
{
    ErlHeapFragment *hfrag;
    Eterm tag = ((ErtsSignal *) sig)->common.tag;
    
    /*
     * Heap altact message and heap frag altact message are
     * the only non-message signals, which are allocated as
     * messages, which do not use a combined message / heap
     * fragment.
     */
    if (ERTS_SIG_IS_HEAP_ALTACT_MSG_TAG(tag)) {
        sig->data.heap_frag = NULL;
        return;
    }

    if (ERTS_SIG_IS_HEAP_FRAG_ALTACT_MSG_TAG(tag)) {
        /* Retrieve pointer to heap fragment (may not be NULL). */
        void *attached;
        (void) get_altact_msg_data(sig, &attached, NULL, NULL,
                                   NULL, NULL, NULL, NULL);
        sig->data.heap_frag = hfrag = (ErlHeapFragment *) attached;
        ASSERT(hfrag);
    }
    else {
        /*
         * Using a combined heap fragment...
         */
        switch (ERTS_PROC_SIG_OP(tag)) {

        case ERTS_SIG_Q_OP_ADJ_MSGQ: {
            /* We need to deallocate yield markers if such has been used... */
            ErtsYieldAdjMsgQ *yp;
            switch (ERTS_PROC_SIG_TYPE(tag)) {
            case ERTS_SIG_Q_TYPE_CLA: {
                ErtsCLAData *cla = get_cla_data(sig);
                yp = cla->yield;
                cla->yield = NULL;
                break;
            }
            case ERTS_SIG_Q_TYPE_OFF_HEAP: {
                ErtsAdjOffHeapMsgQData *ohdp = get_move_msgq_off_heap_data(sig);
                yp = ohdp->yield;
                ohdp->yield = NULL;
                break;
            }
            default:
                ERTS_INTERNAL_ERROR("Invalid adjust-message-queue signal type");
                yp = NULL;
                break;
            }
            if (yp) {
                ASSERT(!yp->next.in_msgq && !yp->next.in_sigq);
                ASSERT(!yp->last.in_msgq && !yp->last.in_sigq);
                erts_free(ERTS_ALC_T_SIG_YIELD_DATA, yp);
            }
            break;
        }

        default: {
            ErtsDistExternal *edep = get_external_non_msg_signal(sig);
            if (edep)
                erts_free_dist_ext_copy(edep);
            break;
        }
        }

        sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
        hfrag = sig->hfrag.next;
        erts_cleanup_offheap(&sig->hfrag.off_heap);
    }

    if (hfrag)
        free_message_buffer(hfrag);
}

void
erts_proc_sig_send_altact_msg(Process *c_p, Eterm from, Eterm to, Eterm msg, Eterm token, int prio)
{
    Process *rp;
    ErlHeapFragment *hfrag;
    ErtsProcLocks rp_locks = 0;
    erts_aint32_t rp_state;
    ErtsMessage *mp;
    ErlOffHeap *ohp;
    Uint hsz, to_sz, token_sz, msg_sz;
    Eterm *hp, *end_hp, *itpl, pid, to_copy, token_copy, msg_copy;
    int ix, type, has_token, alias, extra;
#ifdef SHCOPY_SEND
    erts_shcopy_t info;
#else
    erts_literal_area_t litarea;
#endif
#ifdef USE_VM_PROBES
    Eterm utag_copy, utag;
    Uint utag_sz;
#endif

    hsz = 3; /*
              * Base info-tuple with from and message values. See comment below
              * for more info on the content of this info tuple...
              */

    extra = 0;
    if (prio)
        extra |= ERTS_SIG_ALTACT_SIG_X_PRIO;

    ASSERT(is_internal_ref(to) || is_internal_pid(to));
    ASSERT(is_internal_pid(from) || is_atom(from));

    if (is_internal_pid(to)) {
        pid = to;
        alias = 0;
        to_sz = 0;
    }
    else {
        to_sz = size_object(to);
        hsz += to_sz;
        pid = erts_get_pid_of_ref(to);
        extra |= ERTS_SIG_ALTACT_SIG_X_ALIAS;
        alias = !0;
        hsz += 1; /* extra element in info-tuple */
    }
    rp = erts_proc_lookup(pid);
    if (!rp)
        return;

    rp_locks = c_p == rp ? ERTS_PROC_LOCK_MAIN : 0;

    if (c_p && have_seqtrace(token)) {
        seq_trace_update_serial(c_p);
	seq_trace_output(token, msg, SEQ_TRACE_SEND, to, c_p);
    }

#ifdef USE_VM_PROBES
    utag_sz = 0;
    utag = NIL;
    if (c_p && token != NIL && (DT_UTAG_FLAGS(c_p) & DT_UTAG_SPREADING)) {
        utag_sz = size_object(DT_UTAG(c_p));
        utag = DT_UTAG(c_p);
    }
    else if (token == am_have_dt_utag) {
        token = NIL;
    }
    hsz += utag_sz;
#endif

    has_token = !is_nil(token);
    if (has_token) {
        token_sz = size_object(token);
        hsz += 1 /* extra element in info-tuple */ + token_sz;
        extra |= ERTS_SIG_ALTACT_SIG_X_TOKEN;
    }
    else {
        token_sz = 0;
    }

    /*
     * SHCOPY corrupts the heap between copy_shared_calculate(), and
     * copy_shared_perform() by inserting move-markers (like the gc).
     * Make sure we don't use the heap between those instances.
     */

    if (is_immed(msg))
	msg_sz = 0;
    else {
#ifdef SHCOPY_SEND
	INITIALIZE_SHCOPY(info);
	msg_sz = copy_shared_calculate(msg, &info);
#else
	INITIALIZE_LITERAL_PURGE_AREA(litarea);
	msg_sz = size_object_litopt(msg, &litarea);
#endif
	hsz += msg_sz;
    }

    rp_state = erts_atomic32_read_nob(&rp->state);
    if (rp_state & ERTS_PSFLG_OFF_HEAP_MSGQ) {
        type = ERTS_SIG_Q_TYPE_OFF_HEAP;
	mp = erts_alloc_message(hsz, &hp);
	ohp = &mp->hfrag.off_heap;
        hfrag = NULL;
#ifdef DEBUG
        end_hp = hp + hsz;
#endif
    }
    else {
        int on_heap;
        hsz += 2; /* extra elements in info-tuple if we get a heap fragment */
        mp = erts_try_alloc_message_on_heap(rp, &rp_state, &rp_locks,
                                            hsz, &hp, &ohp, &on_heap);
        if (!on_heap) {
            type = ERTS_SIG_Q_TYPE_HEAP_FRAG;
            hfrag = mp->data.heap_frag;
            ASSERT(hfrag);
#ifdef DEBUG
            end_hp = hp + hsz;
#endif
        }
        else {
            /* no need to save heap fragment pointer... */
            Eterm *alloc_end_hp;
            type = ERTS_SIG_Q_TYPE_HEAP;
            alloc_end_hp = hp + hsz;
            end_hp = alloc_end_hp - 2;
            HRelease(rp, alloc_end_hp, end_hp);
            hsz -= 2;
            hfrag = NULL;
        }
    }

    mp->next = NULL;

    if (is_immed(msg))
	msg_copy = msg;
    else {
#ifdef SHCOPY_SEND
	msg_copy = copy_shared_perform(msg, msg_sz, &info, &hp, ohp);
	DESTROY_SHCOPY(info);
#else
	msg_copy = copy_struct_litopt(msg, msg_sz, &hp, ohp, &litarea);
#endif
    }
    to_copy = !to_sz ? to : copy_struct(to, to_sz, &hp, ohp);
    token_copy = !token_sz ? token : copy_struct(token, token_sz, &hp, ohp);
#ifdef USE_VM_PROBES
    utag_copy = (is_immed(utag)
                 ? utag
                 : copy_struct(utag, utag_sz, &hp, ohp));
    ERL_MESSAGE_DT_UTAG(mp) = utag_copy;
#endif

    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALTACT_MSG,
                                                  type, extra);

    /*
     * Info-tuple referred from the ERL_MESSAGE_FROM(mp) field of the
     * message while in transit:
     * - From value - always present
     * - Message value - always present
     * - Alias value - iff send using an alias
     * - Token value - iff message with non-nil token
     * - Low part of heap fragment pointer - iff contained in heap fragment
     * - High part of heap fragment pointer - iff contained in heap fragment
     *
     * That is, the info tuple might be of arity 2-6
     */
    
    ix = 0;
    itpl = hp;
    itpl[++ix] = from;
    itpl[++ix] = msg_copy;
    if (alias)
        itpl[++ix] = to_copy;
    if (has_token)
        itpl[++ix] = token_copy;
    if (type == ERTS_SIG_Q_TYPE_HEAP_FRAG) {
        Uint low, high;
#ifdef ARCH_64
        low = ((UWord) hfrag) & ((UWord) 0xffffffff);
        high = (((UWord) hfrag) >> 32) & ((UWord) 0xffffffff);
#else /* ARCH_32 */
        low = ((UWord) hfrag) & ((UWord) 0xffff);
        high = (((UWord) hfrag) >> 16) & ((UWord) 0xffff);
#endif
        itpl[++ix] = make_small(low);
        itpl[++ix] = make_small(high);
    }

    ASSERT(end_hp == hp + 1 + ix);

    itpl[0] = make_arityval(ix);
    ERL_MESSAGE_FROM(mp) = make_tuple(itpl);

    if (!proc_queue_signal(&c_p->common, from, pid, (ErtsSignal *) mp, 0,
                           ERTS_SIG_Q_OP_ALTACT_MSG)) {
        mp->next = NULL;
        erts_cleanup_messages(mp);
    }

    ERTS_LC_ASSERT(!(rp_locks & ERTS_PROC_LOCKS_ALL_MINOR));
    if (c_p != rp && rp_locks)
        erts_proc_unlock(rp, rp_locks);
    
    if (c_p && hsz > ERTS_MSG_COPY_WORDS_PER_REDUCTION) {
        Uint reds = hsz / ERTS_MSG_COPY_WORDS_PER_REDUCTION;
        if (reds > CONTEXT_REDS)
            reds = CONTEXT_REDS;
        BUMP_REDS(c_p, (int) reds);
    }
}

void
erts_proc_sig_send_dist_altact_msg(Eterm from,
                                   Eterm to,
                                   ErtsDistExternal *edep,
                                   ErlHeapFragment *hfrag,
                                   Eterm token,
                                   int prio)
{
    ErtsMessage* mp;
    Eterm *hp, pid, from_copy, to_copy, token_copy;
    Uint extra_hsz;
    int extra, has_alias, has_token, type;

#ifdef USE_VM_PROBES
    if (token == am_have_dt_utag)
	token = NIL;
#endif

    extra = 0;
    if (prio)
        extra |= ERTS_SIG_ALTACT_SIG_X_PRIO;

    if (is_internal_pid(to)) {
        pid = to;
        has_alias = 0;
    }
    else {
        ASSERT(is_ref(to));
        has_alias = !0;
        pid = erts_get_pid_of_ref(to);
        extra |= ERTS_SIG_ALTACT_SIG_X_ALIAS;
        if (!is_internal_pid(pid)) {
            if (hfrag) {
                /* Fragmented message... */
                erts_free_dist_ext_copy(erts_get_dist_ext(hfrag));
                free_message_buffer(hfrag);
            }
            return;
        }
    }

    has_token = !is_nil(token);
    if (has_token)
        extra |= ERTS_SIG_ALTACT_SIG_X_TOKEN;

    extra_hsz = 0;
    if (prio)
        extra_hsz++; /* from */
    if (has_alias)
        extra_hsz++;
    if (has_token)
        extra_hsz++;

    if (hfrag) {
        /*
         * Fragmented message. Data already allocated in heap fragment
         * including 'from', 'token' and 'to' ref.
         */
        mp = erts_alloc_message(extra_hsz, &hp);
        from_copy = from;
        to_copy = to;
        token_copy = token;
        mp->hfrag.next = hfrag;
        type = ERTS_SIG_Q_TYPE_DIST_FRAG;
    } else {
        /* Un-fragmented message, allocate space for from, alias,
           token and dist_ext in message. */
        Uint dist_ext_sz = erts_dist_ext_size(edep) / sizeof(Eterm);
        Uint from_sz = 0, to_sz = 0, token_sz = 0, usz, asz;

        usz = extra_hsz;
        if (prio) {
            from_sz = size_object(from);
            usz += from_sz;
        }
        if (has_alias) {
            to_sz = size_object(to);
            usz += to_sz;
        }
        if (has_token) {
            token_sz = size_object(token);
            usz += token_sz;
        }

        asz = usz + dist_ext_sz;

        mp = erts_alloc_message(asz, &hp);
        hp += extra_hsz;
        from_copy = (!prio
                     ? NIL
                     : copy_struct(from, from_sz, &hp, &mp->hfrag.off_heap));
        to_copy = (!has_alias
                   ? NIL
                   : copy_struct(to, to_sz, &hp, &mp->hfrag.off_heap));
        token_copy = (!has_token
                      ? NIL
                      : copy_struct(token, token_sz, &hp, &mp->hfrag.off_heap));
        ASSERT(usz == hp - &mp->hfrag.mem[0]);
        mp->hfrag.used_size = usz;
        erts_make_dist_ext_copy(edep, erts_get_dist_ext(&mp->hfrag));
        type = ERTS_SIG_Q_TYPE_DIST;
    }

    if (extra_hsz) {
        int ix = 0;
        hp = &mp->hfrag.mem[0];
        if (prio)
            hp[ix++] = from_copy;
        if (has_alias)
            hp[ix++] = to_copy;
        if (has_token)
            hp[ix++] = token_copy;
    }

    ERL_MESSAGE_FROM(mp) = edep->dep->sysname;
    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALTACT_MSG,
                                                  type, extra);

    if (!proc_queue_signal(NULL, from, pid, (ErtsSignal *) mp, 0,
                           ERTS_SIG_Q_OP_ALTACT_MSG)) {
        mp->next = NULL;
        erts_cleanup_messages(mp);
    }
}

/**
 * @brief Send a persistent monitor triggered signal to a process.
 *
 * Used by monitors that are not auto disabled such as for
 * example 'time_offset' monitors.
 */
static void
erts_proc_sig_send_persistent_monitor_msg(Uint16 type, Eterm key,
                                          Eterm from, Eterm to,
                                          Eterm msg, Uint msg_sz,
                                          int force_flush) {
    ErtsPersistMonMsg *prst_mon;
    ErtsMessage *mp;
    ErlHeapFragment *hfrag;
    Eterm *hp, *start_hp, message;
    ErlOffHeap *ohp;
    Uint hsz = sizeof(ErtsPersistMonMsg) + msg_sz;

    /*
     * Allocate message combined with heap fragment...
     */
    mp = erts_alloc_message(hsz, &hp);
    hfrag = &mp->hfrag;
    mp->next = NULL;
    ohp = &hfrag->off_heap;
    start_hp = hp;

    ASSERT(msg_sz == size_object(msg));
    message = copy_struct(msg, msg_sz, &hp, ohp);
    hfrag->used_size = hp - start_hp;

    prst_mon = (ErtsPersistMonMsg *) (char *) hp;
    prst_mon->message = message;

    switch (type) {
    case ERTS_MON_TYPE_NODES:
        ASSERT(is_small(key));
        prst_mon->key = key;
        break;

    case ERTS_MON_TYPE_TIME_OFFSET:
        ASSERT(is_internal_ref(key));
        ASSERT(is_tuple_arity(message, 5));

        prst_mon->key = tuple_val(message)[2];

        ASSERT(eq(prst_mon->key, key));
        break;

    default:
        ERTS_INTERNAL_ERROR("Invalid persistent monitor type");
        prst_mon->key = key;
        break;
    }

    ASSERT(is_immed(from));

    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_PERSISTENT_MON_MSG,
                                                  type, 0);
    ERL_MESSAGE_FROM(mp) = from;
    ERL_MESSAGE_TOKEN(mp) = am_undefined;

    if (!proc_queue_signal(NULL, from, to, (ErtsSignal *) mp, force_flush,
                           ERTS_SIG_Q_OP_PERSISTENT_MON_MSG)) {
        mp->next = NULL;
        erts_cleanup_messages(mp);
    }
}

void
erts_proc_sig_send_monitor_nodes_msg(Eterm key, Eterm to,
                                     Eterm msg, Uint msg_sz) {
    erts_proc_sig_send_persistent_monitor_msg(ERTS_MON_TYPE_NODES,
                                              key, am_system, to,
                                              msg, msg_sz, 1);
}

void
erts_proc_sig_send_monitor_time_offset_msg(Eterm key, Eterm to,
                                           Eterm msg, Uint msg_sz) {
    erts_proc_sig_send_persistent_monitor_msg(ERTS_MON_TYPE_TIME_OFFSET,
                                              key, am_clock_service, to,
                                              msg, msg_sz, 0);

}

static ERTS_INLINE Eterm
get_persist_mon_msg(ErtsMessage *sig, Eterm *msg)
{
    ErtsPersistMonMsg *prst_mon;
    prst_mon = ((ErtsPersistMonMsg *)
                (char *) (&sig->hfrag.mem[0]
                          + sig->hfrag.used_size));
    *msg = prst_mon->message;
    return prst_mon->key;
}

void
erts_proc_sig_send_exit(ErtsPTabElementCommon *sender, Eterm from, Eterm to,
                        Eterm reason, Eterm token, int normal_kills, int prio)
{
    Eterm from_tag, from_type;

    ASSERT(sender == NULL || sender->id == from);

    if (is_immed(from)) {
        ASSERT(is_internal_pid(from) || is_internal_port(from));
        from_tag = from;
        from_type = is_internal_port(from) ? am_port : am_process;
    }
    else {
        DistEntry *dep;
        ASSERT(is_external_pid(from));
        dep = external_pid_dist_entry(from);
        from_tag = dep->sysname;
        from_type = am_process;
    }

    send_gen_exit_signal(sender, from_tag, from, from_type, to,
                         ERTS_SIG_Q_OP_EXIT, reason, NULL, NULL, NIL, token,
                         normal_kills, 0, 0, prio);
}

void
erts_proc_sig_send_dist_exit(DistEntry *dep,
                             Eterm from, Eterm to,
                             ErtsDistExternal *dist_ext,
                             ErlHeapFragment *hfrag,
                             Eterm reason, Eterm token, int prio)
{
    send_gen_exit_signal(NULL, dep->sysname, from, am_process, to,
                         ERTS_SIG_Q_OP_EXIT, reason, dist_ext, hfrag, NIL, token,
                         0, 0, 0, prio);
}

void
erts_proc_sig_send_link_exit_noconnection(ErtsLink *lnk)
{
    Eterm to, from_tag, from_item;
    ErtsLink *olnk;
    ErtsELink *elnk;
    Uint32 conn_id;

    to = lnk->other.item;

    ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);
    ASSERT(ERTS_ML_GET_TYPE(lnk) == ERTS_LNK_TYPE_DIST_PROC);

    olnk = erts_link_to_other(lnk, &elnk);

    from_item = olnk->other.item;
    from_tag = elnk->dist->nodename;
    conn_id = elnk->dist->connection_id;

    send_gen_exit_signal(NULL, from_tag, from_item, am_process, to,
                         ERTS_SIG_Q_OP_EXIT_LINKED, am_noconnection, NULL, NULL,
                         NIL, NIL, 0, !0, conn_id, 0);

    erts_link_release(lnk);
}

void
erts_proc_sig_send_link_exit(ErtsPTabElementCommon *sender, Eterm from,
                             ErtsLink *lnk, Eterm reason, Eterm token)
{
    Eterm to, from_type;

    ASSERT(sender == NULL || sender->id == from);
    ASSERT(lnk);

    to = lnk->other.item;

    ASSERT(is_internal_pid(from) || is_internal_port(from));

    from_type = is_internal_port(from) ? am_port : am_process;

    send_gen_exit_signal(sender, from, from, from_type, to,
                         ERTS_SIG_Q_OP_EXIT_LINKED, reason, NULL, NULL, NIL,
                         token, 0, 0, 0, 0);

    erts_link_release(lnk);
}

int
erts_proc_sig_send_link(ErtsPTabElementCommon *sender, Eterm from,
                        Eterm to, ErtsLink *lnk)
{
    ErtsSignal *sig;
    Uint32 type = ERTS_ML_GET_TYPE(lnk);

    ASSERT(!sender || sender->id == from);
    ASSERT(lnk && eq(from, lnk->other.item));
    ASSERT(is_internal_pid(to));

    sig = (ErtsSignal *) lnk;
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_LINK,
                                             type, 0);

    return proc_queue_signal(sender, from, to, sig, 0, ERTS_SIG_Q_OP_LINK);
}

int
erts_proc_sig_send_nif_select(Eterm to, ErtsMessage *msg) {
    ErtsSignal *sig = (ErtsSignal*)msg;

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_NIF_SELECT,
                                             ERTS_SIG_Q_TYPE_UNDEFINED,
                                             0);
    return proc_queue_signal(NULL, am_system, to, sig, 0, ERTS_SIG_Q_OP_NIF_SELECT);
}

ErtsSigUnlinkOp *
erts_proc_sig_make_unlink_op(ErtsPTabElementCommon *sender, Eterm from)
{
    ErtsSigUnlinkOp *sulnk;

    ASSERT(sender->id == from);

    sulnk = erts_alloc(ERTS_ALC_T_SIG_DATA, sizeof(ErtsSigUnlinkOp));
    sulnk->from = from;
    sulnk->id = erts_proc_sig_new_unlink_id(sender);

    return sulnk;
}

Uint64
erts_proc_sig_send_unlink(ErtsPTabElementCommon *sender, Eterm from,
                          ErtsLink *lnk)
{
    int res;
    ErtsSignal *sig;
    Eterm to;
    ErtsSigUnlinkOp *sulnk;
    Uint64 id;

    ASSERT(ERTS_ML_GET_TYPE(lnk) != ERTS_LNK_TYPE_PROC
           || ERTS_ML_GET_TYPE(lnk) != ERTS_LNK_TYPE_PORT);
    ASSERT(lnk->flags & ERTS_ML_FLG_IN_TABLE);

    sulnk = erts_proc_sig_make_unlink_op(sender, from);
    id = sulnk->id;
    sig = (ErtsSignal *) sulnk;
    to = lnk->other.item;
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_UNLINK,
                                             ERTS_ML_GET_TYPE(lnk), 0);

    ASSERT(is_internal_pid(to));
    res = proc_queue_signal(sender, from, to, sig, 0, ERTS_SIG_Q_OP_UNLINK);
    if (res == 0) {
        erts_proc_sig_destroy_unlink_op(sulnk);
        return 0;
    }
    return id;
}

void
erts_proc_sig_send_unlink_ack(ErtsPTabElementCommon *sender, Eterm from,
                              ErtsSigUnlinkOp *sulnk)
{
    ErtsSignal *sig = (ErtsSignal *) sulnk;
    Eterm to = sulnk->from;
    Uint16 type;

    ASSERT(is_internal_pid(to));
    ASSERT(is_internal_pid(from) || is_internal_port(from));

    sulnk->from = from;
    type = is_internal_pid(from) ? ERTS_LNK_TYPE_PROC : ERTS_LNK_TYPE_PORT;
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_UNLINK_ACK,
                                             type, 0);

    if (!proc_queue_signal(sender, from, to, sig, 0, ERTS_SIG_Q_OP_UNLINK_ACK)) {
        erts_proc_sig_destroy_unlink_op(sulnk);
    }
}

void
erts_proc_sig_send_dist_link_exit(DistEntry *dep,
                                  Eterm from, Eterm to,
                                  ErtsDistExternal *dist_ext,
                                  ErlHeapFragment *hfrag,
                                  Eterm reason, Eterm token)
{
    send_gen_exit_signal(NULL, dep->sysname, from, am_process, to,
                         ERTS_SIG_Q_OP_EXIT_LINKED, reason, dist_ext, hfrag,
                         NIL, token, 0, 0, 0, 0);

}

static void
reply_dist_unlink_ack(Process *c_p, ErtsSigDistUnlinkOp *sdulnk);

void
erts_proc_sig_send_dist_unlink(DistEntry *dep, Uint32 conn_id,
                               Eterm from, Eterm to, Uint64 id)
{
    /* Remote to local */
    ErtsSignal *sig;

    ASSERT(is_internal_pid(to));
    ASSERT(is_external_pid(from));
    ASSERT(dep == external_pid_dist_entry(from));

    sig = (ErtsSignal *) make_sig_dist_unlink_op(ERTS_SIG_Q_OP_UNLINK,
                                                 dep->sysname, conn_id,
                                                 to, from, id);

    if (!proc_queue_signal(NULL, from, to, sig, 0,
                           ERTS_SIG_Q_OP_UNLINK)) {
        reply_dist_unlink_ack(NULL, (ErtsSigDistUnlinkOp *) sig);
    }
}

void
erts_proc_sig_send_dist_unlink_ack(DistEntry *dep,
                                   Uint32 conn_id, Eterm from, Eterm to,
                                   Uint64 id)
{
    /* Remote to local */
    ErtsSignal *sig;

    ASSERT(is_internal_pid(to));
    ASSERT(is_external_pid(from));
    ASSERT(dep == external_pid_dist_entry(from));

    sig = (ErtsSignal *) make_sig_dist_unlink_op(ERTS_SIG_Q_OP_UNLINK_ACK,
                                                 dep->sysname, conn_id,
                                                 to, from, id);

    if (!proc_queue_signal(NULL, from, to, sig, 0,
                           ERTS_SIG_Q_OP_UNLINK_ACK)) {
        destroy_sig_dist_unlink_op((ErtsSigDistUnlinkOp *) sig);
    }
}

static void
reply_dist_unlink_ack(Process *c_p, ErtsSigDistUnlinkOp *sdulnk)
{
    /* Local to remote */
    ASSERT(is_external_pid(sdulnk->remote));

    /*
     * 'id' is zero if the other side not understand
     * unlink-ack signals...
     */
    if (sdulnk->id) {
        DistEntry *dep = external_pid_dist_entry(sdulnk->remote);

        /*
         * Do not set up new a connection; only send unlink ack
         * on the same connection which the unlink operation was
         * received on...
         */
        if (dep != erts_this_dist_entry && sdulnk->nodename == dep->sysname) {
            ErtsDSigSendContext ctx;
            int code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                         ERTS_DSP_NO_LOCK, 1, 1, 0);
            switch (code) {
            case ERTS_DSIG_PREP_CONNECTED:
            case ERTS_DSIG_PREP_PENDING:
                if (sdulnk->connection_id == ctx.connection_id) {
                    code = erts_dsig_send_unlink_ack(&ctx,
                                                     sdulnk->local,
                                                     sdulnk->remote,
                                                     sdulnk->id);
                    ASSERT(code == ERTS_DSIG_SEND_OK);
                }
                break;
            default:
                break;
            }
        }
    }

    destroy_sig_dist_unlink_op(sdulnk);
}

void
erts_proc_sig_send_dist_monitor_down(DistEntry *dep, Eterm ref,
                                     Eterm from, Eterm to,
                                     ErtsDistExternal *dist_ext,
                                     ErlHeapFragment *hfrag,
                                     Eterm reason)
{
    Eterm monitored, heap[3];

    if (is_atom(from))
        monitored = TUPLE2(&heap[0], from, dep->sysname);
    else
        monitored = from;

    send_gen_exit_signal(NULL, dep->sysname, monitored, am_process,
                         to, ERTS_SIG_Q_OP_MONITOR_DOWN,
                         reason, dist_ext, hfrag, ref, NIL, 0, 0, 0, 0);
}

void
erts_proc_sig_send_monitor_down(ErtsPTabElementCommon *sender, Eterm from,
                                ErtsMonitor *mon, Eterm reason)
{
    Eterm to;

    ASSERT(erts_monitor_is_target(mon));
    ASSERT(!erts_monitor_is_in_table(mon));

    to = mon->other.item;
    ASSERT(is_internal_pid(to));

    if (is_immed(reason)) {
        /* Pass signal using old monitor structure... */
        ErtsSignal *sig;

    send_using_monitor_struct:

        mon->other.item = reason; /* Pass immed reason via other.item... */
        sig = (ErtsSignal *) mon;
        sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_MONITOR_DOWN,
                                                 ERTS_ML_GET_TYPE(mon), 0);
        if (proc_queue_signal(sender, from, to, sig,
                              !(is_pid(from) || is_port(from)),
                              ERTS_SIG_Q_OP_MONITOR_DOWN)) {
            return; /* receiver will destroy mon structure */
        }
    }
    else {
        ErtsMonitorData *mdp = erts_monitor_to_data(mon);
        Eterm from_tag, monitored, from_type, heap[3];

        if (ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_SUSPEND) {
            /*
             * Set reason to 'undefined', since exit
             * reason is not used for suspend monitors,
             * and send using monitor structure. This
             * since we don't want to trigger
             * unnecessary memory allocation etc...
             */
            reason = am_undefined;
            goto send_using_monitor_struct;
        }

        if (!(mon->flags & ERTS_ML_FLG_NAME)) {
            from_tag = monitored = mdp->origin.other.item;
            from_type = is_internal_port(from_tag) ? am_port : am_process;
            if (is_external_pid(from_tag)) {
                DistEntry *dep = external_pid_dist_entry(from_tag);
                from_tag = dep->sysname;
            }
        }
        else {
            ErtsMonitorDataExtended *mdep;
            Eterm name, node;
            mdep = (ErtsMonitorDataExtended *) mdp;
            name = mdep->u.name;
            ASSERT(is_atom(name));
            if (mdep->dist) {
                node = mdep->dist->nodename;
                from_tag = node;
                from_type = am_process;
            }
            else {
                node = erts_this_dist_entry->sysname;
                from_tag = mdp->origin.other.item;
                from_type = is_internal_port(from_tag) ? am_port : am_process;
            }
            ASSERT(is_internal_port(from_tag)
                   || is_internal_pid(from_tag)
                   || is_atom(from_tag));
            monitored = TUPLE2(&heap[0], name, node);
        }

        send_gen_exit_signal(sender, from_tag, monitored, from_type,
                             to, ERTS_SIG_Q_OP_MONITOR_DOWN,
                             reason, NULL, NULL, mdp->ref, NIL,
                             0, 0, 0, 0);
    }

    erts_monitor_release(mon);
}

void
erts_proc_sig_send_dist_demonitor(Eterm from, Eterm to, Eterm ref)
{
    ErtsSigDistProcDemonitor *dmon;
    ErtsSignal *sig;
    Eterm *hp;
    ErlOffHeap oh;
    size_t size;

    ERTS_INIT_OFF_HEAP(&oh);

    ASSERT(is_external_pid(from));
    ASSERT(is_internal_pid(to));

    size = sizeof(ErtsSigDistProcDemonitor) - sizeof(Eterm);
    ASSERT(is_ref(ref));
    size += NC_HEAP_SIZE(ref)*sizeof(Eterm);

    dmon = erts_alloc(ERTS_ALC_T_DIST_DEMONITOR, size);

    hp = &dmon->heap[0];
    dmon->ref = STORE_NC(&hp, &oh, ref);
    sig = (ErtsSignal *) dmon;

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_DEMONITOR,
                                             ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR,
                                             0);

    if (!proc_queue_signal(NULL, from, to, sig, 0, ERTS_SIG_Q_OP_DEMONITOR)) {
        destroy_dist_proc_demonitor(dmon);
    }
}

void
erts_proc_sig_send_demonitor(ErtsPTabElementCommon *sender, Eterm from,
                             int system, ErtsMonitor *mon)
{
    ErtsSignal *sig = (ErtsSignal *) mon;
    Uint32 type = ERTS_ML_GET_TYPE(mon);
    Eterm to = mon->other.item;

    ASSERT(is_internal_pid(to) || to == am_undefined);
    ASSERT(erts_monitor_is_origin(mon));
    ASSERT(!erts_monitor_is_in_table(mon));
    ASSERT(!system || sender == NULL);

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_DEMONITOR,
                                             type, 0);

    if (is_not_internal_pid(to)
        || !proc_queue_signal(sender, from, to, sig,
                              !(system || (is_pid(from) || is_port(from))),
                              ERTS_SIG_Q_OP_DEMONITOR)) {
        erts_monitor_release(mon);
    }
}

int
erts_proc_sig_send_monitor(ErtsPTabElementCommon *sender, Eterm from,
                           ErtsMonitor *mon, Eterm to)
{
    ErtsSignal *sig = (ErtsSignal *) mon;
    Uint32 type = ERTS_ML_GET_TYPE(mon);

    ASSERT(is_internal_pid(to) || to == am_undefined);
    ASSERT(erts_monitor_is_target(mon));

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_MONITOR,
                                             type, 0);

    return proc_queue_signal(sender, from, to, sig, 0, ERTS_SIG_Q_OP_MONITOR);
}

void
erts_proc_sig_send_group_leader(Process *c_p, Eterm to, Eterm gl, Eterm ref)
{
    int res;
    ErtsSigGroupLeader *sgl;
    Eterm *hp;
    Uint gl_sz, ref_sz, size;
    erts_aint_t init_flags = ERTS_SIG_GL_FLG_ACTIVE|ERTS_SIG_GL_FLG_RECEIVER;
    if (c_p)
        init_flags |= ERTS_SIG_GL_FLG_SENDER;

    ASSERT(c_p ? is_internal_ref(ref) : ref == NIL);

    gl_sz = is_immed(gl) ? 0 : size_object(gl);
    ref_sz = is_immed(ref) ? 0 : size_object(ref);
    
    size = sizeof(ErtsSigGroupLeader);

    size += (gl_sz + ref_sz - 1) * sizeof(Eterm);

    sgl = erts_alloc(ERTS_ALC_T_SIG_DATA, size);

    erts_atomic_init_nob(&sgl->flags, init_flags);

    ERTS_INIT_OFF_HEAP(&sgl->oh);

    hp = &sgl->heap[0];

    sgl->group_leader = is_immed(gl) ? gl : copy_struct(gl, gl_sz, &hp, &sgl->oh);
    sgl->reply_to = c_p ? c_p->common.id : NIL;
    sgl->ref = is_immed(ref) ? ref : copy_struct(ref, ref_sz, &hp, &sgl->oh);

    sgl->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_GROUP_LEADER,
                                             ERTS_SIG_Q_TYPE_UNDEFINED,
                                             0);

    res = proc_queue_signal(c_p ? &c_p->common : NULL, sgl->reply_to, to,
                            (ErtsSignal *)sgl, 0, ERTS_SIG_Q_OP_GROUP_LEADER);

    if (!res) {
        destroy_sig_group_leader(sgl);
    } else if (c_p) {
        erts_aint_t flags, rm_flags = ERTS_SIG_GL_FLG_SENDER;
        int prio_res = maybe_elevate_sig_handling_prio(c_p, -1, to);
        if (!prio_res)
            rm_flags |= ERTS_SIG_GL_FLG_ACTIVE;
        flags = erts_atomic_read_band_nob(&sgl->flags, ~rm_flags);
        if (!prio_res && (flags & ERTS_SIG_GL_FLG_ACTIVE))
            res = 0; /* We deactivated signal... */
        if ((flags & ~rm_flags) == 0)
            destroy_sig_group_leader(sgl);
    }

    if (!res && c_p)
        group_leader_reply(c_p, c_p->common.id, ref, 0);
}

int
erts_proc_sig_send_is_alive_request(Process *c_p, Eterm to, Eterm ref)
{
    ErlHeapFragment *hfrag;
    Uint hsz;
    Eterm *hp, *start_hp, ref_cpy, msg;
    ErlOffHeap *ohp;
    ErtsMessage *mp;
    ErtsIsAliveRequest *alive_req;

    ASSERT(is_internal_ordinary_ref(ref));

    hsz = ERTS_REF_THING_SIZE + 3 + sizeof(ErtsIsAliveRequest)/sizeof(Eterm);

    mp = erts_alloc_message(hsz, &hp);
    hfrag = &mp->hfrag;
    mp->next = NULL;
    ohp = &hfrag->off_heap;
    start_hp = hp;

    ref_cpy = STORE_NC(&hp, ohp, ref);
    msg = TUPLE2(hp, ref_cpy, am_false); /* default res 'false' */
    hp += 3;

    hfrag->used_size = hp - start_hp;

    alive_req = (ErtsIsAliveRequest *) (char *) hp;
    alive_req->message = msg;
    alive_req->requester = c_p->common.id;

    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_IS_ALIVE,
                                                  ERTS_SIG_Q_TYPE_UNDEFINED,
                                                  0);

    if (proc_queue_signal(&c_p->common, c_p->common.id, to,
                          (ErtsSignal *)mp, 0, ERTS_SIG_Q_OP_IS_ALIVE)) {
        (void) maybe_elevate_sig_handling_prio(c_p, -1, to);
        return !0;
    }
    else {
        /* It wasn't alive; reply to ourselves... */
        mp->next = NULL;
        mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
        erts_queue_message(c_p, ERTS_PROC_LOCK_MAIN, mp, msg, am_system);
        return 0;
    }
}

int
erts_proc_sig_send_process_info_request(Process *c_p,
                                        Eterm to,
                                        int *item_ix,
                                        Eterm *item_extra,
                                        int len,
                                        int flags,
                                        Uint reserve_size,
                                        Eterm ref)
{
    Uint size, item_ix_offs, extra_offs, extra_hfrag_offs,
        extra_hsz = 0, *extra_hszs = NULL;
    ErtsProcessInfoSig *pis;
    int res;

    if (item_extra) {
        int i;
        extra_hszs = erts_alloc(ERTS_ALC_T_TMP, sizeof(Uint)*len);
        for (i = 0; i < len; i++) {
            Eterm extra = item_extra[i];
            extra_hszs[i] = (is_non_value(extra) || is_immed(extra)
                             ? 0
                             : size_object(extra));
            extra_hsz += extra_hszs[i];
        }
    }

    size = ERTS_ALC_WORD_ALIGN_SIZE(sizeof(ErtsProcessInfoSig));
    item_ix_offs = size;
    size += ERTS_ALC_WORD_ALIGN_SIZE(len * sizeof(int));
    if (!item_extra) {
        extra_offs = 0;
        extra_hfrag_offs = 0;
    }
    else {
        extra_offs = size;
        size += ERTS_ALC_WORD_ALIGN_SIZE(len * sizeof(Eterm));
        extra_hfrag_offs = size;
        if (extra_hsz)
            size += ERTS_HEAP_FRAG_SIZE(extra_hsz);
    }
    pis = erts_alloc(ERTS_ALC_T_SIG_DATA, size);
    pis->item_ix = (int *) (((char *) pis) + item_ix_offs);
    if (!item_extra) {
        ASSERT(!extra_offs);
        pis->item_extra = NULL;
        pis->extra_hfrag = NULL;
    }
    else {
        int i;
        Eterm *extra_hp;
        ErlOffHeap *extra_off_heap;
#ifdef DEBUG
        Eterm *end_hp = NULL;
#endif
        ASSERT(extra_offs);
        pis->item_extra = (Eterm *) (((char *) pis) + extra_offs);
        if (!extra_hsz) {
            extra_hp = NULL;
            extra_off_heap = NULL;
            pis->extra_hfrag = NULL;
        }
        else {
            pis->extra_hfrag = (ErlHeapFragment *) (((char *) pis)
                                                    + extra_hfrag_offs);
            ERTS_INIT_HEAP_FRAG(pis->extra_hfrag, extra_hsz, extra_hsz);
            extra_hp = &pis->extra_hfrag->mem[0];
#ifdef DEBUG
            end_hp = extra_hp + extra_hsz;
#endif
            extra_off_heap = &pis->extra_hfrag->off_heap;
        }
        for (i = 0; i < len; i++) {
            if (!extra_hsz || !extra_hszs[i]) {
                pis->item_extra[i] = item_extra[i];
            }
            else {
                pis->item_extra[i] = copy_struct(item_extra[i], extra_hszs[i],
                                                 &extra_hp, extra_off_heap);
            }
        }
        ASSERT(extra_hp == end_hp);
        erts_free(ERTS_ALC_T_TMP, extra_hszs);
        reserve_size += sizeof(Eterm)*extra_hsz;
    }

    ASSERT(c_p);
    ASSERT(item_ix);
    ASSERT(len > 0);
    ASSERT(is_internal_ordinary_ref(ref));

    pis->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_PROCESS_INFO,
                                             0, 0);

    pis->requester = c_p->common.id;
    sys_memcpy((void *) &pis->oref_thing,
               (void *) internal_ref_val(ref),
               sizeof(ErtsORefThing));
    pis->ref = make_internal_ref((char *) &pis->oref_thing);
    pis->reserve_size = reserve_size;
    pis->len = len;
    pis->flags = flags;
    sys_memcpy((void *) &pis->item_ix[0],
               (void *) item_ix,
               sizeof(int)*len);

    res = proc_queue_signal(&c_p->common, c_p->common.id, to,
                            (ErtsSignal *)pis, 0, ERTS_SIG_Q_OP_PROCESS_INFO);
    if (res) {
        (void) maybe_elevate_sig_handling_prio(c_p, -1, to);
    } else {
        destroy_process_info_sig(pis);
    }

    return res;
}

void
erts_proc_sig_send_sync_suspend(Process *c_p, Eterm to, Eterm tag, Eterm reply)
{
    ErlHeapFragment *hfrag;
    Uint hsz, tag_sz;
    Eterm *hp, *start_hp, tag_cpy, msg, default_reply;
    ErlOffHeap *ohp;
    ErtsMessage *mp;
    ErtsSyncSuspendRequest *ssusp;
    int async_suspend;

    tag_sz = size_object(tag);

    hsz = 3 + tag_sz + sizeof(ErtsSyncSuspendRequest)/sizeof(Eterm);

    mp = erts_alloc_message(hsz, &hp);
    hfrag = &mp->hfrag;
    mp->next = NULL;
    ohp = &hfrag->off_heap;
    start_hp = hp;

    tag_cpy = copy_struct(tag, tag_sz, &hp, ohp);

    async_suspend = is_non_value(reply);
    default_reply = async_suspend ? am_suspended : reply;

    msg = TUPLE2(hp, tag_cpy, default_reply);
    hp += 3;

    hfrag->used_size = hp - start_hp;

    ssusp = (ErtsSyncSuspendRequest *) (char *) hp;
    ssusp->message = msg;
    ssusp->requester = c_p->common.id;
    ssusp->async = async_suspend;

    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_SYNC_SUSPEND,
                                                  ERTS_SIG_Q_TYPE_UNDEFINED,
                                                  0);

    if (proc_queue_signal(&c_p->common, c_p->common.id, to,
                          (ErtsSignal *)mp, 0, ERTS_SIG_Q_OP_SYNC_SUSPEND)) {
        (void) maybe_elevate_sig_handling_prio(c_p, -1, to);
    } else {
        Eterm *tp;
        /* It wasn't alive; reply to ourselves... */
        mp->next = NULL;
        mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
        tp = tuple_val(msg);
        tp[2] = async_suspend ? am_badarg : am_exited;
        erts_queue_message(c_p, ERTS_PROC_LOCK_MAIN,
                           mp, msg, am_system);
    }
}

int
erts_proc_sig_send_dist_spawn_reply(Eterm node,
                                    Eterm ref,
                                    Eterm to,
                                    ErtsLink *lnk,
                                    Eterm result,
                                    Eterm token)
{
    Uint hsz, ref_sz, result_sz, token_sz;
    ErtsDistSpawnReplySigData *datap;
    Eterm msg, ref_copy, result_copy, res_type,
        token_copy, *hp, *hp_start, *patch_point;
    ErlHeapFragment *hfrag;
    ErlOffHeap *ohp;
    ErtsMessage *mp;
    Eterm ordered_from;
    int force_flush, has_token = !is_nil(token);

    ASSERT(is_atom(node));

    /*
     * A response message to a spawn_request() operation
     * looks like this:
     *    {Tag, Ref, ok|error, Pid|ErrorAtom}
     *
     * Tag is stored in its own heap fragment in the
     * (pending) monitor struct and can be attached
     * when creating the resulting message on
     * reception of this signal.
     */
    
    hsz = ref_sz = size_object(ref);
    hsz += 5 /* 4-tuple */;
    if (is_atom(result)) {
        res_type = am_error;
        result_sz = 0;
    }
    else {
        ASSERT(is_external_pid(result));
        res_type = am_ok;
        result_sz = size_object(result);
        hsz += result_sz;
    }

    if (has_token) {
        token_sz = is_immed(token) ? 0 : size_object(token);
        hsz += token_sz;
        hsz += sizeof(ErtsDistSeqTokenSpawnReplySigData)/sizeof(Eterm);
    }
    else {
        token_sz = 0;
        hsz += sizeof(ErtsDistSpawnReplySigData)/sizeof(Eterm);
    }

    mp = erts_alloc_message(hsz, &hp);
    hp_start = hp;
    hfrag = &mp->hfrag;
    mp->next = NULL;
    ohp = &hfrag->off_heap;

    ref_copy = copy_struct(ref, ref_sz, &hp, ohp);
    result_copy = (is_atom(result)
                   ? result
                   : copy_struct(result, result_sz, &hp, ohp));
    msg = TUPLE4(hp,
                 am_undefined,
                 ref_copy,
                 res_type,
                 result_copy);

    patch_point = &hp[1];
    ASSERT(*patch_point == am_undefined);
    
    hp += 5;

    token_copy = (!token_sz
                  ? token
                  : copy_struct(token, token_sz, &hp, ohp));
    
    hfrag->used_size = hp - hp_start;

    datap = (ErtsDistSpawnReplySigData *) (char *) hp;
    datap->message = msg;
    datap->ref = ref_copy;
    datap->result = result_copy;
    datap->link = lnk;
    datap->patch_point = patch_point;
    if (has_token)
        ((ErtsDistSeqTokenSpawnReplySigData *) datap)->token = token_copy;

    ERL_MESSAGE_TERM(mp) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_DIST_SPAWN_REPLY,
                                                  ERTS_SIG_Q_TYPE_UNDEFINED,
                                                  has_token);
    ERL_MESSAGE_FROM(mp) = node;

    /*
     * Sent from spawn-service at node, but we need to order this
     * signal against signals sent from the spawned process, so
     * we need to pass the pid of the spawned process as from
     * parameter or flush if connection was lost...
     */
    if (is_external_pid(result)) {
        force_flush = 0;
        ordered_from = result;
    }
    else {
        force_flush = result == am_noconnection;
        ordered_from = am_spawn_service;
    }
    if (!proc_queue_signal(NULL, ordered_from, to, (ErtsSignal *)mp,
                           force_flush, ERTS_SIG_Q_OP_DIST_SPAWN_REPLY)) {
        mp->next = NULL;
        mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
        ERL_MESSAGE_TERM(mp) = msg;
        erts_cleanup_messages(mp);
        return 0;
    }

    return !0;
}

Eterm
erts_proc_sig_send_rpc_request(Process *c_p,
                               Eterm to,
                               int reply,
                               Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                               void *arg)
{
    return erts_proc_sig_send_rpc_request_prio(c_p, to, reply, func, arg, -1);
}

Eterm
erts_proc_sig_send_rpc_request_prio(Process *c_p,
                                    Eterm to,
                                    int reply,
                                    Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                                    void *arg,
                                    int prio)
{
    Eterm res, from;
    ErtsPTabElementCommon *sender;
    ErtsProcSigRPC *sig = erts_alloc(ERTS_ALC_T_SIG_DATA,
                                     sizeof(ErtsProcSigRPC));
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_RPC,
                                             ERTS_SIG_Q_TYPE_UNDEFINED,
                                             0);
    sig->requester = reply ? c_p->common.id : NIL;
    sig->func = func;
    sig->arg = arg;

    if (!reply) {
        res = am_ok;
        sig->ref = am_ok;
    }
    else {
        res = erts_make_ref(c_p);

        sys_memcpy((void *) &sig->oref_thing,
                   (void *) internal_ref_val(res),
                   sizeof(ErtsORefThing));

        sig->ref = make_internal_ref(&sig->oref_thing);

        erts_msgq_set_save_end(c_p);
    }

    if (c_p) {
        sender = &c_p->common;
        from = c_p->common.id;
    }
    else {
        sender = NULL;
        from = am_system;
    }

    if (proc_queue_signal(sender, from, to, (ErtsSignal *)sig,
                          0, ERTS_SIG_Q_OP_RPC)) {
        (void) maybe_elevate_sig_handling_prio(c_p, prio, to);
    } else {
        erts_free(ERTS_ALC_T_SIG_DATA, sig);
        res = THE_NON_VALUE;
        if (reply)
            erts_msgq_set_save_first(c_p);
    }

    return res;
}


void
erts_proc_sig_send_cla_request(Process *c_p, Eterm to, Eterm req_id)
{
    ErtsMessage *sig;
    ErlHeapFragment *hfrag;
    ErlOffHeap *ohp;
    Eterm req_id_cpy, *hp, *start_hp;
    Uint hsz, req_id_sz;
    ErtsCLAData *cla;

    hsz = sizeof(ErtsCLAData)/sizeof(Uint);
    if (hsz < 4) {
        /*
         * Need room to overwrite the ErtsCLAData part with a
         * 3-tuple when reusing the signal for the reply...
         */
        hsz = 4;
    }

    req_id_sz = size_object(req_id);
    hsz += req_id_sz;

    sig = erts_alloc_message(hsz, &hp);
    hfrag = &sig->hfrag;
    sig->next = NULL;
    ohp = &hfrag->off_heap;
    start_hp = hp;

    req_id_cpy = copy_struct(req_id, req_id_sz, &hp, ohp);

    cla = (ErtsCLAData *) (char *) hp;
    hfrag->used_size = hp - start_hp;

    cla->yield = NULL;
    cla->requester = c_p->common.id;
    cla->request_id = req_id_cpy;

    ERL_MESSAGE_TERM(sig) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ADJ_MSGQ,
                                                   ERTS_SIG_Q_TYPE_CLA,
                                                   0);
    ERL_MESSAGE_FROM(sig) = c_p->common.id;
#ifdef USE_VM_PROBES
    ERL_MESSAGE_DT_UTAG(sig) = NIL;
#endif

    if (!proc_queue_signal(&c_p->common, c_p->common.id, to, (ErtsSignal *)sig,
                           0, ERTS_SIG_Q_OP_ADJ_MSGQ)) {
        send_cla_reply(c_p, sig, c_p->common.id, req_id_cpy, am_ok);
    }
}

void
erts_proc_sig_send_move_msgq_off_heap(Eterm to)
{
    ErtsMessage *sig;
    Eterm *hp;
    Uint hsz;
    ErtsAdjOffHeapMsgQData *ohdp;
    ASSERT(is_internal_pid(to));

    hsz = sizeof(ErtsAdjOffHeapMsgQData)/sizeof(Uint);
    sig = erts_alloc_message(hsz, &hp);

    ohdp = (ErtsAdjOffHeapMsgQData *) (char *) hp;
    ohdp->yield = NULL;

    sig->hfrag.used_size = 0;

    ERL_MESSAGE_TERM(sig) = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ADJ_MSGQ,
                                                   ERTS_SIG_Q_TYPE_OFF_HEAP,
                                                   0);
    ERL_MESSAGE_FROM(sig) = to;
    ERL_MESSAGE_TOKEN(sig) = am_undefined;
#ifdef USE_VM_PROBES
    ERL_MESSAGE_DT_UTAG(sig) = NIL;
#endif

    if (!proc_queue_signal(NULL, am_system, to, (ErtsSignal *)sig, 0,
                           ERTS_SIG_Q_OP_ADJ_MSGQ)) {
        sig->next = NULL;
        erts_cleanup_messages(sig);
    }
}

void
erts_proc_sig_init_flush_signals(Process *c_p, int flags, Eterm id)
{
    int force_flush_buffers = 0;
    ErtsSignal *sig;

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    ASSERT(!(c_p->sig_qs.flags & (FS_FLUSHING_SIGS|FS_FLUSHED_SIGS)));
    ASSERT(flags);
    ASSERT((flags & ~ERTS_PROC_SIG_FLUSH_FLGS) == 0);
    ASSERT(!(flags & ERTS_PROC_SIG_FLUSH_FLG_FROM_ID)
           || is_internal_pid(id) || is_internal_port(id));
    
    sig = erts_alloc(ERTS_ALC_T_SIG_DATA, sizeof(ErtsNonMsgSignal));
    sig->common.next = NULL;
    sig->common.specific.attachment = NULL;
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_FLUSH,
					     ERTS_SIG_Q_TYPE_UNDEFINED,
					     0);
    switch (flags) {
    case ERTS_PROC_SIG_FLUSH_FLG_FROM_ALL:
        id = c_p->common.id;
        force_flush_buffers = !0;
        /* Fall through... */
    case ERTS_PROC_SIG_FLUSH_FLG_FROM_ID:
        if (!proc_queue_signal(NULL, id, c_p->common.id, sig,
                               force_flush_buffers, ERTS_SIG_Q_OP_FLUSH))
            ERTS_INTERNAL_ERROR("Failed to send flush signal to ourselves");
        break;
    default:
        break;
    }

    erts_set_gc_state(c_p, 0);

    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
    erts_proc_sig_fetch(c_p);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

    c_p->sig_qs.flags |= FS_FLUSHING_SIGS;

}

static int
handle_rpc(Process *c_p, ErtsProcSigRPC *rpc, int cnt, int limit, int *yieldp)
{
    Process *rp;
    ErlHeapFragment *bp = NULL;
    Eterm res;
    Uint hsz;
    int reds, out_cnt;

    /*
     * reds in:
     *  Reductions left.
     *
     * reds out:
     *  Absolute value of reds out equals consumed
     *  amount of reds. If a negative value, force
     *  a yield.
     */

    reds = (limit - cnt) / ERTS_SIG_REDS_CNT_FACTOR;
    if (reds <= 0)
        reds = 1;

    res = (*rpc->func)(c_p, rpc->arg, &reds, &bp);

    if (reds < 0) {
        /* Force yield... */
        *yieldp = !0;
        reds *= -1;
    }

    out_cnt = reds*ERTS_SIG_REDS_CNT_FACTOR;

    hsz = 3 + sizeof(ErtsORefThing)/sizeof(Eterm);

    rp = erts_proc_lookup(rpc->requester);
    if (!rp) {
        if (bp)
            free_message_buffer(bp);
    }
    else {
        Eterm *hp, msg, ref;
        ErtsMessage *mp = erts_alloc_message(hsz, &hp);
        
        sys_memcpy((void *) hp, (void *) &rpc->oref_thing,
                   sizeof(rpc->oref_thing));

        ref = make_internal_ref(hp);
        hp += sizeof(rpc->oref_thing)/sizeof(Eterm);
        msg = TUPLE2(hp, ref, res);

        mp->hfrag.next = bp;
        ERL_MESSAGE_TOKEN(mp) = am_undefined;
        erts_queue_proc_message(c_p, rp, 0, mp, msg);
    }

    erts_free(ERTS_ALC_T_SIG_DATA, rpc);

    return out_cnt;
}

static void
is_alive_response(Process *c_p, ErtsMessage *mp, int is_alive)
{
    /*
     * Sender prepared the message for us. Just patch
     * the result if necessary. The default prepared
     * result is 'false'.
     */
    Process *rp;
    ErtsIsAliveRequest *alive_req;

    alive_req = (ErtsIsAliveRequest *) (char *) (&mp->hfrag.mem[0]
                                                 + mp->hfrag.used_size);


    ASSERT(ERTS_SIG_IS_NON_MSG(mp));
    ASSERT(ERTS_PROC_SIG_OP(((ErtsSignal *) mp)->common.tag)
           == ERTS_SIG_Q_OP_IS_ALIVE);
    ASSERT(mp->hfrag.alloc_size > mp->hfrag.used_size);
    ASSERT((mp->hfrag.alloc_size - mp->hfrag.used_size)*sizeof(UWord)
           >= sizeof(ErtsIsAliveRequest));
    ASSERT(is_internal_pid(alive_req->requester));
    ASSERT(alive_req->requester != c_p->common.id);
    ASSERT(is_tuple_arity(alive_req->message, 2));
    ASSERT(is_internal_ordinary_ref(tuple_val(alive_req->message)[1]));
    ASSERT(tuple_val(alive_req->message)[2] == am_false);

    ERL_MESSAGE_TERM(mp) = alive_req->message;
    mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
    mp->next = NULL;

    rp = erts_proc_lookup(alive_req->requester);
    if (!rp)
        erts_cleanup_messages(mp);
    else {
        if (is_alive) { /* patch result... */
            Eterm *tp = tuple_val(alive_req->message);
            tp[2] = am_true;
        }
        erts_queue_message(rp, 0, mp, alive_req->message, am_system);
    }
}


static ERTS_INLINE void
adjust_tracing_state(Process *c_p, ErtsSigRecvTracing *tracing, int setup)
{
    const Uint flgs = ERTS_P_ALL_TRACE_FLAGS(c_p);

    if (!(flgs & (F_TRACE_PROCS | F_TRACE_RECEIVE))) {
        tracing->messages.active = 0;
        tracing->messages.receive_trace = 0;
        tracing->messages.next = NULL;
        tracing->procs = 0;
        tracing->active = 0;
    }
    else {
        int procs_trace = !!(flgs & F_TRACE_PROCS);
        int recv_trace = !!(flgs & F_TRACE_RECEIVE);
        /* procs tracing enabled? */

        tracing->procs = procs_trace;

        /* message receive tracing enabled? */
        tracing->messages.receive_trace = recv_trace;
        if (recv_trace) {
            if (tracing->messages.bp_ix < 0)
                tracing->messages.bp_ix = erts_active_bp_ix();
        }
        if (setup) {
            if (recv_trace)
                tracing->messages.next = &c_p->sig_qs.cont;
            else
                tracing->messages.next = NULL;
        }
        tracing->messages.active = recv_trace;
        tracing->active = recv_trace | procs_trace;
    }

#if defined(USE_VM_PROBES)
    /* vm probe message_queued enabled? */

    tracing->messages.vm_probes = DTRACE_ENABLED(message_queued);
    if (tracing->messages.vm_probes) {
        dtrace_proc_str(c_p, tracing->messages.receiver_name);
        tracing->messages.active = !0;
        tracing->active = !0;
        if (setup && !tracing->messages.next)
            tracing->messages.next = &c_p->sig_qs.cont;
    }

#endif
}

static ERTS_INLINE void
setup_tracing_state(Process *c_p, ErtsSigRecvTracing *tracing)
{
    tracing->messages.bp_ix = -1;
    adjust_tracing_state(c_p, tracing, !0);
}

static ERTS_INLINE void
remove_innerq_sig(Process *c_p, ErtsMessage *sig, ErtsMessage **next_sig)
{
    /*
     * Remove signal from message queue (inner queue).
     */
    ASSERT(c_p->sig_qs.cont_last != &sig->next);
    ASSERT(c_p->sig_qs.nmsigs.next != &sig->next);
    ASSERT(c_p->sig_qs.nmsigs.last != &sig->next);

    if (c_p->sig_qs.save == &sig->next)
        c_p->sig_qs.save = next_sig;
    if (c_p->sig_qs.last == &sig->next)
        c_p->sig_qs.last = next_sig;
    if (sig->next && ERTS_SIG_IS_RECV_MARKER(sig->next))
        ((ErtsRecvMarker *) sig->next)->prev_next = next_sig;
    *next_sig = sig->next;
}

static ERTS_INLINE void
remove_middleq_sig(Process *c_p, ErtsMessage *sig,
                   ErtsMessage **next_sig, ErtsMessage ***next_nm_sig)
{
    /*
     * Remove signal from (middle) signal queue.
     */
    ASSERT(c_p->sig_qs.save != &sig->next);
    ASSERT(c_p->sig_qs.last != &sig->next);

    if (c_p->sig_qs.cont_last == &sig->next)
	c_p->sig_qs.cont_last = next_sig;
    if (*next_nm_sig == &sig->next)
        *next_nm_sig = next_sig;
    if (c_p->sig_qs.nmsigs.last == &sig->next)
        c_p->sig_qs.nmsigs.last = next_sig;

    *next_sig = sig->next;
}

static ERTS_INLINE void
remove_nm_sig(Process *c_p, ErtsMessage *sig, ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig = *next_nm_sig;
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    ASSERT(*next_sig == sig);
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    remove_middleq_sig(c_p, sig, next_sig, next_nm_sig);
}

static ERTS_INLINE void
inc_converted_msgs_len(Process *c_p,
                       ErtsSigRecvTracing *tracing,
                       ErtsMessage ***next_nm_sig,
                       Sint len)
{
    ASSERT(len > 0);
    if (!tracing->messages.active) {
        /* The converted messages were moved into the message queue... */
        c_p->sig_qs.mq_len += len;
        erts_chk_sys_mon_long_msgq_on(c_p);
    }
    else {
        /*
         * The converted messages will be moved into the message queue
         * when traced...
         */
        if (!*next_nm_sig) {
            c_p->sig_qs.mlenoffs += len;
        }
       else {
            ErtsNonMsgSignal *nmsig = (ErtsNonMsgSignal *) **next_nm_sig;
            ASSERT(nmsig);
            nmsig->mlenoffs += len;
        }
    }
}

static ERTS_INLINE void
convert_to_msg(Process *c_p, ErtsSigRecvTracing *tracing, ErtsMessage *sig,
               ErtsMessage *msg, ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig = *next_nm_sig;
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    *next_sig = msg;
    remove_middleq_sig(c_p, sig, &msg->next, next_nm_sig);
    inc_converted_msgs_len(c_p, tracing, next_nm_sig, 1);
}

static ERTS_INLINE void
convert_to_msgs(Process *c_p, ErtsSigRecvTracing *tracing, ErtsMessage *sig,
                Uint no_msgs, ErtsMessage *first_msg, ErtsMessage *last_msg,
                ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig = *next_nm_sig;
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    *next_sig = first_msg;
    remove_middleq_sig(c_p, sig, &last_msg->next, next_nm_sig);
    inc_converted_msgs_len(c_p, tracing, next_nm_sig, no_msgs);
}

static ERTS_INLINE void
insert_messages(Process *c_p, ErtsSigRecvTracing *tracing, ErtsMessage **next,
                ErtsMessage *first, ErtsMessage *last, Sint no_msgs,
                ErtsMessage ***next_nm_sig)
{
    last->next = *next;
    if (c_p->sig_qs.cont_last == next)
	c_p->sig_qs.cont_last = &last->next;
    if (*next_nm_sig == next)
        *next_nm_sig = &last->next;
    if (c_p->sig_qs.nmsigs.last == next)
        c_p->sig_qs.nmsigs.last = &last->next;
    inc_converted_msgs_len(c_p, tracing, next_nm_sig, no_msgs);
    *next = first;
}

static ERTS_INLINE void
remove_innerq_m_sig(Process *c_p, ErtsMessage *sig, ErtsMessage **next_sig)
{
    /* Removing message... */
    ASSERT(!ERTS_SIG_IS_NON_MSG(sig));
    c_p->sig_qs.mq_len--;
    ASSERT(c_p->sig_qs.mq_len >= 0);
    erts_chk_sys_mon_long_msgq_off(c_p);
    remove_innerq_sig(c_p, sig, next_sig);
}

static ERTS_INLINE void
convert_prepared_sig_to_msg_attached(Process *c_p, ErtsSigRecvTracing *tracing,
                                     ErtsMessage *sig, Eterm msg,
                                     Eterm token, void *data_attached,
                                     ErtsMessage ***next_nm_sig)
{
    /*
     * Everything is already there except for the reference to
     * the message and the combined hfrag marker that needs to be
     * restored...
     */
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    sig->data.attached = data_attached;
    ERL_MESSAGE_TERM(sig) = msg;
    ERL_MESSAGE_TOKEN(sig) = token;
    inc_converted_msgs_len(c_p, tracing, next_nm_sig, 1);
}

static ERTS_INLINE void
convert_prepared_sig_to_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                            ErtsMessage *sig, Eterm msg,
                            Eterm token, ErtsMessage ***next_nm_sig)
{
    convert_prepared_sig_to_msg_attached(c_p, tracing, sig, msg, token,
                                         ERTS_MSG_COMBINED_HFRAG,
                                         next_nm_sig);
}

static ERTS_INLINE void
convert_prepared_sig_to_external_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                                     ErtsMessage *sig, Eterm token,
                                     ErtsMessage ***next_nm_sig)
{
    /*
     * Everything is already there except for the reference to
     * the message and the combined hfrag marker that needs to be
     * restored...
     */
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    sig->data.attached = &sig->hfrag; /* <- will be adjusted to
                                       * ERTS_MSG_COMBINED_HFRAG
                                       * when message is decoded */
    ERL_MESSAGE_TERM(sig) = THE_NON_VALUE;
    ERL_MESSAGE_TOKEN(sig) = token;
    inc_converted_msgs_len(c_p, tracing, next_nm_sig, 1);
}

static ERTS_INLINE Eterm
get_heap_frag_eterm(ErlHeapFragment **hfpp, Eterm *valp)
{
    Eterm term;
    ErlHeapFragment *hfp;
    ASSERT(hfpp);
    if (is_immed(*valp)) {
        *hfpp = NULL;
        term = *valp;
    }
    else {
        ASSERT(is_CP(*valp));
        *hfpp = hfp = (ErlHeapFragment *) cp_val(*valp);
        ASSERT(hfp->alloc_size == hfp->used_size + 1);
        term = hfp->mem[hfp->used_size];
        ASSERT(size_object(term) == hfp->used_size);
    }
    *valp = NIL;
    return term;
}

static ERTS_INLINE Eterm
save_heap_frag_eterm(Process *c_p, ErtsMessage *mp, Eterm *value)
{
    ErlHeapFragment *hfrag;
    Eterm term = get_heap_frag_eterm(&hfrag, value);
    if (hfrag) {
        if (mp->data.attached == ERTS_MSG_COMBINED_HFRAG) {
            hfrag->next = mp->hfrag.next;
            mp->hfrag.next = hfrag;
        }
        else if (!mp->data.heap_frag) {
            erts_link_mbuf_to_proc(c_p, hfrag);
        }
        else {
            hfrag->next = mp->data.heap_frag;
            mp->data.heap_frag = hfrag;
        }
    }
    return term;
}

static ERTS_INLINE Eterm
copy_heap_frag_eterm(Process *c_p, ErtsMessage *mp, Eterm value)
{
    ErlHeapFragment *hfrag;
    Eterm *hp, term_sz, term, term_cpy, val;
    val = value;
    term = get_heap_frag_eterm(&hfrag, &val);
    if (!hfrag)
        return term;
    term_sz = hfrag->used_size;
    if (!mp->data.attached) {
        hp = HAlloc(c_p, term_sz);
        term_cpy = copy_struct(term, term_sz, &hp, &c_p->off_heap);
    }
    else {
        ErlHeapFragment *hfrag_cpy = new_message_buffer(term_sz);
        hp = &hfrag_cpy->mem[0];
        term_cpy = copy_struct(term, term_sz, &hp, &hfrag_cpy->off_heap);
        if (mp->data.attached == ERTS_MSG_COMBINED_HFRAG) {
            hfrag_cpy->next = mp->hfrag.next;
            mp->hfrag.next = hfrag_cpy;
        }
        else {
            ASSERT(mp->data.heap_frag);
            hfrag_cpy->next = mp->data.heap_frag;
            mp->data.heap_frag = hfrag_cpy;
        }
    }
    return term_cpy;
}

/*
 * Receive markers
 */

#if defined(DEBUG) || defined(ERTS_PROC_SIG_HARD_DEBUG)

#define ERTS_SIG_RECV_MARK_HANDLED ((void *) 4711)

#define ERTS_SIG_DBG_IS_HANDLED_RECV_MARKER(S)			\
    (ERTS_SIG_IS_RECV_MARKER((S))				\
     && (((ErtsSignal *) (S))->common.specific.attachment	\
	 == ERTS_SIG_RECV_MARK_HANDLED))
#define ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(S)			\
    (((ErtsSignal *) (S))->common.specific.attachment		\
     = ERTS_SIG_RECV_MARK_HANDLED)

#else

#define ERTS_SIG_DBG_IS_HANDLED_RECV_MARKER(S) 0
#define ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(S)

#endif

static ERTS_INLINE void
recv_marker_deallocate(Process *c_p, ErtsRecvMarker *markp)
{
    ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;
    int ix, nix;

    ASSERT(markp->mark_type == ERTS_RECV_MARKER_TYPE_RECV);
    
    ASSERT(blkp);
    ERTS_HDBG_CHK_RECV_MRKS(c_p);

    nix = markp->next_ix;
    ASSERT(nix >= 0);

    ix = ERTS_RECV_MARKER_IX__(blkp, markp);

    if (nix == ix) {
	ASSERT(markp->prev_ix == ix);
	erts_free(ERTS_ALC_T_RECV_MARK_BLK, blkp);
	c_p->sig_qs.recv_mrk_blk = NULL;
    }
    else {
	int pix = markp->prev_ix;
	ASSERT(pix >= 0);

	if (blkp->ref[ix] == am_undefined) {
	    ASSERT(blkp->unused > 0);
	    blkp->unused--;
	}

	blkp->marker[pix].next_ix = nix;
	blkp->marker[nix].prev_ix = pix;

	if (blkp->used_ix == ix)
	    blkp->used_ix = nix;

	blkp->marker[ix].next_ix = blkp->free_ix;
	blkp->free_ix = ix;
	blkp->ref[ix] = am_free;
#ifdef DEBUG
	markp->used = 0;
#endif

	ERTS_HDBG_CHK_RECV_MRKS(c_p);
    }
}

static ERTS_INLINE void
recv_marker_dequeue(Process *c_p, ErtsRecvMarker *markp)
{
    ErtsMessage *sigp;

    ASSERT(markp->mark_type == ERTS_RECV_MARKER_TYPE_RECV);
    ASSERT(markp->proc == c_p);

    if (markp->in_sigq <= 0) {
        /* Not in signal queue or marked for removal... */
        return;
    }
    
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    sigp = (ErtsMessage *) markp;

    ASSERT(ERTS_SIG_IS_RECV_MARKER(sigp));
    ASSERT(!markp->in_msgq || markp->prev_next);

    if (!markp->in_msgq) {
        markp->in_sigq = -1; /* Mark for removal... */
	markp->set_save = 0;
    }
    else {
        remove_innerq_sig(c_p, sigp, markp->prev_next);
        markp->in_prioq = markp->in_msgq = markp->in_sigq = 0;
	ASSERT(!markp->set_save);
#ifdef DEBUG
        markp->prev_next = NULL;
#endif
	recv_marker_deallocate(c_p, markp);
    }

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
}


static ERTS_INLINE Eterm
recv_marker_uniq(Process *c_p, Eterm *uniqp)
{
    Eterm res = *uniqp;
    if (res == am_new_uniq) {
	Sint64 val = MIN_SMALL + c_p->uniq++;
	Uint hsz = ERTS_SINT64_HEAP_SIZE(val);
	if (hsz == 0)
	    res = make_small((Sint) val);
	else {
	    Eterm *hp = HAlloc(c_p, hsz);
	    res = erts_sint64_to_big(val, &hp);
	}
	*uniqp = res;
    }
    return res;
}

static ERTS_INLINE ErtsRecvMarker *
recv_marker_alloc_block(Process *c_p, ErtsRecvMarkerBlock **blkpp,
			int *ixp, Eterm *uniqp)
{
    ErtsRecvMarkerBlock *blkp;
    ErtsRecvMarker *markp;
    int ix;

    blkp = (ErtsRecvMarkerBlock *) erts_alloc(ERTS_ALC_T_RECV_MARK_BLK,
					      sizeof(ErtsRecvMarkerBlock));
    *blkpp = blkp;

    /* Allocate marker for 'uniqp' in index zero... */    
    *ixp = 0;
    blkp->ref[0] = recv_marker_uniq(c_p, uniqp);
    blkp->marker[0].mark_type = ERTS_RECV_MARKER_TYPE_RECV;
    markp = &blkp->marker[0];
    markp->next_ix = markp->prev_ix = 0;
    blkp->used_ix = 0;

    /* Put the rest in a free list in the ref words... */
    blkp->free_ix = 1;
    for (ix = 1; ix < ERTS_RECV_MARKER_BLOCK_SIZE; ix++) {
	blkp->ref[ix] = am_free;
        blkp->marker[ix].mark_type = ERTS_RECV_MARKER_TYPE_RECV;
	if (ix == ERTS_RECV_MARKER_BLOCK_SIZE - 1)
	    blkp->marker[ix].next_ix = -1; /* End of list */
	else
	    blkp->marker[ix].next_ix = ix + 1;
    }

    blkp->unused = 0;
    blkp->pending_set_save_ix = -1;

#ifdef DEBUG
    for (ix = 0; ix < ERTS_RECV_MARKER_BLOCK_SIZE; ix++) {
	blkp->marker[ix].used = ix == 0 ? !0 : 0;
	blkp->marker[ix].proc = c_p;
    }	
#endif

    ERTS_HDBG_CHK_RECV_MRKS(c_p);

    return markp;
}

static ERTS_INLINE ErtsRecvMarker *
recv_marker_reuse(Process *c_p, int *ixp)
{
    /*
     * All markers used; reuse the least recently
     * allocated one...
     */
    ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;
    ErtsRecvMarker *markp;
    ErtsMessage *sigp;
    int ix, used_ix;

    /*
     * 'used_ix' points to the least recently
     * allocated marker. We reuse least recently
     * and preferably unused marker.
     *
     * In order to reuse a marker it needs to
     * be in the message queue. We search from the
     * least recently allocated towards the most
     * recently allocated. Once we find a marker
     * not in the message queue, i.e, in the middle
     * signal queue, we know that the rest cannot
     * be in the middle queue either.
     */

    used_ix = blkp->used_ix;
    markp = &blkp->marker[used_ix];
    if (!markp->in_msgq)
	return NULL;
    if (!blkp->unused || blkp->ref[used_ix] == am_undefined) {
    use_least_recently_allocated:
	if (blkp->ref[used_ix] == am_undefined)
	    blkp->unused--;
	ix = used_ix;
	blkp->used_ix = used_ix = markp->next_ix;
    }
    else {
	int pix, nix;

	ix = markp->next_ix;
	ASSERT(ix != used_ix);
	while (!0) {
	    markp = &blkp->marker[ix];
	    if (!markp->in_msgq)
		goto use_least_recently_allocated;
	    if (blkp->ref[ix] == am_undefined) {
		/* use this one... */
		ASSERT(blkp->unused > 0);
		blkp->unused--;
		break;
	    }
	    ix = markp->next_ix;
	    ASSERT(ix != used_ix);
	}
	/*
	 * Move this marker to be most recently
	 * allocated marker (prev_ix of used_ix),
	 * so that the search property still holds...
	 */
	pix = markp->prev_ix;
	nix = markp->next_ix;
	blkp->marker[pix].next_ix = nix;
	blkp->marker[nix].prev_ix = pix;

	pix = blkp->marker[used_ix].prev_ix;
	blkp->marker[used_ix].prev_ix = ix;
	blkp->marker[pix].next_ix = ix;
	markp->next_ix = used_ix;
	markp->prev_ix = pix;
    }

    *ixp = ix;

    ASSERT(markp->in_sigq);
    ASSERT(markp->in_msgq);
    ASSERT(!markp->set_save);	

    sigp = (ErtsMessage *) markp;

    ASSERT(ERTS_SIG_IS_RECV_MARKER(sigp));

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    remove_innerq_sig(c_p, sigp, markp->prev_next);
    markp->in_prioq = markp->in_msgq = markp->in_sigq = 0;
#ifdef DEBUG
    markp->prev_next = NULL;
#endif

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    return markp;
}

static ERTS_INLINE ErtsRecvMarker *
recv_marker_alloc(Process *c_p, ErtsRecvMarkerBlock **blkpp,
		  int *ixp, Eterm *uniqp)
{
    ErtsRecvMarkerBlock *blkp = *blkpp;
    ErtsRecvMarker *markp;
    int ix;

    ASSERT(is_small(*uniqp) || is_big(*uniqp) || *uniqp == am_new_uniq
	   || *uniqp == NIL || is_internal_ref(*uniqp));

    if (!blkp)
	return recv_marker_alloc_block(c_p, blkpp, ixp, uniqp);

    ERTS_HDBG_CHK_RECV_MRKS(c_p);

    ix = blkp->free_ix;
    if (ix < 0) {
	markp = recv_marker_reuse(c_p, &ix);
	if (!markp)
	    return NULL;
    }
    else {
	int used_ix = blkp->used_ix;
	ASSERT(blkp->ref[ix] == am_free);
	markp = &blkp->marker[ix];
	blkp->free_ix = markp->next_ix;
	ASSERT(-1 <= blkp->free_ix
	       && blkp->free_ix < ERTS_RECV_MARKER_BLOCK_SIZE); 
	markp->prev_ix = blkp->marker[used_ix].prev_ix;
	markp->next_ix = used_ix;
#ifdef DEBUG
	markp->used = !0;
#endif
	blkp->marker[markp->prev_ix].next_ix = ix;
	blkp->marker[used_ix].prev_ix = ix;
    }

    *ixp = ix;

    blkp->ref[ix] = recv_marker_uniq(c_p, uniqp);

    ERTS_HDBG_CHK_RECV_MRKS(c_p);

    return markp;
}

static ERTS_INLINE void
recv_marker_insert(Process *c_p, ErtsRecvMarker *markp, int setting)
{
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    markp->sig.common.next = NULL;
    markp->sig.common.specific.next = NULL;
    markp->sig.common.tag = ERTS_RECV_MARKER_TAG;

    ERTS_HDBG_PRIVQ_LEN(c_p);
    markp->pass = 0;
    markp->set_save = 0;
    markp->in_sigq = 1;
    markp->in_prioq = 0;
    if (!c_p->sig_qs.cont) {
        /* Insert in message queue... */
        markp->in_msgq = !0;
        ASSERT(c_p->sig_qs.first);
        markp->prev_next = c_p->sig_qs.last;
        *c_p->sig_qs.last = (ErtsMessage *) &markp->sig;
        c_p->sig_qs.last = &markp->sig.common.next;

        if (!setting && *c_p->sig_qs.save == (ErtsMessage *) &markp->sig) {
            /*
             * This can happen when a recv marker recently entered the message
             * queue via erts_proc_sig_handle_incoming() through the midddle
             * signal queue...
             */
            markp->pass++;
            c_p->sig_qs.save = c_p->sig_qs.last;
        }
        
	ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(&markp->sig);
    }
    else {
        /* Insert in (middle) signal queue... */
        markp->in_msgq = 0;
#ifdef DEBUG
        markp->prev_next = NULL;
#endif
        if (!c_p->sig_qs.nmsigs.last) {
            ASSERT(!c_p->sig_qs.nmsigs.next);
            c_p->sig_qs.nmsigs.next = c_p->sig_qs.cont_last;
        }
        else {
            ErtsSignal *lsig = (ErtsSignal *) *c_p->sig_qs.nmsigs.last;
            ASSERT(c_p->sig_qs.nmsigs.next);
            ASSERT(lsig && !lsig->common.specific.next);
            lsig->common.specific.next = c_p->sig_qs.cont_last;
        }

	c_p->sig_qs.nmsigs.last = c_p->sig_qs.cont_last;

        *c_p->sig_qs.cont_last = (ErtsMessage *) &markp->sig;
        c_p->sig_qs.cont_last = &markp->sig.common.next;
        markp->sig.nm_sig.mlenoffs = c_p->sig_qs.mlenoffs;
        c_p->sig_qs.mlenoffs = 0;
    }
    ERTS_HDBG_PRIVQ_LEN(c_p);
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
}

Eterm
erts_msgq_recv_marker_create_insert(Process *c_p, Eterm uniq)
{
    int ix;
    Eterm new_uniq = uniq;
    ErtsRecvMarkerBlock **blkpp = &c_p->sig_qs.recv_mrk_blk;
    ErtsRecvMarker *markp = recv_marker_alloc(c_p, blkpp, &ix, &new_uniq);
    if (!markp)
	return am_undefined;
    recv_marker_insert(c_p, markp, 0);
    ASSERT(is_small(new_uniq) || is_big(new_uniq) || new_uniq == NIL
	   || is_internal_ref(new_uniq));
    return new_uniq;
}

void
erts_msgq_recv_marker_create_insert_set_save(Process *c_p, Eterm id)
{
    int ix = -1; /* Shut up faulty warning... */
    ErtsRecvMarkerBlock **blkpp = &c_p->sig_qs.recv_mrk_blk;
    ErtsRecvMarker *markp = recv_marker_alloc(c_p, blkpp, &ix, &id);

    if (markp) {
	recv_marker_insert(c_p, markp, !0);
	erts_msgq_recv_marker_set_save__(c_p, *blkpp, markp, ix);
	ASSERT(markp->in_sigq > 0);
	ASSERT(!markp->in_msgq);
	ASSERT(markp->set_save);
	ASSERT(ix >= 0);
	ASSERT((*blkpp)->pending_set_save_ix == ix);
	
	/*
	 * The save pointer will be set when the marker
	 * enters the message queue, and then the marker
	 * will immediately be removed...
	 */
	markp->in_sigq = -1;
    }
}

static ERTS_INLINE void
remove_prio_q_marker(Process *c_p, ErtsRecvMarker *markp)
{
    remove_innerq_sig(c_p, (ErtsMessage *) markp, markp->prev_next);
    markp->in_msgq = markp->in_sigq = 0;
}

static ErtsMessage **
msgq_pass_recv_markers_prioq_end(Process *c_p, ErtsMessage **markpp);

static ERTS_INLINE ErtsMessage **
msgq_pass_recv_markers(Process *c_p, ErtsMessage **markpp, int leading)
{
    ErtsMessage **sigpp = markpp;
    ErtsMessage *sigp = *sigpp;
    ASSERT(ERTS_SIG_IS_RECV_MARKER(sigp));
    do {
	ErtsRecvMarker *markp = (ErtsRecvMarker *) sigp;
        switch (markp->mark_type) {
        case ERTS_RECV_MARKER_TYPE_RECV:
            if (leading || ++markp->pass > ERTS_RECV_MARKER_PASS_MAX) {
                recv_marker_dequeue(c_p, markp);
                break;
            }
            /* Fall through... */
        case ERTS_RECV_MARKER_TYPE_YIELD:
        pass_it:
            sigpp = &markp->sig.common.next;
            break;
        case ERTS_RECV_MARKER_TYPE_PRIO_Q_END:
            ASSERT(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK);
            if (leading
                && ERTS_MQ_GET_SAVE_INFO(c_p) == FS_SET_SAVE_INFO_FIRST) {
                /* empty prio queue; remove end marker */
                sigpp = markp->prev_next;
                remove_prio_q_marker(c_p, markp);
                c_p->sig_qs.flags &= ~(FS_PRIO_MQ_SAVE|FS_PRIO_MQ_END_MARK);
            }
            else {
                c_p->sig_qs.flags &= ~FS_PRIO_MQ_SAVE;
                if (ERTS_MQ_GET_SAVE_INFO(c_p) != FS_SET_SAVE_INFO_FIRST)
                    return msgq_pass_recv_markers_prioq_end(c_p, sigpp);
                goto pass_it;
            }
        case ERTS_RECV_MARKER_TYPE_PRIO_Q_CONT:
            /* remove it */
            sigpp = markp->prev_next;
            remove_prio_q_marker(c_p, markp);
            break;
        default:
            ERTS_INTERNAL_ERROR("Invalid recv marker");
            break;
        }
        sigp = *sigpp;
    } while (sigp && ERTS_SIG_IS_RECV_MARKER(sigp));

    return sigpp;
}

static ErtsMessage **
msgq_pass_recv_markers_prioq_end(Process *c_p, ErtsMessage **pq_endpp)
{
    /*
     * We've reached the end of the prio queue and may need to move
     * the save pointer to some place else than the next message...
     */
    ErtsMessage **sigpp = pq_endpp;
    ErtsMessage *sigp = *sigpp;

    ASSERT(ERTS_SIG_IS_RECV_MARKER((ErtsRecvMarker *) sigp));
    ASSERT(((ErtsRecvMarker *) sigp)->mark_type
           == ERTS_RECV_MARKER_TYPE_PRIO_Q_END);
    ASSERT(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK);
    ASSERT(get_prio_queue_info(c_p));

    switch (ERTS_MQ_GET_SAVE_INFO(c_p)) {

    case FS_SET_SAVE_INFO_FIRST:
        sigpp = &sigp->next;
        break;

    case FS_SET_SAVE_INFO_LAST:
        /* 
         * The save pointer should always point past the end of the prio
         * queue when these are set.
         */
        ERTS_INTERNAL_ERROR("Invalid message queue state");
        break;

    case FS_SET_SAVE_INFO_MARK: {
        /* Continue at previously marked place... */
        ErtsPrioQInfo *pq_info = get_prio_queue_info(c_p);
        ErtsRecvMarker *pq_cont = &pq_info->marker[ERTS_PRIO_Q_MARK_CONT];

        ASSERT(pq_cont->mark_type == ERTS_RECV_MARKER_TYPE_PRIO_Q_CONT);
        ASSERT(pq_cont->in_msgq);

        ERTS_MQ_SET_SAVE_INFO(c_p, pq_info->saved_save_info);

        sigpp = pq_cont->prev_next;
        remove_prio_q_marker(c_p, pq_cont);
        break;
    }

    case FS_SET_SAVE_INFO_RCVM: {
        /* Continue at identified recv-marker... */
        ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;
        ErtsRecvMarker *markp;
        int ix = blkp->set_save_ix;

        ASSERT(blkp);

        if (ix < 0 || ERTS_RECV_MARKER_BLOCK_SIZE <= ix)
            ERTS_INTERNAL_ERROR("Invalid message queue state");

        markp = &blkp->marker[ix];

        ASSERT(markp->in_sigq);

        sigpp = (markp->in_msgq
                 ? &markp->sig.common.next
                 : erts_msgq_recv_marker_pending_set_save__(c_p, blkp,
                                                            markp, ix));
        break;
    }

    default:
        ERTS_INTERNAL_ERROR("Non-existing message queue state");
        break;
    }

    sigp = *sigpp;

    /* Pass possible receive markers... */

    while (sigp && ERTS_SIG_IS_RECV_MARKER(sigp)) {
	ErtsRecvMarker *markp = (ErtsRecvMarker *) sigp;
        switch (markp->mark_type) {
        case ERTS_RECV_MARKER_TYPE_RECV:
            if (++markp->pass > ERTS_RECV_MARKER_PASS_MAX) {
                recv_marker_dequeue(c_p, markp);
                break;
            }
            /* Fall through... */
        case ERTS_RECV_MARKER_TYPE_YIELD:
            sigpp = &markp->sig.common.next;
            break;
        case ERTS_RECV_MARKER_TYPE_PRIO_Q_END:
            ERTS_INTERNAL_ERROR("Unexpected prio msgq end marker");
            break;
        case ERTS_RECV_MARKER_TYPE_PRIO_Q_CONT:
            /* remove it */
            sigpp = markp->prev_next;
            remove_prio_q_marker(c_p, markp);
            break;
        default:
            ERTS_INTERNAL_ERROR("Invalid recv marker");
            break;
        }
        sigp = *sigpp;
    }

    return sigpp;

}

void
erts_msgq_remove_leading_recv_markers_set_save_first(Process *c_p)
{
    /*
     * Receive markers in the front of the queue does not
     * add any value, so we just remove them. We need to
     * keep and pass non-recv markers though...
     */
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    if (c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK) {
        c_p->sig_qs.flags |= FS_PRIO_MQ_SAVE;
    }
    else {
        ASSERT(!(c_p->sig_qs.flags & FS_PRIO_MQ_SAVE));
    }

    ERTS_MQ_SET_SAVE_INFO(c_p, FS_SET_SAVE_INFO_FIRST);
    c_p->sig_qs.save = msgq_pass_recv_markers(c_p, &c_p->sig_qs.first, !0);

    ASSERT(!*c_p->sig_qs.save || ERTS_SIG_IS_MSG(*c_p->sig_qs.save));
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
}

ErtsMessage **
erts_msgq_pass_recv_markers(Process *c_p, ErtsMessage **markpp)
{
    ErtsMessage **sigpp;
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    sigpp = msgq_pass_recv_markers(c_p, markpp, 0);

    ASSERT(!*sigpp || ERTS_SIG_IS_MSG(*sigpp));
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    return sigpp;
}

/*
 * Handle signals...
 */

static ERTS_INLINE int
handle_exit_signal(Process *c_p, ErtsSigRecvTracing *tracing,
                   ErtsMessage *sig, ErtsMessage ***next_nm_sig,
                   int *exited)
{
    ErtsMessage *conv_msg = NULL;
    ErtsExitSignalData *xsigd = NULL;
    Eterm tag = ((ErtsSignal *) sig)->common.tag;
    int op = ERTS_PROC_SIG_OP(tag);
    int extra = ERTS_PROC_SIG_XTRA(tag);
    int prio = 0;
    int prio_link_removed = 0;
    int destroy = 0;
    int ignore = 0;
    int save = 0;
    int exit = 0;
    int linked = 0;
    int cnt = 1;
    Eterm reason;
    Eterm from;

    ERTS_UNDEF(reason, THE_NON_VALUE);
    ASSERT(ERTS_PROC_SIG_TYPE(tag) == ERTS_SIG_Q_TYPE_GEN_EXIT);

    xsigd = get_exit_signal_data(sig);
    from = xsigd->from;

    if (extra & ERTS_SIG_ALTACT_SIG_X_ALIAS) {
        ErtsMonitor *mon;
        Uint32 flags;
        ASSERT(is_internal_ref(xsigd->u.ref));
        mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), xsigd->u.ref);
        flags = mon ? mon->flags : (Uint32) 0;
        if (!(flags & ERTS_ML_STATE_ALIAS_MASK)
            | !!(flags & ERTS_ML_FLG_SPAWN_PENDING)) {
            destroy = ignore = !0; /* Not an active alias... */
        }
        else {
            prio = (!!(extra & ERTS_SIG_ALTACT_SIG_X_PRIO)
                    & !!(flags & ERTS_ML_FLG_PRIO_ALIAS));
        }
    }
    else if (op == ERTS_SIG_Q_OP_EXIT_LINKED) {
        ErtsLink *lnk, *dlnk = NULL;
        ErtsELink *elnk = NULL;
        lnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p), from);
        if (!lnk)
            ignore = destroy = !0; /* No longer active */
        else if (ERTS_ML_GET_TYPE(lnk) != ERTS_LNK_TYPE_DIST_PROC
                 && ERTS_ML_GET_TYPE(lnk) != ERTS_LNK_TYPE_DIST_PORT) {
            if (((ErtsILink *) lnk)->unlinking)
                ignore = destroy = !0; /* No longer active */
            else
                linked = !0;
        }
        else {
            dlnk = erts_link_to_other(lnk, &elnk);
            if (elnk->unlinking)
                ignore = destroy = !0; /* No longer active */
            else
                linked = !0;
            if ((xsigd->u.link.flags & ERTS_SIG_LNK_X_FLAG_CONNECTION_LOST)
                && xsigd->u.link.connection_id != elnk->dist->connection_id) {
                /*
                 * The exit signal is due to loss of connection. The link
                 * that triggered this was setup before that connection
                 * was lost, but was later unlinked. After that, the
                 * current link was setup using a new connection. That is,
                 * current link should be left unaffected, and the signal
                 * should be silently dropped.
                 */
                linked = 0;
                lnk = NULL;
                ignore = destroy = !0;
            }
        }
        if (lnk) {
            if (lnk->flags & ERTS_ML_FLG_PRIO_ML) {
                prio_link_removed = !0;
                prio = linked;
            }
            /* Remove link... */
            erts_link_tree_delete(&ERTS_P_LINKS(c_p), lnk);
            if (!elnk)
                erts_link_internal_release(lnk);
            else if (erts_link_dist_delete(dlnk))
                erts_link_release_both(&elnk->ld);
            else
                erts_link_release(lnk);
        }
    }

    if (!ignore) {
        /* This GEN_EXIT was received from another node, decode the exit reason */
        if (ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig))
            erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, sig, 1);

        reason = xsigd->reason;

        if (is_non_value(reason)) {
            /* Bad distribution message; remove it from queue... */
            ignore = !0;
            destroy = !0;
        }
    }

    if (!ignore) {

        if ((op != ERTS_SIG_Q_OP_EXIT || reason != am_kill)
            && (c_p->flags & F_TRAP_EXIT)) {
            Eterm token = (!(extra & ERTS_SIG_ALTACT_SIG_X_TOKEN)
                           ? NIL
                           : ((ErtsSeqTokenExitSignalData *) xsigd)->token);
            if (prio) {
                remove_nm_sig(c_p, sig, next_nm_sig);
                cnt += insert_prepared_prio_msg(c_p, tracing, sig,
                                                xsigd->message, token,
                                                next_nm_sig);
            }
            else {
                convert_prepared_sig_to_msg(c_p, tracing, sig,
                                            xsigd->message, token,
                                            next_nm_sig);
            }
            conv_msg = sig;
        }
        else if (reason == am_normal
                 && !(xsigd->u.link.flags & ERTS_SIG_LNK_X_FLAG_NORMAL_KILLS)) {
            /* Ignore it... */
            destroy = !0;
            ignore = !0;
        }
        else {
            /* Terminate... */
            save = !0;
            exit = !0;
            if (op == ERTS_SIG_Q_OP_EXIT && reason == am_kill)
                reason = am_killed;
        }
    }

    if (ignore|exit) {
        remove_nm_sig(c_p, sig, next_nm_sig);
        if (exit) {
            if (save) {
                sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
                ERL_MESSAGE_TERM(sig) = xsigd->message;
                erts_save_message_in_proc(c_p, sig);
            }
            /* Exit process... */
            erts_set_self_exiting(c_p, reason);

            cnt++;
        }
    }

    if (!exit) {
        if (conv_msg)
            erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
        if (linked && tracing->procs) {
            ASSERT(op == ERTS_SIG_Q_OP_EXIT_LINKED);
            getting_unlinked(c_p, from);
        }
    }

    if (destroy) {
        cnt++;
        sig->next = NULL;
        erts_cleanup_messages(sig);
    }

    if (prio_link_removed) {
        /*
         * This needs to be done after the message has been inserted; otherwise,
         * the prio message might not be prioritized due to the prio queue being
         * removed prior to the message insertion.
         */
        erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_LINK);
    }

    *exited = exit;

    return cnt;
}

static ERTS_INLINE int
convert_prepared_down_message(Process *c_p, ErtsSigRecvTracing *tracing,
                              int prio, ErtsMessage *sig, Eterm msg,
                              ErtsMessage ***next_nm_sig)
{
    int cnt = 0;
    if (prio) {
        remove_nm_sig(c_p, sig, next_nm_sig);
        cnt += insert_prepared_prio_msg(c_p, tracing, sig, msg, am_undefined,
                                        next_nm_sig);
    }
    else {
        convert_prepared_sig_to_msg(c_p, tracing, sig, msg, am_undefined,
                                    next_nm_sig);
        cnt++;
    }
    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    return cnt;
}

static int
convert_to_down_message(Process *c_p,
                        ErtsSigRecvTracing *tracing,
                        int *priop,
                        ErtsMessage *sig,
                        ErtsMonitorData *mdp,
                        ErtsMonitor **omon,
                        Uint32 mon_type,
                        ErtsMessage ***next_nm_sig)
{
    int cnt = 0;
    Eterm node = am_undefined;
    ErtsMessage *mp;
    ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
    Uint hsz;
    Eterm *hp, ref, from, type, reason, tag;
    ErlOffHeap *ohp;

    ASSERT(mdp);
    ASSERT((mdp->origin.flags & ERTS_ML_FLGS_SAME)
           == (mdp->u.target.flags & ERTS_ML_FLGS_SAME));

    /* reason is mdp->u.target.other.item */
    reason = mdp->u.target.other.item;
    ASSERT(is_immed(reason));
    ASSERT(&mdp->origin == *omon);
           
    if (mdp->origin.flags & ERTS_ML_FLG_SPAWN_PENDING) {
        /*
         * Create a spawn_request() error message and replace
         * the signal with it...
         */
        ErtsMonitorDataExtended *mdep;

        /* Should only happen when connection breaks... */
        ASSERT(reason == am_noconnection);

        *priop = !!(mdp->origin.flags & ERTS_ML_FLG_PRIO_ML);

        if (mdp->origin.flags & (ERTS_ML_FLG_SPAWN_ABANDONED
                                 | ERTS_ML_FLG_SPAWN_NO_EMSG)) {
            /*
             * Operation has been abandoned or
             * error message has been disabled...
             */
            erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), *omon);
            erts_monitor_release(*omon);
            *omon = NULL;
            return 1;
        }

        cnt += 4;

        mdep = (ErtsMonitorDataExtended *) mdp;
        hsz = 5; /* 4-tuple */

        ASSERT(is_ref(mdp->ref));
        hsz += NC_HEAP_SIZE(mdp->ref);

        mp = erts_alloc_message_heap(c_p, &locks, hsz, &hp, &ohp);
        if (locks != ERTS_PROC_LOCK_MAIN)
            erts_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
        /*
         * The tag to patch into the resulting message
         * is stored in mdep->u.name via a little trick
         * (see pending_flag in erts_monitor_create()).
         */

        tag = save_heap_frag_eterm(c_p, mp, &mdep->u.name);
        
        /* Restore to normal monitor */
        ASSERT(mdep->u.name == NIL);
        mdp->origin.flags &= ~ERTS_ML_FLGS_SPAWN;

        ref = STORE_NC(&hp, ohp, mdp->ref);
        
        ERL_MESSAGE_FROM(mp) = am_undefined;
        ERL_MESSAGE_TERM(mp) = TUPLE4(hp, tag, ref, am_error, reason);

    }
    else {
        /*
         * Create a 'DOWN' message and replace the signal
         * with it...
         */
        Eterm message;

        hsz = 6; /* 5-tuple */

        if (mdp->origin.flags & ERTS_ML_FLG_NAME)
            hsz += 3;  /* reg name 2-tuple */
        else {
            ASSERT(is_pid(mdp->origin.other.item)
                   || is_port(mdp->origin.other.item));
            hsz += NC_HEAP_SIZE(mdp->origin.other.item);
        }

        ASSERT(is_ref(mdp->ref));
        hsz += NC_HEAP_SIZE(mdp->ref);

        mp = erts_alloc_message_heap(c_p, &locks, hsz, &hp, &ohp);

        if (locks != ERTS_PROC_LOCK_MAIN)
            erts_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);

        cnt += 4;

        ref = STORE_NC(&hp, ohp, mdp->ref);

        if (!(mdp->origin.flags & ERTS_ML_FLG_NAME)) {
            from = STORE_NC(&hp, ohp, mdp->origin.other.item);
        }
        else {
            ErtsMonitorDataExtended *mdep;
            ASSERT(mdp->origin.flags & ERTS_ML_FLG_EXTENDED);
            mdep = (ErtsMonitorDataExtended *) mdp;
            ASSERT(is_atom(mdep->u.name));
            if (mdep->dist)
                node = mdep->dist->nodename;
            else
                node = erts_this_dist_entry->sysname;
            from = TUPLE2(hp, mdep->u.name, node);
            hp += 3;
        }

        ASSERT(ERTS_ML_GET_TYPE(&mdp->origin) == mon_type);
        switch (mon_type) {
        case ERTS_MON_TYPE_PORT:
            type = am_port;
            if (mdp->origin.other.item == am_undefined) {
                /* failed by name... */
                ERL_MESSAGE_FROM(mp) = am_system;
            }
            else {
                ASSERT(is_internal_port(mdp->origin.other.item));
                ERL_MESSAGE_FROM(mp) = mdp->origin.other.item;
            }
            break;
        case ERTS_MON_TYPE_DIST_PORT: {
#ifdef DEBUG
            ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
#endif
            ASSERT(mdp->origin.flags & ERTS_ML_FLG_EXTENDED);
            type = am_port;
            ASSERT(node == am_undefined);
            ASSERT(!mdep->dist);
            ASSERT(is_external_port(from)
                   && (external_port_dist_entry(from)
                       == erts_this_dist_entry));
            node = erts_this_dist_entry->sysname;
            ASSERT(is_atom(node) && node != am_undefined);
            ERL_MESSAGE_FROM(mp) = node;
            break;
        }
        case ERTS_MON_TYPE_PROC:
            type = am_process;
            if (mdp->origin.other.item == am_undefined) {
                /* failed by name... */
                ERL_MESSAGE_FROM(mp) = am_system;
            }
            else {
                ASSERT(is_internal_pid(mdp->origin.other.item));
                ERL_MESSAGE_FROM(mp) = mdp->origin.other.item;
            }
            break;
        case ERTS_MON_TYPE_DIST_PROC:
            type = am_process;
            if (node == am_undefined) {
                ErtsMonitorDataExtended *mdep;
                ASSERT(mdp->origin.flags & ERTS_ML_FLG_EXTENDED);
                mdep = (ErtsMonitorDataExtended *) mdp;
                if (mdep->dist)
                    node = mdep->dist->nodename;
                else {
                    ASSERT(is_external_pid(from));
                    ASSERT(external_pid_dist_entry(from)
                           == erts_this_dist_entry);
                    node = erts_this_dist_entry->sysname;
                }
            }
            ASSERT(is_atom(node) && node != am_undefined);
            ERL_MESSAGE_FROM(mp) = node;
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected monitor type");
            type = am_undefined;
            ERL_MESSAGE_FROM(mp) = am_undefined;
            break;
        }

        if (!(mdp->origin.flags & ERTS_ML_FLG_TAG))
            tag = am_DOWN;
        else {
            Eterm *tag_storage;
            if (mdp->origin.flags & ERTS_ML_FLG_EXTENDED)
                tag_storage = &((ErtsMonitorDataExtended *) mdp)->heap[0];
            else
                tag_storage = &((ErtsMonitorDataTagHeap *) mdp)->heap[0];
            tag = save_heap_frag_eterm(c_p, mp, tag_storage);
        }

        message = TUPLE5(hp, tag, ref, type, from, reason);
        hp += 6;

        *priop = !!((*omon)->flags & ERTS_ML_FLG_PRIO_ML);
        if (*priop) {
            remove_nm_sig(c_p, sig, next_nm_sig);
            cnt += insert_prepared_prio_msg_attached(c_p, tracing, mp,
                                                     mp->data.attached, message,
                                                     am_undefined,
                                                     next_nm_sig);
            goto notify_new_message;
        }

        ERL_MESSAGE_TERM(mp) = message;
    }

    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    /* Replace original signal with the exit message... */
    convert_to_msg(c_p, tracing, sig, mp, next_nm_sig);

    cnt += 4;

notify_new_message:

    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);

    return cnt;
}

static ERTS_INLINE int
convert_to_nodedown_messages(Process *c_p,
                             ErtsSigRecvTracing *tracing,
                             ErtsMessage *sig,
                             ErtsMonitorData *mdp,
                             ErtsMessage ***next_nm_sig)
{
    int cnt = 1;
    Uint n;
    ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;

    ASSERT((mdp->origin.flags & ERTS_ML_FLGS_SAME)
           == (mdp->u.target.flags & ERTS_ML_FLGS_SAME));
    ASSERT(mdp->origin.flags & ERTS_ML_FLG_EXTENDED);

    n = mdep->u.refc;

    if (n == 0)
        remove_nm_sig(c_p, sig, next_nm_sig);
    else {
        Uint i;
        ErtsMessage *nd_first = NULL;
        ErtsMessage *nd_last = NULL;
        ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
        Eterm node = mdep->dist->nodename;

        ASSERT(is_atom(node));
        ASSERT(n > 0);

        for (i = 0; i < n; i++) {
            ErtsMessage *mp;
            ErlOffHeap *ohp;
            Eterm *hp;

            mp = erts_alloc_message_heap(c_p, &locks, 3, &hp, &ohp);

            ERL_MESSAGE_TERM(mp) = TUPLE2(hp, am_nodedown, node);
            ERL_MESSAGE_FROM(mp) = am_system;
            ERL_MESSAGE_TOKEN(mp) = am_undefined;
            mp->next = nd_first;
            nd_first = mp;
            if (!nd_last)
                nd_last = mp;
            cnt++;
        }

        if (locks != ERTS_PROC_LOCK_MAIN)
            erts_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);

        /* Replace signal with 'nodedown' messages */
        convert_to_msgs(c_p, tracing, sig, n, nd_first, nd_last, next_nm_sig);

        erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    }
    return cnt;
}

static int
handle_nodedown(Process *c_p,
                ErtsSigRecvTracing *tracing,
                ErtsMessage *sig,
                ErtsMonitorData *mdp,
                ErtsMessage ***next_nm_sig)
{
    ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
    ErtsMonitor *omon = &mdp->origin;
    int not_in_subtab = !(omon->flags & ERTS_ML_FLG_IN_SUBTABLE);
    int cnt = 1;

    ASSERT(erts_monitor_is_in_table(omon));

    if (not_in_subtab & !mdep->uptr.node_monitors)
        erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), omon);
    else if (not_in_subtab) {
        ErtsMonitor *sub_mon;
        ErtsMonitorDataExtended *sub_mdep;
        sub_mon = erts_monitor_list_last(mdep->uptr.node_monitors);
        ASSERT(sub_mon);
        erts_monitor_list_delete(&mdep->uptr.node_monitors, sub_mon);
        sub_mon->flags &= ~ERTS_ML_FLG_IN_SUBTABLE;
        sub_mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(sub_mon);
        ASSERT(!sub_mdep->uptr.node_monitors);
        sub_mdep->uptr.node_monitors = mdep->uptr.node_monitors;
        mdep->uptr.node_monitors = NULL;
        erts_monitor_tree_replace(&ERTS_P_MONITORS(c_p), omon, sub_mon);
        cnt += 2;
    }
    else {
        ErtsMonitorDataExtended *top_mdep;
        ErtsMonitor *top_mon;
        ASSERT(is_atom(omon->other.item));
        ASSERT(!mdep->uptr.node_monitors);
        top_mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p),
                                           omon->other.item);
        ASSERT(top_mon);
        top_mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(top_mon);
        ASSERT(top_mdep->uptr.node_monitors);
        erts_monitor_list_delete(&top_mdep->uptr.node_monitors, omon);
        omon->flags &= ~ERTS_ML_FLG_IN_SUBTABLE;
        cnt += 3;
    }

    return cnt
        + convert_to_nodedown_messages(c_p, tracing, sig, mdp, next_nm_sig);
}

static void
handle_persistent_mon_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                          Uint32 type, ErtsMonitor *mon, ErtsMessage *sig,
                          Eterm msg, ErtsMessage ***next_nm_sig)
{
    if (!(mon->flags & ERTS_ML_FLG_PRIO_ML)) {
        convert_prepared_sig_to_msg(c_p, tracing, sig, msg, am_undefined,
                                    next_nm_sig);
    }
    else {
        remove_nm_sig(c_p, sig, next_nm_sig);
        insert_prepared_prio_msg(c_p, tracing, sig, msg, am_undefined,
                                 next_nm_sig);
    }

    switch (type) {

    case ERTS_MON_TYPE_TIME_OFFSET:
        ASSERT(ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_TIME_OFFSET);
        if (mon->flags & ERTS_ML_FLG_TAG) {
            ErtsMonitorData *mdp = erts_monitor_to_data(mon);
            Eterm *tpl, tag_storage;
            ASSERT(is_tuple_arity(msg, 5));
            tpl = tuple_val(msg);
            ASSERT(tpl[1] == am_CHANGE);
            if (mon->flags & ERTS_ML_FLG_EXTENDED)
                tag_storage = ((ErtsMonitorDataExtended *) mdp)->heap[0];
            else
                tag_storage = ((ErtsMonitorDataTagHeap *) mdp)->heap[0];
            tpl[1] = copy_heap_frag_eterm(c_p, sig, tag_storage);
        }

        break;

    case ERTS_MON_TYPE_NODES: {
        ErtsMonitorDataExtended *mdep;
        Uint n;
        ASSERT(ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_NODES);
        ASSERT(!(mon->flags & ERTS_ML_FLG_PRIO_ML));
        mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
        ERTS_ML_ASSERT(mdep->u.refc > 0);
        n = mdep->u.refc;
        n--;
        if (n > 0) {
            ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
            ErtsMessage *first = NULL, *prev, *last;
            Uint hsz = size_object(msg);
            Uint i;
            ERTS_UNDEF(last,NULL);

            for (i = 0; i < n; i++) {
                Eterm *hp;
                ErlOffHeap *ohp;

                last = erts_alloc_message_heap(c_p, &locks, hsz, &hp, &ohp);

                if (!first)
                    first = last;
                else
                    prev->next = last;
                prev = last;

                ERL_MESSAGE_TERM(last) = copy_struct(msg, hsz, &hp, ohp);

#ifdef USE_VM_PROBES
                ASSERT(is_immed(ERL_MESSAGE_DT_UTAG(sig)));
                ERL_MESSAGE_DT_UTAG(last) = ERL_MESSAGE_DT_UTAG(sig);
#endif
                ASSERT(is_immed(ERL_MESSAGE_TOKEN(sig)));
                ERL_MESSAGE_TOKEN(last) = ERL_MESSAGE_TOKEN(sig);
                ASSERT(is_immed(ERL_MESSAGE_FROM(sig)));
                ERL_MESSAGE_FROM(last) = ERL_MESSAGE_FROM(sig);

            }
            if (locks != ERTS_PROC_LOCK_MAIN)
                erts_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
            insert_messages(c_p, tracing, &sig->next, first, last,
                            n, next_nm_sig);
        }
        break;
    }

    default:
        ERTS_INTERNAL_ERROR("Invalid type");
        break;
    }

    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
}

static void
group_leader_reply(Process *c_p, Eterm to, Eterm ref, int success)
{
    Process *rp = erts_proc_lookup(to);

    if (rp) {
        ErtsProcLocks locks;
        Uint sz;
        Eterm *hp, msg, ref_cpy, result;
        ErlOffHeap *ohp;
        ErtsMessage *mp;

        ASSERT(is_internal_ref(ref));

        locks = c_p == rp ? ERTS_PROC_LOCK_MAIN : 0;
        sz = size_object(ref);

        mp = erts_alloc_message_heap(rp, &locks, sz+3,
                                     &hp, &ohp);

        ref_cpy = copy_struct(ref, sz, &hp, ohp);
        result = success ? am_true : am_badarg;
        msg = TUPLE2(hp, ref_cpy, result);

        erts_queue_message(rp, locks, mp, msg, am_system);

        if (c_p == rp)
            locks &= ~ERTS_PROC_LOCK_MAIN;

        if (locks)
            erts_proc_unlock(rp, locks);
    }
}

static void
handle_group_leader(Process *c_p, ErtsSigGroupLeader *sgl)
{
    erts_aint_t flags;

    flags = erts_atomic_read_band_nob(&sgl->flags, ~ERTS_SIG_GL_FLG_ACTIVE);
    if (flags & ERTS_SIG_GL_FLG_ACTIVE) {
        int res = erts_set_group_leader(c_p, sgl->group_leader);
        if (is_internal_pid(sgl->reply_to))
            group_leader_reply(c_p, sgl->reply_to, sgl->ref, res);
    }

    flags = erts_atomic_read_band_nob(&sgl->flags, ~ERTS_SIG_GL_FLG_RECEIVER);
    if ((flags & ~ERTS_SIG_GL_FLG_RECEIVER) == 0)
        destroy_sig_group_leader(sgl);
}

static int
handle_process_info(Process *c_p, ErtsSigRecvTracing *tracing,
                    ErtsMessage *sig, ErtsMessage ***next_nm_sig,
                    int is_alive)
{
    ErtsProcessInfoSig *pisig = (ErtsProcessInfoSig *) sig;
    Uint reds = 0;
    Process *rp;

    ASSERT(!!is_alive == !(erts_atomic32_read_nob(&c_p->state)
                           & ERTS_PSFLG_EXITING));
    if (is_alive) {
        if (pisig->flags & ERTS_PI_FLAG_NEED_MSGQ) {
            /*
             * Request requires message queue to be updated before inspection.
             * Move messages part of message queue into inner signal queue...
             */
            ASSERT(tracing);

            if (*next_nm_sig != &c_p->sig_qs.cont) {
                if (c_p->sig_qs.save == &c_p->sig_qs.cont)
                    c_p->sig_qs.save = c_p->sig_qs.last;
                if (ERTS_SIG_IS_RECV_MARKER(c_p->sig_qs.cont)) {
                    ErtsRecvMarker *markp = (ErtsRecvMarker *) c_p->sig_qs.cont;
                    markp->prev_next = c_p->sig_qs.last;
                }
                if (*next_nm_sig == tracing->messages.next)
                    tracing->messages.next = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = *next_nm_sig;

                ASSERT(*next_nm_sig);
                c_p->sig_qs.cont = **next_nm_sig;
                if (c_p->sig_qs.nmsigs.last == *next_nm_sig)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
                *next_nm_sig = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = NULL;
            }
        }

        if (!pisig->common.specific.next) {
            /*
             * No more signals in middle queue...
             *
             * Process-info 'status' needs sig-q
             * process flag to be updated in order
             * to show accurate result...
             */
            erts_atomic32_read_band_nob(&c_p->state,
                                        ~ERTS_PSFLG_SIG_Q);
        }
        remove_nm_sig(c_p, sig, next_nm_sig);
    }

    rp = erts_proc_lookup(pisig->requester);
    ASSERT(c_p != rp);
    if (rp) {
        Eterm msg, res, ref, *hp;
        ErtsProcLocks locks = 0;
        ErtsHeapFactory hfact;
        ErtsMessage *mp;
        Uint reserve_size = 3 + sizeof(pisig->oref_thing)/sizeof(Eterm);

        if (!is_alive) {
            ErlOffHeap *ohp;
            mp = erts_alloc_message_heap(rp, &locks, reserve_size, &hp, &ohp);
            res = am_undefined;
        }
        else {
            ErlHeapFragment *hfrag;

            reserve_size += pisig->reserve_size;

            mp = erts_alloc_message(0, NULL);
            hfrag = new_message_buffer(reserve_size);
            mp->data.heap_frag = hfrag;
            erts_factory_selfcontained_message_init(&hfact, mp, &hfrag->mem[0]);

            res = erts_process_info(c_p, &hfact, c_p, ERTS_PROC_LOCK_MAIN,
                                    pisig->item_ix, pisig->item_extra, pisig->len,
                                    pisig->flags, reserve_size, &reds);

            hp = erts_produce_heap(&hfact,
                                   3 + sizeof(pisig->oref_thing)/sizeof(Eterm),
                                   0);
        }

        sys_memcpy((void *) hp, (void *) &pisig->oref_thing,
                   sizeof(pisig->oref_thing));
        ref = make_internal_ref(hp);
        hp += sizeof(pisig->oref_thing)/sizeof(Eterm);

        msg = TUPLE2(hp, ref, res);

        if (is_alive)
            erts_factory_trim_and_close(&hfact, &msg, 1);

        ERL_MESSAGE_TOKEN(mp) = am_undefined;
        erts_queue_proc_message(c_p, rp, locks, mp, msg);

        if (!is_alive && locks)
            erts_proc_unlock(rp, locks);
    }

    destroy_process_info_sig(pisig);

    if (reds > INT_MAX/8)
        reds = INT_MAX/8;

    return ((int) reds)*4 + 8;
}

static void
activate_suspend_monitor(Process *c_p, ErtsMonitorSuspend *msp)
{
    erts_aint_t mstate;

    erts_pause_proc_timer(c_p);
    mstate = erts_atomic_read_bor_acqb(&msp->state,
                                       ERTS_MSUSPEND_STATE_FLG_ACTIVE);
    ASSERT(!(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE)); (void) mstate;
    erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
}

static void
handle_suspend(Process *c_p, ErtsMonitor *mon, int *yieldp)
{
    erts_aint32_t state = erts_atomic32_read_nob(&c_p->state);

    ASSERT(ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_SUSPEND);

    if (!(state & ERTS_PSFLG_DIRTY_RUNNING)) {
        ErtsMonitorSuspend *msp = (ErtsMonitorSuspend *) erts_monitor_to_data(mon);
        activate_suspend_monitor(c_p, msp);
        *yieldp = !0;
    }
    else {
        /* Executing dirty; delay suspend... */
        ErtsProcSigPendingSuspend *psusp;
        ErtsMonitorSuspend *msp;

        psusp = ERTS_PROC_GET_PENDING_SUSPEND(c_p);
        if (!psusp) {
            psusp = erts_alloc(ERTS_ALC_T_SIG_DATA,
                               sizeof(ErtsProcSigPendingSuspend));
            psusp->mon = NULL;
            psusp->sync = NULL;
            ERTS_PROC_SET_PENDING_SUSPEND(c_p, (void *) psusp);
        }

        msp = (ErtsMonitorSuspend *) erts_monitor_to_data(mon);

        msp->next = psusp->mon;
        psusp->mon = msp;

        erts_atomic32_inc_nob(&msp->md.refc);
    }
}

static void
sync_suspend_reply(Process *c_p, ErtsMessage *mp, erts_aint32_t state)
{
    /*
     * Sender prepared the message for us. Just patch
     * the result if necessary. The default prepared
     * result is 'false'.
     */
    Process *rp;
    ErtsSyncSuspendRequest *ssusp;

    ssusp = (ErtsSyncSuspendRequest *) (char *) (&mp->hfrag.mem[0]
                                                 + mp->hfrag.used_size);

    ASSERT(ERTS_SIG_IS_NON_MSG(mp));
    ASSERT(ERTS_PROC_SIG_OP(((ErtsSignal *) mp)->common.tag)
           == ERTS_SIG_Q_OP_SYNC_SUSPEND);
    ASSERT(mp->hfrag.alloc_size > mp->hfrag.used_size);
    ASSERT((mp->hfrag.alloc_size - mp->hfrag.used_size)*sizeof(UWord)
           >= sizeof(ErtsSyncSuspendRequest));
    ASSERT(is_internal_pid(ssusp->requester));
    ASSERT(ssusp->requester != c_p->common.id);
    ASSERT(is_tuple_arity(ssusp->message, 2));
    ASSERT(is_immed(tuple_val(ssusp->message)[2]));

    ERL_MESSAGE_TERM(mp) = ssusp->message;
    mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
    mp->next = NULL;

    rp = erts_proc_lookup(ssusp->requester);
    if (!rp)
        erts_cleanup_messages(mp);
    else {
        if ((state & (ERTS_PSFLG_EXITING
                      | ERTS_PSFLG_SUSPENDED)) != ERTS_PSFLG_SUSPENDED) {
            /* Not suspended -> patch result... */
            if (state & ERTS_PSFLG_EXITING) {
                Eterm *tp = tuple_val(ssusp->message);
                tp[2] = ssusp->async ? am_exited : am_badarg;
            }
            else {
                Eterm *tp = tuple_val(ssusp->message);
                ASSERT(!(state & ERTS_PSFLG_SUSPENDED));
                tp[2] = ssusp->async ? am_not_suspended : am_internal_error;
            }
        }
        ERL_MESSAGE_TOKEN(mp) = am_undefined;
        erts_queue_proc_message(c_p, rp, 0, mp, ssusp->message);
    }
}

static void
handle_sync_suspend(Process *c_p, ErtsMessage *mp)
{
    ErtsProcSigPendingSuspend *psusp;

    psusp = (ErtsProcSigPendingSuspend *) ERTS_PROC_GET_PENDING_SUSPEND(c_p);
    if (!psusp)
        sync_suspend_reply(c_p, mp, erts_atomic32_read_nob(&c_p->state));
    else {
        mp->next = psusp->sync;
        psusp->sync = mp;
    }
}

int
erts_proc_sig_decode_dist(Process *proc, ErtsProcLocks proc_locks,
                          ErtsMessage *msgp, int force_off_heap)
{
    ErtsHeapFactory factory;
    ErlHeapFragment *hfrag;
    Eterm msg;
    Sint need;
    ErtsDistExternal *edep;
    ErtsExitSignalData *xsigd = NULL;

    edep = erts_proc_sig_get_external(msgp);
    if (!ERTS_SIG_IS_EXTERNAL_MSG(msgp))
        xsigd = get_exit_signal_data(msgp);

    if (edep->heap_size >= 0)
	need = edep->heap_size;
    else {
	need = erts_decode_dist_ext_size(edep, 1, 1);
	if (need < 0) {
	    /* bad signal; remove it... */
	    return 0;
	}

	edep->heap_size = need;
    }

    if (ERTS_SIG_IS_NON_MSG(msgp)) {
        switch (ERTS_PROC_SIG_OP(ERL_MESSAGE_TERM(msgp))) {
        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
            /* {'EXIT', From, Reason} */
            need += 4;
            break;
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            /* {'DOWN', Ref, process, From, Reason} */
            need += 6; /* 5-tuple */
            break;
        default:
            ERTS_INTERNAL_ERROR("Invalid exit signal op");
            break;
        }
    }

    hfrag = new_message_buffer(need);
    erts_factory_heap_frag_init(&factory, hfrag);

    ASSERT(edep->heap_size >= 0);

    msg = erts_decode_dist_ext(&factory, edep, 1);

    if (is_non_value(msg)) {
        erts_factory_undo(&factory);
        return 0;
    }

    if (ERTS_SIG_IS_MSG(msgp)) {
        ERL_MESSAGE_TERM(msgp) = msg;
        if (msgp->data.heap_frag == &msgp->hfrag)
            msgp->data.heap_frag = ERTS_MSG_COMBINED_HFRAG;
    } else {
        switch (ERTS_PROC_SIG_OP(ERL_MESSAGE_TERM(msgp))) {
        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
            /* {'EXIT', From, Reason} */
            erts_reserve_heap(&factory, 4);
            xsigd->message = TUPLE3(factory.hp, am_EXIT, xsigd->from, msg);
            factory.hp += 4;
            break;
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            /* {'DOWN', Ref, process, From, Reason} */
            erts_reserve_heap(&factory, 6);
            xsigd->message = TUPLE5(factory.hp, am_DOWN, xsigd->u.ref, am_process, xsigd->from, msg);
            factory.hp += 6;
            break;
        }
        xsigd->reason = msg;
    }

    erts_free_dist_ext_copy(edep);

    erts_factory_close(&factory);

    hfrag = factory.heap_frags;
    while (hfrag->next)
        hfrag = hfrag->next;

    if (ERTS_SIG_IS_MSG(msgp) && msgp->data.heap_frag != ERTS_MSG_COMBINED_HFRAG) {
        hfrag->next = msgp->data.heap_frag;
        msgp->data.heap_frag = factory.heap_frags;
    } else {
        hfrag->next = msgp->hfrag.next;
        msgp->hfrag.next = factory.heap_frags;
    }

    return 1;
}

void
erts_proc_sig_handle_pending_suspend(Process *c_p)
{
    ErtsMonitorSuspend *msp;
    ErtsMessage *sync;
    ErtsProcSigPendingSuspend *psusp;
    erts_aint32_t state = erts_atomic32_read_nob(&c_p->state);

    psusp = (ErtsProcSigPendingSuspend *) ERTS_PROC_GET_PENDING_SUSPEND(c_p);

    msp = psusp->mon;
    
    while (msp) {
        ErtsMonitorSuspend *next_msp = msp->next;
        msp->next = NULL;
        if (!(state & ERTS_PSFLG_EXITING)
            && erts_monitor_is_in_table(&msp->md.u.target)) {
            activate_suspend_monitor(c_p, msp);
        }

        erts_monitor_release(&msp->md.u.target);

        msp = next_msp;
    }

    sync = psusp->sync;

    while (sync) {
        ErtsMessage *next_sync = sync->next;
        sync->next = NULL;
        sync_suspend_reply(c_p, sync, state);
        sync = next_sync;
    }

    erts_free(ERTS_ALC_T_SIG_DATA, psusp);

    ERTS_PROC_SET_PENDING_SUSPEND(c_p, NULL);
}

static int
handle_dist_spawn_reply(Process *c_p, ErtsSigRecvTracing *tracing,
                        ErtsMessage *sig, ErtsMessage ***next_nm_sig)
{
    ErtsDistSpawnReplySigData *datap = get_dist_spawn_reply_data(sig);
    ErtsMonitorDataExtended *mdep;
    Eterm msg = datap->message;
    Eterm result = datap->result;
    ErtsMonitor *omon;
    int adjust_monitor;
    ErlHeapFragment *tag_hfrag;
    int convert_to_message = !0;
    int cnt = 1;

    ASSERT(is_atom(result) || is_external_pid(result));
    ASSERT(is_atom(result) || size_object(result) == EXTERNAL_PID_HEAP_SIZE);

    omon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), datap->ref);

    if (!omon || !(omon->flags & ERTS_ML_FLG_SPAWN_PENDING)) {
        /* Stale reply; remove link that was setup... */
        ErtsLink *lnk = datap->link;
        if (lnk) {
            ErtsELink *elnk;
            ErtsLink *dlnk = erts_link_to_other(lnk, &elnk);
            if (erts_link_dist_delete(dlnk))
                erts_link_release_both(&elnk->ld);
            else
                erts_link_release(lnk);
        }
        remove_nm_sig(c_p, sig, next_nm_sig);
        sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
        ERL_MESSAGE_TERM(sig) = msg;
        sig->next = NULL;;
        erts_cleanup_messages(sig);
        return ++cnt;
    }

    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(omon);

#ifdef DEBUG
    {
        Eterm *tp;
        int i, start, stop;
        ASSERT(erts_monitor_is_in_table(omon));
        ASSERT(omon->flags & ERTS_ML_FLG_SPAWN_PENDING);
        if (is_atom(result)) {
            ASSERT(!datap->link);
        }
        else {
            ASSERT(!datap->link || (omon->flags & ERTS_ML_FLG_SPAWN_LINK));
            ASSERT(!(omon->flags & ERTS_ML_FLG_SPAWN_LINK) || datap->link);
        }
        ASSERT(omon->other.item == am_pending);
        ASSERT(is_tuple_arity(datap->message, 4));
        tp = tuple_val(datap->message);
        ASSERT(tp[1] == am_undefined); /* patch point */
        ASSERT(is_internal_ref(tp[2]));
        ASSERT((tp[3] == am_ok && is_external_pid(tp[4]))
               || (tp[3] == am_error && is_atom(tp[4])));
        start = 0;
        stop = EXTERNAL_PID_HEAP_SIZE;
        if (omon->flags & ERTS_ML_FLG_TAG) {
            start++;
            stop++;
        }
        for (i = start; i < stop; i++) {
            ASSERT(is_non_value(mdep->heap[i]));
        }
    }
#endif

    /*
     * The tag to patch into the resulting message
     * is stored in mdep->u.name via a little trick
     * (see pending_flag in erts_monitor_create()).
     */
    *datap->patch_point = get_heap_frag_eterm(&tag_hfrag, &mdep->u.name);
    /*
     * get_heap_frag_eterm() above will also write
     * NIL to mdep->u.name, restoring it to a normal
     * monitor...
     */
    
    if (is_atom(result)) { /* Spawn error; cleanup... */
        /* Dist code should not have created a link on failure... */

        ASSERT(is_not_atom(result) || !datap->link);
        /* delete monitor structure... */
        adjust_monitor = 0;
        if (omon->flags & (ERTS_ML_FLG_SPAWN_ABANDONED
                           | ERTS_ML_FLG_SPAWN_NO_EMSG))
            convert_to_message = 0;
        if (omon->flags & ERTS_ML_FLG_PRIO_ML)
            erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_MONITOR);
    }
    else if (omon->flags & ERTS_ML_FLG_SPAWN_ABANDONED) {
        /*
         * Spawn operation has been abandoned and
         * link option was passed. Send exit signal
         * with exit reason 'abandoned'...
         */
        DistEntry *dep;
        ErtsMonLnkDist *dist;
        ErtsMonitorDataExtended *mdep;
        ErtsLink *lnk;
        
        mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(omon);
        dist = mdep->dist;

        ASSERT(omon->flags & ERTS_ML_FLG_SPAWN_LINK);
        ASSERT(!(omon->flags & ERTS_ML_FLG_PRIO_ML));

        lnk = datap->link;
        if (lnk) {
            ErtsELink *elnk;
            ErtsLink *dlnk;
            dlnk = erts_link_to_other(lnk, &elnk);
            if (erts_link_dist_delete(dlnk))
                erts_link_release_both(&elnk->ld);
            else
                erts_link_release(lnk);
        }
        
        ASSERT(is_external_pid(result));
        dep = external_pid_dist_entry(result);

        if (dep != erts_this_dist_entry && dist->nodename == dep->sysname) {
            ErtsDSigSendContext ctx;
            int code = erts_dsig_prepare(&ctx, dep, NULL, 0,
                                         ERTS_DSP_NO_LOCK, 1, 1, 0);
            switch (code) {
            case ERTS_DSIG_PREP_CONNECTED:
            case ERTS_DSIG_PREP_PENDING:
                if (dist->connection_id == ctx.connection_id) {
                    code = erts_dsig_send_exit_tt(&ctx,
                                                  c_p,
                                                  result,
                                                  am_abandoned,
                                                  SEQ_TRACE_TOKEN(c_p));
                    ASSERT(code == ERTS_DSIG_SEND_OK);
                }
                break;
            default:
                break;
            }
        }
        /* delete monitor structure... */
        adjust_monitor = 0;
        /* drop message... */
        convert_to_message = 0;
    }
    else {
        /* Success... */
        ASSERT(is_external_pid(result));

        if (omon->flags & ERTS_ML_FLG_SPAWN_NO_SMSG)
            convert_to_message = 0;

        if (datap->link) {
            cnt++;
            erts_link_tree_insert(&ERTS_P_LINKS(c_p), datap->link);
            if (omon->flags & ERTS_ML_FLG_SPAWN_LINK_PRIO) {
                datap->link->flags |= ERTS_ML_FLG_PRIO_ML;
                erts_proc_sig_prio_item_added(c_p, ERTS_PRIO_ITEM_TYPE_LINK);
            }
            if (tracing->procs)
                linking(c_p, result);
        }
        
        adjust_monitor = !!(omon->flags & ERTS_ML_FLG_SPAWN_MONITOR);
        if (adjust_monitor) {
            /*
             * Insert the actual pid of spawned process
             * in origin part of monitor...
             */
            ErlOffHeap oh;
            ErtsMonitorDataExtended *mdep;
            Eterm *hp;
            mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(omon);
            hp = &(mdep)->heap[(omon->flags & ERTS_ML_FLG_TAG) ? 1 : 0];
            omon->flags &= ~ERTS_ML_FLGS_SPAWN;
            ERTS_INIT_OFF_HEAP(&oh);
            oh.first = mdep->uptr.ohhp;
            omon->other.item = copy_struct(result,
                                           EXTERNAL_PID_HEAP_SIZE,
                                           &hp, &oh);
            mdep->uptr.ohhp = oh.first;
            cnt += 2;
        }
    }

    if (!adjust_monitor) {
        /*
         * Delete monitor; either spawn error
         * or no monitor requested...
         */
        ErtsMonitorData *mdp = erts_monitor_to_data(omon);

        omon->flags &= ~ERTS_ML_FLGS_SPAWN;

        erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), omon);

        if (erts_monitor_dist_delete(&mdp->u.target))
            erts_monitor_release_both(mdp);
        else
            erts_monitor_release(omon);
        cnt += 2;
    }

    if (convert_to_message) {
        Eterm token = (!ERTS_PROC_SIG_XTRA(((ErtsSignal *) sig)->common.tag)
                       ? NIL
                       : ((ErtsDistSeqTokenSpawnReplySigData *) datap)->token);
        convert_prepared_sig_to_msg(c_p, tracing, sig, msg, token, next_nm_sig);
        if (tag_hfrag) {
            /* Save heap fragment of tag in message... */
            ASSERT(sig->data.attached == ERTS_MSG_COMBINED_HFRAG);
            tag_hfrag->next = sig->hfrag.next;
            sig->hfrag.next = tag_hfrag;
        }
        erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    }
    else {
        remove_nm_sig(c_p, sig, next_nm_sig);
        sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
        ERL_MESSAGE_TERM(sig) = msg;
        sig->next = NULL;;
        erts_cleanup_messages(sig);
        if (tag_hfrag) {
            tag_hfrag->next = NULL;
            free_message_buffer(tag_hfrag);
        }
    }
    return cnt;
}

static int
handle_dist_spawn_reply_exiting(Process *c_p,
                                ErtsMessage *sig,
                                ErtsProcExitContext *pe_ctxt_p)
{
    ErtsDistSpawnReplySigData *datap = get_dist_spawn_reply_data(sig);
    Eterm result = datap->result;
    Eterm msg = datap->message;
    ErtsMonitorData *mdp;
    ErtsMonitor *omon;
    int cnt = 1;

    ASSERT(is_atom(result) || is_external_pid(result));
    ASSERT(is_atom(result) || size_object(result) == EXTERNAL_PID_HEAP_SIZE);

    omon = erts_monitor_tree_lookup(pe_ctxt_p->pend_spawn_monitors, datap->ref);
    if (!omon) {
        /* May happen when connection concurrently close... */
        ErtsLink *lnk = datap->link;
        if (lnk) {
            ErtsELink *elnk;
            ErtsLink *dlnk = erts_link_to_other(lnk, &elnk);
            if (erts_link_dist_delete(dlnk))
                erts_link_release_both(&elnk->ld);
            else
                erts_link_release(lnk);
        }
        cnt++;
    }
    else {
        ASSERT(omon->flags & ERTS_ML_FLG_SPAWN_PENDING);
        ASSERT(!datap->link || is_external_pid(result));

        erts_monitor_tree_delete(&pe_ctxt_p->pend_spawn_monitors, omon);
        mdp = erts_monitor_to_data(omon);

        if (!erts_dist_pend_spawn_exit_delete(&mdp->u.target))
            mdp = NULL; /* Connection closed/closing... */
        cnt++;

        if (is_external_pid(result)) {
            if ((omon->flags & ERTS_ML_FLG_SPAWN_MONITOR) && mdp) {
                ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
                erts_proc_exit_dist_demonitor(c_p,
                                              external_pid_dist_entry(result),
                                              mdep->dist->connection_id,
                                              datap->ref,
                                              result);
                cnt++;
            }
            ASSERT(!datap->link || (omon->flags & ERTS_ML_FLG_SPAWN_LINK));
            ASSERT(!(omon->flags & ERTS_ML_FLG_SPAWN_LINK) || datap->link);

            if (datap->link) {
                /* unless operation has been abandoned... */
                if (omon->flags & ERTS_ML_FLG_SPAWN_ABANDONED) {
                    ErtsProcExitContext pectxt = {c_p, am_abandoned};
                    erts_proc_exit_handle_link(datap->link, (void *) &pectxt, -1);
                }
                else {
                    /* This link exit *should* have actual reason... */
                    erts_proc_exit_handle_link(datap->link,
                                               (void *) pe_ctxt_p,
                                               -1);
                }
                cnt++;
            }
        }
        if (mdp)
            erts_monitor_release_both(mdp);
        else
            erts_monitor_release(omon);
        cnt++;
    }
    sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
    ERL_MESSAGE_TERM(sig) = msg;
    sig->next = NULL;
    erts_cleanup_messages(sig);
    cnt++;
    return cnt;
}

static int
handle_altact_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                  ErtsMessage *sig, ErtsMessage ***next_nm_sig)
{
    void *data_attached;
    Eterm from, sender, alias, msg, token;
    ErtsMonitor *mon;
    int type, prio, cnt = 0, prio_alias_deactivated = 0;

    type = get_altact_msg_data(sig, &data_attached, &msg, &from,
                               &prio, &sender, &alias, &token);

    ASSERT(is_internal_pid(from) || is_atom(from));

    if (!is_value(alias)) {
        prio = 0;
    }
    else {
        Uint32 flags;

        ASSERT(is_internal_pid_ref(alias));

        mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), alias);
        flags = mon ? mon->flags : (Uint32) 0;
        if (!(flags & ERTS_ML_STATE_ALIAS_MASK)
            | !!(flags & ERTS_ML_FLG_SPAWN_PENDING)) {
            /*
             * Not an alias (never has been, not anymore, or not yet);
             * drop message...
             */
            remove_nm_sig(c_p, sig, next_nm_sig);
            sig->next = NULL;;
            erts_cleanup_messages(sig);
            return 2;
        }

        if (prio && !(flags & ERTS_ML_FLG_PRIO_ALIAS))
            prio = 0;

        if ((flags & ERTS_ML_STATE_ALIAS_MASK) == ERTS_ML_STATE_ALIAS_ONCE) {
            mon->flags &= ~ERTS_ML_STATE_ALIAS_MASK;

            erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), mon);
            if (flags & ERTS_ML_FLG_PRIO_ALIAS)
                prio_alias_deactivated = !0;

            switch (ERTS_ML_GET_TYPE(mon)) {
            case ERTS_MON_TYPE_DIST_PORT:
            case ERTS_MON_TYPE_ALIAS:
                erts_monitor_release(mon);
                break;
            case ERTS_MON_TYPE_PROC:
                erts_proc_sig_send_demonitor(&c_p->common, c_p->common.id, 0, mon);
                break;
            case ERTS_MON_TYPE_DIST_PROC: {
                ErtsMonitorData *mdp;
                ErtsMonLnkDist *dist;
                DistEntry *dep;
                Eterm watched;
                mdp = erts_monitor_to_data(mon);
                dist = ((ErtsMonitorDataExtended *) mdp)->dist;
                ASSERT(dist);
                if (flags & ERTS_ML_FLG_NAME) {
                    watched = ((ErtsMonitorDataExtended *) mdp)->u.name;
                    ASSERT(is_atom(watched));
                    dep = erts_sysname_to_connected_dist_entry(dist->nodename);
                }
                else {
                    watched = mon->other.item;
                    ASSERT(is_external_pid(watched));
                    dep = external_pid_dist_entry(watched);
                }
                erts_proc_exit_dist_demonitor(c_p, dep, dist->connection_id,
                                              mdp->ref, watched);
                if (!erts_monitor_dist_delete(&mdp->u.target))
                    erts_monitor_release(mon);
                else
                    erts_monitor_release_both(mdp);
                break;
            }
            case ERTS_MON_TYPE_TIME_OFFSET:
                erts_demonitor_time_offset(mon);
                break;
            case ERTS_MON_TYPE_PORT: {
                Port *prt;
                ASSERT(is_internal_port(mon->other.item));
                prt = erts_port_lookup(mon->other.item, ERTS_PORT_SFLGS_DEAD);
                if (!prt || erts_port_demonitor(c_p, prt, mon) == ERTS_PORT_OP_DROPPED)
                    erts_monitor_release(mon);
                break;
            }
            default:
                break;
            }
        }
    }

    ERL_MESSAGE_FROM(sig) = from;

    if (prio)
        remove_nm_sig(c_p, sig, next_nm_sig);

    switch (type) {

    case ERTS_SIG_Q_TYPE_HEAP:
    case ERTS_SIG_Q_TYPE_OFF_HEAP:
    case ERTS_SIG_Q_TYPE_HEAP_FRAG:
        if (!prio) {
            convert_prepared_sig_to_msg_attached(c_p, tracing, sig, msg, token,
                                                 data_attached, next_nm_sig);
        }
        else {
            cnt += insert_prepared_prio_msg_attached(c_p, tracing, sig,
                                                     data_attached, msg, token,
                                                     next_nm_sig);
        }
        cnt++;
        break;

    case ERTS_SIG_Q_TYPE_DIST:
    {
        ErtsMessage *mp;
        ERTS_UNDEF(mp, NULL);

        /*
         * Convert to external message...
         *
         * See erts_proc_sig_send_dist_altact_msg() for info on
         * how the signal was constructed...
         */
        if (prio) {
            mp = sig;
            ERL_MESSAGE_TERM(mp) = THE_NON_VALUE;
            ERL_MESSAGE_TOKEN(mp) = token;
            sig->data.attached = &sig->hfrag; /* <- will be adjusted to
                                               * ERTS_MSG_COMBINED_HFRAG
                                               * when message is decoded */
        }
        else {
            convert_prepared_sig_to_external_msg(c_p, tracing, sig, token,
                                                 next_nm_sig);
            cnt++;
        }

        if (0) {
        case ERTS_SIG_Q_TYPE_DIST_FRAG:

            /*
             * Fragmented message. Need to replace message
             * reference...
             */
            mp = erts_alloc_message(0, NULL);
            sys_memcpy((void *) &mp->m[0],
                       (void *) &sig->m[0],
                       ERL_MESSAGE_REF_ARRAY_SZ*sizeof(Eterm));
            ERL_MESSAGE_TERM(mp) = THE_NON_VALUE;
            ERL_MESSAGE_TOKEN(mp) = token;
            ASSERT(sig->hfrag.next);
            mp->data.heap_frag = sig->hfrag.next;

            if (!prio) {
                /* Replace original signal with the external message... */
                convert_to_msg(c_p, tracing, sig, mp, next_nm_sig);
            }

            ERL_MESSAGE_TERM(sig) = NIL;
            sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
            sig->hfrag.next = NULL;
            sig->next = NULL;
            erts_cleanup_messages(sig);
            cnt += 8;
        }

        if (prio) {
            cnt += 50; /* Decode is expensive... */
            if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, mp, !0)) {
                /* drop faulty encoded external message... */
                return cnt;
            }
            cnt += insert_prepared_prio_msg(c_p, tracing, mp, ERL_MESSAGE_TERM(mp),
                                            token, next_nm_sig);
        }
        break;
    }

    default:
        ERTS_INTERNAL_ERROR("Invalid altact message type");
        break;
    }

    if (prio_alias_deactivated) {
        /*
         * This needs to be done after the message has been inserted; otherwise,
         * the prio message might not be prioritized due to the prio queue being
         * removed prior to the message insertion.
         */
        erts_proc_sig_prio_item_deleted(c_p, ERTS_PRIO_ITEM_TYPE_ALIAS);
    }

    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    return cnt;
}


/*
 * Called in order to handle incoming signals.
 */

int
erts_proc_sig_handle_incoming(Process *c_p, erts_aint32_t *statep,
                              int *redsp, int max_reds, int local_only)
{
    Eterm tag;
    erts_aint32_t state = *statep;
    int yield, cnt, limit, abs_lim, msg_tracing, save_in_msgq;
    ErtsMessage *sig, ***next_nm_sig;
    ErtsSigRecvTracing tracing;
    ErtsSavedNMSignals delayed_nm_signals = {0};

    ASSERT(!(c_p->sig_qs.flags & (FS_WAIT_HANDLE_SIGS|FS_HANDLING_SIGS)));

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    if (!local_only && !(c_p->sig_qs.flags & FS_FLUSHING_SIGS)) {
        if ((ERTS_PSFLG_NMSG_SIG_IN_Q|ERTS_PSFLG_MSG_SIG_IN_Q) & state) {
            erts_proc_sig_queue_lock(c_p);
            erts_proc_sig_fetch(c_p);
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
        }
    }

    c_p->sig_qs.flags |= FS_HANDLING_SIGS;

    limit = *redsp;
    *redsp = 0;
    yield = 0;
    save_in_msgq = !0;

    if (!c_p->sig_qs.cont) {
        *statep = state;
        ASSERT(!(c_p->sig_qs.flags & FS_WAIT_HANDLE_SIGS));
        c_p->sig_qs.flags &= ~FS_HANDLING_SIGS;
        return !0;
    }

    if (state & ERTS_PSFLG_EXITING) {
        *statep = state;
        ASSERT(!(c_p->sig_qs.flags & FS_WAIT_HANDLE_SIGS));
        c_p->sig_qs.flags &= ~FS_HANDLING_SIGS;
        return 0;
    }

    ERTS_HDBG_PRIVQ_LEN(c_p);

    next_nm_sig = &c_p->sig_qs.nmsigs.next;

    setup_tracing_state(c_p, &tracing);
    msg_tracing = tracing.messages.active;

    limit *= ERTS_SIG_REDS_CNT_FACTOR;
    abs_lim = ERTS_SIG_REDS_CNT_FACTOR*max_reds;
    if (limit > abs_lim)
        limit = abs_lim;

    cnt = 0;

    do {

        if (msg_tracing) {
	    int tres;
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
	    tres = handle_msg_tracing(c_p, &tracing, next_nm_sig);
            if (tres != 0) {
                ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
		if (tres < 0)
		    yield = !0;
                break; /* tracing limit or end... */
            }
#ifdef DEBUG
            if (*next_nm_sig) {
                ErtsNonMsgSignal *nm_sig = (ErtsNonMsgSignal *) **next_nm_sig;
                ASSERT(nm_sig);
                ASSERT(ERTS_SIG_IS_NON_MSG_TAG(nm_sig->tag));
                ASSERT(nm_sig->mlenoffs == 0);
            }
#endif
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
        }

        if (!*next_nm_sig)
            break;

        sig = **next_nm_sig;

        ASSERT(sig);
        ASSERT(ERTS_SIG_IS_NON_MSG(sig));
        ASSERT(((ErtsNonMsgSignal *) sig)->mlenoffs >= 0);

        tag = ((ErtsNonMsgSignal *) sig)->tag;
        c_p->sig_qs.mq_len += ((ErtsNonMsgSignal *) sig)->mlenoffs;
        erts_chk_sys_mon_long_msgq_on(c_p);

        switch (ERTS_PROC_SIG_OP(tag)) {

        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED: {
            int exited;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            cnt += handle_exit_signal(c_p, &tracing, sig,
                                      next_nm_sig, &exited);

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            if (exited)
                goto stop; /* terminated by signal */
            /* ignored or converted to exit message... */
            break;
        }

        case ERTS_SIG_Q_OP_MONITOR_DOWN: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);
            ErtsExitSignalData *xsigd = NULL;
            ErtsMonitorData *mdp = NULL;
            ErtsMonitor *omon = NULL, *tmon = NULL;
            int prio = 0;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            switch (type) {
            case ERTS_MON_TYPE_DIST_PROC:
            case ERTS_MON_TYPE_DIST_PORT:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_PORT:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    omon = &mdp->origin;
                    cnt += convert_to_down_message(c_p, &tracing, &prio,
                                                   sig, mdp, &omon,
                                                   type, next_nm_sig);
                }
                break;
            case ERTS_SIG_Q_TYPE_GEN_EXIT:
                xsigd = get_exit_signal_data(sig);

                /* This GEN_EXIT was received from another node, decode the exit reason */
                if (ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig))
                    if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, sig, 1))
                        break; /* Decode failed, just remove signal */

                omon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p),
                                                xsigd->u.ref);
                if (omon) {
                    ASSERT(erts_monitor_is_origin(omon));
                    if (ERTS_ML_GET_TYPE(omon) == ERTS_MON_TYPE_ALIAS) {
                        omon = NULL;
                        break;
                    }
                    if (omon->flags & ERTS_ML_FLG_SPAWN_PENDING) {
                        handle_missing_spawn_reply(c_p, omon);
                        /*
                         * We leave the pending spawn monitor as is,
                         * so that the nodedown will trigger an error
                         * spawn_reply...
                         */
                        omon = NULL; 
                        cnt += 4;
                        break;
                    }
                    mdp = erts_monitor_to_data(omon);
                    if (ERTS_ML_GET_TYPE(omon) == ERTS_MON_TYPE_DIST_PROC) {
                        if (erts_monitor_dist_delete(&mdp->u.target))
                            tmon = &mdp->u.target;
                    }
                    ASSERT(!(omon->flags & ERTS_ML_FLGS_SPAWN));
                    prio = !!(omon->flags & ERTS_ML_FLG_PRIO_ML);
                    cnt += convert_prepared_down_message(c_p, &tracing, prio, sig,
                                                         xsigd->message,
                                                         next_nm_sig);
                    if (omon->flags & ERTS_ML_FLG_TAG) {
                        Eterm *tpl, *tag_storage;
                        ASSERT(is_tuple_arity(xsigd->message, 5));
                        tpl = tuple_val(xsigd->message);
                        ASSERT(tpl[1] == am_DOWN);
                        if (mdp->origin.flags & ERTS_ML_FLG_EXTENDED)
                            tag_storage = &((ErtsMonitorDataExtended *) mdp)->heap[0];
                        else
                            tag_storage = &((ErtsMonitorDataTagHeap *) mdp)->heap[0];
                        tpl[1] = save_heap_frag_eterm(c_p, sig, tag_storage);
                    }
                }
                break;
            case ERTS_MON_TYPE_NODE:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    omon = &mdp->origin;
                    cnt += handle_nodedown(c_p, &tracing, sig, mdp, next_nm_sig);
                }
                break;
            case ERTS_MON_TYPE_SUSPEND:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    omon = &mdp->origin;
                    remove_nm_sig(c_p, sig, next_nm_sig);
                }
                break;
            default:
                ERTS_INTERNAL_ERROR("invalid monitor type");
                break;
            }

            if (!omon) {
                ASSERT(!prio);
                remove_nm_sig(c_p, sig, next_nm_sig);
                if (xsigd) {
                    sig->next = NULL;
                    erts_cleanup_messages(sig);
                }
                if (tmon)
                    erts_monitor_release(tmon);
            }
            else {
                switch (omon->flags & ERTS_ML_STATE_ALIAS_MASK) {
                case ERTS_ML_STATE_ALIAS_UNALIAS: {
                    Uint32 add_flags;
                    ErtsMonitorData *amdp;
                    ASSERT(is_internal_pid_ref(mdp->ref));
                    amdp = erts_monitor_create(ERTS_MON_TYPE_ALIAS,
                                               mdp->ref, c_p->common.id,
                                               NIL, NIL, THE_NON_VALUE);
                    add_flags = omon->flags & (ERTS_ML_STATE_ALIAS_MASK
                                               | ERTS_ML_FLG_PRIO_ALIAS);
                    amdp->origin.flags |= add_flags;
                    omon->flags &= ~(ERTS_ML_STATE_ALIAS_MASK
                                     | ERTS_ML_FLG_PRIO_ALIAS);
                    erts_monitor_tree_replace(&ERTS_P_MONITORS(c_p),
                                              omon,
                                              &amdp->origin);
                    break;
                }
                case ERTS_ML_STATE_ALIAS_ONCE:
                case ERTS_ML_STATE_ALIAS_DEMONITOR:
                    ASSERT(is_internal_pid_ref(mdp->ref));
                    /* fall through... */
                default:
                    if (type != ERTS_MON_TYPE_NODE)
                        erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), omon);
                    break;
                }

                if (prio) {
                    /*
                     * This needs to be done after the message has been
                     * inserted; otherwise, the prio message might not be
                     * prioritized due to the prio queue being removed prior
                     * to the message insertion.
                     */
                    erts_proc_sig_prio_item_deleted(
                        c_p, ERTS_PRIO_ITEM_TYPE_MONITOR);
                }

                if (tmon)
                    erts_monitor_release_both(mdp);
                else
                    erts_monitor_release(omon);
            }

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);
            ErtsMonitor *mon;
            Eterm msg;
            Eterm key;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            key = get_persist_mon_msg(sig, &msg);

            cnt++;
            mon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), key);
            if (mon) {
                ASSERT(erts_monitor_is_origin(mon));
                handle_persistent_mon_msg(c_p, &tracing, type, mon, sig,
                                          msg, next_nm_sig);

                if ((mon->flags & ERTS_ML_STATE_ALIAS_MASK)
                    == ERTS_ML_STATE_ALIAS_ONCE) {
                    mon->flags &= ~ERTS_ML_STATE_ALIAS_MASK;
                }
            }
            else {
                cnt++;
                remove_nm_sig(c_p, sig, next_nm_sig);
                sig->next = NULL;
                erts_cleanup_messages(sig);
            }

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_MONITOR: {
            ErtsMonitor *mon = (ErtsMonitor *) sig;

            ASSERT(erts_monitor_is_target(mon));
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);

            if (ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_DIST_PROC)
                erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), mon);
            else {
                erts_monitor_list_insert(&ERTS_P_LT_MONITORS(c_p), mon);
                if (ERTS_ML_GET_TYPE(mon) == ERTS_MON_TYPE_SUSPEND)
                    handle_suspend(c_p, mon, &yield);
            }
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            cnt += 2;
            break;
        }

        case ERTS_SIG_Q_OP_DEMONITOR: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);

            if (type == ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR) {
                ErtsMonitor *tmon;
                ErtsSigDistProcDemonitor *dmon;
                dmon = (ErtsSigDistProcDemonitor *) sig;
                tmon = erts_monitor_tree_lookup(ERTS_P_MONITORS(c_p), dmon->ref);
                destroy_dist_proc_demonitor(dmon);
                cnt++;
                if (tmon) {
                    ErtsMonitorData *mdp = erts_monitor_to_data(tmon);
                    ASSERT(erts_monitor_is_target(tmon));
                    erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), tmon);
                    if (!erts_monitor_dist_delete(&mdp->origin))
                        erts_monitor_release(tmon);
                    else
                        erts_monitor_release_both(mdp);
                    cnt += 2;
                }
            }
            else {
                ErtsMonitor *omon = (ErtsMonitor *) sig;
                ErtsMonitorData *mdp = erts_monitor_to_data(omon);
                ASSERT(ERTS_ML_GET_TYPE(omon) == type);
                ASSERT(erts_monitor_is_origin(omon));
                ASSERT(!erts_monitor_is_in_table(omon));
                if (!erts_monitor_is_in_table(&mdp->u.target))
                    erts_monitor_release(omon);
                else {
                    ErtsMonitor *tmon = &mdp->u.target;
                    ASSERT(ERTS_ML_GET_TYPE(tmon) == type);
                    if (type == ERTS_MON_TYPE_DIST_PROC)
                        erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p), tmon);
                    else {
                        erts_monitor_list_delete(&ERTS_P_LT_MONITORS(c_p), tmon);
                        switch (type) {
                        case ERTS_MON_TYPE_RESOURCE:
                            erts_nif_demonitored((ErtsResource *) tmon->other.ptr);
                            cnt++;
                            break;
                        case ERTS_MON_TYPE_SUSPEND: {
                            ErtsMonitorSuspend *msp;
                            erts_aint_t mstate;
                            msp = (ErtsMonitorSuspend *) erts_monitor_to_data(tmon);
                            mstate = erts_atomic_read_band_acqb(
                                &msp->state, ~ERTS_MSUSPEND_STATE_FLG_ACTIVE);
                            if (mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE) {
                                erts_resume(c_p, ERTS_PROC_LOCK_MAIN);
                                erts_resume_paused_proc_timer(c_p);
                            }
                            break;
                        }
                        default:
                            break;
                        }
                    }
                    erts_monitor_release_both(mdp);
                    cnt++;
                }
                cnt++;
            }
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_LINK: {
            ErtsLink *lnk, *nlnk = (ErtsLink *) sig;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);
            lnk = erts_link_tree_lookup_insert(&ERTS_P_LINKS(c_p), nlnk);
            if (!lnk) {
                if (tracing.procs)
                    getting_linked(c_p, nlnk->other.item);
            }
            else {
                /* Already linked or unlinking... */
                ASSERT(ERTS_ML_GET_TYPE(nlnk) != ERTS_LNK_TYPE_DIST_PORT);
                if (ERTS_ML_GET_TYPE(nlnk) != ERTS_LNK_TYPE_DIST_PROC)
                    erts_link_internal_release(nlnk);
                else {
                    ErtsELink *elnk;
                    ErtsLink *dlnk = erts_link_to_other(nlnk, &elnk);
                    if (erts_link_dist_delete(dlnk))
                        erts_link_release_both(&elnk->ld);
                    else
                        erts_link_release(nlnk);
                }
            }

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_UNLINK: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);
            ErtsLink *llnk;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
                ErtsSigDistUnlinkOp *sdulnk = (ErtsSigDistUnlinkOp *) sig;
                ASSERT(is_external_pid(sdulnk->remote));
                llnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p), sdulnk->remote);
                if (llnk) {
                    ErtsELink *elnk;
                    ErtsLink *dlnk = erts_link_to_other(llnk, &elnk);
                    if (!elnk->unlinking) {
                        erts_link_tree_delete(&ERTS_P_LINKS(c_p), llnk);
                        if (erts_link_dist_delete(dlnk))
                            erts_link_release_both(&elnk->ld);
                        else
                            erts_link_release(llnk);
                        cnt += 8;
                        if (tracing.procs)
                            getting_unlinked(c_p, sdulnk->remote);
                    }
                }
                reply_dist_unlink_ack(c_p, sdulnk);
                cnt++;
            }
            else {
                ErtsSigUnlinkOp *sulnk = (ErtsSigUnlinkOp *) sig;
                llnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p),
                                             sulnk->from);
                if (llnk && !((ErtsILink *) llnk)->unlinking) {
                    if (tracing.procs)
                        getting_unlinked(c_p, sulnk->from);
                    erts_link_tree_delete(&ERTS_P_LINKS(c_p), llnk);
                    erts_link_release(llnk);
                    cnt += 4;
                }
                if (is_internal_pid(sulnk->from)) {
                    erts_proc_sig_send_unlink_ack(&c_p->common, c_p->common.id,
                                                  sulnk);
                } else {
                    Port *prt;
                    ASSERT(is_internal_port(sulnk->from));
                    prt = erts_port_lookup(sulnk->from,
                                           ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
                    if (prt)
                        erts_port_unlink_ack(c_p, prt, sulnk);
                    else
                        erts_proc_sig_destroy_unlink_op(sulnk);
                }
            }

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_UNLINK_ACK: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);
            
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
                ErtsSigDistUnlinkOp *sdulnk;
                ErtsLink *lnk;
                sdulnk = (ErtsSigDistUnlinkOp *) sig;
                lnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p),
                                            sdulnk->remote);
                if (lnk) {
                    ErtsELink *elnk = erts_link_to_elink(lnk);
                    if (elnk->unlinking == sdulnk->id) {
                        erts_link_tree_delete(&ERTS_P_LINKS(c_p), lnk);
                        if (erts_link_dist_delete(&elnk->ld.dist))
                            erts_link_release_both(&elnk->ld);
                        else
                            erts_link_release(lnk);
                        cnt += 8;
                    }
                }
                destroy_sig_dist_unlink_op(sdulnk);
            }
            else {
                ErtsSigUnlinkOp *sulnk;
                ErtsILink *ilnk;

                sulnk = (ErtsSigUnlinkOp *) sig;
                ilnk = (ErtsILink *) erts_link_tree_lookup(ERTS_P_LINKS(c_p),
                                                           sulnk->from);

                if (ilnk && ilnk->unlinking == sulnk->id) {
                    erts_link_tree_delete(&ERTS_P_LINKS(c_p), &ilnk->link);
                    erts_link_internal_release(&ilnk->link);
                    cnt += 4;
                }
                erts_proc_sig_destroy_unlink_op(sulnk);
            }
            
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_GROUP_LEADER: {
            ErtsSigGroupLeader *sgl = (ErtsSigGroupLeader *) sig;
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            remove_nm_sig(c_p, sig, next_nm_sig);
            handle_group_leader(c_p, sgl);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_IS_ALIVE:
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            remove_nm_sig(c_p, sig, next_nm_sig);
            is_alive_response(c_p, sig, !0);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;

        case ERTS_SIG_Q_OP_PROCESS_INFO:
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            handle_process_info(c_p, &tracing, sig, next_nm_sig, !0);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;

        case ERTS_SIG_Q_OP_SYNC_SUSPEND:
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            remove_nm_sig(c_p, sig, next_nm_sig);
            handle_sync_suspend(c_p, sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;

        case ERTS_SIG_Q_OP_RPC:
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            remove_nm_sig(c_p, sig, next_nm_sig);
            cnt += handle_rpc(c_p, (ErtsProcSigRPC *) sig, cnt,
                              limit, &yield);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;

        case ERTS_SIG_Q_OP_ADJ_MSGQ: {
            int adj_limit, adj_cnt, min_adj_limit;
            /*
             * This may require a substantial amount of work and we
             * want to get it over and done with in a reasonable
             * amount of time, so we bump up the limit for it a bit...
             */
            min_adj_limit = ERTS_SIG_REDS_CNT_FACTOR*CONTEXT_REDS/6;
            if (sig->next)
                adj_limit = min_adj_limit;
            else {
                adj_limit = limit - cnt;
                if (adj_limit < min_adj_limit)
                    adj_limit = min_adj_limit;
            }
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
	    switch (ERTS_PROC_SIG_TYPE(tag)) {
	    case ERTS_SIG_Q_TYPE_CLA:
		adj_cnt = handle_cla(c_p, sig, next_nm_sig, 0, adj_limit,
                                     &delayed_nm_signals);
		break;
	    case ERTS_SIG_Q_TYPE_OFF_HEAP:
		adj_cnt = handle_move_msgq_off_heap(c_p, sig, next_nm_sig,
                                                    0, adj_limit,
                                                    &delayed_nm_signals);
		break;
	    default:
		ERTS_INTERNAL_ERROR("Invalid adjust-message-queue signal type");
		break;
	    }
            cnt += adj_cnt;
            limit += adj_cnt;
            if (limit > abs_lim)
                abs_lim = limit;
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

	case ERTS_SIG_Q_OP_FLUSH:
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
	    ASSERT(c_p->sig_qs.flags & FS_FLUSHING_SIGS);
	    c_p->sig_qs.flags |= FS_FLUSHED_SIGS;
            remove_nm_sig(c_p, sig, next_nm_sig);
	    erts_free(ERTS_ALC_T_SIG_DATA, sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
	    /*
	     * The caller has been exclusively handling signals until this
	     * point. Break out and let the process continue with other
	     * things as well...
	     */
	    goto stop;

        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            msg_tracing = handle_trace_change_state(c_p, &tracing,
                                                    type, sig,
                                                    next_nm_sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            
            break;
        }
            
        case ERTS_SIG_Q_OP_DIST_SPAWN_REPLY: {
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            cnt += handle_dist_spawn_reply(c_p, &tracing, sig, next_nm_sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_ALTACT_MSG: {
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            cnt += handle_altact_msg(c_p, &tracing, sig, next_nm_sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_RECV_MARK: {
            ErtsRecvMarker *markp = (ErtsRecvMarker *) sig;
            ASSERT(markp->in_sigq);
            ASSERT(markp->mark_type == ERTS_RECV_MARKER_TYPE_RECV);

            if (markp->in_sigq < 0) {
                /* Marked for removal... */
                if (markp->set_save) {
                    c_p->sig_qs.save = *next_nm_sig;
                    ASSERT(c_p->sig_qs.recv_mrk_blk);
                    ASSERT(c_p->sig_qs.recv_mrk_blk->pending_set_save_ix
			   == ERTS_RECV_MARKER_IX__(c_p->sig_qs.recv_mrk_blk,
						    markp));
                    ASSERT(ERTS_MQ_GET_SAVE_INFO(c_p) == FS_SET_SAVE_INFO_LAST
                           /* mark due to erts_msgq_set_save_end() */);
                    c_p->sig_qs.recv_mrk_blk->pending_set_save_ix = -1;
		    save_in_msgq = 0;
                    /* all recv-markers are outside of prio queue */
                    c_p->sig_qs.flags &= ~FS_PRIO_MQ_SAVE;
                }
                markp->in_prioq = markp->in_msgq = markp->in_sigq = markp->set_save = 0;
                remove_nm_sig(c_p, sig, next_nm_sig);
		recv_marker_deallocate(c_p, markp);
            }
            else {
                ASSERT(!markp->in_prioq); /* It is not possible that a reference
                                             corresponding to this marker have
                                             been seen in a message in the prio
                                             queue... */
                markp->prev_next = *next_nm_sig;
                ASSERT(*markp->prev_next == sig);
                *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;

		ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(sig);

                markp->in_msgq = !0;
                if (markp->set_save) {
                    c_p->sig_qs.save = &markp->sig.common.next;
                    markp->set_save = 0;
                    ASSERT(c_p->sig_qs.recv_mrk_blk);
                    ASSERT(c_p->sig_qs.recv_mrk_blk->pending_set_save_ix
			   == ERTS_RECV_MARKER_IX__(c_p->sig_qs.recv_mrk_blk,
						    markp));
                    ASSERT(ERTS_MQ_GET_SAVE_INFO(c_p) == FS_SET_SAVE_INFO_RCVM);
                    ASSERT(c_p->sig_qs.recv_mrk_blk->pending_set_save_ix
                           == c_p->sig_qs.recv_mrk_blk->set_save_ix);
                    c_p->sig_qs.recv_mrk_blk->pending_set_save_ix = -1;
		    save_in_msgq = 0;
                    /* all recv-markers are outside of prio queue */
                    c_p->sig_qs.flags &= ~FS_PRIO_MQ_SAVE;
                }
            }

            break;
        }
#if ERTS_POLL_USE_SCHEDULER_POLLING
        case ERTS_SIG_Q_OP_NIF_SELECT: {

            Eterm msg = erts_io_handle_nif_select(sig);

            convert_prepared_sig_to_msg(c_p, &tracing, sig, msg, am_undefined, next_nm_sig);

            cnt += 4;

            erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);

            break;
        }
#endif

        default:
            ERTS_INTERNAL_ERROR("Unknown signal");
            break;
        }

        cnt++;

    } while (cnt <= limit
	     || stretch_limit(c_p, &tracing, abs_lim, &limit, save_in_msgq));

stop: {
        int res;

        if (c_p->sig_qs.save == &c_p->sig_qs.cont)
            c_p->sig_qs.save = c_p->sig_qs.last;

        if (ERTS_UNLIKELY(msg_tracing != 0)) {
            /*
             * All messages that has been traced should
             * be moved to inner queue. Next signal in
             * middle queue should either be next message
             * to trace or next non-message signal.
             */
            ASSERT(tracing.messages.next);

            /*
             * If we yielded right after we handled a receive
             * marker, we might point to a receive marker that
             * should be included in the message queue. Adjust
             * 'tracing.messages.next' if that is the case...
             */
            if (*tracing.messages.next
                && ERTS_SIG_IS_RECV_MARKER(*tracing.messages.next)
                && ((ErtsRecvMarker *) *tracing.messages.next)->in_msgq) {

                tracing.messages.next = &(*tracing.messages.next)->next;

                /* There can only be one such receive marker... */
                ASSERT(!(*tracing.messages.next
                         && ERTS_SIG_IS_RECV_MARKER(*tracing.messages.next)
                         && ((ErtsRecvMarker *) *tracing.messages.next)->in_msgq));
            }
            
            if (*next_nm_sig) {
                if (*next_nm_sig == tracing.messages.next)
                    *next_nm_sig = &c_p->sig_qs.cont;
                if (c_p->sig_qs.nmsigs.last == tracing.messages.next)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
		state = erts_atomic32_read_nob(&c_p->state);
            }
            else {
                ASSERT(!c_p->sig_qs.nmsigs.next);
                c_p->sig_qs.nmsigs.last = NULL;
                state = erts_atomic32_read_band_nob(&c_p->state,
                                                    ~ERTS_PSFLG_SIG_Q);
                state &= ~ERTS_PSFLG_SIG_Q;
            }

            if (tracing.messages.next != &c_p->sig_qs.cont) {
                if (ERTS_SIG_IS_RECV_MARKER(c_p->sig_qs.cont)) {
                    ErtsRecvMarker *markp = (ErtsRecvMarker *) c_p->sig_qs.cont;
                    markp->prev_next = c_p->sig_qs.last;
                }

                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = tracing.messages.next;

                c_p->sig_qs.cont = *tracing.messages.next;
                if (!c_p->sig_qs.cont)
                    c_p->sig_qs.cont_last = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = NULL;
            }

            res = !c_p->sig_qs.cont;
        }
        else if (*next_nm_sig) {
            ErtsSignal *next;
            /*
             * All messages prior to next non-message
             * signal should be moved to inner queue.
             * Next non-message signal to handle should
             * be first in middle queue.
             */
            next = (ErtsSignal *) **next_nm_sig;

            ASSERT(next);
            ASSERT(ERTS_SIG_IS_NON_MSG(next));
            ASSERT(next->nm_sig.mlenoffs >= 0);
            c_p->sig_qs.mq_len += next->nm_sig.mlenoffs;
            next->nm_sig.mlenoffs = 0;
            erts_chk_sys_mon_long_msgq_on(c_p);

            if (*next_nm_sig != &c_p->sig_qs.cont) {
                if (ERTS_SIG_IS_RECV_MARKER(c_p->sig_qs.cont)) {
                    ErtsRecvMarker *markp = (ErtsRecvMarker *) c_p->sig_qs.cont;
                    markp->prev_next = c_p->sig_qs.last;
                }

                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = *next_nm_sig;

                c_p->sig_qs.cont = **next_nm_sig;
                if (c_p->sig_qs.nmsigs.last == *next_nm_sig)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
                *next_nm_sig = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = NULL;
            }

            ASSERT(c_p->sig_qs.cont);

            state = erts_atomic32_read_nob(&c_p->state);

            res = 0;
        }
        else {
            /*
             * All non-message signals handled. All
             * messages should be moved to inner queue.
             * Middle queue should be empty.
             */
            ASSERT(!c_p->sig_qs.nmsigs.next);
            c_p->sig_qs.nmsigs.last = NULL;

            ASSERT(c_p->sig_qs.mlenoffs >= 0);
            c_p->sig_qs.mq_len += c_p->sig_qs.mlenoffs;
            c_p->sig_qs.mlenoffs = 0;
            erts_chk_sys_mon_long_msgq_on(c_p);

            if (c_p->sig_qs.cont_last != &c_p->sig_qs.cont) {
                if (ERTS_SIG_IS_RECV_MARKER(c_p->sig_qs.cont)) {
                    ErtsRecvMarker *markp = (ErtsRecvMarker *) c_p->sig_qs.cont;
                    markp->prev_next = c_p->sig_qs.last;
                }

                ASSERT(!*c_p->sig_qs.last);
                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = c_p->sig_qs.cont_last;
                ASSERT(!*c_p->sig_qs.last);

                c_p->sig_qs.cont_last = &c_p->sig_qs.cont;
                c_p->sig_qs.cont = NULL;
            }

            ASSERT(!c_p->sig_qs.cont);

            state = erts_atomic32_read_band_nob(&c_p->state,
                                                ~ERTS_PSFLG_SIG_Q);
            state &= ~ERTS_PSFLG_SIG_Q;
            res = !0;
        }

	if (!(state & (ERTS_PSFLG_SIG_Q
                       | ERTS_PSFLG_NMSG_SIG_IN_Q
                       | ERTS_PSFLG_MSG_SIG_IN_Q))) {
	    /*
	     * We know we do not have any outstanding signals
	     * from ourselves...
	     */
	    (void) erts_atomic32_read_band_nob(&c_p->xstate,
                                               ~ERTS_PXSFLG_MAYBE_SELF_SIGS);
	}

        if (delayed_nm_signals.first) {
            /*
             * We do this after clearing ERTS_PXSFLG_MAYBE_SELF_SIGS
             * since there currently are no signals that can be delayed
             * that should be counted as originating from the process
             * itself. If such signals appear in the future this has to
             * be accounted for...
             *
             * The adjust message queue data "signal" does originate from
             * the process itself, but it is not conseptually a signal.
             */
            state = restore_delayed_nm_signals(c_p, &delayed_nm_signals);
        }

	*statep = state;

        /* Ensure that 'save' doesn't point to a receive marker... */
        if (*c_p->sig_qs.save
            && ERTS_SIG_IS_RECV_MARKER(*c_p->sig_qs.save)) {
            c_p->sig_qs.save = erts_msgq_pass_recv_markers(c_p,
							   c_p->sig_qs.save);
        }

        if (c_p->sig_qs.mq_len && (c_p->flags & F_HIBERNATED)) {
            erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
        }

        ERTS_HDBG_PRIVQ_LEN(c_p);

        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

        *redsp = cnt/ERTS_SIG_REDS_CNT_FACTOR + 1;

        if (yield) {
            int vreds = max_reds - *redsp;
            if (vreds > 0) {
                ErtsSchedulerData *esdp = erts_get_scheduler_data();
                esdp->virtual_reds += vreds;
            }
            *redsp = max_reds;
        }

        if (c_p->sig_qs.flags & FS_WAIT_HANDLE_SIGS)
            wake_handle_signals(c_p);
        else
            c_p->sig_qs.flags &= ~FS_HANDLING_SIGS;

        return res;
    }
}

static int
stretch_limit(Process *c_p, ErtsSigRecvTracing *tp,
              int abs_lim, int *limp, int save_in_msgq)
{
    ErtsMessage **sigpp;
    int lim, in_msgq;
    /*
     * Stretch limit up to a maximum of 'abs_lim' if
     * there currently are no messages available to
     * inspect by 'receive' and it might be possible
     * to get messages available by processing
     * signals (or trace messages).
     */

    lim = *limp;
    ASSERT(abs_lim >= lim);
    if (abs_lim == lim)
        return 0;

    /*
     * We cannot use erts_msgq_peek_msg() to inspect
     * save pointer here! At this point save pointer has
     * not been moved passed possible receive markers...
     *
     * Also note that the save pointer might point into
     * message queue as well as middle signal queue (if a
     * receive marker with 'set_save' set just arrived).
     */
    if (c_p->sig_qs.save == c_p->sig_qs.last) {
	sigpp = &c_p->sig_qs.cont;
	in_msgq = 0;
    }
    else {
	sigpp = c_p->sig_qs.save;
	in_msgq = save_in_msgq;
    }

    while (!0) {
	Eterm tag;
	if (!(*sigpp))
	    return 0; /* No signals to process available... */

	if (!in_msgq)
	    break;

	if (tp->messages.next == sigpp)
	    break;

	tag = ((ErtsSignal *) *sigpp)->common.tag;

	if (ERTS_SIG_IS_MSG_TAG(tag))
	    return 0; /* Have message to inspect... */

	ASSERT(tag == ERTS_RECV_MARKER_TAG);

	/*
	 * Pass the recv marker without punishing it
	 * by increasing the 'pass' field...
	 */
	sigpp = &(*sigpp)->next;
	if (sigpp == c_p->sig_qs.last) {
	    sigpp = &c_p->sig_qs.cont;
	    in_msgq = 0;
	}
    }

    /*
     * Stretch the limit so we can process some more signals
     * in order to try to make messages available in message
     * queue...
     */
    lim += ERTS_SIG_REDS_CNT_FACTOR*100;
    if (lim > abs_lim)
        lim = abs_lim;
    *limp = lim;
    return !0;
}

static void destroy_prio_q_info(Process *c_p,
                                ErtsPrioQInfo *pq_info,
                                int terminating);

int
erts_proc_sig_handle_exit(Process *c_p, Sint *redsp,
                          ErtsProcExitContext *pe_ctxt_p)
{
    int yield = 0;
    int cnt;
    Sint limit;
    ErtsMessage *sig, ***next_nm_sig;

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    if (c_p->sig_qs.flags & (FS_PRIO_MQ|FS_PRIO_MQ_PENDING_RM)) {
        ErtsPrioQInfo *pq_info = get_prio_queue_info(c_p);
        destroy_prio_q_info(c_p, pq_info, !0);
    }

    limit = *redsp;
    limit *= ERTS_SIG_REDS_CNT_FACTOR;

    *redsp = 1;

    next_nm_sig = &c_p->sig_qs.nmsigs.next;

    if (!*next_nm_sig) {
        ASSERT(!c_p->sig_qs.nmsigs.last);
        return !0; /* done... */
    }

    cnt = 0;

    ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, NULL, next_nm_sig);

    do {
        Eterm tag;
        Uint16 type;
        int op;

        sig = **next_nm_sig;

        ASSERT(sig);
        ASSERT(ERTS_SIG_IS_NON_MSG(sig));

        tag = ((ErtsSignal *) sig)->common.tag;
        type = ERTS_PROC_SIG_TYPE(tag);
        op = ERTS_PROC_SIG_OP(tag);

        remove_nm_sig(c_p, sig, next_nm_sig);
 
        ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, NULL, next_nm_sig);

        cnt++;

        switch (op) {

        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            switch (type) {
            case ERTS_SIG_Q_TYPE_GEN_EXIT:
                sig->next = NULL;
                erts_cleanup_messages(sig);
                break;
            case ERTS_LNK_TYPE_PORT:
            case ERTS_LNK_TYPE_PROC:
            case ERTS_LNK_TYPE_DIST_PROC:
            case ERTS_LNK_TYPE_DIST_PORT:
                erts_link_release((ErtsLink *) sig);
                break;
            case ERTS_MON_TYPE_PORT:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_DIST_PROC:
            case ERTS_MON_TYPE_DIST_PORT:
            case ERTS_MON_TYPE_NODE:
            case ERTS_MON_TYPE_NODES:
            case ERTS_MON_TYPE_SUSPEND:
                erts_monitor_release((ErtsMonitor *) sig);
                break;
            default:
                ERTS_INTERNAL_ERROR("Unexpected sig type");
                break;
            }
            break;

        case ERTS_SIG_Q_OP_NIF_SELECT:
        case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
        case ERTS_SIG_Q_OP_ALTACT_MSG:
            sig->next = NULL;
            erts_cleanup_messages(sig);
            break;

        case ERTS_SIG_Q_OP_MONITOR: {
            ErtsProcExitContext pectxt = {c_p, am_noproc, NULL, NULL,
                                          NULL, NULL, NIL, 0};
            erts_proc_exit_handle_monitor((ErtsMonitor *) sig,
                                          (void *) &pectxt, -1);
            cnt += 4;
            break;
        }

        case ERTS_SIG_Q_OP_DEMONITOR:
            if (type == ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR)
                destroy_dist_proc_demonitor((ErtsSigDistProcDemonitor *) sig);
            else
                erts_monitor_release((ErtsMonitor *) sig);
            break;

        case ERTS_SIG_Q_OP_LINK: {
            ErtsProcExitContext pectxt = {c_p, am_noproc};
            erts_proc_exit_handle_link((ErtsLink *) sig, (void *) &pectxt, -1);
            break;
        }

        case ERTS_SIG_Q_OP_UNLINK:
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
                reply_dist_unlink_ack(c_p, (ErtsSigDistUnlinkOp *) sig);
            } else if (is_internal_pid(((ErtsSigUnlinkOp *) sig)->from)) {
                erts_proc_sig_send_unlink_ack(&c_p->common, c_p->common.id,
                                              (ErtsSigUnlinkOp *) sig);
            } else {
                Port *prt;
                ASSERT(is_internal_port(((ErtsSigUnlinkOp *) sig)->from));
                prt = erts_port_lookup(((ErtsSigUnlinkOp *) sig)->from,
                                       ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
                if (prt)
                    erts_port_unlink_ack(c_p, prt, (ErtsSigUnlinkOp *) sig);
                else
                    erts_proc_sig_destroy_unlink_op((ErtsSigUnlinkOp *) sig);
            }
            break;

        case ERTS_SIG_Q_OP_UNLINK_ACK:
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK)
                destroy_sig_dist_unlink_op((ErtsSigDistUnlinkOp *) sig);
            else
                erts_proc_sig_destroy_unlink_op((ErtsSigUnlinkOp *) sig);
            break;

        case ERTS_SIG_Q_OP_GROUP_LEADER: {
            ErtsSigGroupLeader *sgl = (ErtsSigGroupLeader *) sig;
            handle_group_leader(c_p, sgl);
            break;
        }

        case ERTS_SIG_Q_OP_IS_ALIVE:
            is_alive_response(c_p, sig, 0);
            break;

        case ERTS_SIG_Q_OP_PROCESS_INFO:
            handle_process_info(c_p, NULL, sig, next_nm_sig, 0);
            break;

        case ERTS_SIG_Q_OP_SYNC_SUSPEND:
            handle_sync_suspend(c_p, sig);
            break;

        case ERTS_SIG_Q_OP_RPC:
            yield = 0;
            cnt += handle_rpc(c_p, (ErtsProcSigRPC *) sig,
                              cnt, limit, &yield);
            break;

        case ERTS_SIG_Q_OP_DIST_SPAWN_REPLY: {
            cnt += handle_dist_spawn_reply_exiting(c_p, sig, pe_ctxt_p);

            break;
        }

        case ERTS_SIG_Q_OP_ADJ_MSGQ:
	    switch (ERTS_PROC_SIG_TYPE(tag)) {
	    case ERTS_SIG_Q_TYPE_CLA:
		handle_cla(c_p, sig, next_nm_sig, !0, limit, NULL);
		break;
	    case ERTS_SIG_Q_TYPE_OFF_HEAP:
		handle_move_msgq_off_heap(c_p, sig, next_nm_sig, !0,
                                          limit, NULL);
		break;
	    default:
		ERTS_INTERNAL_ERROR("Invalid adjust-message-queue signal type");
		break;
	    }
            break;

	case ERTS_SIG_Q_OP_FLUSH:
	    ASSERT(c_p->sig_qs.flags & FS_FLUSHING_SIGS);
	    c_p->sig_qs.flags |= FS_FLUSHED_SIGS;
	    erts_free(ERTS_ALC_T_SIG_DATA, sig); 
	    break;

        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
            destroy_trace_info((ErtsSigTraceInfo *) sig);
            break;

        case ERTS_SIG_Q_OP_RECV_MARK: {
            ErtsRecvMarker *markp = (ErtsRecvMarker *) sig;
            ASSERT(markp->mark_type == ERTS_RECV_MARKER_TYPE_RECV);
            markp->in_msgq = markp->in_sigq = markp->set_save = 0;
            recv_marker_deallocate(c_p, markp);
            break;
        }

        default:
            ERTS_INTERNAL_ERROR("Unknown signal");
            break;
        }

    } while (cnt <= limit && !yield && *next_nm_sig);

    *redsp += cnt / ERTS_SIG_REDS_CNT_FACTOR;

    if (*next_nm_sig)
        return 0;

    ASSERT(!c_p->sig_qs.nmsigs.next);
    c_p->sig_qs.nmsigs.last = NULL;
    (void) erts_atomic32_read_band_nob(&c_p->state,
                                       ~ERTS_PSFLG_SIG_Q);
    return !0;
}

static ERTS_INLINE void
clear_seq_trace_token(ErtsMessage *sig)
{
    if (ERTS_SIG_IS_MSG((ErtsSignal *) sig))
        ERTS_CLEAR_SEQ_TOKEN(sig);
    else {
        Uint tag = ((ErtsSignal *) sig)->common.tag;

        switch (ERTS_PROC_SIG_OP(tag)) {

        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            switch (ERTS_PROC_SIG_TYPE(tag)) {
            case ERTS_SIG_Q_TYPE_GEN_EXIT:
                clear_seq_token_gen_exit(sig);
                break;
            case ERTS_LNK_TYPE_PORT:
            case ERTS_LNK_TYPE_PROC:
            case ERTS_LNK_TYPE_DIST_PROC:
            case ERTS_LNK_TYPE_DIST_PORT:
            case ERTS_MON_TYPE_PORT:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_DIST_PROC:
            case ERTS_MON_TYPE_DIST_PORT:
            case ERTS_MON_TYPE_NODE:
            case ERTS_MON_TYPE_NODES:
            case ERTS_MON_TYPE_SUSPEND:
            case ERTS_MON_TYPE_TIME_OFFSET:
                break;
            default:
                ERTS_INTERNAL_ERROR("Unexpected sig type");
                break;
            }
            break;

        case ERTS_SIG_Q_OP_DIST_SPAWN_REPLY:
            clear_seq_token_spawn_reply(sig);
            break;
        case ERTS_SIG_Q_OP_ALTACT_MSG:
            clear_seq_token_altact_msg(sig);
            break;

        case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
        case ERTS_SIG_Q_OP_MONITOR:
        case ERTS_SIG_Q_OP_DEMONITOR:
        case ERTS_SIG_Q_OP_LINK:
        case ERTS_SIG_Q_OP_UNLINK:
        case ERTS_SIG_Q_OP_UNLINK_ACK:
        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
        case ERTS_SIG_Q_OP_GROUP_LEADER:
        case ERTS_SIG_Q_OP_IS_ALIVE:
        case ERTS_SIG_Q_OP_PROCESS_INFO:
        case ERTS_SIG_Q_OP_SYNC_SUSPEND:
        case ERTS_SIG_Q_OP_RPC:
        case ERTS_SIG_Q_OP_RECV_MARK:
        case ERTS_SIG_Q_OP_ADJ_MSGQ:
	case ERTS_SIG_Q_OP_FLUSH:
        case ERTS_SIG_Q_OP_NIF_SELECT:
            break;

        default:
            ERTS_INTERNAL_ERROR("Unknown signal");
            break;
        }
    }
}

void
erts_proc_sig_clear_seq_trace_tokens(Process *c_p)
{
    int ix;
    ErtsSignalInQueueBufferArray *bap;
    int unget_info;
    ErtsMessage *qs[] = {c_p->sig_qs.first,
                         c_p->sig_qs.cont,
                         c_p->sig_inq.first};

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

    for (ix = 0; ix < sizeof(qs)/sizeof(qs[0]); ix++) {
        ErtsMessage *sigp;
        for (sigp = qs[ix]; sigp; sigp = sigp->next)
            clear_seq_trace_token(sigp);
    }

    bap = erts_proc_sig_queue_get_buffers(c_p, &unget_info);
    if (bap) {
        for (ix = 0; ix < ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS; ix++) {
            ErtsSignalInQueueBuffer* bp = &bap->slots[ix];
            if (bp->b.alive) {
                ErtsMessage *sigp;
                for (sigp = bp->b.queue.first; sigp; sigp = sigp->next)
                    clear_seq_trace_token(sigp);
            }
        }
        erts_proc_sig_queue_unget_buffers(bap, unget_info);
    }
}

Uint
erts_proc_sig_signal_size(ErtsSignal *sig)
{
    Eterm tag;
    Uint16 type;
    int op;
    Uint size = 0;

    ASSERT(sig);
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));

    tag = sig->common.tag;
    type = ERTS_PROC_SIG_TYPE(tag);
    op = ERTS_PROC_SIG_OP(tag);

    switch (op) {
    case ERTS_SIG_Q_OP_EXIT:
    case ERTS_SIG_Q_OP_EXIT_LINKED:
    case ERTS_SIG_Q_OP_MONITOR_DOWN:
        switch (type) {
        case ERTS_SIG_Q_TYPE_GEN_EXIT:
            size = ((ErtsMessage *) sig)->hfrag.alloc_size;
            size *= sizeof(Eterm);
            size += sizeof(ErtsMessage) - sizeof(Eterm);
            break;
        case ERTS_LNK_TYPE_PORT:
        case ERTS_LNK_TYPE_PROC:
        case ERTS_LNK_TYPE_DIST_PROC:
        case ERTS_LNK_TYPE_DIST_PORT:
            size = erts_link_size((ErtsLink *) sig);
            break;
        case ERTS_MON_TYPE_PORT:
        case ERTS_MON_TYPE_PROC:
        case ERTS_MON_TYPE_DIST_PROC:
        case ERTS_MON_TYPE_DIST_PORT:
        case ERTS_MON_TYPE_NODE:
        case ERTS_MON_TYPE_SUSPEND:
            size = erts_monitor_size((ErtsMonitor *) sig);
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected sig type");
            break;
        }
        break;

    case ERTS_SIG_Q_OP_ADJ_MSGQ:
    case ERTS_SIG_Q_OP_SYNC_SUSPEND:
    case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
    case ERTS_SIG_Q_OP_IS_ALIVE:
    case ERTS_SIG_Q_OP_DIST_SPAWN_REPLY:
    case ERTS_SIG_Q_OP_NIF_SELECT:
    {
        ErlHeapFragment *hf;
        size = sizeof(ErtsMessageRef);
        size += ERTS_HEAP_FRAG_SIZE(((ErtsMessage *) sig)->hfrag.alloc_size);
        for (hf = ((ErtsMessage *) sig)->hfrag.next; hf; hf = hf->next)
            size += ERTS_HEAP_FRAG_SIZE(hf->alloc_size);
        break;
    }

    case ERTS_SIG_Q_OP_ALTACT_MSG: {
        ErlHeapFragment *hf;

        size = sizeof(ErtsMessageRef);

        switch (type) {
        case ERTS_SIG_Q_TYPE_OFF_HEAP:
            size += ERTS_HEAP_FRAG_SIZE(((ErtsMessage *) sig)->hfrag.alloc_size);
            hf = ((ErtsMessage *) sig)->hfrag.next;
            if (0) {
            case ERTS_SIG_Q_TYPE_HEAP_FRAG:
                hf = ((ErtsMessage *) sig)->data.heap_frag;
            }
            for (; hf; hf = hf->next)
                size += ERTS_HEAP_FRAG_SIZE(hf->alloc_size);
            break;
        case ERTS_SIG_Q_TYPE_HEAP:
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected sig type");
        }
        break;
    }

    case ERTS_SIG_Q_OP_DEMONITOR:
        if (type == ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR) {
            size = NC_HEAP_SIZE(((ErtsSigDistProcDemonitor *) sig)->ref);
            size--;
            size *= sizeof(Eterm);
            size += sizeof(ErtsSigDistProcDemonitor);
            break;
        }
        /* Fall through... */

    case ERTS_SIG_Q_OP_MONITOR:
        size = erts_monitor_size((ErtsMonitor *) sig);
        break;

    case ERTS_SIG_Q_OP_UNLINK:
    case ERTS_SIG_Q_OP_UNLINK_ACK:
        if (type != ERTS_SIG_Q_TYPE_DIST_LINK)
            size = sizeof(ErtsSigUnlinkOp);
        else {
            size = NC_HEAP_SIZE(((ErtsSigDistUnlinkOp *) sig)->remote);
            size--;
            size *= sizeof(Eterm);
            size += sizeof(ErtsSigDistUnlinkOp);
        }
        break;
    case ERTS_SIG_Q_OP_LINK:
        size = erts_link_size((ErtsLink *) sig);
        break;

    case ERTS_SIG_Q_OP_GROUP_LEADER: {
        ErtsSigGroupLeader *sgl = (ErtsSigGroupLeader *) sig;
        size = size_object(sgl->group_leader);
        size += size_object(sgl->ref);
        size *= sizeof(Eterm);
        size += sizeof(ErtsSigGroupLeader) - sizeof(Eterm);
        break;
    }

    case ERTS_SIG_Q_OP_FLUSH:
	size = sizeof(ErtsNonMsgSignal);
	break;

    case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
        size = sizeof(ErtsSigTraceInfo);
        break;

    case ERTS_SIG_Q_OP_PROCESS_INFO: {
        ErtsProcessInfoSig *pisig = (ErtsProcessInfoSig *) sig;
        size = sizeof(ErtsProcessInfoSig);
        size += (pisig->len - 1) * sizeof(int);
        break;
    }

    case ERTS_SIG_Q_OP_RPC:
        size = sizeof(ErtsProcSigRPC);
        break;

    case ERTS_SIG_Q_OP_RECV_MARK:
        size = sizeof(ErtsRecvMarker);
        break;
        
    default:
        ERTS_INTERNAL_ERROR("Unknown signal");
        break;
    }

    return size;
}

int
erts_proc_sig_receive_helper(Process *c_p,
                             int fcalls,
                             int neg_o_reds,
                             ErtsMessage **msgpp,
                             int *get_outp)
{
    ErtsMessage *msgp;
    int reds, consumed_reds, left_reds, max_reds;

    /*
     * Called from the loop-rec instruction when receive
     * has reached end of inner (private) queue. This function
     * tries to move more messages into the inner queue
     * for the receive to handle. This by, processing the
     * middle (private) queue and/or moving signals from
     * the outer (public) queue into the middle queue.
     *
     * If this function succeeds in making more messages
     * available in the inner queue, *msgpp points to next
     * message. If *msgpp is set to NULL when:
     * -- process became exiting. *get_outp is set to a
     *    value greater than zero.
     * -- process needs to yield. *get_outp is set to a
     *    value less than zero.
     * -- no more messages exist in any of the queues.
     *    *get_outp is set to zero and the message queue
     *    lock remains locked. This so the process can
     *    make its way to the appropriate wait instruction
     *    without racing with new incoming messages.
     */

    ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);
    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    ASSERT(!*msgpp);

    left_reds = fcalls - neg_o_reds;
    consumed_reds = 0;

    while (!0) {
        erts_aint32_t state;

        if (!c_p->sig_qs.cont) {

            consumed_reds += 4;
            left_reds -= 4;
            erts_proc_sig_queue_lock(c_p);
            erts_proc_sig_fetch(c_p);
            /*
             * Messages may have been moved directly to
             * inner queue...
             */
            msgp = erts_msgq_peek_msg(c_p);
            if (msgp) {
                erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
                *get_outp = 0;
                *msgpp = msgp;
                return consumed_reds;
            }

            if (!c_p->sig_qs.cont) {
                /*
                 * No messages! Return with message queue
                 * locked and let the process continue
                 * to wait instruction...
                 */
                *get_outp = 0;
                *msgpp = NULL;

                return consumed_reds;
            }
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

            if (left_reds <= 0)
                break; /* Yield */

            /* handle newly arrived signals... */
        }

        reds = ERTS_SIG_HANDLE_REDS_MAX_PREFERED;
#ifdef DEBUG
        /* test that it works also with very few reds */
        max_reds = left_reds;
        if (reds > left_reds)
            reds = left_reds;
#else
        /* At least work preferred amount of reds... */
        max_reds = left_reds;
        if (max_reds < reds)
            max_reds = reds;
#endif
	state = erts_atomic32_read_nob(&c_p->state);
        (void) erts_proc_sig_handle_incoming(c_p, &state, &reds,
                                             max_reds, !0);
        consumed_reds += reds;
        left_reds -= reds;

        /* we may have exited or suspended by an incoming signal... */

        if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_SUSPENDED)) {
            if (state & ERTS_PSFLG_SUSPENDED)
                break; /* Yield */

            /*
             * Process need to schedule out in order
             * to terminate. Prepare this a bit...
             */
            ASSERT(state & ERTS_PSFLG_EXITING);
            ASSERT(c_p->flags & F_DELAY_GC);

            c_p->flags &= ~F_DELAY_GC;
            c_p->arity = 0;
            c_p->current = NULL;

            *get_outp = 1;
            *msgpp = NULL;

            return consumed_reds;
        }

        msgp = erts_msgq_peek_msg(c_p);
        if (msgp) {
            *get_outp = 0;
            *msgpp = msgp;
            return consumed_reds;
        }

        if (left_reds <= 0)
            break; /* yield */

        /* Go fetch again... */
    }
    
    /* Yield... */

    *get_outp = -1;
    *msgpp = NULL;

    ASSERT(consumed_reds >= (fcalls - neg_o_reds));
    return consumed_reds;
}

static void
init_yield_marker(Process *c_p, ErtsRecvMarker *mrkp)
{
    mrkp->prev_next = NULL;
    mrkp->mark_type = (char) ERTS_RECV_MARKER_TYPE_YIELD;
    mrkp->pass = (char) 100;
    mrkp->set_save = (char) 0;
    mrkp->in_sigq = (char) 0;
    mrkp->in_msgq = (char) 0;
    mrkp->in_prioq = (char) 0;
    mrkp->prev_ix = (char) -100;
    mrkp->next_ix = (char) -100;
#ifdef DEBUG
    mrkp->used = (char) !0;
    mrkp->proc = c_p;
#endif
    mrkp->sig.common.next = NULL;
    mrkp->sig.common.specific.attachment = NULL;
    mrkp->sig.common.tag = ERTS_RECV_MARKER_TAG;
}

static void
remove_yield_marker(Process *c_p, ErtsRecvMarker *mrkp)
{
    ASSERT(mrkp);
    ASSERT(mrkp->mark_type == ERTS_RECV_MARKER_TYPE_YIELD);
    ASSERT(mrkp->in_msgq);
    remove_innerq_sig(c_p, (ErtsMessage *) mrkp, mrkp->prev_next);
    mrkp->in_msgq = 0;
    mrkp->in_sigq = 0;
    mrkp->in_prioq = 0;
    mrkp->prev_next = NULL;
    mrkp->sig.common.next = NULL;
}

static ErtsYieldAdjMsgQ *
create_yield_adj_msgq_data(Process *c_p)
{
    ErtsYieldAdjMsgQ *yp = erts_alloc(ERTS_ALC_T_SIG_YIELD_DATA,
                                      sizeof(ErtsYieldAdjMsgQ));
    init_yield_marker(c_p, &yp->next);
    init_yield_marker(c_p, &yp->last);
    return yp;
}

static ERTS_INLINE void
insert_adj_msgq_yield_markers(Process *c_p,
                              ErtsYieldAdjMsgQ *yp,
                              ErtsMessage **nextpp,
                              ErtsMessage ***next_nm_sig,
                              ErtsSavedNMSignals *saved_sigs)
{
    ErtsMessage *sig, *nextp;

    ASSERT(yp);
    ASSERT(nextpp);
    ASSERT(next_nm_sig && *next_nm_sig && **next_nm_sig);
    ASSERT(!yp->next.in_msgq);

    sig = **next_nm_sig;

    ASSERT(ERTS_PROC_SIG_OP(ERL_MESSAGE_TERM(sig))
           == ERTS_SIG_Q_OP_ADJ_MSGQ);

    /*
     * Insert 'next' yield marker. This is in the inner queue or
     * in the beginning of the middle queue where we've already
     * begun using 'prev_next' pointers for receive markers,
     * so if a receive marker follow, we need to update it.
     */
    yp->next.in_msgq = !0;
    yp->next.in_sigq = !0;
    yp->next.prev_next = nextpp;
    yp->next.sig.common.next = nextp = *nextpp;
    *nextpp = (ErtsMessage *) &yp->next;

    ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(&yp->next);

    if (nextp && ERTS_SIG_IS_RECV_MARKER(nextp)) {
        ErtsRecvMarker *next_mrkp = (ErtsRecvMarker *) nextp;
        next_mrkp->prev_next = &yp->next.sig.common.next;
    }

    if (yp->last.in_msgq) {
        remove_nm_sig(c_p, sig, next_nm_sig);
    }
    else {
        /*
         * Replace adj-msgq signal with 'last' yield marker.
         *
         * This is in the middle queue after the point where
         * we've begun using 'prev_next' pointers for receive
         * markers, so if a receive marker follow, we do not
         * need to adjust its 'prev_next'.
         */
        ErtsMessage **next_sig = *next_nm_sig;
        yp->last.in_msgq = (char) !0;
        yp->last.in_sigq = (char) !0;
        yp->last.prev_next = next_sig;
        *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
        *next_sig = (ErtsMessage *) &yp->last;
        remove_middleq_sig(c_p, sig, &yp->last.sig.common.next, next_nm_sig);

        ERTS_SIG_DBG_RECV_MARK_SET_HANDLED(&yp->last);
    }

    save_delayed_nm_signal(saved_sigs, sig);
}

static ERTS_INLINE void
destroy_adj_msgq_yield_markers(Process *c_p, ErtsYieldAdjMsgQ **ypp)
{
    ErtsYieldAdjMsgQ *yp = *ypp;
    if (yp) {
        if (yp->next.in_msgq)
            remove_yield_marker(c_p, &yp->next);
        if (yp->last.in_msgq)
            remove_yield_marker(c_p, &yp->last);
        erts_free(ERTS_ALC_T_SIG_YIELD_DATA, yp);
        *ypp = NULL;
    }
}

static Uint
area_literal_size(Eterm* start, Eterm* end, char* lit_start, Uint lit_size)
{
    Eterm* p;
    Eterm val;
    Uint sz = 0;

    for (p = start; p < end; p++) {
        val = *p;
        switch (primary_tag(val)) {
        case TAG_PRIMARY_BOXED:
        case TAG_PRIMARY_LIST:
            if (ErtsInArea(val, lit_start, lit_size)) {
                sz += size_object(val);
            }
            break;
        case TAG_PRIMARY_HEADER:
            if (!header_is_transparent(val)) {
                Eterm *new_p;
                if (val == HEADER_SUB_BITS) {
                    ErlSubBits *sb = (ErlSubBits*) p;
                    if (ErtsInArea(sb->orig, lit_start, lit_size)) {
                        sz += size_object(sb->orig);
                    }
                }
                new_p = p + thing_arityval(val);
                ASSERT(start <= new_p && new_p < end);
                p = new_p;
            }
        }
    }
    return sz;
}

static ERTS_INLINE void
area_literal_copy(Eterm **hpp, ErlOffHeap *ohp,
                  Eterm *start, Eterm *end,
                  char *lit_start, Uint lit_size) {
    Eterm* p;
    Eterm val;
    Uint sz;

    for (p = start; p < end; p++) {
        val = *p;
        switch (primary_tag(val)) {
        case TAG_PRIMARY_BOXED:
        case TAG_PRIMARY_LIST:
            if (ErtsInArea(val, lit_start, lit_size)) {
                sz = size_object(val);
                val = copy_struct(val, sz, hpp, ohp);
                *p = val; 
            }
            break;
        case TAG_PRIMARY_HEADER:
            if (!header_is_transparent(val)) {
                Eterm* new_p;

                if (val == HEADER_SUB_BITS) {
                    /* Match contexts and writable binaries should never be
                     * present in signals. */
                    ASSERT(erl_sub_bits_is_normal((ErlSubBits*)p));

                    /* Make sure to copy the `orig` field if needed. It's the
                     * last field inside the thing structure so we can handle
                     * it by pretending it's not part of the thing. */
                    new_p = p + thing_arityval(val) - 1;
                } else {
                    new_p = p + thing_arityval(val);
                }

                ASSERT(start <= new_p && new_p < end);
                p = new_p;
            }
        }
    }
}

static void
send_cla_reply(Process *c_p, ErtsMessage *sig, Eterm to,
               Eterm req_id, Eterm result)
{
    Process *rp;

    /*
     * The incoming signal is reused as reply message to
     * the requester. It has already been partially prepared.
     * Request id is already in place in the combined message
     * heap fragment and do not need to be copied.
     */

    ASSERT(is_value(result) && is_immed(result));
    ASSERT(is_internal_pid(to));
    ASSERT(((Sint) sig->hfrag.alloc_size)
           - ((Sint) sig->hfrag.used_size)
           >= 4); /* Room for 3-tuple... */

    sig->next = NULL;
    sig->data.attached = ERTS_MSG_COMBINED_HFRAG;

    rp = erts_proc_lookup(to);
    if (!rp)
        erts_cleanup_messages(sig);
    else {
        Eterm rp_locks;
        Eterm *hp, reply_msg;

        hp = &sig->hfrag.mem[0] + sig->hfrag.used_size;
        reply_msg = TUPLE3(hp, am_copy_literals, req_id, result);
        sig->hfrag.used_size += 4;

        if (c_p == rp)
            rp_locks = ERTS_PROC_LOCK_MAIN;
        else
            rp_locks = 0;

        ERL_MESSAGE_TOKEN(sig) = am_undefined;

        erts_queue_proc_message(c_p, rp, rp_locks,
                                sig, reply_msg);
    }
}

static int
handle_cla(Process *c_p,
           ErtsMessage *sig,
           ErtsMessage ***next_nm_sig,
           int exiting,
           int limit,
           ErtsSavedNMSignals *saved_nm_sigs)
{
    ErtsCLAData *cla;
    ErtsMessage *msg, *endp;
    ErtsLiteralArea *la;
    char *literals;
    Uint lit_bsize;
    int nmsgs, reds, stretch_yield_limit = 0;
    Eterm result = am_ok;
    Uint64 cnt = 0;

    cnt++;

    cla = get_cla_data(sig);
    if (exiting) {
        /* signal already removed... */
        goto done;
    }

    /*
     * If we need to perform a literal GC, all signals *must* already
     * have been handled before the GC. Note that only the message
     * queue (signals before this signal) needs to be scanned since the
     * request have been passed through the signal queue after we set up
     * the literal area to copy. No literals in the area of interest
     * can therefore occur behind this signal.
     */

    msg = c_p->sig_qs.first;
    if (!msg)
        msg = c_p->sig_qs.cont;

    if (!cla->yield) {
        endp = sig;
    }
    else {
        if (!cla->yield->next.in_msgq) {
            /* All messages already handled... */
            ASSERT(!cla->yield->last.in_msgq);
            stretch_yield_limit = !0;
            endp = msg = sig;
        }
        else {
            ASSERT(!!cla->yield->last.in_msgq);
            msg = cla->yield->next.sig.common.next;
            endp = (ErtsMessage *) &cla->yield->last;
            remove_yield_marker(c_p, &cla->yield->next);
        }
    }

    ASSERT(!cla->yield || !cla->yield->next.in_msgq);
    
    la = ERTS_COPY_LITERAL_AREA();
    if (!la) {
        ASSERT(0);
        remove_nm_sig(c_p, sig, next_nm_sig);
        goto done;
    }

    ASSERT(la);

    literals = (char *) &la->start[0];
    lit_bsize = (char *) la->end - literals;

    nmsgs = 0;
    while (msg != endp) {
        ASSERT(!!msg);
        nmsgs++;
        if (nmsgs >= ERTS_PROC_SIG_ADJ_MSGQ_MSGS_FACTOR) {
            cnt++;
            nmsgs = 0;
        }
        if (ERTS_SIG_IS_INTERNAL_MSG(msg)) {
            ErlHeapFragment *first_hfrag, *hf, **last_hfrag;
            int in_refs = 0, in_heap_frags = 0;
            Uint scanned = 0, lit_sz = 0;

            /*
             * If a literal to copy is found in the message, we make
             * an explicit copy of it in a heap fragment and attach
             * that heap fragment to the message. Each message needs
             * to be self contained, we cannot save the literal at
             * any other place than in the message itself.
             */

	    /*
	     * Literals directly from message references should only
	     * be able to appear in the first message reference, i.e.,
	     * the message itself...
	     */
	    if (ErtsInArea(msg->m[0], literals, lit_bsize)) {
		in_refs++;
		lit_sz += size_object(msg->m[0]);
	    }

#ifdef DEBUG
	    {
		int i;
		for (i = 1; i < ERL_MESSAGE_REF_ARRAY_SZ; i++) {
		    ASSERT(!ErtsInArea(msg->m[i], literals, lit_bsize));
		}
	    }
#endif

            if (msg->data.attached == ERTS_MSG_COMBINED_HFRAG) {
                first_hfrag = &msg->hfrag;
                last_hfrag = &msg->hfrag.next;
            }
            else {
                first_hfrag = msg->data.heap_frag;
                last_hfrag = &msg->data.heap_frag;
            }

            for (hf = first_hfrag; hf; hf = hf->next) {
                Uint sz = hf->used_size;
                Uint lsz = area_literal_size(&hf->mem[0],
                                             &hf->mem[sz],
                                             literals, lit_bsize);
                if (lsz)
                    in_heap_frags++;
                lit_sz += lsz;
                scanned += sz;
                last_hfrag = &hf->next;
            }

            cnt += scanned/ERTS_PROC_SIG_ADJ_MSGQ_SCAN_FACTOR;

            if (lit_sz > 0) {
                ErlHeapFragment *new_hfrag = new_message_buffer(lit_sz);
                ErlOffHeap *ohp = &new_hfrag->off_heap;
                Eterm *hp = new_hfrag->mem;

                if (in_refs) {
		    if (ErtsInArea(msg->m[0], literals, lit_bsize)) {
			Uint sz = size_object(msg->m[0]);
			msg->m[0] = copy_struct(msg->m[0], sz, &hp, ohp);
                    }
                }

                if (in_heap_frags) {
                
                    for (hf = first_hfrag; hf; hf = hf->next) {
                        area_literal_copy(&hp, ohp, &hf->mem[0],
                                          &hf->mem[hf->used_size],
                                          literals, lit_bsize);
                    }

                }

                /* link new hfrag last */
                ASSERT(new_hfrag->used_size == hp - &new_hfrag->mem[0]);
                new_hfrag->next = NULL;
                ASSERT(!*last_hfrag);
                *last_hfrag = new_hfrag;

                cnt += scanned/ERTS_PROC_SIG_ADJ_MSGQ_SCAN_FACTOR;
                cnt += lit_sz/ERTS_PROC_SIG_ADJ_MSGQ_COPY_FACTOR;
            }
        }

        if (cnt > limit) { /* yield... */
            ErtsMessage **nextpp = !msg->next ? &c_p->sig_qs.cont : &msg->next;
            ASSERT(*nextpp);
            if (*nextpp == endp)
                break; /* we're at the end; no point yielding here... */
            if (!cla->yield)
                cla->yield = create_yield_adj_msgq_data(c_p);
            insert_adj_msgq_yield_markers(c_p, cla->yield, nextpp,
                                          next_nm_sig, saved_nm_sigs);
            return cnt;
        }

        msg = msg->next;
        if (!msg)
            msg = c_p->sig_qs.cont;
    }

    remove_nm_sig(c_p, sig, next_nm_sig);

    reds = erts_check_copy_literals_gc_need_max_reds(c_p);
    cnt++;
    if (reds > CONTEXT_REDS)
        result = am_check_gc;
    else if (stretch_yield_limit
             || cnt + reds*ERTS_SIG_REDS_CNT_FACTOR <= limit) {
        reds = 0;
        if (erts_check_copy_literals_gc_need(c_p, &reds, literals, lit_bsize))
            result = am_need_gc;
        cnt += reds * ERTS_SIG_REDS_CNT_FACTOR;
    }
    else {
        /* yield... */
        if (!cla->yield)
            cla->yield = create_yield_adj_msgq_data(c_p);
        else if (!!cla->yield->last.in_msgq)
            remove_yield_marker(c_p, &cla->yield->last);
        ASSERT(!cla->yield->next.in_msgq);
        save_delayed_nm_signal(saved_nm_sigs, sig);
        return cnt;
    }

done:

    destroy_adj_msgq_yield_markers(c_p, &cla->yield);

    send_cla_reply(c_p, sig, cla->requester, cla->request_id, result);

    if (cnt > CONTEXT_REDS*ERTS_SIG_REDS_CNT_FACTOR)
        return CONTEXT_REDS*ERTS_SIG_REDS_CNT_FACTOR;
    return cnt;
}

static int
handle_move_msgq_off_heap(Process *c_p,
			  ErtsMessage *sig,
			  ErtsMessage ***next_nm_sig,
			  int exiting,
                          int limit,
                          ErtsSavedNMSignals *saved_nm_sigs)
{
    ErtsAdjOffHeapMsgQData *ohdp;
    ErtsMessage *msg, *endp;
    int nmsgs;
    Uint64 cnt = 0;

    /*
     * This job was first initiated when the process changed to off heap
     * message queue management. ERTS_PSFLG_OFF_HEAP_MSGQ was set at
     * initiation and thread progress was made before this signal was
     * sent. That is, all signals after this signal already are off heap
     * and do not have to be inspected.
     *
     * The management state might, however, have been changed again
     * (multiple times) since initiation. The ERTS_PSFLG_OFF_HEAP_MSGQ has
     * however been set since the operation was first initiated. Check
     * users last requested state (the flags F_OFF_HEAP_MSGQ, and
     * F_ON_HEAP_MSGQ), and make the state consistent with that.
     */

    cnt++;

    ohdp = get_move_msgq_off_heap_data(sig);

    if (exiting) {
	/* signal already removed from queue... */
	goto cleanup;
    }

    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
    ASSERT(c_p->sig_qs.flags & FS_OFF_HEAP_MSGQ_CHNG);
    ASSERT(erts_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_OFF_HEAP_MSGQ);

    if (!(c_p->sig_qs.flags & FS_OFF_HEAP_MSGQ)) {
	/* Someone changed its mind... */
	erts_atomic32_read_band_nob(&c_p->state,
				    ~ERTS_PSFLG_OFF_HEAP_MSGQ);
	goto done;
    }

    msg = c_p->sig_qs.first;
    if (!msg)
        msg = c_p->sig_qs.cont;

    if (!ohdp->yield) {
        endp = sig;
    }
    else {
        ASSERT(!!ohdp->yield->next.in_msgq);
        ASSERT(!!ohdp->yield->last.in_msgq);
        msg = ohdp->yield->next.sig.common.next;
        endp = (ErtsMessage *) &ohdp->yield->last;
        remove_yield_marker(c_p, &ohdp->yield->next);
    }

    ASSERT(!ohdp->yield || !ohdp->yield->next.in_msgq);
    
    nmsgs = 0;
    while (msg != endp) {
        ASSERT(!!msg);
        nmsgs++;
        if (nmsgs >= ERTS_PROC_SIG_ADJ_MSGQ_MSGS_FACTOR) {
            cnt++;
            nmsgs = 0;
        }
	if (ERTS_SIG_IS_INTERNAL_MSG(msg)
	    && !msg->data.attached
	    && ((is_not_immed(ERL_MESSAGE_TERM(msg))
		 && !erts_is_literal(ERL_MESSAGE_TERM(msg),
				     ptr_val(ERL_MESSAGE_TERM(msg))))
#ifdef USE_VM_PROBES
                || is_not_immed(ERL_MESSAGE_DT_UTAG(msg))
#endif
                || is_not_immed(ERL_MESSAGE_TOKEN(msg)))) {
            ErlHeapFragment *hfrag;
	    Eterm *hp;
	    ErlOffHeap *ohp;
#ifdef SHCOPY_SEND
	    erts_shcopy_t info;
#else
	    erts_literal_area_t litarea;
#endif
#ifdef USE_VM_PROBES
            Uint ut_sz = size_object(ERL_MESSAGE_DT_UTAG(msg));
#endif
            Uint t_sz = size_object(ERL_MESSAGE_TOKEN(msg));
	    Uint m_sz;
	    Uint h_sz;

	    ASSERT(is_immed(ERL_MESSAGE_FROM(msg)));
	    if (is_immed(ERL_MESSAGE_TERM(msg)))
		m_sz = 0;
	    else {
#ifdef SHCOPY_SEND
		INITIALIZE_SHCOPY(info);
		m_sz = copy_shared_calculate(ERL_MESSAGE_TERM(msg), &info);
#else
		INITIALIZE_LITERAL_PURGE_AREA(litarea);
		m_sz = size_object_litopt(ERL_MESSAGE_TERM(msg), &litarea);
#endif
	    }

	    h_sz = m_sz + t_sz;
#ifdef USE_VM_PROBES
	    h_sz += ut_sz;
#endif
	    ASSERT(h_sz);

	    hfrag = new_message_buffer(h_sz);
	    hp = hfrag->mem;
	    ohp = &hfrag->off_heap;

	    if (is_not_immed(ERL_MESSAGE_TERM(msg))) {
		Eterm m = ERL_MESSAGE_TERM(msg);
		Eterm m_cpy;
#ifdef SHCOPY_SEND
		m_cpy = copy_shared_perform(m, m_sz, &info, &hp, ohp);
		DESTROY_SHCOPY(info);
#else
		m_cpy = copy_struct_litopt(m, m_sz, &hp, ohp, &litarea);
#endif
		ERL_MESSAGE_TERM(msg) = m_cpy;
	    }
	    if (is_not_immed(ERL_MESSAGE_TOKEN(msg)))
		ERL_MESSAGE_TOKEN(msg) = copy_struct(ERL_MESSAGE_TOKEN(msg),
						     t_sz, &hp, ohp);
#ifdef USE_VM_PROBES
	    if (is_not_immed(ERL_MESSAGE_DT_UTAG(msg)))
		ERL_MESSAGE_DT_UTAG(msg) = copy_struct(ERL_MESSAGE_DT_UTAG(msg),
						       ut_sz, &hp, ohp);
#endif
	    msg->data.heap_frag = hfrag;
	    cnt += h_sz/ERTS_PROC_SIG_ADJ_MSGQ_COPY_FACTOR;
	}

        if (cnt > limit) { /* yield... */
            ErtsMessage **nextpp = !msg->next ? &c_p->sig_qs.cont : &msg->next;
            ASSERT(*nextpp);
            if (*nextpp == endp)
                break; /* we're at the end; no point yielding... */
            if (!ohdp->yield)
                ohdp->yield = create_yield_adj_msgq_data(c_p);
            insert_adj_msgq_yield_markers(c_p, ohdp->yield, nextpp,
                                          next_nm_sig, saved_nm_sigs);
            return cnt;
        }

        msg = msg->next;
        if (!msg)
            msg = c_p->sig_qs.cont;
    }

done:

    remove_nm_sig(c_p, sig, next_nm_sig);

cleanup:

    destroy_adj_msgq_yield_markers(c_p, &ohdp->yield);
    sig->next = NULL;
    sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
    erts_cleanup_messages(sig);

    c_p->sig_qs.flags &= ~FS_OFF_HEAP_MSGQ_CHNG;

    if (cnt > CONTEXT_REDS*ERTS_SIG_REDS_CNT_FACTOR)
        return CONTEXT_REDS*ERTS_SIG_REDS_CNT_FACTOR;
    return cnt;
}


static int
handle_trace_change_state(Process *c_p,
                          ErtsSigRecvTracing *tracing,
                          Uint16 type,
                          ErtsMessage *sig,
                          ErtsMessage ***next_nm_sig)
{
    ErtsSigTraceInfo *trace_info = (ErtsSigTraceInfo *) sig;
    ErtsMessage **next = *next_nm_sig;
    ErtsTracerRef *ref;
    ErtsTraceSession *session;
    int msgs_active, old_msgs_active = !!tracing->messages.active;

    ASSERT(sig == *next);

    erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    session=trace_info->session;
    ASSERT(session);
    if(!(ref = get_tracer_ref(&c_p->common, session)))
        ref = new_tracer_ref(&c_p->common, session);
    ref->flags |= trace_info->flags_on;
    ref->flags &= ~trace_info->flags_off;
    ERTS_P_ALL_TRACE_FLAGS(c_p) = erts_sum_all_trace_flags(&c_p->common);
    if (is_value(trace_info->tracer))
        erts_tracer_replace(&c_p->common, ref, trace_info->tracer);

    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    remove_nm_sig(c_p, sig, next_nm_sig);
    destroy_trace_info(trace_info);
    /*
     * Adjust tracing state according to modifications made by
     * the trace info signal...
     */
    adjust_tracing_state(c_p, tracing, 0);
    msgs_active = !!tracing->messages.active;

    if (old_msgs_active ^ msgs_active) {
        if (msgs_active) {
            ASSERT(!tracing->messages.next);
            tracing->messages.next = next;
        }
        else {
            ASSERT(tracing->messages.next);
            tracing->messages.next = NULL;
        }
    }

    ASSERT(!msgs_active || tracing->messages.next);

    return msgs_active;
}

static void
getting_unlinked(Process *c_p, Eterm unlinker)
{
    trace_proc(c_p, ERTS_PROC_LOCK_MAIN, c_p,
               am_getting_unlinked, unlinker);
}

static void
getting_linked(Process *c_p, Eterm linker)
{
    trace_proc(c_p, ERTS_PROC_LOCK_MAIN, c_p,
               am_getting_linked, linker);
}

static void
linking(Process *c_p, Eterm to)
{
    trace_proc(c_p, ERTS_PROC_LOCK_MAIN, c_p,
               am_link, to);
}

static void
handle_message_enqueued_tracing(Process *c_p,
                                ErtsSigRecvTracing *tracing,
                                ErtsMessage *msg)
{
    ASSERT(ERTS_SIG_IS_INTERNAL_MSG(msg));

#if defined(USE_VM_PROBES)
    if (tracing->messages.vm_probes && DTRACE_ENABLED(message_queued)) {
        Sint tok_label = 0;
        Sint tok_lastcnt = 0;
        Sint tok_serial = 0;
        Eterm seq_trace_token = ERL_MESSAGE_TOKEN(msg);

        if (seq_trace_token != NIL && is_tuple(seq_trace_token)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(seq_trace_token);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(seq_trace_token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(seq_trace_token));
        }
        /* Message intentionally not passed... */
        DTRACE6(message_queued,
                tracing->messages.receiver_name,
                size_object(ERL_MESSAGE_TERM(msg)),
                c_p->sig_qs.mq_len,
                tok_label, tok_lastcnt, tok_serial);
    }
#endif

    if (tracing->messages.receive_trace) {
        trace_receive(c_p,
                      ERL_MESSAGE_FROM(msg),
                      ERL_MESSAGE_TERM(msg));
    }
}

static int
handle_msg_tracing(Process *c_p, ErtsSigRecvTracing *tracing,
                   ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig, *sig;
    Sint *mlenoffsp;
    int cnt = 0, limit = ERTS_PROC_SIG_TRACE_COUNT_LIMIT;

    ASSERT(tracing->messages.next);
    next_sig = tracing->messages.next;
    sig = *next_sig;

    if (!sig) {
        ASSERT(!*next_nm_sig);
        return 1; /* end... */
    }
    
    if (ERTS_SIG_IS_RECV_MARKER(sig) && ((ErtsRecvMarker *) sig)->in_msgq) {
	/*
	 * Skip already handled receive marker that just entered
	 * the message queue...
	 */
        next_sig = &sig->next;
        sig = *next_sig;
	ASSERT(!sig || !ERTS_SIG_IS_RECV_MARKER(sig)
	       || !((ErtsRecvMarker *) sig)->in_msgq);
    }

    /*
     * We keep 'mlenoffs' up to date for everyting we do, so that the signal queue
     * is up to data all the time. By this we might switch over to not do
     * receive tracing at any time without having to adjust the signal queue.
     */
    if (!*next_nm_sig) {
        mlenoffsp = &c_p->sig_qs.mlenoffs;
    }
    else {
        ErtsNonMsgSignal *nmsig = (ErtsNonMsgSignal *) **next_nm_sig;
        ASSERT(nmsig);
        mlenoffsp = &nmsig->mlenoffs;
    }

    /*
     * Receive tracing active. Handle all messages
     * until next non-message signal...
     */

    while (sig && ERTS_SIG_IS_MSG(sig)) {
        if (cnt > limit) {
            tracing->messages.next = next_sig;
            return -1; /* Yield... */
        }
        ASSERT(*mlenoffsp > 0);
        (*mlenoffsp)--;
        if (ERTS_SIG_IS_EXTERNAL_MSG(sig)) {
            cnt += 50; /* Decode is expensive... */
            if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN,
                                           sig, 0)) {
                /* Bad dist message; remove it... */
                remove_middleq_sig(c_p, sig, next_sig, next_nm_sig);
                sig->next = NULL;
                erts_cleanup_messages(sig);
                sig = *next_sig;
                continue;
            }
        }

        c_p->sig_qs.mq_len++;
        erts_chk_sys_mon_long_msgq_on(c_p);
        handle_message_enqueued_tracing(c_p, tracing, sig);
        cnt++;
        next_sig = &(*next_sig)->next;
        sig = *next_sig;
    }

    tracing->messages.next = next_sig;

    if (!sig) {
        ASSERT(!*next_nm_sig);
        return 1; /* end... */
    }

    ASSERT(*next_nm_sig);
    ASSERT(**next_nm_sig == sig);

    /* Next signal a non-message signal... */
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));

    /*
     * Return and handle the non-message signal...
     */

    return 0;
}

static void
handle_missing_spawn_reply(Process *c_p, ErtsMonitor *omon)
{
    ErtsMonitorData *mdp;
    ErtsMonitorDataExtended *mdep;
    erts_dsprintf_buf_t *dsbufp;
    Eterm nodename;
    DistEntry *dep;

    /* Terminate connection to the node and report it... */

    if (ERTS_ML_GET_TYPE(omon) != ERTS_MON_TYPE_DIST_PROC)
        ERTS_INTERNAL_ERROR("non-distributed missing spawn_reply");
    
    mdp = erts_monitor_to_data(omon);
    ASSERT(mdp->origin.flags & ERTS_ML_FLG_EXTENDED);
    mdep = (ErtsMonitorDataExtended *) mdp;
    ASSERT(mdep->dist);
    nodename = mdep->dist->nodename;
    ASSERT(is_atom(nodename));

    dep = erts_find_dist_entry(nodename);
    if (dep)
        erts_kill_dist_connection(dep, mdep->dist->connection_id);

    dsbufp = erts_create_logger_dsbuf();
    erts_dsprintf(dsbufp,
                  "Missing 'spawn_reply' signal from the node %T "
                  "detected by %T on the node %T. The node %T "
                  "probably suffers from the bug with ticket id "
                  "OTP-17737.",
                  nodename, c_p->common.id,
                  erts_this_dist_entry->sysname, nodename);
    erts_send_error_to_logger_nogl(dsbufp);
}

Uint
erts_proc_sig_prep_msgq_for_inspection(Process *c_p,
                                       Process *rp,
                                       ErtsProcLocks rp_locks,
                                       int info_on_self,
                                       ErtsMessageInfo *mip,
                                       Sint *msgq_len_p)
{
    Uint tot_heap_size;
    ErtsMessage *mp, **mpp;
    Sint i;
    int self_on_heap;

    /*
     * Prepare the message queue (inner signal queue)
     * for inspection by process_info().
     *
     * - Decode all messages on external format
     * - Remove all corrupt dist messages from queue
     * - Save pointer to, and heap size need of each
     *   message in the mip array.
     * - Return total heap size need for all messages
     *   that needs to be copied.
     *
     */

    ASSERT(!info_on_self || c_p == rp);

    self_on_heap = info_on_self && !(c_p->sig_qs.flags & FS_OFF_HEAP_MSGQ);

    tot_heap_size = 0;
    i = 0;
    mpp = &rp->sig_qs.first;
    mp = rp->sig_qs.first;
    while (mp) {
	Eterm msg = ERL_MESSAGE_TERM(mp);

	if (msg != ERTS_RECV_MARKER_TAG) {

	    mip[i].size = 0;

	    if (ERTS_SIG_IS_EXTERNAL_MSG(mp)) {
		/* decode it... */
		if (!erts_proc_sig_decode_dist(rp, rp_locks, mp, !0)) {
		    ErtsMessage *bad_mp = mp;
		    /*
		     * Bad distribution message; remove
		     * it from the queue...
		     */

		    ASSERT(*mpp == bad_mp);

		    remove_innerq_m_sig(rp, mp, mpp);

		    mp = *mpp;

		    bad_mp->next = NULL;
		    erts_cleanup_messages(bad_mp);
		    continue;
		}

		msg = ERL_MESSAGE_TERM(mp);
	    }

	    ASSERT(is_value(msg));

	    if (is_not_immed(msg) && (!self_on_heap || mp->data.attached)) {
		Uint sz = size_object(msg);
		mip[i].size = sz;
		tot_heap_size += sz;
	    }

	    mip[i].msgp = mp;
	    i++;
	}

        mpp = &mp->next;
	mp = mp->next;
    }

    ASSERT(c_p->sig_qs.mq_len == i);

    *msgq_len_p = i;

    return tot_heap_size;
}

static ERTS_INLINE void
move_msg_to_heap(Process *c_p, ErtsMessage *mp)
{
    /*
     * We leave not yet decoded distribution messages
     * as they are in the queue since it is not
     * possible to determine a maximum size until
     * actual decoding...
     *
     * We also leave combined messages as they are...
     */
    if (ERTS_SIG_IS_INTERNAL_MSG(mp)
        && mp->data.attached
        && mp->data.attached != ERTS_MSG_COMBINED_HFRAG) {
        ErlHeapFragment *bp;
        
        bp = erts_message_to_heap_frag(mp);

        if (bp->next)
            erts_move_multi_frags(&c_p->htop, &c_p->off_heap, bp,
                                  mp->m, ERL_MESSAGE_REF_ARRAY_SZ, 0);
        else
            erts_copy_one_frag(&c_p->htop, &c_p->off_heap, bp,
                               mp->m, ERL_MESSAGE_REF_ARRAY_SZ);

        mp->data.heap_frag = NULL;
        free_message_buffer(bp);
    }
}

void
erts_proc_sig_move_msgs_to_heap(Process *c_p)
{
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

    ERTS_FOREACH_SIG_PRIVQS(c_p, sig, move_msg_to_heap(c_p, sig));

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
}


BIF_RETTYPE
erts_internal_dirty_process_handle_signals_1(BIF_ALIST_1)
{
    erts_aint32_t state, dirty, noproc;
    int busy;
    Process *rp;

    if (BIF_P != erts_dirty_process_signal_handler
        && BIF_P != erts_dirty_process_signal_handler_high
        && BIF_P != erts_dirty_process_signal_handler_max)
        BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (is_not_internal_pid(BIF_ARG_1))
        BIF_RET(am_false);

    rp = erts_proc_lookup_raw(BIF_ARG_1);
    if (!rp)
        BIF_RET(am_noproc);

    state = erts_atomic32_read_acqb(&rp->state);
    dirty = ERTS_PROC_IN_DIRTY_STATE(state);
    if (!dirty)
        BIF_RET(am_normal);

    if (!(state & (ERTS_PSFLG_SIG_Q
                   | ERTS_PSFLG_NMSG_SIG_IN_Q
                   | ERTS_PSFLG_MSG_SIG_IN_Q))) {
        BIF_RET(am_ok);
    }

    busy = erts_proc_trylock(rp, ERTS_PROC_LOCK_MAIN) == EBUSY;

    state = erts_atomic32_read_mb(&rp->state);
    noproc = (state & ERTS_PSFLG_FREE);
    dirty = ERTS_PROC_NEED_DIRTY_SIG_HANDLING(state);

    if (busy) {
        if (noproc)
            BIF_RET(am_noproc);
        if (dirty)
            BIF_RET(am_more); /* try again... */
        BIF_RET(am_normal); /* will handle signals itself... */
    }
    else {
        int done;
        Eterm res = am_false;
        int reds = 0;

        if (noproc)
            res = am_noproc;
        else if (!dirty)
            res = am_normal; /* will handle signals itself... */
        else if (rp->sig_qs.flags & FS_HANDLING_SIGS)
            res = am_busy;
        else {
            reds = ERTS_BIF_REDS_LEFT(BIF_P);
            done = erts_proc_sig_handle_incoming(rp, &state, &reds,
                                                 reds, 0);
            if (done || (state & ERTS_PSFLG_EXITING))
                res = am_ok;
            else
                res = am_more;
        }

        erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

        if (reds)
            BUMP_REDS(BIF_P, reds);

        BIF_RET(res);
    }
}

/*
 * Priority message queue...
 */

static ERTS_INLINE void
insert_prio_q_marker(Process *c_p, ErtsRecvMarker *mark, ErtsMessage **next)
{
    if (c_p->sig_qs.last == next)
        c_p->sig_qs.last = &mark->sig.common.next;
    if (c_p->sig_qs.save == next)
        c_p->sig_qs.save = &mark->sig.common.next;
    if (*next && ERTS_SIG_IS_RECV_MARKER(*next))
        ((ErtsRecvMarker *) *next)->prev_next = &mark->sig.common.next;
    mark->prev_next = next;
    mark->sig.common.next = *next;
    *next = (ErtsMessage *) mark;

    mark->in_sigq = mark->in_msgq = !0;
}

static void
create_prio_q_info(Process *c_p)
{
    ErtsPrioQInfo *pq_info, *prev_pq_info;
    int i;

    pq_info = erts_alloc(ERTS_ALC_T_PRIO_Q_INFO, sizeof(ErtsPrioQInfo));

    pq_info->saved_save_info = 0;
    pq_info->refc = 0;
#ifdef DEBUG
    pq_info->alias = 0;
    pq_info->link = 0;
    pq_info->monitor = 0;
#endif

    for (i = ERTS_PRIO_Q_MARK_IX_MIN; i <= ERTS_PRIO_Q_MARK_IX_MAX; i++) {

        pq_info->marker[i].sig.common.next = NULL;
        pq_info->marker[i].sig.common.specific.attachment = NULL;
        pq_info->marker[i].sig.common.tag = ERTS_RECV_MARKER_TAG;
        pq_info->marker[i].prev_next = NULL;
        pq_info->marker[i].in_sigq = 0;
        pq_info->marker[i].in_msgq = 0;
        pq_info->marker[i].in_prioq = 0;

        /* not used... */

        pq_info->marker[i].next_ix = -100;
        pq_info->marker[i].pass = (char) 100;
        pq_info->marker[i].set_save = (char) 0;
        pq_info->marker[i].prev_ix = (char) -100;

        /* specific for the different markers... */
        switch (i) {
        case ERTS_PRIO_Q_MARK_END:
            pq_info->marker[i].mark_type = ERTS_RECV_MARKER_TYPE_PRIO_Q_END;
            break;
        case ERTS_PRIO_Q_MARK_CONT:
            pq_info->marker[i].mark_type = ERTS_RECV_MARKER_TYPE_PRIO_Q_CONT;
            break;
        default:
            ERTS_INTERNAL_ERROR("Unknown prio queue marker");
            break;
        }

#ifdef DEBUG
        pq_info->marker[i].used = (char) !0;
        pq_info->marker[i].proc = c_p;
#endif

    }

    prev_pq_info = ERTS_PROC_SET_PRIO_Q_INFO(c_p, pq_info);

    ASSERT(!prev_pq_info); (void) prev_pq_info;
}

static void
destroy_prio_q_info(Process *c_p, ErtsPrioQInfo *pq_info, int terminating)
{
    ErtsPrioQInfo *prev_pq_info;
    int i;

    ASSERT(terminating || !(c_p->sig_qs.flags & FS_PRIO_MQ_SAVE));
    ASSERT(terminating || !(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK));
    ASSERT(terminating || !pq_info->refc);
    ASSERT(terminating || !pq_info->alias);
    ASSERT(terminating || !pq_info->link);
    ASSERT(terminating || !pq_info->monitor);

    for (i = ERTS_PRIO_Q_MARK_IX_MIN; i <= ERTS_PRIO_Q_MARK_IX_MAX; i++) {
        ErtsRecvMarker *mark = &pq_info->marker[i];
        if (mark->in_msgq)
            remove_prio_q_marker(c_p, mark);
    }

    prev_pq_info = ERTS_PROC_SET_PRIO_Q_INFO(c_p, NULL);
    ASSERT(prev_pq_info == pq_info); (void) prev_pq_info;

    erts_free(ERTS_ALC_T_PRIO_Q_INFO, (void *) pq_info);

    if (c_p->sig_qs.recv_mrk_blk) {
        ErtsRecvMarkerBlock *blk = c_p->sig_qs.recv_mrk_blk;
        int i;
        for (i = 0; i < ERTS_RECV_MARKER_BLOCK_SIZE; i++) {
            blk->marker[i].in_prioq = 0;
        }
    }
}

static ERTS_INLINE int
is_same_internal_ref(Eterm x, Eterm y)
{
    int i;
    Uint xlen, ylen;
    Uint32 *xnum, *ynum;

    ERTS_CT_ASSERT(ERTS_REF_NUMBERS == 3 && ERTS_PID_REF_NUMBERS >= 3);

    ASSERT(is_internal_ref(x) && is_internal_ref(y));

    ASSERT(internal_ref_no_numbers(x) >= 3);
    ASSERT(internal_ref_no_numbers(y) >= 3);

    xnum = internal_ref_numbers(x);
    ynum = internal_ref_numbers(y);

    if (xnum[0] != ynum[0] || xnum[1] != ynum[1] || xnum[2] != ynum[2])
        return 0;

    xlen = internal_ref_no_numbers(x);
    ylen = internal_ref_no_numbers(y);

    if (xlen != ylen)
        return 0;

    for (i = 3; i < xlen; i++) {
        if (xnum[i] != ynum[i])
            return 0;
    }

    return !0;
}

static void
prio_queue_check_recv_marks(Eterm obj, Eterm *ref,
                            int *pq, int *n_pqp,
                            int *not_pq, int *n_not_pqp)
{
    Eterm *ptr;
    int arity, n_pq, n_not_pq;
    DECLARE_ESTACK(s);

    n_pq = *n_pqp;
    n_not_pq = *n_not_pqp;

    ASSERT(n_not_pq != 0);

    while (!0) {

	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    ptr = list_val(obj);
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr;
                ptr = boxed_val(obj);
                hdr = *ptr;
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    arity = header_arity(hdr);
		    if (arity == 0) {
                        ASSERT(obj == ERTS_GLOBAL_LIT_EMPTY_TUPLE);
			goto pop_next;
		    }
                    ptr = tuple_val(obj);
		    while (arity-- > 1) {
			obj = *++ptr;
			if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			}
		    }
		    obj = *++ptr;
		    break;
                case FUN_SUBTAG:
                    {
                        const ErlFunThing* funp = (ErlFunThing*)fun_val(obj);

                        ASSERT(ERL_FUN_SIZE == (1 + thing_arityval(hdr)));

                        for (int i = 1; i < fun_num_free(funp); i++) {
                            obj = funp->env[i];
                            if (!IS_CONST(obj)) {
                                ESTACK_PUSH(s, obj);
                            }
                        }

                        if (fun_num_free(funp) > 0) {
                            obj = funp->env[0];
                            break;
                        }

                        goto pop_next;
		    }
		case MAP_SUBTAG:
		    switch (MAP_HEADER_TYPE(hdr)) {
			case MAP_HEADER_TAG_FLATMAP_HEAD :
                            {
                                Uint n;
                                flatmap_t *mp;
                                mp  = (flatmap_t*)flatmap_val(obj);
                                ptr = (Eterm *)mp;
                                n   = flatmap_get_size(mp) + 1;
                                ASSERT(flatmap_get_size(mp)
                                       <= MAP_SMALL_MAP_LIMIT);
                                ptr += 2; /* hdr + size words */
                                while (n--) {
                                    obj = *ptr++;
                                    if (!IS_CONST(obj)) {
                                        ESTACK_PUSH(s, obj);
                                    }
                                }
                                goto pop_next;
                            }
			case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
			case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
			case MAP_HEADER_TAG_HAMT_NODE_BITMAP :
			    {
				Eterm *head;
				Uint sz;
				head  = hashmap_val(obj);
				sz    = hashmap_bitcount(MAP_HEADER_VAL(hdr));
				head += 1 + header_arity(hdr);

				if (sz == 0) {
				    goto pop_next;
				}
				while(sz-- > 1) {
				    obj = head[sz];
				    if (!IS_CONST(obj)) {
					ESTACK_PUSH(s, obj);
				    }
				}
				obj = head[0];
			    }
			    break;
			default:
			    erts_exit(ERTS_ABORT_EXIT,
                                      "prio_queue_check_recv_marks(): "
                                      "bad hashmap type %d\n",
                                      MAP_HEADER_TYPE(hdr));
		    }
		    break;
                case REF_SUBTAG: {
                    int i = 0;
                    while (i < n_not_pq) {
                        int r = not_pq[i];
                        ASSERT(0 <= r && r < ERTS_RECV_MARKER_BLOCK_SIZE);
                        if (is_same_internal_ref(obj, ref[r])) {
                            /* this recv marker needs to be pq... */
                            pq[n_pq++] = not_pq[i];
                            n_not_pq--;
                            if (i < n_not_pq) {
                                not_pq[i] = not_pq[n_not_pq];
                                continue;
                            }
                            break;
                        }
                        i++;
                    }
                    if (n_not_pq == 0)
                        goto done;
                }
		default:
		    goto pop_next;
		}
	    }
	    break;
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
            done:
		DESTROY_ESTACK(s);

                *n_pqp = n_pq;
                *n_not_pqp = n_not_pq;
		return;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT,
                      "prio_queue_check_recv_marks(): bad tag for %#x\n",
                      obj);
	}
    }
}

static int
insert_prepared_prio_msg_attached(Process *c_p, ErtsSigRecvTracing *tracing,
                                  ErtsMessage *sig, void *data_attached,
                                  Eterm message, Eterm token,
                                  ErtsMessage ***next_nm_sig)
{
    int i, cnt = 1, empty_prio_q = !(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK);
    ErtsRecvMarkerBlock *blk = c_p->sig_qs.recv_mrk_blk;
    ErtsPrioQInfo *pq_info = get_prio_queue_info(c_p);
    ErtsRecvMarker *pq_end = &pq_info->marker[ERTS_PRIO_Q_MARK_END];

    ASSERT(pq_end->mark_type == ERTS_RECV_MARKER_TYPE_PRIO_Q_END);
    ASSERT(c_p->sig_qs.flags & FS_PRIO_MQ);

    if (blk) {
        int n_pq = 0, n_not_pq = 0;
        int pq[ERTS_RECV_MARKER_BLOCK_SIZE], not_pq[ERTS_RECV_MARKER_BLOCK_SIZE];

        /*
         * Check if references corresponding to receive markers not already
         * seen in the prio queue can be found in this prio message.
         */

        for (i = 0; i < ERTS_RECV_MARKER_BLOCK_SIZE; i++) {
            if (empty_prio_q)
                blk->marker[i].in_prioq = 0;
            if (blk->marker[i].in_sigq
                && !blk->marker[i].in_prioq
                && is_internal_ref(blk->ref[i])) {
                not_pq[n_not_pq++] = i;
            }
        }

        if (n_not_pq) {

            /* inspect message */
            prio_queue_check_recv_marks(message, &blk->ref[0],
                                        &pq[0], &n_pq,
                                        &not_pq[0], &n_not_pq);

            for (i = 0; i < n_pq; i++) {
                int mix;
                ErtsRecvMarker *mark;

                mix = pq[i];

                ASSERT(0 <= mix && mix < ERTS_RECV_MARKER_BLOCK_SIZE);

                mark = &blk->marker[mix];

                ASSERT(!mark->in_prioq);

                /* Note that this ref has been seen in the prio queue... */
                mark->in_prioq = !0;
                cnt += 4;
            }
        }
    }

    if (c_p->sig_qs.flags & FS_PRIO_MQ_SAVE) {
        /*
         * Save pointer points into the prio queue, we know that it points to
         * a point before where we insert this prio message. We also know that
         * we already have determined where to continue when we reach the end of
         * the prio queue, so nothing more to do than inserting the message...
         */
        ASSERT(!empty_prio_q);
        ASSERT(pq_end->in_msgq);
    }
    else {
        /* Save pointer *do not* point into the prio queue. */

        int set_save = !0;

        switch (ERTS_MQ_GET_SAVE_INFO(c_p)) {

        case FS_SET_SAVE_INFO_FIRST:
            /*
             * We must set the save pointer otherwise we might miss to match
             * out this prio message.
             */
            break;

        case FS_SET_SAVE_INFO_LAST:
            /*
             * This process is waiting for a message sent from the emulator
             * containing a reference, but we don't know which reference that
             * is. However, messages containing such references are never sent
             * as priority messages, so we can safely leave the save marker
             * where it is.
             */
            set_save = 0;
            break;

        case FS_SET_SAVE_INFO_MARK:
            /*
             * The save mark should *only* be set while save pointer points
             * into the prio queue...
             */
            ERTS_INTERNAL_ERROR("Invalid message queue state");
            break;

        case FS_SET_SAVE_INFO_RCVM:

            i = blk->set_save_ix;

            if (i < 0 || ERTS_RECV_MARKER_BLOCK_SIZE <= i)
                ERTS_INTERNAL_ERROR("Invalid message queue state");

            if (blk && !blk->marker[i].in_prioq) {
                /*
                 * We know that the reference of the receive marker we are
                 * waiting for has not been seen in the prio queue, so we
                 * can safely continue where we are...
                 */
                set_save = 0;
            }
            break;

        default:

            ERTS_INTERNAL_ERROR("Non-existing message queue state");
            break;
        }

        if (empty_prio_q) {
            /* end marker not in queue; insert it */
            ASSERT(!pq_end->in_msgq);
            insert_prio_q_marker(c_p, pq_end, &c_p->sig_qs.first);
            c_p->sig_qs.flags |= FS_PRIO_MQ_END_MARK;
        }

        if (set_save) {
            ErtsRecvMarker *pq_cont = &pq_info->marker[ERTS_PRIO_Q_MARK_CONT];
            ASSERT(pq_cont->mark_type == ERTS_RECV_MARKER_TYPE_PRIO_Q_CONT);
            if (c_p->sig_qs.save == *next_nm_sig) {
                /*
                 * We do *not* want to handle the continuation marker we are
                 * about to insert as next non-msg signal; adjust
                 * '*next_nm_sig'...
                 */
                *next_nm_sig = &pq_cont->sig.common.next;
            }
            /*
             * Save a marker where save pointer points now, so we know where to
             * continue when we reach the end of the prio queue.
             */
            if (pq_cont->in_msgq)
                remove_prio_q_marker(c_p, pq_cont);
            insert_prio_q_marker(c_p, pq_cont, c_p->sig_qs.save);
            pq_info->saved_save_info = ERTS_MQ_GET_SAVE_INFO(c_p);
            ERTS_MQ_SET_SAVE_INFO(c_p, FS_SET_SAVE_INFO_MARK);

            /*
             * Set save pointer to the end of the prio queue, so it will precede
             * the prio message that we are about to insert.
             */
            c_p->sig_qs.save = pq_end->prev_next;
            c_p->sig_qs.flags |= FS_PRIO_MQ_SAVE;
            ASSERT(*c_p->sig_qs.save == (ErtsMessage *) pq_end);
        }
    }

    sig->data.attached = data_attached;

    ERL_MESSAGE_TERM(sig) = message;
    ERL_MESSAGE_TOKEN(sig) = token;

    if (tracing->messages.active) {
        if (ERTS_SIG_IS_EXTERNAL_MSG(sig)) {
            cnt += 50; /* Decode is expensive... */
            if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN,
                                           sig, 0)) {
                /* Bad dist message; drop it... */
                sig->next = NULL;
                erts_cleanup_messages(sig);
                return cnt;
            }
        }
        handle_message_enqueued_tracing(c_p, tracing, sig);
    }

    /* Append the actual prio message to the prio queue... */

    *pq_end->prev_next = sig;
    pq_end->prev_next = &sig->next;
    sig->next = (ErtsMessage *) pq_end;

    c_p->sig_qs.mq_len++;
    erts_chk_sys_mon_long_msgq_on(c_p);

    return cnt;
}

static int
insert_prepared_prio_msg(Process *c_p, ErtsSigRecvTracing *tracing,
                         ErtsMessage *sig, Eterm message, Eterm token,
                         ErtsMessage ***next_nm_sig)
{
    return insert_prepared_prio_msg_attached(c_p, tracing, sig,
                                             ERTS_MSG_COMBINED_HFRAG,
                                             message, token,
                                             next_nm_sig);
}

static void
send_try_destroy_prio_q_info(void *vpid);

static Eterm
try_destroy_prio_q_info(Process *c_p, void *arg, int *redsp, ErlHeapFragment **hpp)
{
    ErtsPrioQInfo *pq_info = get_prio_queue_info(c_p);

    ASSERT(pq_info);
    ASSERT(c_p->sig_qs.flags & FS_PRIO_MQ_PENDING_RM);

    if (c_p->sig_qs.flags & FS_PRIO_MQ) {
        /* Prio messages have been enabled again; do not remove info... */
        c_p->sig_qs.flags &= ~FS_PRIO_MQ_PENDING_RM;
        return THE_NON_VALUE;
    }

    if (c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK) {
        /* Cannot remove until prio queue is empty; try again in 5 minutes... */
        erts_start_timer_callback(5*60*1000, send_try_destroy_prio_q_info,
                                  (void *) c_p->common.id);
        return THE_NON_VALUE;
    }

    destroy_prio_q_info(c_p, pq_info, 0);
    c_p->sig_qs.flags &= ~FS_PRIO_MQ_PENDING_RM;

    return THE_NON_VALUE;
}

static void
send_try_destroy_prio_q_info(void *vpid)
{
    ASSERT(is_internal_pid((Eterm) vpid));
    (void) erts_proc_sig_send_rpc_request(NULL, (Eterm) vpid, 0,
                                          try_destroy_prio_q_info,
                                          NULL);

}

static void
install_prio_msg_queue(Process *c_p)
{
    ASSERT(!(c_p->sig_qs.flags & FS_PRIO_MQ));

    c_p->sig_qs.flags |= FS_PRIO_MQ;

    /*
     * If FS_PRIO_MQ_PENDING_RM is set, the prio q info is already there
     * and we just aborted the operation. We leave the flag and let the
     * scheduled job remove the flag. If we remove it here, we might end
     * up with multiple remove jobs...
     */
    if (!(c_p->sig_qs.flags & FS_PRIO_MQ_PENDING_RM))
        create_prio_q_info(c_p);

    ASSERT(!(c_p->sig_qs.flags & FS_PRIO_MQ_SAVE));
    ASSERT(!(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK));
    ASSERT(!!get_prio_queue_info(c_p));
}

static void
uninstall_prio_msg_queue(Process *c_p, ErtsPrioQInfo *pq_info)
{
    ASSERT(pq_info == get_prio_queue_info(c_p));

    ASSERT(c_p->sig_qs.flags & FS_PRIO_MQ);

    c_p->sig_qs.flags &= ~FS_PRIO_MQ;

    if (c_p->sig_qs.flags & FS_PRIO_MQ_PENDING_RM) {
        /* removal already ongoing... */
        return;
    }

    if (!(c_p->sig_qs.flags & FS_PRIO_MQ_END_MARK)) {
        /* Prio queue empty so we can safely remove it at once... */
        destroy_prio_q_info(c_p, pq_info, 0);
    }
    else {
        /*
         * Prio queue not empty so we cannot remove it now, set a timer
         * and try again in 10 seconds...
         */
        c_p->sig_qs.flags |= FS_PRIO_MQ_PENDING_RM;
        (void) erts_start_timer_callback(10*1000,
                                         send_try_destroy_prio_q_info,
                                         (void *) c_p->common.id);
    }
}

void
erts_proc_sig_prio_item_deleted(Process *c_p, ErtsPrioItemType type)
{
    ErtsPrioQInfo *pq_info = get_prio_queue_info(c_p);
    int uninstall_pq = --pq_info->refc == 0;

#ifdef DEBUG
    switch (type) {
    case ERTS_PRIO_ITEM_TYPE_ALIAS:
        --pq_info->alias;
        break;
    case ERTS_PRIO_ITEM_TYPE_LINK:
        --pq_info->link;
        break;
    case ERTS_PRIO_ITEM_TYPE_MONITOR:
        --pq_info->monitor;
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid priority item type");
        uninstall_pq = 0;
        break;
    }
    ASSERT(pq_info->refc >= 0 && pq_info->alias >= 0 && pq_info->link >= 0
           && pq_info->monitor >= 0);
    ASSERT(pq_info->refc == pq_info->alias + pq_info->link + pq_info->monitor);
#endif


    if (uninstall_pq)
        uninstall_prio_msg_queue(c_p, pq_info);
}

void
erts_proc_sig_prio_item_added(Process *c_p, ErtsPrioItemType type)
{
    ErtsPrioQInfo *pq_info;

    if (!(c_p->sig_qs.flags & FS_PRIO_MQ))
        install_prio_msg_queue(c_p);

    pq_info = get_prio_queue_info(c_p);
    ++pq_info->refc;

#ifdef DEBUG
    switch (type) {
    case ERTS_PRIO_ITEM_TYPE_ALIAS:
        ++pq_info->alias;
        break;
    case ERTS_PRIO_ITEM_TYPE_LINK:
        ++pq_info->link;
        break;
    case ERTS_PRIO_ITEM_TYPE_MONITOR:
        ++pq_info->monitor;
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid priority item type");
        break;
    }
    ASSERT(pq_info->refc >= 0 && pq_info->alias >= 0 && pq_info->link >= 0
           && pq_info->monitor >= 0);
    ASSERT(pq_info->refc == pq_info->alias + pq_info->link + pq_info->monitor);
#endif

}

/* Cleanup */

void
erts_proc_sig_cleanup_queues(Process *c_p)
{
    ErtsMessage *queues[] = {
	c_p->sig_qs.first, /* Message queue (inner signal queue) */
	c_p->sig_qs.cont   /* Private signal queue (middle signal queue) */
    };
    int i;

    for (i = 0; i < sizeof(queues)/sizeof(queues[0]); i++) {
	ErtsMessage *sig = queues[i];
	while (sig) {
	    ErtsMessage *free_sig = sig;
	    sig = sig->next;
	    if (ERTS_SIG_IS_RECV_MARKER(free_sig)) {
                ErtsRecvMarker *recv_mark = (ErtsRecvMarker *) free_sig;
                ASSERT(recv_mark->mark_type == ERTS_RECV_MARKER_TYPE_RECV);
                recv_marker_deallocate(c_p, recv_mark);
            }
	    else {
                ASSERT(ERTS_SIG_IS_MSG(free_sig));
		free_sig->next = NULL;
		erts_cleanup_messages(free_sig);
	    }
	}
    }

#ifdef DEBUG
    /*
     * External signal queue (outer signal queue)
     * should already have been taken care of...
     */
    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
    ASSERT(!c_p->sig_inq.first);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
#endif
}

void
erts_proc_sig_do_wait_dirty_handle_signals__(Process *c_p)
{
    /*
     * A dirty process signal handler is currently handling
     * signals for this process, so it is not safe for this
     * process to continue to execute. This process needs to
     * wait for the dirty signal handling to complete before
     * it can continue executing. This since otherwise the
     * signal queue can be seen in an inconsistent state.
     *
     * This should be a quite rare event. This only occurs
     * when all of the following occurs:
     * * The process is executing dirty and receives a
     *   signal.
     * * A dirty process signal handler starts handling
     *   signals for the process and unlocks the main
     *   lock while doing so. This can currently only
     *   occur if handling an 'unlink' signal from a port, or
     *   when handling an alias message where the alias
     *   has been created when monitoring a port using
     *   '{alias, reply_demonitor}' option.
     * * While the dirty process signal handler is handling
     *   signals for the process, the process access the
     *   signal queue in another thread.
     *
     * If the above happens, the scheduler executing the
     * process will wait here until the dirty process signal
     * handler is done with the process...
     */
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    erts_tse_t *event;

    ASSERT(c_p == erts_get_current_process());
    ASSERT(esdp);
    ASSERT(esdp->ssi);
    ASSERT(esdp->ssi->event);
    ASSERT(c_p->sig_qs.flags & FS_HANDLING_SIGS);
    ASSERT(!(c_p->sig_qs.flags & FS_WAIT_HANDLE_SIGS));

    event = esdp->ssi->event;
    (void) ERTS_PROC_SET_TS_EVENT(c_p, event);
    c_p->sig_qs.flags |= FS_WAIT_HANDLE_SIGS;

    erts_tse_use(event);

    do {
        ASSERT(c_p->sig_qs.flags & FS_WAIT_HANDLE_SIGS);
        erts_tse_reset(event);
        erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
        erts_tse_wait(event);
        erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    } while (c_p->sig_qs.flags & FS_HANDLING_SIGS);

    erts_tse_return(event);
    c_p->sig_qs.flags &= ~FS_WAIT_HANDLE_SIGS;
    (void) ERTS_PROC_SET_TS_EVENT(c_p, NULL);
}

static void
wake_handle_signals(Process *proc)
{
    /*
     * Wake scheduler waiting in erts_proc_sig_check_wait_dirty_handle_signals()
     * (above)...
     *
     * This function should only be called by a dirty process signal handler
     * process...
     */
    erts_tse_t *event = ERTS_PROC_GET_TS_EVENT(proc);
#ifdef DEBUG
    Process *c_p = erts_get_current_process();
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(proc));
    ASSERT(proc->sig_qs.flags & FS_WAIT_HANDLE_SIGS);
    ERTS_ASSERT(c_p == erts_dirty_process_signal_handler_max
                || c_p == erts_dirty_process_signal_handler_high
                || c_p == erts_dirty_process_signal_handler);
    ASSERT(event);
#endif

    proc->sig_qs.flags &= ~FS_HANDLING_SIGS;
    erts_tse_set(event);
}

/* Debug */

static void
debug_foreach_sig_heap_frags(ErlHeapFragment *hfrag,
                             void (*oh_func)(ErlOffHeap *, void *),
                             void *arg)
{
    ErlHeapFragment *hf = hfrag;
    while (hf) {
        oh_func(&(hf->off_heap), arg);
        hf = hf->next;
    }
}

static void
debug_foreach_sig_fake_oh(Eterm term,
                          void (*oh_func)(ErlOffHeap *, void *),
                          void *arg)
{
    if (is_external(term)) {
        ErlOffHeap oh;
        oh.overhead = 0;
        oh.first = ((struct erl_off_heap_header *)
                    (char *) external_thing_ptr(term));
        ASSERT(!oh.first->next);
        oh_func(&oh, arg);
    }

}

static void
debug_foreach_sig_external(ErtsMessage *msgp,
                           void (*ext_func)(ErtsDistExternal *, void *),
                           void *arg)
{
    ext_func(erts_proc_sig_get_external(msgp), arg);
}

void
erts_proc_sig_debug_foreach_sig(Process *c_p,
                                void (*msg_func)(ErtsMessage *, void *),
                                void (*oh_func)(ErlOffHeap *, void *),
                                ErtsMonitorFunc mon_func,
                                ErtsLinkFunc lnk_func,
                                void (*ext_func)(ErtsDistExternal *, void *),
                                void *arg)
{
    ErtsMessage *queue[] = {c_p->sig_qs.first, c_p->sig_qs.cont, c_p->sig_inq.first};
    int qix;

    for (qix = 0; qix < sizeof(queue)/sizeof(queue[0]); qix++) {
        ErtsMessage *sig;
        for (sig = queue[qix]; sig; sig = sig->next) {

            if (ERTS_SIG_IS_MSG(sig)) {
                msg_func(sig, arg);
            } else {
                Eterm tag;
                Uint16 type;
                int op;

                ASSERT(sig);
                ASSERT(ERTS_SIG_IS_NON_MSG(sig));

                tag = ((ErtsSignal *) sig)->common.tag;
                type = ERTS_PROC_SIG_TYPE(tag);
                op = ERTS_PROC_SIG_OP(tag);

                switch (op) {

                case ERTS_SIG_Q_OP_EXIT:
                case ERTS_SIG_Q_OP_EXIT_LINKED:
                case ERTS_SIG_Q_OP_MONITOR_DOWN:
                    switch (type) {
                    case ERTS_SIG_Q_TYPE_GEN_EXIT:
                        if (!ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig))
                            debug_foreach_sig_heap_frags(&sig->hfrag, oh_func, arg);
                        else {
                            oh_func(&sig->hfrag.off_heap, arg);
                            debug_foreach_sig_external(sig, ext_func, arg);
                        }
                        break;
                    case ERTS_LNK_TYPE_PORT:
                    case ERTS_LNK_TYPE_PROC:
                    case ERTS_LNK_TYPE_DIST_PROC:
                    case ERTS_LNK_TYPE_DIST_PORT:
                        lnk_func((ErtsLink *) sig, arg, -1);
                        break;
                    case ERTS_MON_TYPE_PORT:
                    case ERTS_MON_TYPE_PROC:
                    case ERTS_MON_TYPE_DIST_PROC:
                    case ERTS_MON_TYPE_DIST_PORT:
                    case ERTS_MON_TYPE_NODE:
                        mon_func((ErtsMonitor *) sig, arg, -1);
                        break;
                    default:
                        ERTS_INTERNAL_ERROR("Unexpected sig type");
                        break;
                    }
                    break;

                case ERTS_SIG_Q_OP_ADJ_MSGQ:
                case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
                    debug_foreach_sig_heap_frags(&sig->hfrag, oh_func, arg);
                    break;

                case ERTS_SIG_Q_OP_ALTACT_MSG: {
                    void *attached;
                    ErlHeapFragment *hfp;
                    (void) get_altact_msg_data(sig, &attached, NULL, NULL,
                                               NULL, NULL, NULL, NULL);
                    if (!attached)
                        break; /* on heap */
                    if (attached == ERTS_MSG_COMBINED_HFRAG)
                        hfp = &sig->hfrag;
                    else
                        hfp = (ErlHeapFragment *) attached;
                    debug_foreach_sig_heap_frags(hfp, oh_func, arg);
                    break;
                }

                case ERTS_SIG_Q_OP_DEMONITOR:
                    if (type == ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR) {
                        debug_foreach_sig_fake_oh(((ErtsSigDistProcDemonitor *) sig)->ref,
                                                  oh_func, arg);
                        break;
                    }
                    /* Fall through... */

                case ERTS_SIG_Q_OP_MONITOR:
                    mon_func((ErtsMonitor *) sig, arg, -1);
                    break;

                case ERTS_SIG_Q_OP_UNLINK:
                    if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
                        debug_foreach_sig_fake_oh(((ErtsSigDistUnlinkOp *) sig)->remote,
                                                  oh_func, arg);
                    }
                    break;
                    
                case ERTS_SIG_Q_OP_UNLINK_ACK:
                    break;
                    
                case ERTS_SIG_Q_OP_LINK:
                    lnk_func((ErtsLink *) sig, arg, -1);
                    break;

                case ERTS_SIG_Q_OP_GROUP_LEADER: {
                    ErtsSigGroupLeader *sgl = (ErtsSigGroupLeader *) sig;
                    oh_func(&sgl->oh, arg);
                    break;
                }

                case ERTS_SIG_Q_OP_IS_ALIVE:
                case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
                case ERTS_SIG_Q_OP_PROCESS_INFO:
                case ERTS_SIG_Q_OP_RECV_MARK:
		case ERTS_SIG_Q_OP_FLUSH:
                case ERTS_SIG_Q_OP_RPC:
                case ERTS_SIG_Q_OP_NIF_SELECT:
                    break;

                default:
                    ERTS_INTERNAL_ERROR("Unknown signal");
                    break;
                }

            }
        }
    }
}

#ifdef ERTS_PROC_SIG_HARD_DEBUG_RECV_MARKER

void
erl_proc_sig_hdbg_chk_recv_marker_block(Process *c_p)
{
    int ix, used, unused, free;
    ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;

    if (!blkp)
	return;

    unused = used = 0;
    ix = blkp->used_ix;
    ERTS_ASSERT(0 <= ix && ix < ERTS_RECV_MARKER_BLOCK_SIZE);

    do {
	int pix, nix;
	ErtsRecvMarker *markp = &blkp->marker[ix];
	Eterm ref = blkp->ref[ix];
	
	ERTS_ASSERT(is_internal_ref(ref)
		    || is_small(ref)
		    || is_big(ref)
		    || ref == am_undefined
		    || is_nil(ref));

	if (ref == am_undefined)
	    unused++;

	ASSERT(markp->used);

	pix = markp->prev_ix;
	nix = markp->next_ix;

	ERTS_ASSERT(0 <= pix && pix < ERTS_RECV_MARKER_BLOCK_SIZE);
	ERTS_ASSERT(0 <= nix && nix < ERTS_RECV_MARKER_BLOCK_SIZE);
	ERTS_ASSERT(blkp->marker[pix].next_ix == ix);
	ERTS_ASSERT(blkp->marker[nix].prev_ix == ix);

	used++;
	ERTS_ASSERT(used <= ERTS_RECV_MARKER_BLOCK_SIZE);

	ix = nix;
    } while (ix != blkp->used_ix);
    
    ERTS_ASSERT(unused == blkp->unused);

    free = 0;

    ix = blkp->free_ix;
    if (ix >= 0) {
	ERTS_ASSERT(ix < ERTS_RECV_MARKER_BLOCK_SIZE);
	
	do {
	    Eterm ref = blkp->ref[ix];
	    ERTS_ASSERT(ref == am_free);
	    ASSERT(!blkp->marker[ix].used);
	    free++;
	    ERTS_ASSERT(free < ERTS_RECV_MARKER_BLOCK_SIZE);
	    ix = blkp->marker[ix].next_ix;
	} while (ix >= 0);
    }

    ERTS_ASSERT(used + free == ERTS_RECV_MARKER_BLOCK_SIZE);

    ERTS_ASSERT(old_recv_marker == 0 || old_recv_marker == 1);
}

#endif /* ERTS_PROC_SIG_HARD_DEBUG_RECV_MARKER */


#ifdef ERTS_PROC_SIG_HARD_DEBUG

static void
chk_eterm(Process *c_p, int privq, ErtsMessage *mp, Eterm term)
{
    ErlHeapFragment *bp;
    Eterm *ptr = NULL;

    switch (primary_tag(term)) {
    case TAG_PRIMARY_IMMED1:
        return;
    case TAG_PRIMARY_LIST:
        ptr = list_val(term);
        ERTS_ASSERT(!is_header(CAR(ptr)));
        ERTS_ASSERT(!is_header(CDR(ptr)));
        break;
    case TAG_PRIMARY_BOXED:
        ptr = boxed_val(term);
        ERTS_ASSERT(is_header(*ptr));
        break;
    case TAG_PRIMARY_HEADER:
    default:
        ERTS_INTERNAL_ERROR("Not valid term");
        break;
    }

    if (erts_is_literal(term, ptr))
        return;

    for (bp = erts_message_to_heap_frag(mp); bp; bp = bp->next) {
        if (bp->mem <= ptr && ptr < bp->mem + bp->used_size)
            return;
    }

    ASSERT(erts_dbg_within_proc(ptr, c_p, NULL));
}

static Sint
proc_sig_hdbg_check_queue(Process *proc,
                          int privq,
                          ErtsMessage **sig_next,
                          ErtsMessage **sig_last,
                          ErtsMessage **sig_nm_next,
                          ErtsMessage **sig_nm_last,
                          ErtsSigRecvTracing *tracing,
                          int *found_set_save_recv_marker_p,
                          erts_aint32_t nmsig_psflg,
                          erts_aint32_t msig_psflg)
{
    ErtsMessage **next, *sig, **nm_next, **nm_last;
    int last_nm_sig_found, nm_sigs = 0, found_next_trace = 0,
        found_save = 0, last_sig_found = 0, recv_marker = 0,
        recv_marker_set_save = 0;
    Sint msg_len = 0;
    ErtsMessage **next_trace = tracing ? tracing->messages.next : NULL;
    ErtsMessage **save = proc->sig_qs.save;

    if (!privq) {
        ErtsSignal *sig = (ErtsSignal *) *sig_next;
        if (sig->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK) {

        }
    }

    nm_next = sig_nm_next;
    nm_last = sig_nm_last;
    next = sig_next;
    sig = *sig_next;

    last_nm_sig_found = !nm_last;
    if (last_nm_sig_found)
        ERTS_ASSERT(!nm_next);
    else
        ERTS_ASSERT(nm_next);

    while (1) {
        ErtsSignal *nm_sig;

        if (next == sig_last) {
            ERTS_ASSERT(!*next);
            last_sig_found = 1;
        }

        if (next == save)
            found_save = 1;

        if (next == next_trace) {
            found_next_trace = 1;
            ERTS_ASSERT(nm_sigs == 0);
        }

        while (sig
	       && (ERTS_SIG_IS_MSG(sig)
		   || (ERTS_SIG_DBG_IS_HANDLED_RECV_MARKER(sig)))) {
            int i;
            if (ERTS_SIG_IS_RECV_MARKER(sig)) {
                ErtsRecvMarker *markp = (ErtsRecvMarker *) sig;
                recv_marker++;
                ERTS_ASSERT(!markp->set_save);
                ERTS_ASSERT(next == markp->prev_next);
            }
            else {
                if (ERTS_SIG_IS_EXTERNAL_MSG(sig))
                    i = 1;
                else
                    i = 0;
                for (; i < ERL_MESSAGE_REF_ARRAY_SZ; i++)
                    chk_eterm(proc, privq, sig, sig->m[i]);
                msg_len++;
            }

            next = &sig->next;
            sig = sig->next;

            if (next == sig_last) {
                ERTS_ASSERT(!*next);
                last_sig_found = 1;
            }

            if (next == save)
                found_save = 1;

            if (next == next_trace) {
                found_next_trace = 1;
                ERTS_ASSERT(nm_sigs == 0);
            }
        }

        if (!sig)
            break;

        if (ERTS_SIG_IS_RECV_MARKER(sig)) {
            ErtsRecvMarker *markp = (ErtsRecvMarker *) sig;
	    ErtsRecvMarkerBlock *blkp = proc->sig_qs.recv_mrk_blk;
	    ERTS_ASSERT(blkp);
            recv_marker++;
            if (markp->set_save) {
                ERTS_ASSERT(blkp->pending_set_save_ix
			    == ERTS_RECV_MARKER_IX__(blkp, markp));
                recv_marker_set_save++;
            }
            if (privq < 0)
                ERTS_ASSERT(next == markp->prev_next);
        }
        
        nm_sig = (ErtsSignal *) sig;

        if (nm_sig->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK) {
            ERTS_ASSERT(!privq);
            ERTS_ASSERT(sig == *sig_next);
        }
        else {
            nm_sigs++;

            ERTS_ASSERT(!last_nm_sig_found);
            ERTS_ASSERT(ERTS_SIG_IS_NON_MSG(sig));

            ERTS_ASSERT(nm_next == next);

            if (nm_last == next) {
                ERTS_ASSERT(!nm_sig->common.specific.next);
                last_nm_sig_found = 1;
            }

            nm_next = nm_sig->common.specific.next;

        }

        next = &nm_sig->common.next;
        sig = nm_sig->common.next;

    }

    if (found_set_save_recv_marker_p)
        (*found_set_save_recv_marker_p) += recv_marker_set_save;

    if (!privq) {
        /* outer queue */
        ERTS_ASSERT(!found_save);
        ERTS_ASSERT(!recv_marker);
    }
    else if (privq > 0) {
        /* middle queue */
        ERTS_ASSERT(!next_trace || found_next_trace);
        ERTS_ASSERT(!found_save);
    }
    else {
        /* inner queue */
        ERTS_ASSERT(!found_next_trace);
        ERTS_ASSERT(nm_sigs == 0);
        ERTS_ASSERT(found_save);
    }

    ERTS_ASSERT(last_nm_sig_found);
    ERTS_ASSERT(last_sig_found);

    if (nmsig_psflg|msig_psflg) {
        erts_aint32_t state = erts_atomic32_read_nob(&proc->state);
        int using_buffers = !!erts_atomic_read_nob(&proc->sig_inq_buffers);
        if (nmsig_psflg) {
            ERTS_ASSERT(nm_sigs
                        ? !!(state & nmsig_psflg)
                        : (!(state & nmsig_psflg) || using_buffers));
        }
        if (msig_psflg) {
            ERTS_ASSERT(msg_len
                        ? !!(state & msig_psflg)
                        : (!(state & msig_psflg) || using_buffers));
        }
    }

    return msg_len;
}

void
erts_proc_sig_hdbg_check_priv_queue(Process *p, int qlock, char *what, char *file, int line)
{
    int found_set_save_recv_marker = 0;
    Sint len, len1, len2;
    ErtsRecvMarkerBlock *blkp = p->sig_qs.recv_mrk_blk;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking()
                   || ERTS_PROC_IS_EXITING(p)
                   || (ERTS_PROC_LOCK_MAIN
                       & erts_proc_lc_my_proc_locks(p)));
    len1 = proc_sig_hdbg_check_queue(p,
                                     -1,
                                     &p->sig_qs.first,
                                     p->sig_qs.last,
                                     NULL,
                                     NULL,
                                     NULL,
                                     &found_set_save_recv_marker,
                                     0,
                                     0);
    len2 = proc_sig_hdbg_check_queue(p,
                                     1,
                                     &p->sig_qs.cont,
                                     p->sig_qs.cont_last,
                                     p->sig_qs.nmsigs.next,
                                     p->sig_qs.nmsigs.last,
                                     NULL,
                                     &found_set_save_recv_marker,
                                     ERTS_PSFLG_SIG_Q,
                                     0);
    ERTS_ASSERT(found_set_save_recv_marker == 1
                || found_set_save_recv_marker == 0);
    ERTS_ASSERT(found_set_save_recv_marker || !blkp || blkp->pending_set_save_ix < 0);
    ERTS_ASSERT(!found_set_save_recv_marker || blkp->pending_set_save_ix >= 0);
    len = proc_sig_privqs_len(p, qlock);
    ERTS_ASSERT(len == len1 + len2);
}

void
erts_proc_sig_hdbg_check_in_queue(Process *p, struct ErtsSignalInQueue_ *buffer,
                                  char *what, char *file, int line)
{
    Sint len;
    int nmsig_flag, msig_flag;
    ERTS_LC_ASSERT(&p->sig_inq != buffer
                   || erts_thr_progress_is_blocking()
                   || ERTS_PROC_IS_EXITING(p)
                   || (ERTS_PROC_LOCK_MSGQ
                       & erts_proc_lc_my_proc_locks(p)));
    if (buffer != &p->sig_inq) {
        nmsig_flag = 0;
        msig_flag = 0;
    }
    else {
        nmsig_flag = ERTS_PSFLG_NMSG_SIG_IN_Q;
        msig_flag = ERTS_PSFLG_MSG_SIG_IN_Q;
    }

    len = proc_sig_hdbg_check_queue(p,
                                    0,
                                    &p->sig_inq.first,
                                    p->sig_inq.last,
                                    p->sig_inq.nmsigs.next,
                                    p->sig_inq.nmsigs.last,
                                    NULL,
                                    NULL,
                                    nmsig_flag,
                                    msig_flag);
}

#endif /* ERTS_PROC_SIG_HARD_DEBUG */

void erts_proc_sig_queue_lock(Process* proc)
{
    if (EBUSY == erts_proc_trylock(proc, ERTS_PROC_LOCK_MSGQ)) {
        erts_proc_lock(proc, ERTS_PROC_LOCK_MSGQ);
        proc->sig_inq_contention_counter += 1;
    } else if(proc->sig_inq_contention_counter > 0) {
        proc->sig_inq_contention_counter -= 1;
    }
}

static void proc_sig_queue_lock_buffer(ErtsSignalInQueueBuffer* slot)
{
    erts_mtx_lock(&slot->b.lock);
}

static void proc_sig_queue_unlock_buffer(ErtsSignalInQueueBuffer* slot)
{
    erts_mtx_unlock(&slot->b.lock);
}

int
erts_proc_sig_queue_try_enqueue_to_buffer(Eterm from,
                                          Process* receiver,
                                          ErtsProcLocks receiver_locks,
                                          ErtsMessage* first,
                                          ErtsMessage** last,
                                          ErtsMessage** last_next,
                                          Uint len)
{
    int need_unget_buffers;
    ErtsSignalInQueueBufferArray* buffers;
    if ((receiver_locks & ERTS_PROC_LOCK_MSGQ) ||
        NULL == (buffers = erts_proc_sig_queue_get_buffers(receiver, &need_unget_buffers))) {
        /* We never need to unget the buffers array if we do not get it */
        return 0;
    } else {
        int nonmsg = ERTS_SIG_IS_NON_MSG(first);
        int restarted = 0;
        ErtsSignalInQueueBuffer* buffer;
        Uint64 nonempty_slots_before;
        Uint32 slot, state;

        ERTS_UNDEF(nonempty_slots_before, 0);

        ASSERT(is_value(from));

        /* Use the sender id to hash to an outer signal queue buffer. This
         * guarantees that all signals from the same process are ordered in
         * send order. */
        slot = erts_internal_hash(from) %
                ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS;
        buffer = &buffers->slots[slot];

        proc_sig_queue_lock_buffer(buffer);

        if ( ! buffer->b.alive ) {
            /*
             * The enqueue attempt fails if the buffer is dead. This
             * means that the buffer array has got uninstalled.
             */
            proc_sig_queue_unlock_buffer(buffer);
            erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);
            return 0;
        }

        /*
         *  The buffer is alive and locked. This means that it is safe
         *  to insert signals to it
         */

        while (!0) {
            Uint64 slots_before;
            /*
             * This loop body is only executed more than once if another
             * thread is currently flushing buffers at the same time as
             * this operation is performed. The restart of the loop
             * prevents info in 'msgs_in_slots' and 'nonmsgs_in_slots'
             * from being lost. The loop can at most be restarted twice
             * since we have the lock on the buffer. Restarting twice is,
             * however, extremely unlikely to happen...
             */

            if (len && !erts_atomic32_read_nob(&buffers->msgs_in_slots)) {
                erts_atomic32_set_nob(&buffers->msgs_in_slots, !0);
            }

            if (nonmsg && !erts_atomic32_read_nob(&buffers->nonmsgs_in_slots)) {
                erts_atomic32_set_nob(&buffers->nonmsgs_in_slots, !0);
            }

            if (&buffer->b.queue.first == buffer->b.queue.last) {
                /* The buffer is empty so we need to notify the receiver
                 * unless some other slot is nonempty (in that case
                 * another enqueuer has already (or will) notified the
                 * receiver).
                 */
                slots_before = ((Uint64) erts_atomic64_read_bor_mb(
                                    &buffers->nonempty_slots,
                                    (erts_aint64_t)(((Uint64)1) << slot)));
            }
            else {
                slots_before = ((Uint64) erts_atomic64_read_mb(
                                    &buffers->nonempty_slots));
                if (!(slots_before & (((Uint64)1) << slot))) {
                    /*
                     * Someone is flushing buffers and has not yet handled
                     * this buffers. That is, it is no point in continuing
                     * with verification of 'msgs_in_slots' and
                     * 'nonmsgs_in_slots'. We only need this information
                     * while signals are in the buffer and we have not yet
                     * decided to fetch it. We know that it soon *will* be
                     * emptied by the flush operation.
                     */
                    break;
                }
            }

            if (!restarted)
                nonempty_slots_before = slots_before;

            if ((len && !erts_atomic32_read_nob(&buffers->msgs_in_slots))
                || (nonmsg && !erts_atomic32_read_nob(&buffers->nonmsgs_in_slots))) {
                restarted = !0;
                continue; /* restart loop */
            }

            break;
        }

        state = erts_atomic32_read_nob(&receiver->state);

        state = enqueue_signals(!0,
                                receiver,
                                first,
                                last,
                                last_next,
                                len,
                                state,
                                &buffer->b.queue);
        buffer->b.nr_of_enqueues += 1;

        proc_sig_queue_unlock_buffer(buffer);

        /*
         * The signal(s) are inserted into a buffer. However, we are
         * not done because we need to notify the scheduler about that
         * we have new signals.
         */

        if (!nonempty_slots_before) {

            /*
             * There is one situation in which we need to synchronize
             * with the ERTS_PROC_LOCK_MSGQ lock:
             *
             * The buffer we inserted to was empty before we inserted
             * to it, and no other buffer was marked as nonempty. In
             * this case the process might hold the
             * ERTS_PROC_LOCK_MSGQ to check if there are any more
             * messages. If the process does not find any messages,
             * it tells the scheduler to put the process to sleep
             * while still holding the lock. Therefore, we wait until
             * the ERTS_PROC_LOCK_MSGQ is released before we requests
             * the scheduler to schedule the process (with a call to
             * erts_proc_notify_new_message or
             * erts_proc_notify_new_sig) so the request does not get
             * overwritten by the sleep request.
             *
             */

            erts_proc_lock_wait_until_released(receiver, ERTS_PROC_LOCK_MSGQ);
        }

        if (nonmsg) {
            erts_proc_notify_new_sig(receiver, state,
                                     len ? ERTS_PSFLG_ACTIVE : 0);
        }
        else {
            ASSERT(len);
            erts_proc_notify_new_message(receiver, receiver_locks);
        }
        erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);
        return 1;
    }
}


static void sig_inq_concat(ErtsSignalInQueue* q1, ErtsSignalInQueue* q2)
{
    ErtsMessage** first_queue_last = q1->last;
    /* Second queue should not be empty */
    ERTS_HDBG_INQ_LEN(q1);
    ERTS_HDBG_INQ_LEN(q2);
    ASSERT(q2->last != &q2->first);
    if (NULL == q2->nmsigs.next) {
        q1->mlenoffs += q2->mlenoffs;
    }
    else {
        ErtsNonMsgSignal *nmsig = (ErtsNonMsgSignal *) *q2->nmsigs.next;
        ASSERT(nmsig);
        if (NULL == q1->nmsigs.next) {
            /* There is non-message signals in q2 but not in q1 */
            if (q2->nmsigs.next == &q2->first) {
                /* The first message in q2 is a non-message signal
                   (The next pointer to the first non-message signal
                   comes from the first queue) */
                q1->nmsigs.next = first_queue_last;
                ASSERT(nmsig->mlenoffs == 0);
            } else {
                /* Internal message in q2 is the first non-message signal */
                q1->nmsigs.next = q2->nmsigs.next;
            }
            if (q2->nmsigs.next == q2->nmsigs.last) {
                /* Only one non-message signal in q2 (q1->nmsigs.last
                   should be the same as q1->nmsigs.next which is
                   already set up correctly) */
                q1->nmsigs.last = q1->nmsigs.next;
            } else {
                /* More than one non-message signals in q2 */
                q1->nmsigs.last = q2->nmsigs.last;
            }
        }
        else {
            ErtsMessage** first_nmsig_in_q2;
            ASSERT(nmsig);
            /* We have non-message signals in both queues */
            if (q2->nmsigs.next == &q2->first) {
                /* The first signal in q2 is a non-message signal */
                ErtsSignal *sig;
                sig = (ErtsSignal *) *q1->nmsigs.last;
                sig->common.specific.next = first_queue_last;
                first_nmsig_in_q2 = first_queue_last;
                ASSERT(nmsig->mlenoffs == 0);
            } else {
                /* The first signal in q2 is a message signal */
                ErtsSignal *sig;
                sig = (ErtsSignal *) *q1->nmsigs.last;
                sig->common.specific.next = q2->nmsigs.next;
                first_nmsig_in_q2 = q2->nmsigs.next;
            }
            if (q2->nmsigs.last == &q2->first) {
                /* Only one non-message signal in q2 */
                q1->nmsigs.last = first_nmsig_in_q2;
            } else {
                q1->nmsigs.last = q2->nmsigs.last;
            }
        }
        nmsig->mlenoffs += q1->mlenoffs;
        q1->mlenoffs = q2->mlenoffs;
    }
    *q1->last = q2->first;
    q1->last = q2->last;
    ERTS_HDBG_INQ_LEN(q1);
    ASSERT((!q1->nmsigs.next && !q1->nmsigs.last) || (q1->nmsigs.next && q1->nmsigs.last));
}

static Uint proc_sig_queue_flush_buffer(Process* proc,
                                        Uint buffer_index,
                                        ErtsSignalInQueueBufferArray* buffers)
{
    Uint nr_of_enqueues;
    ErtsSignalInQueueBuffer* buf = &buffers->slots[buffer_index];
    proc_sig_queue_lock_buffer(buf);
    if (!buf->b.queue.first) {
#ifdef ERTS_PROC_SIG_HARD_DEBUG
        if (buf->b.alive) {
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &buf->b.queue);
        }
#endif
        nr_of_enqueues = buf->b.nr_of_enqueues;
        ASSERT(nr_of_enqueues == 0);
    }
    else {
        nr_of_enqueues = buf->b.nr_of_enqueues;
        buf->b.nr_of_enqueues = 0;
        ASSERT(nr_of_enqueues > 0);
        if (buf->b.alive) {
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &proc->sig_inq);
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &buf->b.queue);
            sig_inq_concat(&proc->sig_inq, &buf->b.queue);
            buf->b.queue.first = NULL;
            buf->b.queue.last = &buf->b.queue.first;
            buf->b.queue.mlenoffs = 0;
            buf->b.queue.nmsigs.next = NULL;
            buf->b.queue.nmsigs.last = NULL;
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &buf->b.queue);
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &proc->sig_inq);
        }
    }
    proc_sig_queue_unlock_buffer(buf);
    return nr_of_enqueues;
}


static void proc_sig_queue_flush_buffers(Process *proc, ErtsSignalInQueueBufferArray *buffers)
{
    Uint i;
    Uint64 nonempty_slots;

    ASSERT(buffers);
    erts_atomic32_set_nob(&buffers->msgs_in_slots, 0);
    erts_atomic32_set_nob(&buffers->nonmsgs_in_slots, 0);
    nonempty_slots = (Uint64)erts_atomic64_xchg_mb(&buffers->nonempty_slots,
                                                   (erts_aint64_t)((Uint64)0));
    if (nonempty_slots != 0) {
        for(i = 0; i < ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS; i++) {
            Uint64 slot_mask = (((Uint64)1) << i);
            if (nonempty_slots & slot_mask) {
                buffers->nr_of_enqueues +=
                    proc_sig_queue_flush_buffer(proc, i, buffers);
            }
        }
    }
    if (--buffers->nr_of_rounds_left == 0) {
        /* Take decision if we should adapt back to the normal state */
        if(buffers->nr_of_enqueues <
           ERTS_PROC_SIG_INQ_BUFFERED_MIN_NO_ENQUEUES_TO_KEEP) {
            erts_proc_sig_queue_flush_and_deinstall_buffers(proc);
        } else {
            buffers->nr_of_rounds_left =
                ERTS_PROC_SIG_INQ_BUFFERED_MIN_FLUSH_ALL_OPS_BEFORE_CHANGE;
            buffers->nr_of_enqueues = 0;
        }
    }
}


void
erts_proc_sig_queue_flush_buffers(Process* proc)
{
    ErtsSignalInQueueBufferArray* buffers;
    int need_unget_buffers;

    ERTS_LC_ASSERT(ERTS_PROC_IS_EXITING(proc) ||
                   (erts_proc_lc_my_proc_locks(proc) & ERTS_PROC_LOCK_MSGQ));

    buffers = erts_proc_sig_queue_get_buffers(proc, &need_unget_buffers);
    if (buffers) {
        proc_sig_queue_flush_buffers(proc, buffers);
        erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);
    }
}

static void sigq_buffer_array_refc_dec(void *buffers_p)
{
    ErtsSignalInQueueBufferArray* buffers = buffers_p;
    erts_proc_sig_queue_unget_buffers(buffers, 1);
}


static void schedule_sigq_buffer_array_refc_dec(void *buffers_p)
{
    ErtsSignalInQueueBufferArray* buffers = buffers_p;
    erts_schedule_thr_prgr_later_cleanup_op(sigq_buffer_array_refc_dec,
                                            buffers,
                                            &buffers->free_item,
                                            sizeof(ErtsSignalInQueueBufferArray));
}

void erts_proc_sig_queue_flush_and_deinstall_buffers(Process* proc)
{
    Uint i;
    ErtsSignalInQueueBufferArray* buffers;
    int need_unget_buffers;
    ErtsSchedulerData *esdp;

    ERTS_LC_ASSERT(ERTS_PROC_IS_EXITING(proc) ||
                   (erts_proc_lc_my_proc_locks(proc) & ERTS_PROC_LOCK_MSGQ));
    buffers = erts_proc_sig_queue_get_buffers(proc, &need_unget_buffers);

    if (buffers == NULL) {
        return;
    }

    if (!buffers->alive) {
        erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);;
        return;
    }

    buffers->alive = 0;
    proc->sig_inq_contention_counter = 0;

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &proc->sig_inq);
    for (i = 0; i < ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS; i++) {
        proc_sig_queue_lock_buffer(&buffers->slots[i]);

        if (buffers->slots[i].b.queue.first != NULL) {
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &buffers->slots[i].b.queue);
            sig_inq_concat(&proc->sig_inq, &buffers->slots[i].b.queue);
            ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &proc->sig_inq);
        }

        buffers->slots[i].b.alive = 0;

        proc_sig_queue_unlock_buffer(&buffers->slots[i]);
    }

    /* Nothing can be enqueued to the buffer array beyond this point. */

    erts_atomic32_set_nob(&buffers->nonmsgs_in_slots, 0);
    erts_atomic32_set_nob(&buffers->msgs_in_slots, 0);
    erts_atomic64_set_nob(&buffers->nonempty_slots, (erts_aint64_t)0);
    erts_atomic_set_mb(&proc->sig_inq_buffers, (erts_aint_t)NULL);

    erts_proc_sig_queue_unget_buffers(buffers, need_unget_buffers);

    /* Release the buffer array through thread progress, as a managed thread
     * may be holding a reference to it. */
    esdp = erts_get_scheduler_data();
    if (esdp != NULL && esdp->type == ERTS_SCHED_NORMAL) {
        schedule_sigq_buffer_array_refc_dec((void*)buffers);
    } else {
        /* We can't issue cleanup jobs on anything other than normal
         * schedulers, so we move to the first scheduler if required. */
        erts_schedule_misc_aux_work(1,
                                    schedule_sigq_buffer_array_refc_dec,
                                    buffers);
    }
}

void erts_proc_sig_queue_maybe_install_buffers(Process* p, erts_aint32_t state)
{
    int i;
    ErtsSignalInQueueBufferArray* buffers;
    if (!(state & ERTS_PSFLG_OFF_HEAP_MSGQ) ||
        (((ErtsSignalInQueueBufferArray*)erts_atomic_read_nob(&p->sig_inq_buffers)) != NULL) ||
        (!ERTS_PROC_SIG_INQ_BUFFERED_ALWAYS_TURN_ON &&
         p->sig_inq_contention_counter <= ERTS_PROC_SIG_INQ_BUFFERED_CONTENTION_INSTALL_LIMIT)) {
        return;
    }
    p->sig_inq_contention_counter = 0;
    buffers = erts_alloc(ERTS_ALC_T_SIGQ_BUFFERS,
                         sizeof(ErtsSignalInQueueBufferArray));
    erts_atomic64_init_nob(&buffers->nonempty_slots, (erts_aint64_t)(Uint64)0);
    erts_atomic32_init_nob(&buffers->nonmsgs_in_slots, 0);
    erts_atomic32_init_nob(&buffers->msgs_in_slots, 0);
    erts_refc_init(&buffers->dirty_refc, 1);
    buffers->nr_of_enqueues = 0;
    buffers->nr_of_rounds_left =
        ERTS_PROC_SIG_INQ_BUFFERED_MIN_FLUSH_ALL_OPS_BEFORE_CHANGE;
    buffers->alive = 1;
    /* Initialize  slots */
    for(i = 0; i < ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS; i++) {
        buffers->slots[i].b.alive = 1;
        erts_mtx_init(&buffers->slots[i].b.lock,
                      "proc_sig_queue_buffer",
                      NIL,
                      ERTS_LOCK_FLAGS_CATEGORY_PROCESS);
        buffers->slots[i].b.queue.first = NULL;
        buffers->slots[i].b.queue.last = &buffers->slots[i].b.queue.first;
        buffers->slots[i].b.queue.mlenoffs = 0;
        buffers->slots[i].b.queue.nmsigs.next = NULL;
        buffers->slots[i].b.queue.nmsigs.last = NULL;
        buffers->slots[i].b.nr_of_enqueues = 0;
    }
    erts_atomic_set_relb(&p->sig_inq_buffers, (erts_aint_t)buffers);
}

/* Only for test purposes */
int erts_proc_sig_queue_force_buffers(Process* p)
{
    erts_aint32_t state;
    ErtsSignalInQueueBufferArray* buffers;

    erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
    state = erts_atomic32_read_nob(&p->state);
    /* Fake contention */
    p->sig_inq_contention_counter =
        1 + ERTS_PROC_SIG_INQ_BUFFERED_CONTENTION_INSTALL_LIMIT;
    erts_proc_sig_queue_maybe_install_buffers(p, state);
    buffers = ((ErtsSignalInQueueBufferArray*)
               erts_atomic_read_nob(&p->sig_inq_buffers));
    if (buffers) {
        /* "Prevent" buffer deinstallation */
        buffers->nr_of_rounds_left = ERTS_UINT_MAX;
    }
    erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    return buffers != NULL;
}
