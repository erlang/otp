/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018. All Rights Reserved.
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
#include "dtrace-wrapper.h"

#define ERTS_SIG_REDS_CNT_FACTOR 4
#define ERTS_PROC_SIG_TRACE_COUNT_LIMIT 200

/*
 * Note that not all signal are handled using this functionality!
 */

#define ERTS_SIG_Q_OP_MAX 13

#define ERTS_SIG_Q_OP_EXIT                      0  /* Exit signal due to bif call */
#define ERTS_SIG_Q_OP_EXIT_LINKED               1  /* Exit signal due to link break*/
#define ERTS_SIG_Q_OP_MONITOR_DOWN              2
#define ERTS_SIG_Q_OP_MONITOR                   3
#define ERTS_SIG_Q_OP_DEMONITOR                 4
#define ERTS_SIG_Q_OP_LINK                      5
#define ERTS_SIG_Q_OP_UNLINK                    6
#define ERTS_SIG_Q_OP_GROUP_LEADER              7
#define ERTS_SIG_Q_OP_TRACE_CHANGE_STATE        8
#define ERTS_SIG_Q_OP_PERSISTENT_MON_MSG        9
#define ERTS_SIG_Q_OP_IS_ALIVE                  10
#define ERTS_SIG_Q_OP_PROCESS_INFO              11
#define ERTS_SIG_Q_OP_SYNC_SUSPEND              12
#define ERTS_SIG_Q_OP_RPC                       ERTS_SIG_Q_OP_MAX

#define ERTS_SIG_Q_TYPE_MAX (ERTS_MON_LNK_TYPE_MAX + 5)

#define ERTS_SIG_Q_TYPE_UNDEFINED \
    (ERTS_MON_LNK_TYPE_MAX + 1)
#define ERTS_SIG_Q_TYPE_DIST_LINK \
    (ERTS_MON_LNK_TYPE_MAX + 2)
#define ERTS_SIG_Q_TYPE_GEN_EXIT \
    (ERTS_MON_LNK_TYPE_MAX + 3)
#define ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR \
    (ERTS_MON_LNK_TYPE_MAX + 4)
#define ERTS_SIG_Q_TYPE_ADJUST_TRACE_INFO \
    ERTS_SIG_Q_TYPE_MAX

#define ERTS_SIG_IS_GEN_EXIT(sig)                                       \
    (ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag) == ERTS_SIG_Q_TYPE_GEN_EXIT)
#define ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig)                              \
    (ASSERT(ERTS_SIG_IS_GEN_EXIT(sig)),is_non_value(get_exit_signal_data(sig)->reason))

Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler);
Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler_high);
Process *ERTS_WRITE_UNLIKELY(erts_dirty_process_signal_handler_max);

void
erts_proc_sig_queue_init(void)
{
    ERTS_CT_ASSERT(ERTS_SIG_Q_OP_MASK > ERTS_SIG_Q_OP_MAX);
    ERTS_CT_ASSERT(ERTS_SIG_Q_OP_MSGQ_LEN_OFFS_MARK > ERTS_SIG_Q_OP_MAX);
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
        ErtsTracingEvent *event;
    } messages;
} ErtsSigRecvTracing;

typedef struct {
    Eterm message;
    Eterm from;
    Eterm reason;
    union {
        Eterm ref;
        int normal_kills;
    } u;
} ErtsExitSignalData;

typedef struct {
    Eterm message;
    Eterm key;
} ErtsPersistMonMsg;

typedef struct {
    ErtsSignalCommon common;
    Eterm local; /* internal pid (immediate) */
    Eterm remote; /* external pid (heap for it follow) */
    Eterm heap[EXTERNAL_THING_HEAD_SIZE + 1];
} ErtsSigDistLinkOp;

typedef struct {
    ErtsSignalCommon common;
    Uint flags_on;
    Uint flags_off;
    Eterm tracer;
} ErtsSigTraceInfo;

#define ERTS_SIG_GL_FLG_ACTIVE          (((erts_aint_t) 1) << 0)
#define ERTS_SIG_GL_FLG_RECEIVER        (((erts_aint_t) 1) << 1)
#define ERTS_SIG_GL_FLG_SENDER          (((erts_aint_t) 1) << 2)

typedef struct {
    ErtsSignalCommon common;
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
    ErtsSignalCommon common;
    Sint refc;
    Sint delayed_len;
    Sint len_offset;
} ErtsProcSigMsgQLenOffsetMarker;

typedef struct {
    ErtsSignalCommon common;
    ErtsProcSigMsgQLenOffsetMarker marker;
    Sint msgq_len_offset;
    Eterm requester;
    Eterm ref;
    ErtsORefThing oref_thing;
    Uint reserve_size;
    Uint len;
    int flags;
    int item_ix[1]; /* of len size in reality... */
} ErtsProcessInfoSig;

#define ERTS_PROC_SIG_PI_MSGQ_LEN_IGNORE        ((Sint) -1)
#define ERTS_PROC_SIG_PI_MSGQ_LEN_SYNC          ((Sint) -2)

typedef struct {
    ErtsSignalCommon common;
    Eterm requester;
    Eterm (*func)(Process *, void *, int *, ErlHeapFragment **);
    void *arg;
    Eterm ref;
    ErtsORefThing oref_thing;
} ErtsProcSigRPC;

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
static void group_leader_reply(Process *c_p, Eterm to,
                               Eterm ref, int success);
static int stretch_limit(Process *c_p, ErtsSigRecvTracing *tp,
                         int abs_lim, int *limp);

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
                                  ERTS_PSFLG_FREE);             \
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
                          erts_aint32_t sig_psflg);
#else
#define ERTS_PROC_SIG_HDBG_PRIV_CHKQ(P, T, NMN)
#endif

typedef struct {
    ErtsSignalCommon common;
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

static ERTS_INLINE ErtsSigDistLinkOp *
make_sig_dist_link_op(int op, Eterm local, Eterm remote)
{
    Eterm *hp;
    ErlOffHeap oh = {0};
    ErtsSigDistLinkOp *sdlnk = erts_alloc(ERTS_ALC_T_SIG_DATA,
                                          sizeof(ErtsSigDistLinkOp));
    ASSERT(is_internal_pid(local));
    ASSERT(is_external_pid(remote));

    hp = &sdlnk->heap[0];

    sdlnk->common.tag = ERTS_PROC_SIG_MAKE_TAG(op,
                                               ERTS_SIG_Q_TYPE_DIST_LINK,
                                               0);
    sdlnk->local = local;
    sdlnk->remote = STORE_NC(&hp, &oh, remote);

    ASSERT(&sdlnk->heap[0] < hp);
    ASSERT(hp <= &sdlnk->heap[0] + sizeof(sdlnk->heap)/sizeof(sdlnk->heap[0]));
    ASSERT(boxed_val(sdlnk->remote) == &sdlnk->heap[0]);

    return sdlnk;
}

static ERTS_INLINE void
destroy_sig_dist_link_op(ErtsSigDistLinkOp *sdlnk)
{
    ASSERT(is_external_pid(sdlnk->remote));
    ASSERT(boxed_val(sdlnk->remote) == &sdlnk->heap[0]);
    erts_deref_node_entry(((ExternalThing *) &sdlnk->heap[0])->node,
                          make_boxed(&sdlnk->heap[0]));
    erts_free(ERTS_ALC_T_SIG_DATA, sdlnk);
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
destroy_trace_info(ErtsSigTraceInfo *ti)
{
    if (is_value(ti->tracer))
        erts_tracer_update(&ti->tracer, NIL);
    erts_free(ERTS_ALC_T_SIG_DATA, ti);
}

static void
destroy_sig_group_leader(ErtsSigGroupLeader *sgl)
{
    erts_cleanup_offheap(&sgl->oh);
    erts_free(ERTS_ALC_T_SIG_DATA, sgl);
}

static ERTS_INLINE void
sig_enqueue_trace(Process *c_p, ErtsMessage **sigp, int op,
                  Process *rp, ErtsMessage ***last_next)
{
    switch (op) {
    case ERTS_SIG_Q_OP_LINK:
        if (c_p
            && ((!!IS_TRACED(c_p))
                & (ERTS_TRACE_FLAGS(c_p) & (F_TRACE_SOL
                                            | F_TRACE_SOL1)))) {
            ErtsSigTraceInfo *ti;
            Eterm tag;
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
            ti->flags_on = ERTS_TRACE_FLAGS(c_p) & TRACEE_FLAGS;
            if (!(ti->flags_on & F_TRACE_SOL1))
                ti->flags_off = 0;
            else {
                ti->flags_off = F_TRACE_SOL1|F_TRACE_SOL;
                erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
                ERTS_TRACE_FLAGS(c_p) &= ~(F_TRACE_SOL1|F_TRACE_SOL);
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
            }
            erts_tracer_update(&ti->tracer, ERTS_TRACER(c_p));
            *sigp = (ErtsMessage *) ti;
            if (!*last_next || *last_next == sigp)
                *last_next = &ti->common.next;
        }
        break;

#ifdef USE_VM_PROBES
    case ERTS_SIG_Q_OP_EXIT:
    case ERTS_SIG_Q_OP_EXIT_LINKED:

        if (DTRACE_ENABLED(process_exit_signal)) {
            ErtsMessage* sig = *sigp;
            Uint16 type = ERTS_PROC_SIG_TYPE(((ErtsSignal *) sig)->common.tag);
            Eterm reason, from;

            if (type == ERTS_SIG_Q_TYPE_GEN_EXIT) {
                ErtsExitSignalData *xsigd = get_exit_signal_data(sig);
                reason = xsigd->reason;
                from = xsigd->from;
            }
            else {
                ErtsLink *lnk = (ErtsLink *) sig, *olnk;

                ASSERT(type == ERTS_LNK_TYPE_PROC
                       || type == ERTS_LNK_TYPE_PORT
                       || type == ERTS_LNK_TYPE_DIST_PROC);

                olnk = erts_link_to_other(lnk, NULL);
                reason = lnk->other.item;
                from = olnk->other.item;
            }

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

static ERTS_INLINE erts_aint32_t
enqueue_signals(Process *rp, ErtsMessage *first,
                ErtsMessage **last, ErtsMessage **last_next,
                Uint num_msgs,
                erts_aint32_t in_state)
{
    erts_aint32_t state = in_state;
    ErtsMessage **this = rp->sig_inq.last;

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(rp);

    ASSERT(!*this);
    *this = first;
    rp->sig_inq.last = last;

    if (!rp->sig_inq.nmsigs.next) {
        ASSERT(!rp->sig_inq.nmsigs.last);
        if (ERTS_SIG_IS_NON_MSG(first)) {
            rp->sig_inq.nmsigs.next = this;
        }
        else if (last_next) {
            ASSERT(first->next && ERTS_SIG_IS_NON_MSG(first->next));
            rp->sig_inq.nmsigs.next = &first->next;
        }
        else
            goto no_nmsig;

        state = erts_atomic32_read_bor_nob(&rp->state,
                                           ERTS_PSFLG_SIG_IN_Q);
    no_nmsig:
        ASSERT(!(state & ERTS_PSFLG_SIG_IN_Q));
    }
    else {
        ErtsSignal *sig;
        ASSERT(rp->sig_inq.nmsigs.last);

        sig = (ErtsSignal *) *rp->sig_inq.nmsigs.last;

        ASSERT(sig && !sig->common.specific.next);
        ASSERT(state & ERTS_PSFLG_SIG_IN_Q);
        if (ERTS_SIG_IS_NON_MSG(first)) {
            sig->common.specific.next = this;
        }
        else if (last_next) {
            ASSERT(first->next && ERTS_SIG_IS_NON_MSG(first->next));
            sig->common.specific.next = &first->next;
        }
    }

    if (last_next) {
        ASSERT(dbg_count_nmsigs(first) >= 2);
        rp->sig_inq.nmsigs.last = last_next;
    }
    else if (ERTS_SIG_IS_NON_MSG(first)) {
        ASSERT(dbg_count_nmsigs(first) == 1);
        rp->sig_inq.nmsigs.last = this;
    }
    else
        ASSERT(dbg_count_nmsigs(first) == 0);

    rp->sig_inq.len += num_msgs;

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(rp);

    return state;
}

erts_aint32_t erts_enqueue_signals(Process *rp, ErtsMessage *first,
                                   ErtsMessage **last, ErtsMessage **last_next,
                                   Uint num_msgs,
                                   erts_aint32_t in_state)
{
    return enqueue_signals(rp, first, last, last_next, num_msgs, in_state);
}

void
erts_make_dirty_proc_handled(Eterm pid,
                             erts_aint32_t state,
                             erts_aint32_t prio)
{
    Eterm *hp;
    ErtsMessage *mp;
    Process *sig_handler;

    ASSERT(state & ERTS_PSFLG_DIRTY_RUNNING);

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

static void
check_push_msgq_len_offs_marker(Process *rp, ErtsSignal *sig);


static int
proc_queue_signal(Process *c_p, Eterm pid, ErtsSignal *sig, int op)
{
    int res;
    Process *rp;
    ErtsMessage *first, *last, **last_next, **sigp;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int is_normal_sched = !!esdp && esdp->type == ERTS_SCHED_NORMAL;
    erts_aint32_t state;
    ErtsSignal *pend_sig;

    if (is_normal_sched) {
        pend_sig = esdp->pending_signal.sig;
        if (op == ERTS_SIG_Q_OP_MONITOR
            && ((ErtsMonitor*)sig)->type == ERTS_MON_TYPE_PROC) {

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
                erts_proc_sig_send_monitor_down((ErtsMonitor*)sig, am_noproc);
                return 1;
            }
        }
        else if (pend_sig && pid == esdp->pending_signal.to) {
            /* Flush pending signal to maintain signal order */
            esdp->pending_signal.sig = NULL;
            esdp->pending_signal.to = THE_NON_VALUE;

            rp = erts_proc_lookup_raw(pid);
            if (!rp) {
                erts_proc_sig_send_monitor_down((ErtsMonitor*)pend_sig, am_noproc);
                return 0;
            }

            /* Prepend pending signal */
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
            if (!rp)
                return 0;
        }
    }
    else {
        rp = erts_proc_lookup_raw_inc_refc(pid);
        if (!rp)
            return 0;
        pend_sig = NULL;
    }

    first = last = (ErtsMessage *) sig;
    last_next = NULL;
    sigp = &first;

first_last_done:
    sig->common.specific.next = NULL;

    /* may add signals before sig */
    sig_enqueue_trace(c_p, sigp, op, rp, &last_next);

    last->next = NULL;

    erts_proc_lock(rp, ERTS_PROC_LOCK_MSGQ);

    state = erts_atomic32_read_nob(&rp->state);

    if (ERTS_PSFLG_FREE & state)
        res = 0;
    else {
        state = enqueue_signals(rp, first, &last->next, last_next, 0, state);
        if (ERTS_UNLIKELY(op == ERTS_SIG_Q_OP_PROCESS_INFO))
            check_push_msgq_len_offs_marker(rp, sig);
        res = !0;
    }

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);

    if (res == 0) {
        sig_enqueue_trace_cleanup(first, sig);
        if (pend_sig) {
            erts_proc_sig_send_monitor_down((ErtsMonitor*)pend_sig, am_noproc);
            if (sig == pend_sig) {
                /* We did a switch, callers signal is now pending (still ok) */
                ASSERT(esdp->pending_signal.sig);
                res = 1;
            }
        }
    }
    else
        erts_proc_notify_new_sig(rp, state, 0);

    if (!is_normal_sched)
        erts_proc_dec_refc(rp);

    return res;
}

void erts_proc_sig_send_pending(ErtsSchedulerData* esdp)
{
    ErtsSignal* sig = esdp->pending_signal.sig;
    int op;

    ASSERT(esdp && esdp->type == ERTS_SCHED_NORMAL);
    ASSERT(sig);
    ASSERT(is_internal_pid(esdp->pending_signal.to));

    op = ERTS_SIG_Q_OP_MONITOR;
    ASSERT(op == ERTS_PROC_SIG_OP(sig->common.tag));

    if (!proc_queue_signal(NULL, esdp->pending_signal.to, sig, op)) {
        ErtsMonitor* mon = (ErtsMonitor*)sig;
        erts_proc_sig_send_monitor_down(mon, am_noproc);
    }
}

static int
maybe_elevate_sig_handling_prio(Process *c_p, Eterm other)
{
    /*
     * returns:
     *  > 0 -> elevated prio; process alive or exiting
     *  < 0 -> no elevation needed; process alive or exiting
     *    0 -> process terminated (free)
     */
    int res;
    Process *rp;
    erts_aint32_t state, my_prio, other_prio;

    rp = erts_proc_lookup_raw(other);
    if (!rp)
        res = 0;
    else {
        res = -1;
        state = erts_atomic32_read_nob(&c_p->state);
        my_prio = ERTS_PSFLGS_GET_USR_PRIO(state);

        state = erts_atomic32_read_nob(&rp->state);
        other_prio = ERTS_PSFLGS_GET_USR_PRIO(state);

        if (other_prio > my_prio) {
            /* Others prio is lower than mine; elevate it... */
            res = !!erts_sig_prio(other, my_prio);
            if (res) {
                /* ensure handled if dirty executing... */
                state = erts_atomic32_read_nob(&rp->state);
                /*
                 * We ignore ERTS_PSFLG_DIRTY_RUNNING_SYS. For
                 * more info see erts_execute_dirty_system_task()
                 * in erl_process.c.
                 */
                if (state & ERTS_PSFLG_DIRTY_RUNNING)
                    erts_make_dirty_proc_handled(other, state, my_prio);
            }
        }
    }
    return res;
}

void
erts_proc_sig_fetch__(Process *proc)
{
    ASSERT(proc->sig_inq.first);

    if (!proc->sig_inq.nmsigs.next) {
        ASSERT(!(ERTS_PSFLG_SIG_IN_Q
                 & erts_atomic32_read_nob(&proc->state)));
        ASSERT(!proc->sig_inq.nmsigs.last);

        if (proc->sig_qs.cont || ERTS_MSG_RECV_TRACED(proc)) {
            *proc->sig_qs.cont_last = proc->sig_inq.first;
            proc->sig_qs.cont_last = proc->sig_inq.last;
        }
        else {
            *proc->sig_qs.last = proc->sig_inq.first;
            proc->sig_qs.last = proc->sig_inq.last;
        }
    }
    else {
        erts_aint32_t s;
        ASSERT(proc->sig_inq.nmsigs.last);
         if (!proc->sig_qs.nmsigs.last) {
            ASSERT(!proc->sig_qs.nmsigs.next);
            if (proc->sig_inq.nmsigs.next == &proc->sig_inq.first)
                proc->sig_qs.nmsigs.next = proc->sig_qs.cont_last;
            else
                proc->sig_qs.nmsigs.next = proc->sig_inq.nmsigs.next;

            s = erts_atomic32_read_bset_nob(&proc->state,
                                            (ERTS_PSFLG_SIG_Q
                                             | ERTS_PSFLG_SIG_IN_Q),
                                            ERTS_PSFLG_SIG_Q);

            ASSERT((s & (ERTS_PSFLG_SIG_Q|ERTS_PSFLG_SIG_IN_Q))
                   == ERTS_PSFLG_SIG_IN_Q); (void)s;
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

            s = erts_atomic32_read_band_nob(&proc->state,
                                            ~ERTS_PSFLG_SIG_IN_Q);

            ASSERT((s & (ERTS_PSFLG_SIG_Q|ERTS_PSFLG_SIG_IN_Q))
                   == (ERTS_PSFLG_SIG_Q|ERTS_PSFLG_SIG_IN_Q)); (void)s;
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

    proc->sig_qs.len += proc->sig_inq.len;

    proc->sig_inq.first = NULL;
    proc->sig_inq.last = &proc->sig_inq.first;
    proc->sig_inq.len = 0;

}

Sint
erts_proc_sig_fetch_msgq_len_offs__(Process *proc)
{
    ErtsProcSigMsgQLenOffsetMarker *marker
        = (ErtsProcSigMsgQLenOffsetMarker *) proc->sig_inq.first;

    ASSERT(marker->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK);

    if (marker->common.next) {
        Sint len;

        proc->sig_qs.flags |= FS_DELAYED_PSIGQS_LEN;

        /*
         * Prevent update of sig_qs.len in fetch. These
         * updates are done via process-info signal(s)
         * instead...
         */
        len = proc->sig_inq.len;
        marker->delayed_len += len;
        marker->len_offset -= len;
        proc->sig_inq.len = 0;

        /*
         * Temorarily remove marker during fetch...
         */

        proc->sig_inq.first = marker->common.next;
        if (proc->sig_inq.last == &marker->common.next)
            proc->sig_inq.last = &proc->sig_inq.first;
        if (proc->sig_inq.nmsigs.next == &marker->common.next)
            proc->sig_inq.nmsigs.next = &proc->sig_inq.first;
        if (proc->sig_inq.nmsigs.last == &marker->common.next)
            proc->sig_inq.nmsigs.last = &proc->sig_inq.first;

        erts_proc_sig_fetch__(proc);

        marker->common.next = NULL;
        proc->sig_inq.first = (ErtsMessage *) marker;
        proc->sig_inq.last = &marker->common.next;

    }

    return marker->delayed_len;
}

static ERTS_INLINE Sint
proc_sig_privqs_len(Process *c_p, int have_qlock)
{
    Sint res = c_p->sig_qs.len;

    ERTS_LC_ASSERT(!have_qlock
                   ? (ERTS_PROC_LOCK_MAIN
                      == erts_proc_lc_my_proc_locks(c_p))
                   : ((ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_MAIN)
                      == ((ERTS_PROC_LOCK_MSGQ|ERTS_PROC_LOCK_MAIN)
                          & erts_proc_lc_my_proc_locks(c_p))));

    if (c_p->sig_qs.flags & FS_DELAYED_PSIGQS_LEN) {
        ErtsProcSigMsgQLenOffsetMarker *marker;

        if (!have_qlock)
            erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);

        marker = (ErtsProcSigMsgQLenOffsetMarker *) c_p->sig_inq.first;
        ASSERT(marker);
        ASSERT(marker->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK);

        res += marker->delayed_len;

        if (!have_qlock)
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
    }

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
    {
        Sint len = 0;
        ERTS_FOREACH_SIG_PRIVQS(
            c_p, mp,
            {
                if (ERTS_SIG_IS_MSG(mp))
                    len++;
            });
        ERTS_ASSERT(res == len);
    }
#endif

    return res;
}

Sint
erts_proc_sig_privqs_len(Process *c_p)
{
    return proc_sig_privqs_len(c_p, 0);
}

ErtsDistExternal *
erts_proc_sig_get_external(ErtsMessage *msgp)
{
    if (ERTS_SIG_IS_EXTERNAL_MSG(msgp)) {
        return erts_get_dist_ext(msgp->data.heap_frag);
    } else if (ERTS_SIG_IS_NON_MSG(msgp) &&
               ERTS_SIG_IS_GEN_EXIT(msgp) &&
               ERTS_SIG_IS_GEN_EXIT_EXTERNAL(msgp)) {
        ErtsDistExternal *edep;
        ErtsExitSignalData *xsigd = get_exit_signal_data(msgp);
        ASSERT(ERTS_PROC_SIG_TYPE(((ErtsSignal *) msgp)->common.tag) == ERTS_SIG_Q_TYPE_GEN_EXIT);
        ASSERT(is_non_value(xsigd->reason));
        if (msgp->hfrag.next == NULL)
            edep = (ErtsDistExternal*)(xsigd + 1);
        else
            edep = erts_get_dist_ext(msgp->hfrag.next);
        return edep;
    }
    return NULL;
}

static void do_seq_trace_output(Eterm to, Eterm token, Eterm msg);

static void
send_gen_exit_signal(Process *c_p, Eterm from_tag,
                     Eterm from, Eterm to,
                     Sint16 op, Eterm reason, ErtsDistExternal *dist_ext,
                     ErlHeapFragment *dist_ext_hfrag,
                     Eterm ref, Eterm token, int normal_kills)
{
    ErtsExitSignalData *xsigd;
    Eterm *hp, *start_hp, s_reason, s_ref, s_message, s_token, s_from;
    ErtsMessage *mp;
    ErlHeapFragment *hfrag;
    ErlOffHeap *ohp;
    Uint hsz, from_sz, reason_sz, ref_sz, token_sz, dist_ext_sz = 0;
    int seq_trace;
#ifdef USE_VM_PROBES
    Eterm s_utag, utag;
    Uint utag_sz;
#endif

    ASSERT((is_value(reason) && dist_ext == NULL) ||
           (is_non_value(reason) && dist_ext != NULL));

    ASSERT(is_immed(from_tag));

    hsz = sizeof(ErtsExitSignalData)/sizeof(Eterm);

    seq_trace = c_p && have_seqtrace(token);
    if (seq_trace)
        seq_trace_update_send(c_p);

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

    token_sz = size_object(token);
    hsz += token_sz;

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
            /* {'DOWN', Ref, process, From, Reason} */
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

    s_token = copy_struct(token, token_sz, &hp, ohp);
    s_from = copy_struct(from, from_sz, &hp, ohp);
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
            /* {'DOWN', Ref, process, From, Reason} */
            s_message = TUPLE5(hp, am_DOWN, s_ref, am_process, s_from, s_reason);
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
                                                  0);
    ERL_MESSAGE_TOKEN(mp) = s_token;
    ERL_MESSAGE_FROM(mp) = from_tag; /* immediate... */

    hfrag->used_size = hp - start_hp;

    xsigd = (ErtsExitSignalData *) hp;

    xsigd->message = s_message;
    xsigd->from = s_from;
    xsigd->reason = s_reason;
    hfrag->next = dist_ext_hfrag;

    if (is_nil(s_ref))
        xsigd->u.normal_kills = normal_kills;
    else {
        ASSERT(is_ref(s_ref));
        xsigd->u.ref = s_ref;
    }

    hp += sizeof(ErtsExitSignalData)/sizeof(Eterm);

    if (dist_ext != NULL && dist_ext_hfrag == NULL && is_non_value(reason)) {
        erts_make_dist_ext_copy(dist_ext, (ErtsDistExternal *) hp);
        hp += dist_ext_sz;
    }

    ASSERT(hp == mp->hfrag.mem + mp->hfrag.alloc_size);

    if (seq_trace)
        do_seq_trace_output(to, s_token, s_message);

    if (!proc_queue_signal(c_p, to, (ErtsSignal *) mp, op)) {
        mp->next = NULL;
        erts_cleanup_messages(mp);
    }
}

static void
do_seq_trace_output(Eterm to, Eterm token, Eterm msg)
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
        rp = erts_proc_lookup_raw(to);
    else
        rp = erts_proc_lookup_raw_inc_refc(to);

    if (rp) {
        erts_proc_lock(rp, ERTS_PROC_LOCK_MSGQ);

        if (!ERTS_PROC_IS_EXITING(rp))
            seq_trace_output(token, msg, SEQ_TRACE_SEND, to, rp);

        erts_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);

        if (!is_normal_sched)
            erts_proc_dec_refc(rp);
    }
}

void
erts_proc_sig_send_persistent_monitor_msg(Uint16 type, Eterm key,
                                          Eterm from, Eterm to,
                                          Eterm msg, Uint msg_sz)
{
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

    if (!proc_queue_signal(NULL, to, (ErtsSignal *) mp,
                           ERTS_SIG_Q_OP_PERSISTENT_MON_MSG)) {
        mp->next = NULL;
        erts_cleanup_messages(mp);
    }
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
erts_proc_sig_send_exit(Process *c_p, Eterm from, Eterm to,
                        Eterm reason, Eterm token,
                        int normal_kills)
{
    Eterm from_tag;
    ASSERT(!c_p || c_p->common.id == from);
    if (is_immed(from)) {
        ASSERT(is_internal_pid(from) || is_internal_port(from));
        from_tag = from;
    }
    else {
        DistEntry *dep;
        ASSERT(is_external_pid(from));
        dep = external_pid_dist_entry(from);
        from_tag = dep->sysname;
    }
    send_gen_exit_signal(c_p, from_tag, from, to, ERTS_SIG_Q_OP_EXIT,
                         reason, NULL, NULL, NIL, token, normal_kills);
}

void
erts_proc_sig_send_dist_exit(DistEntry *dep,
                             Eterm from, Eterm to,
                             ErtsDistExternal *dist_ext,
                             ErlHeapFragment *hfrag,
                             Eterm reason, Eterm token)
{
    send_gen_exit_signal(NULL, dep->sysname, from, to, ERTS_SIG_Q_OP_EXIT,
                         reason, dist_ext, hfrag, NIL, token, 0);

}

void
erts_proc_sig_send_link_exit(Process *c_p, Eterm from, ErtsLink *lnk,
                             Eterm reason, Eterm token)
{
    Eterm to;
    ASSERT(!c_p || c_p->common.id == from);
    ASSERT(lnk);
    to = lnk->other.item;
    if (is_not_immed(reason) || is_not_nil(token)) {
        ASSERT(is_internal_pid(from) || is_internal_port(from));
        send_gen_exit_signal(c_p, from, from, to, ERTS_SIG_Q_OP_EXIT_LINKED,
                             reason, NULL, NULL, NIL, token, 0);
    }
    else {
        /* Pass signal using old link structure... */
        ErtsSignal *sig = (ErtsSignal *) lnk;
        lnk->other.item = reason; /* pass reason via this other.item */
        sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_EXIT_LINKED,
                                                 lnk->type, 0);
        if (proc_queue_signal(c_p, to, sig, ERTS_SIG_Q_OP_EXIT_LINKED))
            return; /* receiver will destroy lnk structure */
    }
    if (lnk)
        erts_link_release(lnk);
}

int
erts_proc_sig_send_link(Process *c_p, Eterm to, ErtsLink *lnk)
{
    ErtsSignal *sig;
    Uint16 type = lnk->type;

    ASSERT(!c_p || c_p->common.id == lnk->other.item);
    ASSERT(lnk);
    ASSERT(is_internal_pid(to));

    sig = (ErtsSignal *) lnk;
    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_LINK,
                                             type, 0);

    return proc_queue_signal(c_p, to, sig, ERTS_SIG_Q_OP_LINK);
}

void
erts_proc_sig_send_unlink(Process *c_p, ErtsLink *lnk)
{
    ErtsSignal *sig;
    Eterm to;

    ASSERT(lnk);

    sig = (ErtsSignal *) lnk;
    to = lnk->other.item;

    ASSERT(is_internal_pid(to));

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_UNLINK,
                                             lnk->type, 0);

    if (!proc_queue_signal(c_p, to, sig, ERTS_SIG_Q_OP_UNLINK))
        erts_link_release(lnk);
}

void
erts_proc_sig_send_dist_link_exit(DistEntry *dep,
                                  Eterm from, Eterm to,
                                  ErtsDistExternal *dist_ext,
                                  ErlHeapFragment *hfrag,
                                  Eterm reason, Eterm token)
{
    send_gen_exit_signal(NULL, dep->sysname, from, to, ERTS_SIG_Q_OP_EXIT_LINKED,
                         reason, dist_ext, hfrag, NIL, token, 0);

}

void
erts_proc_sig_send_dist_unlink(DistEntry *dep, Eterm from, Eterm to)
{
    ErtsSignal *sig;

    ASSERT(is_internal_pid(to));
    ASSERT(is_external_pid(from));
    ASSERT(dep == external_pid_dist_entry(from));

    sig = (ErtsSignal *) make_sig_dist_link_op(ERTS_SIG_Q_OP_UNLINK,
                                               to, from);

    if (!proc_queue_signal(NULL, to, sig, ERTS_SIG_Q_OP_UNLINK))
        destroy_sig_dist_link_op((ErtsSigDistLinkOp *) sig);
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
    send_gen_exit_signal(NULL, dep->sysname, monitored,
                         to, ERTS_SIG_Q_OP_MONITOR_DOWN,
                         reason, dist_ext, hfrag, ref, NIL, 0);
}

void
erts_proc_sig_send_monitor_down(ErtsMonitor *mon, Eterm reason)
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
                                                 mon->type, 0);
        if (proc_queue_signal(NULL, to, sig, ERTS_SIG_Q_OP_MONITOR_DOWN))
            return; /* receiver will destroy mon structure */
    }
    else {
        ErtsMonitorData *mdp = erts_monitor_to_data(mon);
        Eterm from_tag, monitored, heap[3];

        if (mon->type == ERTS_MON_TYPE_SUSPEND) {
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
            }
            else {
                node = erts_this_dist_entry->sysname;
                from_tag = mdp->origin.other.item;
            }
            ASSERT(is_internal_port(from_tag)
                   || is_internal_pid(from_tag)
                   || is_atom(from_tag));
            monitored = TUPLE2(&heap[0], name, node);
        }
        send_gen_exit_signal(NULL, from_tag, monitored,
                             to, ERTS_SIG_Q_OP_MONITOR_DOWN,
                             reason, NULL, NULL, mdp->ref, NIL, 0);
    }
    erts_monitor_release(mon);
}

void
erts_proc_sig_send_dist_demonitor(Eterm to, Eterm ref)
{
    ErtsSigDistProcDemonitor *dmon;
    ErtsSignal *sig;
    Eterm *hp;
    ErlOffHeap oh;
    size_t size;

    ERTS_INIT_OFF_HEAP(&oh);

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

    if (!proc_queue_signal(NULL, to, sig, ERTS_SIG_Q_OP_DEMONITOR))
        destroy_dist_proc_demonitor(dmon);
}

void
erts_proc_sig_send_demonitor(ErtsMonitor *mon)
{
    ErtsSignal *sig = (ErtsSignal *) mon;
    Uint16 type = mon->type;
    Eterm to = mon->other.item;

    ASSERT(is_internal_pid(to));
    ASSERT(erts_monitor_is_origin(mon));
    ASSERT(!erts_monitor_is_in_table(mon));

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_DEMONITOR,
                                             type, 0);
    
    if (!proc_queue_signal(NULL, to, sig, ERTS_SIG_Q_OP_DEMONITOR))
        erts_monitor_release(mon);
}

int
erts_proc_sig_send_monitor(ErtsMonitor *mon, Eterm to)
{
    ErtsSignal *sig = (ErtsSignal *) mon;
    Uint16 type = mon->type;

    ASSERT(is_internal_pid(to) || to == am_undefined);
    ASSERT(erts_monitor_is_target(mon));

    sig->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_MONITOR,
                                             type, 0);
    
    return proc_queue_signal(NULL, to, sig, ERTS_SIG_Q_OP_MONITOR);
}

void
erts_proc_sig_send_trace_change(Eterm to, Uint on, Uint off, Eterm tracer)
{
    ErtsSigTraceInfo *ti;
    Eterm tag;

    ti = erts_alloc(ERTS_ALC_T_SIG_DATA, sizeof(ErtsSigTraceInfo));
    tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_TRACE_CHANGE_STATE,
                                 ERTS_SIG_Q_TYPE_ADJUST_TRACE_INFO,
                                 0);

    ti->common.tag = tag;
    ti->flags_off = off;
    ti->flags_on = on;
    ti->tracer = NIL;
    if (is_not_nil(tracer))
        erts_tracer_update(&ti->tracer, tracer);

    if (!proc_queue_signal(NULL, to, (ErtsSignal *) ti,
                           ERTS_SIG_Q_OP_TRACE_CHANGE_STATE))
        destroy_trace_info(ti);
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

    res = proc_queue_signal(c_p, to, (ErtsSignal *) sgl,
                            ERTS_SIG_Q_OP_GROUP_LEADER);

    if (!res)
        destroy_sig_group_leader(sgl);
    else if (c_p) {
        erts_aint_t flags, rm_flags = ERTS_SIG_GL_FLG_SENDER;
        int prio_res = maybe_elevate_sig_handling_prio(c_p, to);
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

void
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

    if (proc_queue_signal(c_p, to, (ErtsSignal *) mp, ERTS_SIG_Q_OP_IS_ALIVE))
        (void) maybe_elevate_sig_handling_prio(c_p, to);
    else {
        /* It wasn't alive; reply to ourselves... */
        mp->next = NULL;
        mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
        erts_queue_message(c_p, ERTS_PROC_LOCK_MAIN, mp, msg, am_system);
    }
}

int
erts_proc_sig_send_process_info_request(Process *c_p,
                                        Eterm to,
                                        int *item_ix,
                                        int len,
                                        int need_msgq_len,
                                        int flags,
                                        Uint reserve_size,
                                        Eterm ref)
{
    Uint size = sizeof(ErtsProcessInfoSig) + (len - 1) * sizeof(int);
    ErtsProcessInfoSig *pis = erts_alloc(ERTS_ALC_T_SIG_DATA, size);
    int res;

    ASSERT(c_p);
    ASSERT(item_ix);
    ASSERT(len > 0);
    ASSERT(is_internal_ordinary_ref(ref));

    pis->common.tag = ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_PROCESS_INFO,
                                             0, 0);

    if (!need_msgq_len)
        pis->msgq_len_offset = ERTS_PROC_SIG_PI_MSGQ_LEN_IGNORE;
    else {
        pis->msgq_len_offset = ERTS_PROC_SIG_PI_MSGQ_LEN_SYNC;
        pis->marker.common.next = NULL;
        pis->marker.common.specific.next = NULL;
        pis->marker.common.tag = ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK;
        pis->marker.refc = 0;
        pis->marker.delayed_len = 0;
        pis->marker.len_offset = 0;
    }
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
    res = proc_queue_signal(c_p, to, (ErtsSignal *) pis,
                            ERTS_SIG_Q_OP_PROCESS_INFO);
    if (res)
        (void) maybe_elevate_sig_handling_prio(c_p, to);
    else
        erts_free(ERTS_ALC_T_SIG_DATA, pis);
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

    if (proc_queue_signal(c_p, to, (ErtsSignal *) mp, ERTS_SIG_Q_OP_SYNC_SUSPEND))
        (void) maybe_elevate_sig_handling_prio(c_p, to);
    else {
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

Eterm
erts_proc_sig_send_rpc_request(Process *c_p,
                               Eterm to,
                               int reply,
                               Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                               void *arg)
{
    Eterm res;
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

        ERTS_RECV_MARK_SAVE(c_p);
        ERTS_RECV_MARK_SET(c_p);
    }

    if (proc_queue_signal(c_p, to, (ErtsSignal *) sig, ERTS_SIG_Q_OP_RPC))
        (void) maybe_elevate_sig_handling_prio(c_p, to);
    else {
        erts_free(ERTS_ALC_T_SIG_DATA, sig);
        res = THE_NON_VALUE;
        if (reply)
            JOIN_MESSAGE(c_p);
    }

    return res;
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
    if (!IS_TRACED(c_p) || (ERTS_TRACE_FLAGS(c_p) & F_SENSITIVE)) {
        tracing->messages.active = 0;
        tracing->messages.receive_trace = 0;
        tracing->messages.event = NULL;
        tracing->messages.next = NULL;
        tracing->procs = 0;
        tracing->active = 0;
    }
    else {
        Uint flgs = ERTS_TRACE_FLAGS(c_p);
        int procs_trace = !!(flgs & F_TRACE_PROCS);
        int recv_trace = !!(flgs & F_TRACE_RECEIVE);
        /* procs tracing enabled? */

        tracing->procs = procs_trace;

        /* message receive tracing enabled? */
        tracing->messages.receive_trace = recv_trace;
        if (!recv_trace)
            tracing->messages.event = NULL;
        else {
            if (tracing->messages.bp_ix < 0)
                tracing->messages.bp_ix = erts_active_bp_ix();
            tracing->messages.event = &erts_receive_tracing[tracing->messages.bp_ix];
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
remove_iq_sig(Process *c_p, ErtsMessage *sig, ErtsMessage **next_sig)
{
    /*
     * Remove signal from inner queue.
     */
    ASSERT(c_p->sig_qs.cont_last != &sig->next);
    ASSERT(c_p->sig_qs.nmsigs.next != &sig->next);
    ASSERT(c_p->sig_qs.nmsigs.last != &sig->next);

    if (c_p->sig_qs.save == &sig->next)
        c_p->sig_qs.save = next_sig;
    if (c_p->sig_qs.last == &sig->next)
        c_p->sig_qs.last = next_sig;
    if (c_p->sig_qs.saved_last == &sig->next)
        c_p->sig_qs.saved_last = next_sig;

    *next_sig = sig->next;
}

static ERTS_INLINE void
remove_mq_sig(Process *c_p, ErtsMessage *sig,
           ErtsMessage **next_sig, ErtsMessage ***next_nm_sig)
{
    /*
     * Remove signal from middle queue.
     */
    ASSERT(c_p->sig_qs.save != &sig->next);
    ASSERT(c_p->sig_qs.last != &sig->next);

    if (c_p->sig_qs.cont_last == &sig->next)
	c_p->sig_qs.cont_last = next_sig;
    if (c_p->sig_qs.saved_last == &sig->next)
        c_p->sig_qs.saved_last = next_sig;
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
    remove_mq_sig(c_p, sig, next_sig, next_nm_sig);
}

static ERTS_INLINE void
convert_to_msg(Process *c_p, ErtsMessage *sig, ErtsMessage *msg,
               ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig = *next_nm_sig;
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    c_p->sig_qs.len++;
    *next_sig = msg;
    remove_mq_sig(c_p, sig, &msg->next, next_nm_sig);
}

static ERTS_INLINE void
convert_to_msgs(Process *c_p, ErtsMessage *sig, Uint no_msgs,
                ErtsMessage *first_msg, ErtsMessage *last_msg,
                ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig = *next_nm_sig;
    ASSERT(ERTS_SIG_IS_NON_MSG(sig));
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    c_p->sig_qs.len += no_msgs;
    *next_sig = first_msg;
    remove_mq_sig(c_p, sig, &last_msg->next, next_nm_sig);
}

static ERTS_INLINE void
insert_messages(Process *c_p, ErtsMessage **next, ErtsMessage *first,
                ErtsMessage *last, Uint no_msgs, ErtsMessage ***next_nm_sig)
{
    last->next = *next;
    if (c_p->sig_qs.cont_last == next)
	c_p->sig_qs.cont_last = &last->next;
    if (*next_nm_sig == next)
        *next_nm_sig = &last->next;
    if (c_p->sig_qs.nmsigs.last == next)
        c_p->sig_qs.nmsigs.last = &last->next;
    c_p->sig_qs.len += no_msgs;
    *next = first;
}

static ERTS_INLINE void
remove_mq_m_sig(Process *c_p, ErtsMessage *sig, ErtsMessage **next_sig, ErtsMessage ***next_nm_sig)
{
    /* Removing message... */
    ASSERT(!ERTS_SIG_IS_NON_MSG(sig));
    c_p->sig_qs.len--;
    remove_mq_sig(c_p, sig, next_sig, next_nm_sig);
}

static ERTS_INLINE void
remove_iq_m_sig(Process *c_p, ErtsMessage *sig, ErtsMessage **next_sig)
{
    /* Removing message... */
    ASSERT(!ERTS_SIG_IS_NON_MSG(sig));
    c_p->sig_qs.len--;
    remove_iq_sig(c_p, sig, next_sig);
}

static ERTS_INLINE void
convert_prepared_sig_to_msg(Process *c_p, ErtsMessage *sig, Eterm msg,
                            ErtsMessage ***next_nm_sig)
{
    /*
     * Everything is already there except for the reference to
     * the message and the combined hfrag marker that needs to be
     * restored...
     */
    *next_nm_sig = ((ErtsSignal *) sig)->common.specific.next;
    sig->data.attached = ERTS_MSG_COMBINED_HFRAG;
    ERL_MESSAGE_TERM(sig) = msg;
    c_p->sig_qs.len++;
}

static ERTS_INLINE int
handle_exit_signal(Process *c_p, ErtsSigRecvTracing *tracing,
                   ErtsMessage *sig, ErtsMessage ***next_nm_sig,
                   int *exited)
{
    ErtsMessage *conv_msg = NULL;
    ErtsExitSignalData *xsigd = NULL;
    ErtsLinkData *ldp = NULL; /* Avoid erroneous warning... */
    ErtsLink *dlnk = NULL; /* Avoid erroneous warning... */
    Eterm tag = ((ErtsSignal *) sig)->common.tag;
    Uint16 type = ERTS_PROC_SIG_TYPE(tag);
    int op = ERTS_PROC_SIG_OP(tag);
    int destroy = 0;
    int ignore = 0;
    int save = 0;
    int exit = 0;
    int cnt = 1;
    Eterm reason;
    Eterm from;

    if (type == ERTS_SIG_Q_TYPE_GEN_EXIT) {
        xsigd = get_exit_signal_data(sig);
        from = xsigd->from;
        if (op != ERTS_SIG_Q_OP_EXIT_LINKED)
            ignore = 0;
        else {
            ErtsLink *llnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p), from);
            if (!llnk) {
                /* Link no longer active; ignore... */
                ignore = !0;
                destroy = !0;
            }
            else {
                ignore = 0;
                erts_link_tree_delete(&ERTS_P_LINKS(c_p), llnk);
                if (llnk->type != ERTS_LNK_TYPE_DIST_PROC)
                    erts_link_release(llnk);
                else {
                    dlnk = erts_link_to_other(llnk, &ldp);
                    if (erts_link_dist_delete(dlnk))
                        erts_link_release_both(ldp);
                    else
                        erts_link_release(llnk);
                }
            }
        }

        /* This GEN_EXIT was received from another node, decode the exit reason */
        if (ERTS_SIG_IS_GEN_EXIT_EXTERNAL(sig))
            erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, sig, 1);

        reason = xsigd->reason;

        if (is_non_value(reason)) {
            /* Bad distribution message; remove it from queue... */
            ignore = !0;
            destroy = !0;
        }

        if (!ignore) {

            if ((op != ERTS_SIG_Q_OP_EXIT || reason != am_kill)
                && (c_p->flags & F_TRAP_EXIT)) {
                convert_prepared_sig_to_msg(c_p, sig,
                                            xsigd->message, next_nm_sig);
                conv_msg = sig;
            }
            else if (reason == am_normal && !xsigd->u.normal_kills) {
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
    }
    else { /* Link exit */
        ErtsLink *slnk = (ErtsLink *) sig;
        ErtsLink *llnk = erts_link_to_other(slnk, &ldp);

        ASSERT(type == ERTS_LNK_TYPE_PROC
               || type == ERTS_LNK_TYPE_PORT
               || type == ERTS_LNK_TYPE_DIST_PROC);

        from = llnk->other.item;
        reason = slnk->other.item; /* reason in other.item ... */
        ASSERT(is_pid(from) || is_internal_port(from));
        ASSERT(is_immed(reason));
        ASSERT(op == ERTS_SIG_Q_OP_EXIT_LINKED);
        dlnk = erts_link_tree_key_delete(&ERTS_P_LINKS(c_p), llnk);
        if (!dlnk) {
            ignore = !0; /* Link no longer active; ignore... */
            ldp = NULL;
        }
        else {
            Eterm pid;
            ErtsMessage *mp;
            ErtsProcLocks locks;
            Uint hsz;
            Eterm *hp;
            ErlOffHeap *ohp;
            ignore = 0;
            if (dlnk == llnk)
                dlnk = NULL;
            else
                ldp = NULL;

            ASSERT(is_immed(reason));

            if (!(c_p->flags & F_TRAP_EXIT)) {
                if (reason == am_normal)
                    ignore = !0; /* Ignore it... */
                else
                    exit = !0; /* Terminate... */
            }
            else {

                /*
                 * Create and EXIT message and replace
                 * the original signal with the message...
                 */

                locks = ERTS_PROC_LOCK_MAIN;

                hsz = 4 + NC_HEAP_SIZE(from);

                mp = erts_alloc_message_heap(c_p, &locks, hsz, &hp, &ohp);

                if (locks != ERTS_PROC_LOCK_MAIN)
                    erts_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);

                pid = STORE_NC(&hp, ohp, from);

                ERL_MESSAGE_TERM(mp) = TUPLE3(hp, am_EXIT, pid, reason);
                ERL_MESSAGE_TOKEN(mp) = am_undefined;
                if (is_immed(pid))
                    ERL_MESSAGE_FROM(mp) = pid;
                else {
                    DistEntry *dep;
                    ASSERT(is_external_pid(pid));
                    dep = external_pid_dist_entry(pid);
                    ERL_MESSAGE_FROM(mp) = dep->sysname;
                }

                /* Replace original signal with the exit message... */
                convert_to_msg(c_p, sig, mp, next_nm_sig);

                cnt += 4;

                conv_msg = mp;
            }
        }
        destroy = !0;
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
        if (op == ERTS_SIG_Q_OP_EXIT_LINKED && tracing->procs)
            getting_unlinked(c_p, from);
    }

    if (destroy) {
        cnt++;
        if (type == ERTS_SIG_Q_TYPE_GEN_EXIT) {
            sig->next = NULL;
            erts_cleanup_messages(sig);
        }
        else {
            if (ldp)
                erts_link_release_both(ldp);
            else {
                if (dlnk)
                    erts_link_release(dlnk);
                erts_link_release((ErtsLink *) sig);
            }
        }
    }

    *exited = exit;

    return cnt;
}

static ERTS_INLINE int
convert_prepared_down_message(Process *c_p, ErtsMessage *sig,
                              Eterm msg, ErtsMessage ***next_nm_sig)
{
    convert_prepared_sig_to_msg(c_p, sig, msg, next_nm_sig);
    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    return 1;
}

static int
convert_to_down_message(Process *c_p,
                        ErtsMessage *sig,
                        ErtsMonitorData *mdp,
                        Uint16 mon_type,
                        ErtsMessage ***next_nm_sig)
{
    /*
     * Create a 'DOWN' message and replace the signal
     * with it...
     */
    int cnt = 0;
    Eterm node = am_undefined;
    ErtsMessage *mp;
    ErtsProcLocks locks;
    Uint hsz;
    Eterm *hp, ref, from, type, reason;
    ErlOffHeap *ohp;

    ASSERT(mdp);
    ASSERT((mdp->origin.flags & ERTS_ML_FLGS_SAME)
           == (mdp->target.flags & ERTS_ML_FLGS_SAME));

    hsz = 6; /* 5-tuple */

    if (mdp->origin.flags & ERTS_ML_FLG_NAME)
        hsz += 3;  /* reg name 2-tuple */
    else {
        ASSERT(is_pid(mdp->origin.other.item)
               || is_internal_port(mdp->origin.other.item));
        hsz += NC_HEAP_SIZE(mdp->origin.other.item);
    }

    ASSERT(is_ref(mdp->ref));
    hsz += NC_HEAP_SIZE(mdp->ref);

    locks = ERTS_PROC_LOCK_MAIN;

    /* reason is mdp->target.other.item */
    reason = mdp->target.other.item;
    ASSERT(is_immed(reason));

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

    ASSERT(mdp->origin.type == mon_type);
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
            ASSERT(mdep->dist);
            node = mdep->dist->nodename;
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

    ERL_MESSAGE_TERM(mp) = TUPLE5(hp, am_DOWN, ref,
                                  type, from, reason);
    hp += 6;

    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    /* Replace original signal with the exit message... */
    convert_to_msg(c_p, sig, mp, next_nm_sig);

    cnt += 4;

    erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);

    return cnt;
}

static ERTS_INLINE int
convert_to_nodedown_messages(Process *c_p,
                             ErtsMessage *sig,
                             ErtsMonitorData *mdp,
                             ErtsMessage ***next_nm_sig)
{
    int cnt = 1;
    Uint n;
    ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;

    ASSERT((mdp->origin.flags & ERTS_ML_FLGS_SAME)
           == (mdp->target.flags & ERTS_ML_FLGS_SAME));
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
        convert_to_msgs(c_p, sig, n, nd_first, nd_last, next_nm_sig);

        erts_proc_notify_new_message(c_p, ERTS_PROC_LOCK_MAIN);
    }
    return cnt;
}

static int 
handle_nodedown(Process *c_p,
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

    return cnt + convert_to_nodedown_messages(c_p, sig, mdp, next_nm_sig);
}

static void
handle_persistent_mon_msg(Process *c_p, Uint16 type,
                          ErtsMonitor *mon, ErtsMessage *sig,
                          Eterm msg, ErtsMessage ***next_nm_sig)
{
    convert_prepared_sig_to_msg(c_p, sig, msg, next_nm_sig);

    switch (type) {

    case ERTS_MON_TYPE_TIME_OFFSET:
        ASSERT(mon->type == ERTS_MON_TYPE_TIME_OFFSET);
        break;

    case ERTS_MON_TYPE_NODES: {
        ErtsMonitorDataExtended *mdep;
        Uint n;
        ASSERT(mon->type == ERTS_MON_TYPE_NODES);
        mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
        ERTS_ML_ASSERT(mdep->u.refc > 0);
        n = mdep->u.refc;
        n--;
        if (n > 0) {
            ErtsProcLocks locks = ERTS_PROC_LOCK_MAIN;
            ErtsMessage *first = NULL, *prev, *last;
            Uint hsz = size_object(msg);
            Uint i;

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
            insert_messages(c_p, &sig->next, first, last, n, next_nm_sig);
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

static void
check_push_msgq_len_offs_marker(Process *rp, ErtsSignal *sig)
{
    ErtsProcessInfoSig *pisig = (ErtsProcessInfoSig *) sig;

    ASSERT(ERTS_PROC_SIG_OP(sig->common.tag) == ERTS_SIG_Q_OP_PROCESS_INFO);

    if (pisig->msgq_len_offset == ERTS_PROC_SIG_PI_MSGQ_LEN_SYNC) {
        ErtsProcSigMsgQLenOffsetMarker *mrkr;
        Sint len, msgq_len_offset;
        ErtsMessage *first = rp->sig_inq.first;
        ASSERT(first);
        if (((ErtsSignal *) first)->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK)
            mrkr = (ErtsProcSigMsgQLenOffsetMarker *) first;
        else {
            mrkr = &pisig->marker;

            ASSERT(mrkr->common.tag == ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK);

            mrkr->common.next = first;
            ASSERT(rp->sig_inq.last != &rp->sig_inq.first);
            if (rp->sig_inq.nmsigs.next == &rp->sig_inq.first)
                rp->sig_inq.nmsigs.next = &mrkr->common.next;
            if (rp->sig_inq.nmsigs.last == &rp->sig_inq.first)
                rp->sig_inq.nmsigs.last = &mrkr->common.next;
            rp->sig_inq.first = (ErtsMessage *) mrkr;
        }

        len = rp->sig_inq.len;
        msgq_len_offset = len - mrkr->len_offset;

        mrkr->len_offset = len;
        mrkr->refc++;

        pisig->msgq_len_offset = msgq_len_offset;

#ifdef DEBUG
        /* save pointer to used marker... */
        pisig->marker.common.specific.attachment = (void *) mrkr;
#endif

    }
}

static void
destroy_process_info_request(Process *c_p, ErtsProcessInfoSig *pisig)
{
    int dealloc_pisig = !0;

    if (pisig->msgq_len_offset != ERTS_PROC_SIG_PI_MSGQ_LEN_IGNORE) {
        Sint refc;
        int dealloc_marker = 0;
        ErtsProcSigMsgQLenOffsetMarker *marker;
#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
        Sint delayed_len;
#endif

        ASSERT(pisig->msgq_len_offset >= 0);

        erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
        marker = (ErtsProcSigMsgQLenOffsetMarker *) c_p->sig_inq.first;
        ASSERT(marker);
        ASSERT(marker->refc > 0);
        ASSERT(pisig->marker.common.specific.attachment == (void *) marker);

        marker->delayed_len -= pisig->msgq_len_offset;
#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
        delayed_len = marker->delayed_len;
#endif

        refc = --marker->refc;
        if (refc) {
            if (marker == &pisig->marker) {
                /* Another signal using our marker... */
                dealloc_pisig = 0;
            }
        }
        else {
            /* Marker unused; remove it... */
            ASSERT(marker->delayed_len + marker->len_offset == 0);
#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
            delayed_len += marker->len_offset;
#endif
            if (marker != &pisig->marker)
                dealloc_marker = !0; /* used another signals marker... */
            c_p->sig_inq.first = marker->common.next;
            if (c_p->sig_inq.last == &marker->common.next)
                c_p->sig_inq.last = &c_p->sig_inq.first;
            if (c_p->sig_inq.nmsigs.next == &marker->common.next)
                c_p->sig_inq.nmsigs.next = &c_p->sig_inq.first;
            if (c_p->sig_inq.nmsigs.last == &marker->common.next)
                c_p->sig_inq.nmsigs.last = &c_p->sig_inq.first;
        }
        erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

        if (!refc) {
            c_p->sig_qs.flags &= ~FS_DELAYED_PSIGQS_LEN;
            /* Adjust msg len of inner+middle queue */
            ASSERT(marker->len_offset <= 0);
            c_p->sig_qs.len -= marker->len_offset;

            ASSERT(c_p->sig_qs.len >= 0);
        }

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
        {
            Sint len = 0;
            ERTS_FOREACH_SIG_PRIVQS(
                c_p, mp,
                {
                    if (ERTS_SIG_IS_MSG(mp))
                        len++;
                });
            ERTS_ASSERT(c_p->sig_qs.len + delayed_len == len);
        }
#endif


        if (dealloc_marker) {
            ErtsProcessInfoSig *pisig2
                = (ErtsProcessInfoSig *) (((char *) marker)
                                          - offsetof(ErtsProcessInfoSig,
                                                     marker));
            erts_free(ERTS_ALC_T_SIG_DATA, pisig2);
        }
    }

    if (dealloc_pisig)
        erts_free(ERTS_ALC_T_SIG_DATA, pisig);
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

    if (pisig->msgq_len_offset != ERTS_PROC_SIG_PI_MSGQ_LEN_IGNORE) {
        /*
         * Request requires message queue data to be updated
         * before inspection...
         */

        ASSERT(pisig->msgq_len_offset >= 0);
        /*
         * Update sig_qs.len to reflect the length
         * of the message queue...
         */
        c_p->sig_qs.len += pisig->msgq_len_offset;

        if (is_alive) {
            /*
             * Move messages part of message queue into inner
             * signal queue...
             */
            ASSERT(tracing);

            if (*next_nm_sig != &c_p->sig_qs.cont) {
                if (*next_nm_sig == tracing->messages.next)
                    tracing->messages.next = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = *next_nm_sig;

                c_p->sig_qs.cont = **next_nm_sig;
                if (c_p->sig_qs.nmsigs.last == *next_nm_sig)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
                *next_nm_sig = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = NULL;
            }

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
            {
                Sint len;
                ErtsMessage *mp;
                for (mp = c_p->sig_qs.first, len = 0; mp; mp = mp->next) {
                    ERTS_ASSERT(ERTS_SIG_IS_MSG(mp));
                    len++;
                }
                ERTS_ASSERT(c_p->sig_qs.len == len);
            }
#endif
        }
    }
    if (is_alive) {
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
                                    pisig->item_ix, pisig->len,
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

    destroy_process_info_request(c_p, pisig);

    if (reds > INT_MAX/8)
        reds = INT_MAX/8;

    return ((int) reds)*4 + 8;
}

static void
handle_suspend(Process *c_p, ErtsMonitor *mon, int *yieldp)
{
    erts_aint32_t state = erts_atomic32_read_nob(&c_p->state);

    ASSERT(mon->type == ERTS_MON_TYPE_SUSPEND);

    if (!(state & ERTS_PSFLG_DIRTY_RUNNING)) {
        ErtsMonitorSuspend *msp;
        erts_aint_t mstate;

        msp = (ErtsMonitorSuspend *) erts_monitor_to_data(mon);
        mstate = erts_atomic_read_bor_acqb(&msp->state,
                                           ERTS_MSUSPEND_STATE_FLG_ACTIVE);
        ASSERT(!(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE)); (void) mstate;
        erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
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
            && erts_monitor_is_in_table(&msp->md.target)) {
            erts_aint_t mstate;

            mstate = erts_atomic_read_bor_acqb(&msp->state,
                                               ERTS_MSUSPEND_STATE_FLG_ACTIVE);
            ASSERT(!(mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE)); (void) mstate;
            erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
        }

        erts_monitor_release(&msp->md.target);

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

/*
 * Called in order to handle incoming signals.
 */

int
erts_proc_sig_handle_incoming(Process *c_p, erts_aint32_t *statep,
                              int *redsp, int max_reds, int local_only)
{
    Eterm tag;
    erts_aint32_t state;
    int yield, cnt, limit, abs_lim, msg_tracing;
    ErtsMessage *sig, ***next_nm_sig;
    ErtsSigRecvTracing tracing;

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    state = erts_atomic32_read_nob(&c_p->state);
    if (!local_only) {
        if (ERTS_PSFLG_SIG_IN_Q & state) {
            erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
            erts_proc_sig_fetch(c_p);
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);
        }
    }

    limit = *redsp;
    *redsp = 0;
    yield = 0;

    if (!c_p->sig_qs.cont) {
        *statep = state;
        return !0;
    }

    if (state & ERTS_PSFLG_EXITING) {
        *statep = state;
        return 0;
    }

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
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            if (handle_msg_tracing(c_p, &tracing, next_nm_sig) != 0) {
                ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
                break; /* tracing limit or end... */
            }
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
        }

        if (!*next_nm_sig)
            break;

        sig = **next_nm_sig;

        ASSERT(sig);
        ASSERT(ERTS_SIG_IS_NON_MSG(sig));

        tag = ((ErtsSignal *) sig)->common.tag;

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

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            switch (type) {
            case ERTS_MON_TYPE_DIST_PROC:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_PORT:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    omon = &mdp->origin;
                    erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p),
                                             omon);
                    cnt += convert_to_down_message(c_p, sig, mdp,
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
                    if (omon->type == ERTS_MON_TYPE_DIST_PROC) {
                        mdp = erts_monitor_to_data(omon);
                        if (erts_monitor_dist_delete(&mdp->target))
                            tmon = &mdp->target;
                    }
                    erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p),
                                             omon);
                    cnt += convert_prepared_down_message(c_p, sig,
                                                         xsigd->message,
                                                         next_nm_sig);
                }
                break;
            case ERTS_MON_TYPE_NODE:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    omon = &mdp->origin;
                    cnt += handle_nodedown(c_p, sig, mdp, next_nm_sig);
                }
                break;
            case ERTS_MON_TYPE_SUSPEND:
                tmon = (ErtsMonitor *) sig;
                ASSERT(erts_monitor_is_target(tmon));
                ASSERT(!erts_monitor_is_in_table(tmon));
                mdp = erts_monitor_to_data(tmon);
                if (erts_monitor_is_in_table(&mdp->origin)) {
                    erts_monitor_tree_delete(&ERTS_P_MONITORS(c_p),
                                             &mdp->origin);
                    omon = &mdp->origin;
                    remove_nm_sig(c_p, sig, next_nm_sig);
                }
                break;
            default:
                ERTS_INTERNAL_ERROR("invalid monitor type");
                break;
            }

            if (omon) {
                if (tmon)
                    erts_monitor_release_both(mdp);
                else
                    erts_monitor_release(omon);
            }
            else {
                remove_nm_sig(c_p, sig, next_nm_sig);
                if (xsigd) {
                    sig->next = NULL;
                    erts_cleanup_messages(sig);
                }
                if (tmon)
                    erts_monitor_release(tmon);
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
                handle_persistent_mon_msg(c_p, type, mon, sig,
                                          msg, next_nm_sig);
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

            if (mon->type == ERTS_MON_TYPE_DIST_PROC)
                erts_monitor_tree_insert(&ERTS_P_MONITORS(c_p), mon);
            else {
                erts_monitor_list_insert(&ERTS_P_LT_MONITORS(c_p), mon);
                if (mon->type == ERTS_MON_TYPE_SUSPEND)
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
                ASSERT(omon->type == type);
                ASSERT(erts_monitor_is_origin(omon));
                ASSERT(!erts_monitor_is_in_table(omon));
                if (!erts_monitor_is_in_table(&mdp->target))
                    erts_monitor_release(omon);
                else {
                    ErtsMonitor *tmon = &mdp->target;
                    ASSERT(tmon->type == type);
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
                            mstate = erts_atomic_read_acqb(&msp->state);
                            if (mstate & ERTS_MSUSPEND_STATE_FLG_ACTIVE)
                                erts_resume(c_p, ERTS_PROC_LOCK_MAIN);
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
            ErtsLink *rlnk, *lnk = (ErtsLink *) sig;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);
            rlnk = erts_link_tree_insert_addr_replace(&ERTS_P_LINKS(c_p),
                                                      lnk);
            if (!rlnk) {
                if (tracing.procs)
                    getting_linked(c_p, lnk->other.item);
            }
            else {
                if (rlnk->type != ERTS_LNK_TYPE_DIST_PROC)
                    erts_link_release(rlnk);
                else {
                    ErtsLinkData *ldp;
                    ErtsLink *dlnk = erts_link_to_other(rlnk, &ldp);
                    if (erts_link_dist_delete(dlnk))
                        erts_link_release_both(ldp);
                    else
                        erts_link_release(rlnk);
                }
            }

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            break;
        }

        case ERTS_SIG_Q_OP_UNLINK: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);
            ErtsLinkData *ldp;
            ErtsLink *llnk;

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);

            remove_nm_sig(c_p, sig, next_nm_sig);
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
                ErtsSigDistLinkOp *sdlnk = (ErtsSigDistLinkOp *) sig;
                ASSERT(type == ERTS_SIG_Q_TYPE_DIST_LINK);
                ASSERT(is_external_pid(sdlnk->remote));
                llnk = erts_link_tree_lookup(ERTS_P_LINKS(c_p), sdlnk->remote);
                if (llnk) {
                    ErtsLink *dlnk = erts_link_to_other(llnk, &ldp);
                    erts_link_tree_delete(&ERTS_P_LINKS(c_p), llnk);
                    if (erts_link_dist_delete(dlnk))
                        erts_link_release_both(ldp);
                    else
                        erts_link_release(llnk);
                    cnt += 8;
                    if (tracing.procs)
                        getting_unlinked(c_p, sdlnk->remote);
                }
                destroy_sig_dist_link_op(sdlnk);
                cnt++;
            }
            else {
                ErtsLinkData *ldp;
                ErtsLink *dlnk, *slnk;
                slnk = (ErtsLink *) sig;
                llnk = erts_link_to_other(slnk, &ldp);
                dlnk = erts_link_tree_key_delete(&ERTS_P_LINKS(c_p), llnk);
                if (!dlnk)
                    erts_link_release(slnk);
                else {
                    if (tracing.procs)
                        getting_unlinked(c_p, llnk->other.item);
                    if (dlnk == llnk)
                        erts_link_release_both(ldp);
                    else {
                        erts_link_release(slnk);
                        erts_link_release(dlnk);
                    }
                }
                cnt += 2;
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

        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE: {
            Uint16 type = ERTS_PROC_SIG_TYPE(tag);

            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            msg_tracing = handle_trace_change_state(c_p, &tracing,
                                                    type, sig,
                                                    next_nm_sig);
            ERTS_PROC_SIG_HDBG_PRIV_CHKQ(c_p, &tracing, next_nm_sig);
            
            break;
        }

        default:
            ERTS_INTERNAL_ERROR("Unknown signal");
            break;
        }

        cnt++;

    } while (cnt <= limit || stretch_limit(c_p, &tracing, abs_lim, &limit));

stop: {
        int deferred_save, deferred_saved_last, res;

        deferred_saved_last = !!(c_p->sig_qs.flags & FS_DEFERRED_SAVED_LAST);
        deferred_save = !!(c_p->sig_qs.flags & FS_DEFERRED_SAVE);

        if (!deferred_saved_last)
            deferred_save = 0;
        else {
            if (c_p->sig_qs.saved_last == &c_p->sig_qs.cont) {
                c_p->sig_qs.saved_last = c_p->sig_qs.last;
                c_p->sig_qs.flags &= ~FS_DEFERRED_SAVED_LAST;
                deferred_saved_last = deferred_save = 0;
            }
        }

        if (deferred_save)
            c_p->sig_qs.flags &= ~FS_DEFERRED_SAVE;

        ASSERT(c_p->sig_qs.saved_last != &c_p->sig_qs.cont);

        if (ERTS_UNLIKELY(msg_tracing != 0)) {
            /*
             * All messages that has been traced should
             * be moved to inner queue. Next signal in
             * middle queue should either be next message
             * to trace or next non-message signal.
             */
            ASSERT(tracing.messages.next);
            if (*next_nm_sig) {
                if (*next_nm_sig == tracing.messages.next)
                    *next_nm_sig = &c_p->sig_qs.cont;
                if (c_p->sig_qs.nmsigs.last == tracing.messages.next)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
                *statep = erts_atomic32_read_nob(&c_p->state);
            }
            else {
                ASSERT(!c_p->sig_qs.nmsigs.next);
                c_p->sig_qs.nmsigs.last = NULL;
                state = erts_atomic32_read_band_nob(&c_p->state,
                                                    ~ERTS_PSFLG_SIG_Q);
                state &= ~ERTS_PSFLG_SIG_Q;
                *statep = state;
            }

            if (tracing.messages.next != &c_p->sig_qs.cont) {
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
            /*
             * All messages prior to next non-message
             * signal should be moved to inner queue.
             * Next non-message signal to handle should
             * be first in middle queue.
             */
            ASSERT(**next_nm_sig);
            if (*next_nm_sig != &c_p->sig_qs.cont) {
                *c_p->sig_qs.last = c_p->sig_qs.cont;
                c_p->sig_qs.last = *next_nm_sig;

                c_p->sig_qs.cont = **next_nm_sig;
                if (c_p->sig_qs.nmsigs.last == *next_nm_sig)
                    c_p->sig_qs.nmsigs.last = &c_p->sig_qs.cont;
                *next_nm_sig = &c_p->sig_qs.cont;
                *c_p->sig_qs.last = NULL;
            }

            ASSERT(c_p->sig_qs.cont);

            *statep = erts_atomic32_read_nob(&c_p->state);

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

            if (c_p->sig_qs.cont_last != &c_p->sig_qs.cont) {
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
            *statep = state;
            res = !0;
        }

        if (deferred_saved_last
            && (c_p->sig_qs.saved_last == &c_p->sig_qs.cont)) {
            c_p->sig_qs.saved_last = c_p->sig_qs.last;
            c_p->sig_qs.flags &= ~FS_DEFERRED_SAVED_LAST;
            if (deferred_save)
                c_p->sig_qs.save = c_p->sig_qs.saved_last;
        }
        else if (!res) {
            if (deferred_save) {
                c_p->sig_qs.save = c_p->sig_qs.last;
                ASSERT(!PEEK_MESSAGE(c_p));
            }
        }
        else {
            c_p->sig_qs.flags &= ~FS_DEFERRED_SAVED_LAST;
            if (deferred_save)
                c_p->sig_qs.save = c_p->sig_qs.saved_last;
        }

        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);

        *redsp = cnt/4 + 1;

        if (yield) {
            int vreds = max_reds - *redsp;
            if (vreds > 0) {
                ErtsSchedulerData *esdp = erts_get_scheduler_data();
                esdp->virtual_reds += vreds;
            }
            *redsp = max_reds;
        }

        return res;
    }
}

static int
stretch_limit(Process *c_p, ErtsSigRecvTracing *tp,
              int abs_lim, int *limp)
{
    int lim;
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

    if (!(c_p->sig_qs.flags & FS_DEFERRED_SAVED_LAST)) {
        ErtsSignal *sig;

        if (PEEK_MESSAGE(c_p))
            return 0;
        sig = (ErtsSignal *) c_p->sig_qs.cont;
        if (!sig)
            return 0; /* No signals to process available... */
        if (ERTS_SIG_IS_MSG(sig) && tp->messages.next != &c_p->sig_qs.cont)
            return 0;
    }

    lim += ERTS_SIG_REDS_CNT_FACTOR*100;
    if (lim > abs_lim)
        lim = abs_lim;
    *limp = lim;
    return !0;
}


int
erts_proc_sig_handle_exit(Process *c_p, Sint *redsp)
{
    int cnt;
    Sint limit;
    ErtsMessage *sig, ***next_nm_sig;

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    ASSERT(!(ERTS_PSFLG_SIG_IN_Q & erts_atomic32_read_nob(&c_p->state)));

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
                erts_link_release((ErtsLink *) sig);
                break;
            case ERTS_MON_TYPE_PORT:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_DIST_PROC:
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

        case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
            sig->next = NULL;
            erts_cleanup_messages(sig);
            break;

        case ERTS_SIG_Q_OP_MONITOR: {
            ErtsProcExitContext pectxt = {c_p, am_noproc, NULL, NULL, NIL};
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
            if (type == ERTS_SIG_Q_TYPE_DIST_LINK)
                destroy_sig_dist_link_op((ErtsSigDistLinkOp *) sig);
            else
                erts_link_release((ErtsLink *) sig);
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

        case ERTS_SIG_Q_OP_RPC: {
            int yield = 0;
            handle_rpc(c_p, (ErtsProcSigRPC *) sig,
                       cnt, limit, &yield);
            break;
        }

        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
            destroy_trace_info((ErtsSigTraceInfo *) sig);
            break;

        default:
            ERTS_INTERNAL_ERROR("Unknown signal");
            break;
        }

    } while (cnt >= limit && *next_nm_sig);

    *redsp += cnt / ERTS_SIG_REDS_CNT_FACTOR;

    if (*next_nm_sig)
        return 0;

    ASSERT(!c_p->sig_qs.nmsigs.next);
    c_p->sig_qs.nmsigs.last = NULL;
    (void) erts_atomic32_read_band_nob(&c_p->state,
                                       ~ERTS_PSFLG_SIG_Q);
    return !0;
}

#ifdef USE_VM_PROBES
#  define ERTS_CLEAR_SEQ_TOKEN(MP)                                      \
    ERL_MESSAGE_TOKEN((MP)) = ((ERL_MESSAGE_DT_UTAG((MP)) != NIL)       \
                               ? am_have_dt_utag : NIL)
#else
#  define ERTS_CLEAR_SEQ_TOKEN(MP)                                      \
    ERL_MESSAGE_TOKEN((MP)) = NIL
#endif

static ERTS_INLINE void
clear_seq_trace_token(ErtsMessage *sig)
{
    if (ERTS_SIG_IS_MSG((ErtsSignal *) sig))
        ERTS_CLEAR_SEQ_TOKEN(sig);
    else {
        Uint tag;
        Uint16 op, type;

        tag = ((ErtsSignal *) sig)->common.tag;
        type = ERTS_PROC_SIG_TYPE(tag);
        op = ERTS_PROC_SIG_OP(tag);

        switch (op) {

        case ERTS_SIG_Q_OP_EXIT:
        case ERTS_SIG_Q_OP_EXIT_LINKED:
        case ERTS_SIG_Q_OP_MONITOR_DOWN:
            switch (type) {
            case ERTS_SIG_Q_TYPE_GEN_EXIT:
                ERTS_CLEAR_SEQ_TOKEN(sig);
                break;
            case ERTS_LNK_TYPE_PORT:
            case ERTS_LNK_TYPE_PROC:
            case ERTS_LNK_TYPE_DIST_PROC:
            case ERTS_MON_TYPE_PORT:
            case ERTS_MON_TYPE_PROC:
            case ERTS_MON_TYPE_DIST_PROC:
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

        case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
            ERTS_CLEAR_SEQ_TOKEN(sig);
            break;

        case ERTS_SIG_Q_OP_MONITOR:
        case ERTS_SIG_Q_OP_DEMONITOR:
        case ERTS_SIG_Q_OP_LINK:
        case ERTS_SIG_Q_OP_UNLINK:
        case ERTS_SIG_Q_OP_TRACE_CHANGE_STATE:
        case ERTS_SIG_Q_OP_GROUP_LEADER:
        case ERTS_SIG_Q_OP_IS_ALIVE:
        case ERTS_SIG_Q_OP_PROCESS_INFO:
        case ERTS_SIG_Q_OP_SYNC_SUSPEND:
        case ERTS_SIG_Q_OP_RPC:
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
    erts_proc_sig_fetch(c_p);
    ERTS_FOREACH_SIG_PRIVQS(c_p, sig, clear_seq_trace_token(sig));
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
            size = erts_link_size((ErtsLink *) sig);
            break;
        case ERTS_MON_TYPE_PORT:
        case ERTS_MON_TYPE_PROC:
        case ERTS_MON_TYPE_DIST_PROC:
        case ERTS_MON_TYPE_NODE:
            size = erts_monitor_size((ErtsMonitor *) sig);
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected sig type");
            break;
        }
        break;

    case ERTS_SIG_Q_OP_SYNC_SUSPEND:
    case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
    case ERTS_SIG_Q_OP_IS_ALIVE:
        size = ((ErtsMessage *) sig)->hfrag.alloc_size;
        size *= sizeof(Eterm);
        size += sizeof(ErtsMessage) - sizeof(Eterm);
        break;

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
        if (type == ERTS_SIG_Q_TYPE_DIST_LINK) {
            size = NC_HEAP_SIZE(((ErtsSigDistLinkOp *) sig)->remote);
            size--;
            size *= sizeof(Eterm);
            size += sizeof(ErtsSigDistLinkOp);
            break;
        }
        /* Fall through... */

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
            erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
            erts_proc_sig_fetch(c_p);
            /*
             * Messages may have been moved directly to
             * inner queue...
             */
            msgp = PEEK_MESSAGE(c_p);
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

        msgp = PEEK_MESSAGE(c_p);
        if (msgp) {
            *get_outp = 0;
            *msgpp = msgp;
            return consumed_reds;
        }

        if (left_reds <= 0)
            break; /* yield */

        ASSERT(!c_p->sig_qs.cont);
        /* Go fetch again... */
    }
    
    /* Yield... */

    *get_outp = -1;
    *msgpp = NULL;

    ASSERT(consumed_reds >= (fcalls - neg_o_reds));
    return consumed_reds;
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
    int msgs_active, old_msgs_active = !!tracing->messages.active;

    ASSERT(sig == *next);

    erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);

    ERTS_TRACE_FLAGS(c_p) |= trace_info->flags_on;
    ERTS_TRACE_FLAGS(c_p) &= ~trace_info->flags_off;
    if (is_value(trace_info->tracer))
        erts_tracer_replace(&c_p->common, trace_info->tracer);

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

static ERTS_INLINE void
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
        Sint len = erts_proc_sig_privqs_len(c_p);
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
                len, /* This is NOT message queue len, but its something... */
                tok_label, tok_lastcnt, tok_serial);
    }
#endif

    if (tracing->messages.receive_trace && tracing->messages.event->on) {
        ASSERT(IS_TRACED(c_p));
        trace_receive(c_p,
                      ERL_MESSAGE_FROM(msg),
                      ERL_MESSAGE_TERM(msg),
                      tracing->messages.event);
    }
}

static int
handle_msg_tracing(Process *c_p, ErtsSigRecvTracing *tracing,
                   ErtsMessage ***next_nm_sig)
{
    ErtsMessage **next_sig, *sig;
    int cnt = 0, limit = ERTS_PROC_SIG_TRACE_COUNT_LIMIT;

    ASSERT(tracing->messages.next);
    next_sig = tracing->messages.next;
    sig = *next_sig;

    /*
     * Receive tracing active. Handle all messages
     * until next non-message signal...
     */

    while (sig && ERTS_SIG_IS_MSG(sig)) {
        if (cnt > limit) {
            tracing->messages.next = next_sig;
            return -1; /* Yield... */
        }
        if (ERTS_SIG_IS_EXTERNAL_MSG(sig)) {
            cnt += 50; /* Decode is expensive... */
            if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN,
                                           sig, 0)) {
                /* Bad dist message; remove it... */
                remove_mq_m_sig(c_p, sig, next_sig, next_nm_sig);
                sig->next = NULL;
                erts_cleanup_messages(sig);
                sig = *next_sig;
                continue;
            }
        }
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

Uint
erts_proc_sig_prep_msgq_for_inspection(Process *c_p,
                                       Process *rp,
                                       ErtsProcLocks rp_locks,
                                       int info_on_self,
                                       ErtsMessageInfo *mip)
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

                remove_iq_m_sig(rp, mp, mpp);

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
        mpp = &mp->next;
	mp = mp->next;
    }

    ASSERT(c_p->sig_qs.len == i);

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

    state = erts_atomic32_read_nob(&rp->state);
    dirty = (state & ERTS_PSFLG_DIRTY_RUNNING);
    /*
     * Ignore ERTS_PSFLG_DIRTY_RUNNING_SYS (see
     * comment in erts_execute_dirty_system_task()
     * in erl_process.c).
     */
    if (!dirty)
        BIF_RET(am_normal);

    busy = erts_proc_trylock(rp, ERTS_PROC_LOCK_MAIN) == EBUSY;

    state = erts_atomic32_read_mb(&rp->state);
    noproc = (state & ERTS_PSFLG_FREE);
    dirty = (state & ERTS_PSFLG_DIRTY_RUNNING);

    if (busy) {
        if (noproc)
            BIF_RET(am_noproc);
        if (dirty)
            BIF_RET(am_more); /* try again... */
        BIF_RET(am_normal); /* will handle signals itself... */
    }
    else {
        erts_aint32_t state;
        int done;
        Eterm res = am_false;
        int reds = 0;

        if (noproc)
            res = am_noproc;
        else if (!dirty)
            res = am_normal; /* will handle signals itself... */
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
                        lnk_func((ErtsLink *) sig, arg, -1);
                        break;
                    case ERTS_MON_TYPE_PORT:
                    case ERTS_MON_TYPE_PROC:
                    case ERTS_MON_TYPE_DIST_PROC:
                    case ERTS_MON_TYPE_NODE:
                        mon_func((ErtsMonitor *) sig, arg, -1);
                        break;
                    default:
                        ERTS_INTERNAL_ERROR("Unexpected sig type");
                        break;
                    }
                    break;

                case ERTS_SIG_Q_OP_PERSISTENT_MON_MSG:
                    debug_foreach_sig_heap_frags(&sig->hfrag, oh_func, arg);
                    break;

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
                        debug_foreach_sig_fake_oh(((ErtsSigDistLinkOp *) sig)->remote,
                                                  oh_func, arg);
                        break;
                    }
                    /* Fall through... */

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
                    break;

                default:
                    ERTS_INTERNAL_ERROR("Unknown signal");
                    break;
                }

            }
        }
    }
}

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
                          int *found_saved_last_p,
                          erts_aint32_t sig_psflg)
{
    ErtsMessage **next, *sig, **nm_next, **nm_last;
    int last_nm_sig_found, nm_sigs = 0, found_next_trace = 0,
        found_save = 0, last_sig_found = 0, found_saved_last = 0;
    Sint msg_len = 0;
    ErtsMessage **next_trace = tracing ? tracing->messages.next : NULL;
    ErtsMessage **save = proc->sig_qs.save;
    ErtsMessage **saved_last = proc->sig_qs.saved_last;

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
            ASSERT(!*next);
            last_sig_found = 1;
        }

        if (next == save)
            found_save = 1;

        if (next == saved_last)
            found_saved_last = 1;

        if (next == next_trace) {
            found_next_trace = 1;
            ERTS_ASSERT(nm_sigs == 0);
        }

        while (sig && ERTS_SIG_IS_MSG(sig)) {
            int i;
            if (ERTS_SIG_IS_EXTERNAL_MSG(sig))
                i = 1;
            else
                i = 0;
            for (; i < ERL_MESSAGE_REF_ARRAY_SZ; i++)
                chk_eterm(proc, privq, sig, sig->m[i]);

            msg_len++;
            next = &sig->next;
            sig = sig->next;

            if (next == sig_last) {
                ASSERT(!*next);
                last_sig_found = 1;
            }

            if (next == save)
                found_save = 1;

            if (next == saved_last)
                found_saved_last = 1;

            if (next == next_trace) {
                found_next_trace = 1;
                ERTS_ASSERT(nm_sigs == 0);
            }
        }

        if (!sig)
            break;

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
                ASSERT(!nm_sig->common.specific.next);
                last_nm_sig_found = 1;
            }

            nm_next = nm_sig->common.specific.next;

        }

        next = &nm_sig->common.next;
        sig = nm_sig->common.next;

    }

    if (!privq) {
        /* outer queue */
        ERTS_ASSERT(!found_save);
        ERTS_ASSERT(!found_saved_last);
    }
    else if (privq > 0) {
        /* middle queue */
        ERTS_ASSERT(!next_trace || found_next_trace);
        ERTS_ASSERT(!found_save);
        if (!found_saved_last_p) {
            ERTS_ASSERT(!found_saved_last
                        || (proc->sig_qs.flags & FS_DEFERRED_SAVED_LAST));
        }
        else {
            if (*found_saved_last_p) {
                ERTS_ASSERT(!found_saved_last);
                ERTS_ASSERT(!(proc->sig_qs.flags & FS_DEFERRED_SAVED_LAST));
            }
            else if (saved_last) {
                ERTS_ASSERT(found_saved_last);
                ERTS_ASSERT(proc->sig_qs.flags & FS_DEFERRED_SAVED_LAST);
            }
            *found_saved_last_p |= found_saved_last;
        }
    }
    else {
        /* inner queue */
        ERTS_ASSERT(!found_next_trace);
        ERTS_ASSERT(nm_sigs == 0);
        ERTS_ASSERT(found_save);
        ERTS_ASSERT(!saved_last
                    || (found_saved_last
                        || (proc->sig_qs.flags & FS_DEFERRED_SAVED_LAST)));
        if (found_saved_last_p)
            *found_saved_last_p |= found_saved_last;
    }

    ERTS_ASSERT(last_nm_sig_found);
    ERTS_ASSERT(last_sig_found);

    if (sig_psflg != ERTS_PSFLG_FREE) {
        erts_aint32_t state = erts_atomic32_read_nob(&proc->state);
        ERTS_ASSERT(nm_sigs ? !!(state & sig_psflg) : !(state & sig_psflg));
    }

    return msg_len;
}

void
erts_proc_sig_hdbg_check_priv_queue(Process *p, int qlock, char *what, char *file, int line)
{
    int found_saved_last = 0;
    Sint len, len1, len2;
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
                                     &found_saved_last,
                                     ERTS_PSFLG_FREE);
    len2 = proc_sig_hdbg_check_queue(p,
                                     1,
                                     &p->sig_qs.cont,
                                     p->sig_qs.cont_last,
                                     p->sig_qs.nmsigs.next,
                                     p->sig_qs.nmsigs.last,
                                     NULL,
                                     &found_saved_last,
                                     ERTS_PSFLG_SIG_Q);
    if (p->sig_qs.saved_last)
        ERTS_ASSERT(found_saved_last);
    len = proc_sig_privqs_len(p, qlock);
    ERTS_ASSERT(len == len1 + len2);
}

void
erts_proc_sig_hdbg_check_in_queue(Process *p, char *what, char *file, int line)
{
    Sint len;
    ERTS_LC_ASSERT(erts_thr_progress_is_blocking()
                   || ERTS_PROC_IS_EXITING(p)
                   || (ERTS_PROC_LOCK_MSGQ
                       & erts_proc_lc_my_proc_locks(p)));
    len = proc_sig_hdbg_check_queue(p,
                                    0,
                                    &p->sig_inq.first,
                                    p->sig_inq.last,
                                    p->sig_inq.nmsigs.next,
                                    p->sig_inq.nmsigs.last,
                                    NULL,
                                    NULL,
                                    ERTS_PSFLG_SIG_IN_Q);
    ASSERT(p->sig_inq.len == len); (void)len;
}

#endif /* ERTS_PROC_SIG_HARD_DEBUG */
