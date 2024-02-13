/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
 *              Currently the following signals are handled:
 *              - Messages
 *              - Exit
 *              - Monitor
 *              - Demonitor
 *              - Monitor down
 *              - Persistent monitor message
 *              - Link
 *              - Unlink
 *              - Unlink Ack
 *              - Group leader
 *              - Is process alive
 *              - Process info request
 *              - Suspend request (monitor of suspend type)
 *              - Resume request (demonitor of suspend type)
 *              - Suspend cleanup (monitor down of suspend type)
 *              - Sync suspend
 *              - RPC request
 *              - Trace change
 *
 *              The signal queue consists of three parts:
 *              - Outer queue (sig_inq field in process struct)
 *              - Middle queue (sig_qs field in process struct)
 *              - Inner queue (sig_qs field in process struct)
 *
 *              Incoming signals are placed in the outer queue
 *              by other processes, ports, or by the runtime system
 *              itself. This queue is protected by the msgq process
 *              lock and may be accessed by any other entity. While
 *              a signal is located in the outer queue, it is still
 *              in transit between sender and receiver.
 *
 *              The middle and the inner queues are private to the
 *              receiving process and can only be accessed while
 *              holding the main process lock. The signal changes
 *              from being in transit to being received while in
 *              the middle queue. Non-message signals are handled
 *              immediately upon reception while message signals
 *              are moved into the inner queue.
 *
 *              In the outer and middle queues both message signals
 *              and non-message signals are mixed. Signals in these
 *              queues are referenced using two single linked lists.
 *              One single linked list that go through all signals
 *              in the queue and another single linked list that
 *              goes through only non-message signals. The list
 *              through the non-message signals is used for fast
 *              access to these signals in the middle queue, since
 *              these should be handled immediately upon reception.
 *
 *              The inner queue consists only of one single linked
 *              list through the message signals. A receive
 *              expression can only operate on messages once they
 *              have entered the inner queue.
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_PROC_SIG_QUEUE_H_TYPE__
#define ERTS_PROC_SIG_QUEUE_H_TYPE__

#if 0
#  define ERTS_PROC_SIG_HARD_DEBUG
#endif
#if 0
#  define ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
#endif
#if 0
#  define ERTS_PROC_SIG_HARD_DEBUG_RECV_MARKER
#endif
#if 0
#  define ERTS_PROC_SIG_HARD_DEBUG_SIGQ_BUFFERS
#endif

#define ERTS_HDBG_PRIVQ_LEN__(P)                                        \
    do {                                                                \
        Sint len = 0;                                                   \
        ErtsMessage *sig;                                               \
        ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks((P))                  \
                       & ERTS_PROC_LOCK_MAIN);                          \
        for (sig = (P)->sig_qs.first; sig; sig = sig->next) {           \
            if (ERTS_SIG_IS_MSG(sig))                                   \
                len++;                                                  \
        }                                                               \
        ERTS_ASSERT((P)->sig_qs.mq_len == len);                         \
        for (sig = (P)->sig_qs.cont, len = 0; sig; sig = sig->next) {   \
            if (ERTS_SIG_IS_MSG(sig)) {                                 \
                len++;                                                  \
            }                                                           \
            else {                                                      \
                ErtsNonMsgSignal *nmsig = (ErtsNonMsgSignal *) sig;     \
                ERTS_ASSERT(nmsig->mlenoffs == len);                    \
                len = 0;                                                \
            }                                                           \
        }                                                               \
        ERTS_ASSERT((P)->sig_qs.mlenoffs == len);                       \
    } while (0)
#define ERTS_HDBG_INQ_LEN__(Q)                                          \
    do {                                                                \
        Sint len = 0;                                                   \
        ErtsMessage *sig;                                               \
        for (sig = (Q)->first; sig; sig = sig->next) {                  \
            if (ERTS_SIG_IS_MSG(sig)) {                                 \
                len++;                                                  \
            }                                                           \
            else {                                                      \
                ErtsNonMsgSignal *nmsig = (ErtsNonMsgSignal *) sig;     \
                ERTS_ASSERT(nmsig->mlenoffs == len);                    \
                len = 0;                                                \
            }                                                           \
        }                                                               \
        ERTS_ASSERT((Q)->mlenoffs == len);                              \
    } while (0)

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
#define ERTS_HDBG_PRIVQ_LEN(P) ERTS_HDBG_PRIVQ_LEN__((P))
#define ERTS_HDBG_INQ_LEN(Q) ERTS_HDBG_INQ_LEN((Q))
#else
#define ERTS_HDBG_PRIVQ_LEN(P)
#define ERTS_HDBG_INQ_LEN(Q)
#endif


struct erl_mesg;
struct erl_dist_external;

#define ERTS_SIGNAL_COMMON_FIELDS__     \
    struct erl_mesg *next;              \
    union {                             \
        struct erl_mesg **next;         \
        void *attachment;               \
    } specific;                         \
    Eterm tag


typedef struct {
    ERTS_SIGNAL_COMMON_FIELDS__;
} ErtsSignalCommon;

typedef struct {
    ERTS_SIGNAL_COMMON_FIELDS__;
    Sint mlenoffs; /* Number of msg sigs preceeding the non-msg sig */
} ErtsNonMsgSignal;

/*
 * Note that not all signal are handled using this functionality!
 */

#define ERTS_SIG_Q_OP_MAX 19

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
#define ERTS_SIG_Q_OP_RPC                       13
#define ERTS_SIG_Q_OP_DIST_SPAWN_REPLY          14
#define ERTS_SIG_Q_OP_ALIAS_MSG                 15
#define ERTS_SIG_Q_OP_RECV_MARK                 16
#define ERTS_SIG_Q_OP_UNLINK_ACK                17
#define ERTS_SIG_Q_OP_ADJ_MSGQ                  18
#define ERTS_SIG_Q_OP_FLUSH			ERTS_SIG_Q_OP_MAX

#define ERTS_SIG_Q_TYPE_MAX (ERTS_MON_LNK_TYPE_MAX + 10)

#define ERTS_SIG_Q_TYPE_UNDEFINED \
    (ERTS_MON_LNK_TYPE_MAX + 1)
#define ERTS_SIG_Q_TYPE_DIST_LINK \
    (ERTS_MON_LNK_TYPE_MAX + 2)
#define ERTS_SIG_Q_TYPE_GEN_EXIT \
    (ERTS_MON_LNK_TYPE_MAX + 3)
#define ERTS_SIG_Q_TYPE_DIST_PROC_DEMONITOR \
    (ERTS_MON_LNK_TYPE_MAX + 4)
#define ERTS_SIG_Q_TYPE_ADJUST_TRACE_INFO \
    (ERTS_MON_LNK_TYPE_MAX + 5)
#define ERTS_SIG_Q_TYPE_DIST \
    (ERTS_MON_LNK_TYPE_MAX + 6)
#define ERTS_SIG_Q_TYPE_HEAP \
    (ERTS_MON_LNK_TYPE_MAX + 7)
#define ERTS_SIG_Q_TYPE_OFF_HEAP \
    (ERTS_MON_LNK_TYPE_MAX + 8)
#define ERTS_SIG_Q_TYPE_HEAP_FRAG \
    (ERTS_MON_LNK_TYPE_MAX + 9)
#define ERTS_SIG_Q_TYPE_CLA \
    ERTS_SIG_Q_TYPE_MAX

#define ERTS_SIG_IS_DIST_ALIAS_MSG_TAG(Tag)                          \
    (((Tag) & (ERTS_PROC_SIG_TYPE_MASK                               \
               | ERTS_PROC_SIG_OP_MASK                               \
               | ERTS_PROC_SIG_BASE_TAG_MASK))                       \
     == ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALIAS_MSG,              \
                               ERTS_SIG_Q_TYPE_DIST,                 \
                               0))
#define ERTS_SIG_IS_DIST_ALIAS_MSG(sig)                              \
    ERTS_SIG_IS_DIST_ALIAS_MSG_TAG(((ErtsSignal *) (sig))->common.tag)

#define ERTS_SIG_IS_OFF_HEAP_ALIAS_MSG_TAG(Tag)                      \
    (((Tag) & (ERTS_PROC_SIG_TYPE_MASK                               \
               | ERTS_PROC_SIG_OP_MASK                               \
               | ERTS_PROC_SIG_BASE_TAG_MASK))                       \
     == ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALIAS_MSG,              \
                               ERTS_SIG_Q_TYPE_OFF_HEAP,             \
                               0))
#define ERTS_SIG_IS_OFF_HEAP_ALIAS_MSG(sig)                          \
    ERTS_SIG_IS_OFF_HEAP_ALIAS_MSG_TAG(((ErtsSignal *) (sig))->common.tag)

#define ERTS_SIG_IS_HEAP_ALIAS_MSG_TAG(Tag)                          \
    (((Tag) & (ERTS_PROC_SIG_TYPE_MASK                               \
               | ERTS_PROC_SIG_OP_MASK                               \
               | ERTS_PROC_SIG_BASE_TAG_MASK))                       \
     == ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALIAS_MSG,              \
                               ERTS_SIG_Q_TYPE_HEAP,                 \
                               0))
#define ERTS_SIG_IS_HEAP_ALIAS_MSG(sig)                              \
    ERTS_SIG_IS_HEAP_ALIAS_MSG_TAG(((ErtsSignal *) (sig))->common.tag)

#define ERTS_SIG_IS_HEAP_FRAG_ALIAS_MSG_TAG(Tag)                     \
    (((Tag) & (ERTS_PROC_SIG_TYPE_MASK                               \
               | ERTS_PROC_SIG_OP_MASK                               \
               | ERTS_PROC_SIG_BASE_TAG_MASK))                       \
     == ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_ALIAS_MSG,              \
                               ERTS_SIG_Q_TYPE_HEAP_FRAG,            \
                               0))
#define ERTS_SIG_IS_HEAP_FRAG_ALIAS_MSG(sig)                         \
    ERTS_SIG_IS_HEAP_FRAG_ALIAS_MSG_TAG(((ErtsSignal *) (sig))->common.tag)

#define ERTS_RECV_MARKER_TAG                                         \
    (ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_RECV_MARK,		     \
                            ERTS_SIG_Q_TYPE_UNDEFINED, 0))
#define ERTS_SIG_IS_RECV_MARKER(Sig)                                 \
    (((ErtsSignal *) (Sig))->common.tag == ERTS_RECV_MARKER_TAG)

#define ERTS_RECV_MARKER_PASS_MAX 4

typedef struct {
    ErtsNonMsgSignal common;
    Eterm from;
    Uint64 id;
} ErtsSigUnlinkOp;

#define ERTS_SIG_HANDLE_REDS_MAX_PREFERED (CONTEXT_REDS/40)

#ifdef ERTS_PROC_SIG_HARD_DEBUG
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P, B) \
    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__((P), (B), "")
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P, QL) \
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((P), (QL), "")
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, B , What) \
    erts_proc_sig_hdbg_check_in_queue((P), (B), (What), __FILE__, __LINE__)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, QL, What)              \
    erts_proc_sig_hdbg_check_priv_queue((P), (QL), (What), __FILE__, __LINE__)
struct process;
void erts_proc_sig_hdbg_check_priv_queue(struct process *c_p, int qlock,
                                         char *what, char *file, int line);
struct ErtsSignalInQueue_;
void erts_proc_sig_hdbg_check_in_queue(struct process *c_p,
                                       struct ErtsSignalInQueue_ *buffer,
                                       char *what, char *file, int line);
#else
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P, B)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P, QL)
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, B, What)
#define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, QL, What)
#endif

#ifdef ERTS_PROC_SIG_HARD_DEBUG_RECV_MARKER
#define ERTS_HDBG_CHK_RECV_MRKS(P) \
    erl_proc_sig_hdbg_chk_recv_marker_block((P))
struct process;
void erl_proc_sig_hdbg_chk_recv_marker_block(struct process *c_p);
#else
#define ERTS_HDBG_CHK_RECV_MRKS(P)
#endif

#endif

#if !defined(ERTS_PROC_SIG_QUEUE_H__) && !defined(ERTS_PROC_SIG_QUEUE_TYPE_ONLY)
#define ERTS_PROC_SIG_QUEUE_H__

#include "erl_process.h"
#include "erl_bif_unique.h"


void erts_proc_sig_queue_maybe_install_buffers(Process* p, erts_aint32_t state);
void erts_proc_sig_queue_flush_and_deinstall_buffers(Process* proc);
void erts_proc_sig_queue_flush_buffers(Process* proc);
void erts_proc_sig_queue_lock(Process* proc);
ERTS_GLB_INLINE ErtsSignalInQueueBufferArray*
erts_proc_sig_queue_get_buffers(Process* p, int *need_unread);
ERTS_GLB_INLINE void
erts_proc_sig_queue_unget_buffers(ErtsSignalInQueueBufferArray* buffers,
                                  int need_unget);
int erts_proc_sig_queue_try_enqueue_to_buffer(Eterm from,
                                              Process* receiver,
                                              ErtsProcLocks receiver_locks,
                                              ErtsMessage* first,
                                              ErtsMessage** last,
                                              ErtsMessage** last_next,
                                              Uint len);
int erts_proc_sig_queue_force_buffers(Process*);

#define ERTS_SIG_Q_OP_BITS      8                      
#define ERTS_SIG_Q_OP_SHIFT     0
#define ERTS_SIG_Q_OP_MASK      ((1 << ERTS_SIG_Q_OP_BITS) - 1)

#define ERTS_SIG_Q_TYPE_BITS    8
#define ERTS_SIG_Q_TYPE_SHIFT   ERTS_SIG_Q_OP_BITS
#define ERTS_SIG_Q_TYPE_MASK    ((1 << ERTS_SIG_Q_TYPE_BITS) - 1)

#define ERTS_SIG_Q_NON_X_BITS__ (_HEADER_ARITY_OFFS \
                                 + ERTS_SIG_Q_OP_BITS \
                                 + ERTS_SIG_Q_TYPE_BITS)

#define ERTS_SIG_Q_XTRA_BITS    (32 - ERTS_SIG_Q_NON_X_BITS__)
#define ERTS_SIG_Q_XTRA_SHIFT   (ERTS_SIG_Q_OP_BITS \
                                 + ERTS_SIG_Q_TYPE_BITS)
#define ERTS_SIG_Q_XTRA_MASK    ((1 << ERTS_SIG_Q_XTRA_BITS) - 1)


#define ERTS_PROC_SIG_OP(Tag) \
    ((int) (_unchecked_thing_arityval((Tag)) \
            >> ERTS_SIG_Q_OP_SHIFT) & ERTS_SIG_Q_OP_MASK)

#define ERTS_PROC_SIG_TYPE(Tag) \
    ((Uint16) (_unchecked_thing_arityval((Tag)) \
               >> ERTS_SIG_Q_TYPE_SHIFT) & ERTS_SIG_Q_TYPE_MASK)

#define ERTS_PROC_SIG_XTRA(Tag) \
    ((Uint32) (_unchecked_thing_arityval((Tag)) \
               >> ERTS_SIG_Q_XTRA_SHIFT) & ERTS_SIG_Q_XTRA_MASK)

#define ERTS_PROC_SIG_MAKE_TAG(Op, Type, Xtra)                  \
    (ASSERT(0 <= (Xtra) && (Xtra) <= ERTS_SIG_Q_XTRA_MASK),     \
     _make_header((((Type) & ERTS_SIG_Q_TYPE_MASK)              \
                   << ERTS_SIG_Q_TYPE_SHIFT)                    \
                  | (((Op) & ERTS_SIG_Q_OP_MASK)                \
                     << ERTS_SIG_Q_OP_SHIFT)                    \
                  | (((Xtra) & ERTS_SIG_Q_XTRA_MASK)            \
                     << ERTS_SIG_Q_XTRA_SHIFT),                 \
                  _TAG_HEADER_EXTERNAL_PID))

#define ERTS_PROC_SIG_BASE_TAG_MASK \
    ((1 << _HEADER_ARITY_OFFS)-1)
#define ERTS_PROC_SIG_OP_MASK \
    (ERTS_SIG_Q_OP_MASK << (ERTS_SIG_Q_OP_SHIFT + _HEADER_ARITY_OFFS))
#define ERTS_PROC_SIG_TYPE_MASK \
    (ERTS_SIG_Q_TYPE_MASK << (ERTS_SIG_Q_TYPE_SHIFT + _HEADER_ARITY_OFFS))
#define ERTS_PROC_SIG_XTRA_MASK \
    (ERTS_SIG_Q_XTRA_MASK << (ERTS_SIG_Q_XTRA_SHIFT + _HEADER_ARITY_OFFS))

struct dist_entry_;

#define ERTS_PROC_HAS_INCOMING_SIGNALS(P)                               \
    (!!(erts_atomic32_read_nob(&(P)->state)                             \
        & (ERTS_PSFLG_SIG_Q                                             \
           | ERTS_PSFLG_NMSG_SIG_IN_Q                                   \
           | ERTS_PSFLG_MSG_SIG_IN_Q)))

/*
 * Send operations of currently supported process signals follow...
 */

/**
 *
 * @brief Send an exit signal to a process.
 *
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of local process
 *                              to send signal to.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 * @param[in]     normal_kills  If non-zero, also normal exit
 *                              reason will kill the receiver
 *                              if it is not trapping exit.
 *
 */
void
erts_proc_sig_send_exit(ErtsPTabElementCommon *sender, Eterm from, Eterm to,
                        Eterm reason, Eterm token, int normal_kills);

/**
 *
 * @brief Send an exit signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_link_exit()
 * when the signal arrives via the distribution and
 * therefore no link structure is available.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     dist_ext      The exit reason in external term format
 *
 * @param[in]     hfrag         Heap frag with trace token and dist_ext
 *                              iff available, otherwise NULL.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 */
void
erts_proc_sig_send_dist_exit(DistEntry *dep,
                             Eterm from, Eterm to,
                             ErtsDistExternal *dist_ext,
                             ErlHeapFragment *hfrag,
                             Eterm reason, Eterm token);

/**
 *
 * @brief Send an exit signal due to a link to a process being
 * broken by connection loss.
 *
 * @param[in]     lnk           Pointer to link structure
 *                              from the sending side. It
 *                              should contain information
 *                              about receiver.
 */
void
erts_proc_sig_send_link_exit_noconnection(ErtsLink *lnk);

/**
 *
 * @brief Send an exit signal due to broken link to a process.
 *
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     lnk           Pointer to link structure
 *                              from the sending side. It
 *                              should contain information
 *                              about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 */
void
erts_proc_sig_send_link_exit(ErtsPTabElementCommon *sender, Eterm from,
                             ErtsLink *lnk, Eterm reason, Eterm token);

/**
 *
 * @brief Send an link signal to a process.
 *
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     lnk           Pointer to link structure to
 *                              insert on receiver side.
 *
 * @return                      A non-zero value if
 *                              signal was successfully
 *                              sent. If a zero, value
 *                              the signal was not sent
 *                              due to the receiver not
 *                              existing. The sender
 *                              needs to deallocate the
 *                              link structure.
 *
 */
int
erts_proc_sig_send_link(ErtsPTabElementCommon *sender, Eterm from,
                        Eterm to, ErtsLink *lnk);

/**
 *
 * @brief Create a new unlink identifier
 *
 * The newly created unlink identifier is to be used in an
 * unlink operation.
 *
 * @param[in]     sender        Pointer to the sending process/port.
 *
 * @return                      A new 64-bit unlink identifier
 *                              unique in context of the
 *                              calling process. The identifier
 *                              may be any value but zero.
 */
ERTS_GLB_INLINE
Uint64 erts_proc_sig_new_unlink_id(ErtsPTabElementCommon *sender);

/**
 *
 * @brief Create an unlink op signal structure
 *
 * The structure will contain a newly created unlink
 * identifier to be used in the operation.
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Id (as an erlang term) of
 *                              entity sending the unlink
 *                              signal.
 *
 * @return                      A pointer to the unlink op
 *                              structure.
 */
ErtsSigUnlinkOp *
erts_proc_sig_make_unlink_op(ErtsPTabElementCommon *sender, Eterm from);

/**
 *
 * @brief Destroy an unlink op signal structure
 *
 * @param[in]     sulnk         A pointer to the unlink op
 *                              structure.
 */
void
erts_proc_sig_destroy_unlink_op(ErtsSigUnlinkOp *sulnk);

/**
 *
 * @brief Send an unlink signal to a process.
 *
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Id (as an erlang term) of
 *                              entity sending the unlink
 *                              signal.
 *
 * @param[in]     lnk           Pointer to link structure from
 *                              the sending side. It should
 *                              contain information about
 *                              receiver.
 */
Uint64
erts_proc_sig_send_unlink(ErtsPTabElementCommon *sender, Eterm from,
                          ErtsLink *lnk);

/**
 *
 * @brief Send an unlink acknowledgment signal to a process.
 *
 * 
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Id (as an erlang term) of
 *                              entity sending the unlink
 *                              signal.
 *
 * @param[in]     sulnk         A pointer to the unlink op
 *                              structure. This structure
 *                              was typically received by
 *                              the caller in an unlink
 *                              signal.
 */
void
erts_proc_sig_send_unlink_ack(ErtsPTabElementCommon *sender, Eterm from,
                              ErtsSigUnlinkOp *sulnk);

/**
 *
 * @brief Send an exit signal due to broken link to a process.
 *
 * This function is used instead of erts_proc_sig_send_link_exit()
 * when the signal arrives via the distribution and
 * therefore no link structure is available.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     dist_ext      The exit reason in external term format
 *
 * @param[in]     hfrag         Heap frag with trace token and dist_ext
 *                              iff available, otherwise NULL.
 *
 * @param[in]     reason        Exit reason.
 *
 * @param[in]     token         Seq trace token.
 *
 */
void
erts_proc_sig_send_dist_link_exit(struct dist_entry_ *dep,
                                  Eterm from, Eterm to,
                                  ErtsDistExternal *dist_ext,
                                  ErlHeapFragment *hfrag,
                                  Eterm reason, Eterm token);

/**
 *
 * @brief Send an unlink signal to a local process.
 *
 * This function is used instead of erts_proc_sig_send_unlink()
 * when the signal arrives via the distribution.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     id            Identifier of unlink operation.
 */
void
erts_proc_sig_send_dist_unlink(DistEntry *dep, Uint32 conn_id,
                               Eterm from, Eterm to, Uint64 id);

/**
 *
 * @brief Send an unlink acknowledgment signal to a local process.
 *
 * This function is used instead of erts_proc_sig_send_unlink_ack()
 * when the signal arrives via the distribution.
 *
 * @param[in]     dep           Distribution entry of channel
 *                              that the signal arrived on.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     id            Identifier of unlink operation.
 */
void
erts_proc_sig_send_dist_unlink_ack(DistEntry *dep,
                                   Uint32 conn_id, Eterm from, Eterm to,
                                   Uint64 id);

/**
 *
 * @brief Send a monitor down signal to a process.
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Sending entity, must be provided
 *                              to maintain signal order.
 *
 * @param[in]     mon           Pointer to target monitor
 *                              structure from the sending
 *                              side. It should contain
 *                              information about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_monitor_down(ErtsPTabElementCommon *sender, Eterm from,
                                ErtsMonitor *mon, Eterm reason);

/**
 *
 * @brief Send a demonitor signal to a process.
 *
 * @param[in]     sender            Pointer to the sending process/port,
 *                                  if any, as it may not be possible to
 *                                  resolve the sender (e.g. after it's
 *                                  dead).
 *
 * @param[in]     from              Sending entity, must be provided
 *                                  to maintain signal order.
 *
 * @param[in]     system            Whether the sender is considered a
 *                                  system service, e.g. a NIF monitor,
 *                                  and it's okay to order by `from`
 *                                  even when it's not a pid or port.
 *
 * @param[in]     mon               Pointer to origin monitor
 *                                  structure from the sending
 *                                  side. It should contain
 *                                  information about receiver.
 *
 */
void
erts_proc_sig_send_demonitor(ErtsPTabElementCommon *sender, Eterm from,
                             int system, ErtsMonitor *mon);

/**
 *
 * @brief Send a monitor signal to a process.
 *
 * @param[in]     sender        Pointer to the sending process/port,
 *                              if any, as it may not be possible to
 *                              resolve the sender (e.g. after it's
 *                              dead).
 *
 * @param[in]     from          Sending entity, must be provided
 *                              to maintain signal order.
 *
 * @param[in]     mon           Pointer to target monitor
 *                              structure to insert on
 *                              receiver side.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @return                      A non-zero value if
 *                              signal was successfully
 *                              sent. If a zero, value
 *                              the signal was not sent
 *                              due to the receiver not
 *                              existing. The sender
 *                              needs to deallocate the
 *                              monitor structure.
 *
 */
int
erts_proc_sig_send_monitor(ErtsPTabElementCommon *sender, Eterm from,
                           ErtsMonitor *mon, Eterm to);

/**
 *
 * @brief Send a monitor down signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_monitor_down()
 * when the signal arrives via the distribution and
 * therefore no monitor structure is available.
 *
 * @param[in]     dep           Pointer to distribution entry
 *                              of channel that the signal
 *                              arrived on.
 *
 * @param[in]     ref           Reference identifying the monitor.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     dist_ext      The exit reason in external term format
 *
 * @param[in]     hfrag         Heap frag with trace token and dist_ext
 *                              iff available, otherwise NULL.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_dist_monitor_down(DistEntry *dep, Eterm ref,
                                     Eterm from, Eterm to,
                                     ErtsDistExternal *dist_ext,
                                     ErlHeapFragment *hfrag,
                                     Eterm reason);

/**
 *
 * @brief Send a demonitor signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_demonitor()
 * when the signal arrives via the distribution and
 * no monitor structure is available.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     ref           Reference identifying the monitor.
 *
 */
void
erts_proc_sig_send_dist_demonitor(Eterm from, Eterm to, Eterm ref);

/**
 *
 * @brief Send a persistent "node down" monitor signal to a process
 *
 * @param[in]     key           Monitor key.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     msg           Message template.
 *
 * @param[in]     msg_sz        Heap size of message template.
 *
 */
void
erts_proc_sig_send_monitor_nodes_msg(Eterm key, Eterm to,
                                     Eterm msg, Uint msg_sz);

/**
 *
 * @brief Send a persistent "time offset changed" monitor signal to a process
 *
 * @param[in]     key           Monitor key.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     msg           Message template.
 *
 * @param[in]     msg_sz        Heap size of message template.
 *
 */
void
erts_proc_sig_send_monitor_time_offset_msg(Eterm key, Eterm to,
                                           Eterm msg, Uint msg_sz);

/**
 *
 * @brief Send a group leader signal to a process.
 *
 * Set group-leader of receiving process. If sent locally,
 * a response message '{Ref, Result}' is sent to the original
 * sender when performed where Ref is the reference passed
 * as 'ref' argument, and Result is either 'true' or 'badarg'.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *                              NULL if signal arrived via
 *                              distribution.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     gl            Identifier of new group leader.
 *
 * @param[in]     ref           Reference to use in response
 *                              message to locally sending
 *                              process (i.e., c_p when c_p
 *                              is non-null).
 *
 */
void
erts_proc_sig_send_group_leader(Process *c_p, Eterm to, Eterm gl,
                                Eterm ref);

/**
 *
 * @brief Send an 'is process alive' signal to a process.
 *
 * A response message '{Ref, Result}' is sent to the
 * sender when performed where Ref is the reference passed
 * as 'ref' argument, and Result is either 'true' or 'false'.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *                              NULL if signal arrived via
 *                              distribution.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     ref           Reference to use in response
 *                              message to the sending
 *                              process (i.e., c_p).
 *
 * @returns                     A value != 0 if the request
 *                              was sent; otherwise, 0. If
 *                              the request was not sent the
 *                              process was non-existing.
 */
int
erts_proc_sig_send_is_alive_request(Process *c_p, Eterm to,
                                    Eterm ref);

/**
 *
 * @brief Send a 'process info request' signal to a process.
 *
 * A response message '{Ref, Result}' is sent to the
 * sender when performed where Ref is the reference passed
 * as 'ref' argument, and Result corresponds to return result
 * from erlang:process_info/[1,2].
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *                              NULL if signal arrived via
 *                              distribution.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     item_ix       Info index array to pass to
 *                              erts_process_info()
 *
 * @param[in]     item_extra    Extra terms array to pass to
 *                              erts_process_info()
 *
 * @param[in]     len           Length of info index array and
 *                              extra array if such is provided
 *
 * @param[in]     flags         Flags to pass to
 *                              erts_process_info()
 *
 * @param[in]     reserve_size  Heap size that is known to
 *                              be needed. May not be correct
 *                              though.
 *
 * @param[in]     ref           Reference to use in response
 *                              message to the sending
 *                              process (i.e., c_p).
 *
 */
int
erts_proc_sig_send_process_info_request(Process *c_p,
                                        Eterm to,
                                        int *item_ix,
                                        Eterm *item_extra,
                                        int len,
                                        int flags,
                                        Uint reserve_size,
                                        Eterm ref);

/**
 *
 * @brief Send a 'sync suspend' signal to a process.
 *
 * A response message '{Tag, Reply}' is sent to the
 * sender when performed where Tag is the term passed
 * as 'tag' argument. Reply is either 'suspended',
 * 'not_suspended', 'exited' if the operation is
 * asynchronous; otherwise, the 'reply' argument or
 * 'badarg' if process terminated.
 *
 * This signal does *not* change the suspend state, only
 * reads and reply the state. This signal is typically
 * sent after a suspend request (monitor of suspend type)
 * signal has been sent to the process in order to get a
 * response when the suspend monitor has been processed.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     tag           Tag to use in response
 *                              message to the sending
 *                              process (i.e., c_p).
 *
 * @param[in]     reply         Reply to send if this
 *                              is a synchronous operation;
 *                              otherwise, THE_NON_VALUE.
 */
void
erts_proc_sig_send_sync_suspend(Process *c_p, Eterm to,
                                Eterm tag, Eterm reply);

/**
 *
 * @brief Send an 'rpc' signal to a process.
 *
 * The function 'func' will be executed in the
 * context of the receiving process.
 *
 * If 'reply' is non-zero, a response message '{Ref, Result}' is
 * sent to the sender when 'func' has been called. 'Ref' is the
 * reference returned by this function and 'Result' is the term
 * returned by 'func'. If the return value of 'func' is not an
 * immediate term, 'func' has to allocate a heap fragment where
 * the result is stored and update the heap fragment pointer
 * pointer passed as third argument to point to it.
 *
 * If 'reply' is zero, no automatic response is sent, the return value from
 * 'func' is ignored and this function instead returns am_ok on success.
 *
 * If a receiving process with pid 'to' exists, 'func' will be
 * called in its context. However, note that this might happen
 * when the receiver is in an exiting state.
 *
 * If a reference is returned, the caller of this function
 * *unconditionally* has to enter a receive that match
 * on the returned reference in all clauses as next
 * receive; otherwise, bad things will happen!
 *
 * If THE_NON_VALUE is returned, the receiver did not
 * exist. The signal was not sent, and no specific
 * receive has to be entered by the caller.
 *
 * Minimum priority, that the signal will execute under,
 * will equal the priority of the calling process (c_p).
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     to            Identifier of receiver process.
 *
 * @param[in]     reply         Non-zero if a reply is wanted.
 *
 * @param[in]     func          Function to execute in the context of the
 *                              receiver. First argument will be a pointer to
 *                              the process struct of the receiver process.
 *                              Second argument will be 'arg' (see below). Third
 *                              argument will be a pointer to number of
 *                              reductions. In value is reductions left and
 *                              absolute value out is consumed reduction where a
 *                              negative value will force a yield. Fourth
 *                              argument will be a pointer to a pointer to a
 *                              heap fragment for storage of result returned
 *                              from 'func' (i.e. an 'out' parameter).
 *
 * @param[in]     arg           Void pointer to argument to pass as second
 *                              argument in call of 'func'.
 *
 * @returns                     If the request was sent, an internal ordinary
 *                              reference or the atom ok; otherwise,
 *                              THE_NON_VALUE (non-existing receiver).
 */
Eterm
erts_proc_sig_send_rpc_request(Process *c_p,
                               Eterm to,
                               int reply,
                               Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                               void *arg);
/**
 *
 * @brief Send an 'rpc' signal to a process.
 *
 * The function 'func' will be executed in the
 * context of the receiving process. A response
 * message '{Ref, Result}' is sent to the sender
 * when 'func' has been called. 'Ref' is the reference
 * returned by this function and 'Result' is the
 * term returned by 'func'. If the return value of
 * 'func' is not an immediate term, 'func' has to
 * allocate a heap fragment where the result is stored
 * and update the heap fragment pointer pointer
 * passed as third argument to point to it.
 *
 * If this function returns a reference, 'func' will
 * be called in the context of the receiver. However,
 * note that this might happen when the receiver is in
 * an exiting state. The caller of this function
 * *unconditionally* has to enter a receive that match
 * on the returned reference in all clauses as next
 * receive; otherwise, bad things will happen!
 *
 * If THE_NON_VALUE is returned, the receiver did not
 * exist. The signal was not sent, and no specific
 * receive has to be entered by the caller.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     to            Identifier of receiver process.
 *
 * @param[in]     reply         Non-zero if a reply is wanted.
 *
 * @param[in]     func          Function to execute in the
 *                              context of the receiver.
 *                              First argument will be a
 *                              pointer to the process struct
 *                              of the receiver process.
 *                              Second argument will be 'arg'
 *                              (see below). Third argument
 *                              will be a pointer to a pointer
 *                              to a heap fragment for storage
 *                              of result returned from 'func'
 *                              (i.e. an 'out' parameter).
 *
 * @param[in]     arg           Void pointer to argument
 *                              to pass as second argument
 *                              in call of 'func'.
 *
 * @param[in]     prio          Minimum priority that the
 *                              signal will execute under.
 *                              Either PRIORITY_MAX,
 *                              PRIORITY_HIGH, PRIORITY_NORMAL,
 *                              PRIORITY_LOW, or a negative
 *                              value. A negative value will
 *                              cause a minimum priority that
 *                              equals the priority of the
 *                              calling process (c_p).
 *
 * @returns                     If the request was sent,
 *                              an internal ordinary
 *                              reference; otherwise,
 *                              THE_NON_VALUE (non-existing
 *                              receiver).
 */
Eterm
erts_proc_sig_send_rpc_request_prio(Process *c_p,
                                    Eterm to,
                                    int reply,
                                    Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                                    void *arg,
                                    int prio);

int
erts_proc_sig_send_dist_spawn_reply(Eterm node,
                                    Eterm ref,
                                    Eterm to,
                                    ErtsLink *lnk,
                                    Eterm result,
                                    Eterm token);

/**
 *
 * @brief Send a 'copy literal area request' signal to
 *        a process.
 *
 * The receiver will scan its message queue and then the rest
 * of the process. After the operation has bee performed it will
 * reply with a '{copy_literals, ReqID, Res}' message to the
 * sender where 'Res' equals 'ok' if the receiver is clean or
 * 'need_gc' if a literal GC is needed.
 *
 * Should only be called by the literal-area-collector process!
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     req_id        Request ID (RegID) term.
 */
void
erts_proc_sig_send_cla_request(Process *c_p, Eterm to, Eterm req_id);


/**
 *
 * @brief Send a 'move message queue off heap' signal to
 *        a the sending process itself.
 *
 * When received, all on heap messages will be moved off heap.
 *
 * @param[in]     to            Identifier of receiver.
 *
 */
void
erts_proc_sig_send_move_msgq_off_heap(Eterm to);

/*
 * End of send operations of currently supported process signals.
 */


/**
 *
 * @brief Handle incoming signals.
 *
 * Called by an ordinary scheduler in order to handle incoming
 * signals for a process. The work is done on the middle part
 * of the signal queue. The maximum amount of signals handled
 * is limited by the amount of reductions given when calling.
 * Note that a reduction does not necessarily map to a signal.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[out]    statep        Pointer to process state after
 *                              signal handling. May not be NULL.
 *                              The state should recently have
 *                              been updated before calling
 *                              this function.
 *
 * @param[in,out] redsp         Pointer to an integer containing
 *                              reductions. On input, the amount
 *                              of preferred reductions to be
 *                              used by the call. On output, the
 *                              amount of reductions consumed.
 *
 * @param[in]     max_reds      Absolute maximum of reductions
 *                              to use. If the process cannot
 *                              make progress after the preferred
 *                              amount of reductions has been
 *                              consumed, signal handling may
 *                              proceed up to a maximum of
 *                              'max_reds' in order to make
 *                              the process able to proceed
 *                              with other tasks after handling
 *                              has finished.
 *
 * @param[in]     local_only    If is zero, new signals may be
 *                              fetched from the outer queue and
 *                              put in the middle queue before
 *                              signal handling is performed. If
 *                              non-zero, no new signals will be
 *                              fetched before handling begins.
 *
 * @return                      Returns a non-zero value, when
 *                              no more signals to handle in the
 *                              middle queue remain. A zero
 *                              return value means that there
 *                              remains signals in the middle
 *                              queue.
 */
int
erts_proc_sig_handle_incoming(Process *c_p, erts_aint32_t *statep,
                              int *redsp, int max_reds,
                              int local_only);

/**
 *
 * @brief Handle remaining signals for an exiting process
 *
 * Called as part of termination of a process. It will handle
 * remaining signals.
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in,out] redsp         Pointer to an integer containing
 *                              reductions. On input, the amount
 *                              of maximum reductions to be
 *                              used by the call. On output, the
 *                              amount of reductions consumed.
 *
 * @param[in,out] pe_ctxtp      Process exit context pointer.
 *
 * @return                      Returns a non-zero value, when
 *                              no more signals to handle in the
 *                              middle queue remain. A zero
 *                              return value means that there
 *                              remains signals in the middle
 *                              queue.
 */
int
erts_proc_sig_handle_exit(Process *c_p, Sint *redsp,
                          ErtsProcExitContext *pe_ctxt_p);

/**
 *
 * @brief Helper for loop_rec instruction.
 *
 * This function should only be called from the loop_rec
 * instruction (or equivalents). It is called when loop_rec
 * reach the end of the inner queue (which is the only
 * part of the signal queue that receive is allowed to
 * operate on). When called, this function tries to make
 * more messages available in the inner queue. This by
 * fetching signals from the outer queue to the middle
 * queue and/or processing signals in the middle queue.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]   fcalls          Content of FCALLS in
 *                              process_main()
 *
 * @param[in]   neg_o_reds      Content of neg_o_reds in
 *                              process_main()
 *
 * @param[out]  msgpp           Pointer to pointer to next
 *                              available message to process.
 *                              If *msgpp == NULL, no more
 *                              messages are available.
 *
 * @param[out]  get_outp        Pointer to an integer
 *                              indicating how to respond
 *                              if no more messages are
 *                              available (msgpp). If integer
 *                              is set to zero, loop_rec
 *                              should jump to an appropriate
 *                              wait instruction. If zero,
 *                              the message queue lock remain
 *                              locked since the test for
 *                              more messages was done.
 *                              If the integer is set to a
 *                              value larger that zero, the
 *                              process exited. If the integer
 *                              is set to a value less than
 *                              zero, the process is required
 *                              to yield.
 *
 *
 * @return                      The amount of reductions
 *                              consumed.
 *
 */
int
erts_proc_sig_receive_helper(Process *c_p, int fcalls,
                             int neg_o_reds, ErtsMessage **msgpp,
                             int *get_outp);

/*
 * FROM_ALL   - Flush signals from all local senders (processes
 *              and ports).
 */
#define ERTS_PROC_SIG_FLUSH_FLG_FROM_ALL            (1 << 1)
/*
 * FROM_ID    - Flush signals from process or port identified
 *              by 'id'.
 */
#define ERTS_PROC_SIG_FLUSH_FLG_FROM_ID             (1 << 2)

/*
 * All erts_proc_sig_init_flush_signals() flags.
 */
#define ERTS_PROC_SIG_FLUSH_FLGS                                \
    (ERTS_PROC_SIG_FLUSH_FLG_FROM_ALL                           \
     | ERTS_PROC_SIG_FLUSH_FLG_FROM_ID)

/**
 *
 * @brief Initialize flush of signals from another process or port
 *
 * Inserts a flush signal in the outer signal queue of
 * current process and sets the FS_FLUSHING_SIGS flag in
 * 'c_p->sig_qs.flags'. When the flush signal has been
 * handled the FS_FLUSHED_SIGS flag is set as well.
 *
 * While the flushing is ongoing the process *should* only
 * handle incoming signals and not execute Erlang code. When
 * the functionality that initiated the flush detects that
 * the flush is done by the FS_FLUSHED_SIGS flag being set,
 * it should clear both the FS_FLUSHED_SIGS flag and the
 * FS_FLUSHING_SIGS flag.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *              flags           Flags indicating how to flush.
 *                              (see above).
 *              from            Identifier of sender to flush
 *                              signals from in case the
 *                              FROM_ID flag is set.
 */
void
erts_proc_sig_init_flush_signals(Process *c_p, int flags, Eterm from);

/**
 *
 * @brief Fetch signals from the outer queue
 *
 * Fetches signals from outer queue and places them in the
 * middle queue ready for signal handling. If the middle
 * queue is empty, only message signals were present in the
 * outer queue, and no receive tracing has been enabled on
 * the process, the middle queue is bypassed and messages
 * are delivered directly to the inner queue instead.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 * @returns                     Amount of message signals in
 *                              inner plus middle signal
 *                              queues after fetch completed
 *                              (NOT the message queue
 *                              length).
 */
ERTS_GLB_INLINE Sint erts_proc_sig_fetch(Process *p);

/**
 *
 * @brief Get amount of signals in private queues
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]   max_nmsigs      Maximum amount of signals to
 *                              traverse in order to calculate
 *                              the result. If this limit is
 *                              reached the operation is aborted
 *                              and the current calculated
 *                              result is returned as a negative
 *                              amount. If a negative number
 *                              is given, an unlimited amount
 *                              of signals will be traversed in
 *                              order to calculate the result.
 *
 *
 * @param[in]   max_nmsigs      Maximum amount of non-message
 *                              signals to traverse in order
 *                              to calculate the result. If
 *                              this limit is reached the
 *                              operation is aborted and the
 *                              current calculated result
 *                              is returned as a negative
 *                              amount. If a negative number
 *                              is given, an unlimited amount
 *                              of non-message signals will
 *                              be traversed in order to
 *                              calculate the result.
 *
 * @returns                     Amount of signals in
 *                              inner plus middle signal
 *                              queues after fetch completed
 *                              (this is NOT the message queue
 *                              length). Negative amount
 *                              if the operation was aborted
 *                              (see above).
 */
ERTS_GLB_INLINE Sint
erts_proc_sig_privqs_len(Process *c_p, Sint max_sigs, Sint max_nmsigs);

/**
 * @brief Enqueue a sequence of signals on an in signal queue of
 *        a process
 *
 * The *only* valid scenarios:
 * * One or more message signals and no non-message signals.
 * * One non-message signal followed by one or more message signals
 *
 * @param rp[in]                 Process to which the in signal queue
 *                               belong.
 * @param first[in]              Pointer to the first signal in signal
 *                               sequence.
 * @param last[in]               Pointer to the next pointer of the
 *                               last signal in the sequence. This
 *                               next pointer should equal NULL.
 * @param msg_cnt[in]            Number of message signals in seqence
 * @param in_state[in]           state of rp upon call.
 *
 * @return                       Possibly changed state of rp.
 */
erts_aint32_t
erts_enqueue_signals(Process *rp, ErtsMessage *first,
                     ErtsMessage **last,
                     Uint msg_cnt,
                     erts_aint32_t in_state);

/**
 *
 * @brief Flush pending signal.
 *
 */
void
erts_proc_sig_send_pending(Process *c_p, ErtsSchedulerData* esdp);


void
erts_proc_sig_send_to_alias(Process *c_p, Eterm from, Eterm to,
                            Eterm msg, Eterm token);

void
erts_proc_sig_send_dist_to_alias(Eterm from, Eterm alias,
                                 ErtsDistExternal *edep,
                                 ErlHeapFragment *hfrag, Eterm token);

/**
 *
 * @brief Schedule process to handle enqueued signal(s).
 *
 * @param rp                Receiving process.
 * @param state             'state' of rp.
 * @param enable_flag       Additional state flags to enable, like
 *                          ERTS_PSFLG_ACTIVE if message has been enqueued.
 */
ERTS_GLB_INLINE void erts_proc_notify_new_sig(Process* rp, erts_aint32_t state,
                                              erts_aint32_t enable_flag);
ERTS_GLB_INLINE void erts_proc_notify_new_message(Process *p,
                                                  ErtsProcLocks locks);

void
erts_ensure_dirty_proc_signals_handled(Process *proc,
                                       erts_aint32_t state,
                                       erts_aint32_t prio,
                                       ErtsProcLocks locks);

typedef struct {
    Uint size;
    ErtsMessage *msgp;
} ErtsMessageInfo;

/**
 *
 * @brief Prepare signal queue for inspection by process_info()
 *
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]   rp              Pointer to process struct of
 *                              process to inspect.
 *
 * @param[in]   rp_locks        Process locks held on 'rp'.
 *
 * @param[in]   info_on_self    Integer set to non-zero value
 *                              if caller is inspecting itself;
 *                              otherwise, zero.
 *
 * @param[in]   mip             Pointer to array of
 *                              ErtsMessageInfo structures.
 *
 * @param[out]  msgq_lenp       Pointer to integer containing
 *                              amount of messages.
 */
Uint erts_proc_sig_prep_msgq_for_inspection(Process *c_p,
                                            Process *rp,
                                            ErtsProcLocks rp_locks,
                                            int info_on_self,
                                            ErtsMessageInfo *mip,
                                            Sint *msgq_lenp);

/**
 *
 * @brief Move message data of messages in private queues to heap
 *
 * Move message data of messages in private queues to the heap.
 * This is part of GC of processes that uses on-heap message
 * data.
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 */
void erts_proc_sig_move_msgs_to_heap(Process *c_p);

/**
 *
 * @brief Size of signal in bytes.
 *
 * @param[in]   sig             Signal to inspect.
 *
 */
Uint erts_proc_sig_signal_size(ErtsSignal *sig);


/**
 *
 * @brief Clear seq trace tokens on all signals
 *
 * Assumes thread progress has been blocked!
 *
 * @param[in]   c_p             Pointer to process
 *
 */
void
erts_proc_sig_clear_seq_trace_tokens(Process *c_p);

/**
 *
 * @brief Handle pending suspend requests
 *
 * Should be called by processes when they stop
 * execution on a dirty scheduler if they have
 * pending suspend requests (i.e. when
 * ERTS_PROC_GET_PENDING_SUSPEND(c_p) != NULL).
 *
 * @param[in]   c_p             Pointer to executing
 *                              process
 */
void
erts_proc_sig_handle_pending_suspend(Process *c_p);

/**
 *
 * @brief Decode the reason term in an external signal
 *
 * Any distributed signal with a payload only has the control
 * message decoded by the dist entry. The final decode of the
 * payload is done by the process when it inspects the signal
 * by calling this function.
 *
 * This functions handles both messages and link/monitor exits.
 *
 * Return true if the decode was successful, false otherwise.
 *
 * @param[in]   c_p             Pointer to executing process
 *
 * @param[in]   proc_lock       Locks held by process. Should always be MAIN.
 *
 * @param[in]   msgp            The signal to decode
 *
 * @param[in]   force_off_heap  If the term should be forced to be off-heap
 */
int
erts_proc_sig_decode_dist(Process *proc, ErtsProcLocks proc_locks,
                          ErtsMessage *msgp, int force_off_heap);

/**
 *
 * @brief Check if a newly scheduled process needs to wait for
 *        ongoing dirty handling of signals to complete.
 *
 * @param[in]   c_p             Pointer to executing process.
 *
 * @param[in]   state_in        State of process.
 *
 * @return                      Updated state of process if
 *                              we had to wait; otherwise,
 *                              state_in.
 */
ERTS_GLB_INLINE erts_aint32_t
erts_proc_sig_check_wait_dirty_handle_signals(Process *c_p,
                                              erts_aint32_t state_in);

void erts_proc_sig_do_wait_dirty_handle_signals__(Process *c_p);


ErtsDistExternal *
erts_proc_sig_get_external(ErtsMessage *msgp);

void
erts_proc_sig_cleanup_non_msg_signal(ErtsMessage *sig);


/**
 *
 * @brief Create and insert a receive marker at the end of the
 *        signal queue of the calling process unless the
 *        signal queue is empty.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @return                      A process unique integer
 *                              identifying the unbound
 *                              receive marker, or the atom
 *                              'undefined' if no marker was
 *                              inserted.
 */
ERTS_GLB_INLINE Eterm erts_msgq_recv_marker_insert(Process *c_p);

/**
 *
 * @brief Bind a previously inserted receive marker to a
 *        reference.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     insert_id     Receive marker identifier returned
 *                              by erts_msgq_recv_marker_insert().
 *
 * @param[in]     bind_id       An internal reference to bind
 *                     	        the receive marker to. Other
 *                              terms are allowed, but will
 *                              cause the receive marker
 *                              identified by insert_id to be
 *                              cleared. Note that the special
 *                              literal internal reference
 *                              'erts_old_recv_marker_id' is
 *                              *not* allowed to be passed here!
 */
ERTS_GLB_INLINE void erts_msgq_recv_marker_bind(Process *c_p,
						Eterm insert_id,
						Eterm bind_id);

/**
 *
 * @brief Create, insert, and bind a receive marker at the end
 *        of the signal queue of the calling process and unless
 *        the signal queue is empty.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     id            An internal reference to bind
 *                     	        the receive marker to. Other
 *                              terms are allowed, but will
 *                              be ignored.
 */
ERTS_GLB_INLINE void erts_msgq_recv_marker_insert_bind(Process *c_p,
						       Eterm id);


/**
 *
 * @brief Set the message queue save pointer to the position
 *        identified by the previously inserted receive marker.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     id            Internal reference bound to
 *                              a receive marker. Other terms
 *                              are allowed but will be
 *                              ignored.
 */
ERTS_GLB_INLINE void erts_msgq_recv_marker_set_save(Process *c_p, Eterm id);

/**
 *
 * @brief Clear receive marker corresponding to the argument
 *        id.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     id            Internal reference bound to
 *                              a receive marker or an insert
 *                              id. Other terms are allowed
 *                              but will be ignored.
 */
ERTS_GLB_INLINE void erts_msgq_recv_marker_clear(Process *c_p, Eterm id);


/**
 *
 * @brief Peek on next message (identified by save pointer) in
 *	  message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 */
ERTS_GLB_INLINE ErtsMessage *erts_msgq_peek_msg(Process *c_p);

/**
 *
 * @brief Remove a message from the message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     msgp          A pointer to the message to
 *                              remove from the message queue.
 *
 */
ERTS_GLB_INLINE void erts_msgq_unlink_msg(Process *c_p,
					  ErtsMessage *msgp);

/**
 *
 * @brief Set the save pointer to the start of the message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 */
ERTS_GLB_INLINE void erts_msgq_set_save_first(Process *c_p);

/**
 *
 * @brief Remove a message from the message queue and set
 *        the save pointer to the start of the message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     msgp          A pointer to the message to
 *                              remove from the message queue.
 *
 */
ERTS_GLB_INLINE void erts_msgq_unlink_msg_set_save_first(Process *c_p,
                                                         ErtsMessage *msgp);

/**
 *
 * @brief Advance the save pointer to the next message in the
 *        message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 */
ERTS_GLB_INLINE void erts_msgq_set_save_next(Process *c_p);

/**
 *
 * @brief Set the save pointer to the end of the message queue.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 */
ERTS_GLB_INLINE void erts_msgq_set_save_end(Process *c_p);

/**
 *
 * @brief Cleanup private signal queues at termination of
 *        process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 */
void erts_proc_sig_cleanup_queues(Process *c_p);


/**
 * @brief Initialize this functionality
 */
void erts_proc_sig_queue_init(void);

void
erts_proc_sig_debug_foreach_sig(Process *c_p,
                                void (*msg_func)(ErtsMessage *, void *),
                                void (*oh_func)(ErlOffHeap *, void *),
                                ErtsMonitorFunc mon_func,
                                ErtsLinkFunc lnk_func,
                                void (*ext_func)(ErtsDistExternal *, void *),
                                void *arg);

extern Process *erts_dirty_process_signal_handler;
extern Process *erts_dirty_process_signal_handler_high;
extern Process *erts_dirty_process_signal_handler_max;

/* Helpers... */
void erts_proc_sig_fetch__(Process *proc,
                           ErtsSignalInQueueBufferArray* buffers,
                           int need_unget_buffers);
ERTS_GLB_INLINE void erts_chk_sys_mon_long_msgq_on(Process *proc);
ERTS_GLB_INLINE void erts_chk_sys_mon_long_msgq_off(Process *proc);
ERTS_GLB_INLINE int erts_msgq_eq_recv_mark_id__(Eterm term1, Eterm term2);
ERTS_GLB_INLINE void erts_msgq_recv_marker_set_save__(Process *c_p,
				 ErtsRecvMarkerBlock *blkp,
				 ErtsRecvMarker *markp,
				 int ix);
Eterm erts_msgq_recv_marker_create_insert(Process *c_p, Eterm id);
void erts_msgq_recv_marker_create_insert_set_save(Process *c_p, Eterm id);
ErtsMessage **erts_msgq_pass_recv_markers(Process *c_p,
					  ErtsMessage **markpp);
void erts_msgq_remove_leading_recv_markers_set_save_first(Process *c_p);

#define ERTS_RECV_MARKER_IX__(BLKP, MRKP) \
    ((int) ((MRKP) - &(BLKP)->marker[0]))

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Uint64
erts_proc_sig_new_unlink_id(ErtsPTabElementCommon *sender)
{
    Uint64 id;

    ASSERT(sender);

    if (is_internal_pid(sender->id)) {
        Process *c_p = ErtsContainerStruct(sender, Process, common);
        id = (Uint64) c_p->uniq++;

        if (id == 0) {
            id = (Uint64) c_p->uniq++;
        }
    } else {
        ASSERT(is_internal_port(sender->id));

        id = (Uint64) erts_raw_get_unique_monotonic_integer();
        if (id == 0) {
            id = (Uint64) erts_raw_get_unique_monotonic_integer();
        }
    }

    ASSERT(id != 0);

    return id;
}

ERTS_GLB_INLINE ErtsSignalInQueueBufferArray*
erts_proc_sig_queue_get_buffers(Process* p, int *need_unread)
{
    ErtsThrPrgrDelayHandle dhndl = erts_thr_progress_unmanaged_delay();
    ErtsSignalInQueueBufferArray* buffers =
        (ErtsSignalInQueueBufferArray*)erts_atomic_read_acqb(&p->sig_inq_buffers);
    *need_unread = 0;
    if (ERTS_THR_PRGR_DHANDLE_MANAGED == dhndl) {
        erts_thr_progress_unmanaged_continue(dhndl);
        return buffers;
    }
    if (buffers == NULL) {
        erts_thr_progress_unmanaged_continue(dhndl);
        return NULL;
    }
    erts_refc_inc(&buffers->dirty_refc, 2);
    erts_thr_progress_unmanaged_continue(dhndl);
    *need_unread = 1;
    return buffers;
}

ERTS_GLB_INLINE void
erts_proc_sig_queue_unget_buffers(ErtsSignalInQueueBufferArray* buffers,
                                  int need_unget)
{
    if (!need_unget) {
        return;
    } else {
        int i;
        erts_aint_t refc = erts_refc_dectest(&buffers->dirty_refc, 0);
        if (refc != 0) {
            return;
        }
        ASSERT(!buffers->alive);
        for (i = 0; i < ERTS_PROC_SIG_INQ_BUFFERED_NR_OF_BUFFERS; i++) {
            ASSERT(!buffers->slots[i].b.alive);
            erts_mtx_destroy(&buffers->slots[i].b.lock);
        }
        erts_free(ERTS_ALC_T_SIGQ_BUFFERS, buffers);
    }
}

ERTS_GLB_INLINE void
erts_chk_sys_mon_long_msgq_on(Process *proc)
{
    if (((proc->sig_qs.flags & (FS_MON_MSGQ_LEN|FS_MON_MSGQ_LEN_LONG))
         == FS_MON_MSGQ_LEN)
        && proc->sig_qs.mq_len >= erts_system_monitor_long_msgq_on) {
        proc->sig_qs.flags |= FS_MON_MSGQ_LEN_LONG;
        monitor_generic(proc, am_long_message_queue, am_true);
    }
}

ERTS_GLB_INLINE void
erts_chk_sys_mon_long_msgq_off(Process *proc)
{
    if (((proc->sig_qs.flags & (FS_MON_MSGQ_LEN|FS_MON_MSGQ_LEN_LONG))
         == (FS_MON_MSGQ_LEN|FS_MON_MSGQ_LEN_LONG))
        && proc->sig_qs.mq_len <= erts_system_monitor_long_msgq_off) {
        proc->sig_qs.flags &= ~FS_MON_MSGQ_LEN_LONG;
        monitor_generic(proc, am_long_message_queue, am_false);
    }
}

ERTS_GLB_INLINE Sint
erts_proc_sig_fetch(Process *proc)
{
    Sint res;
    ErtsSignalInQueueBufferArray* buffers;
    int need_unget_buffers;
    ERTS_LC_ASSERT((erts_proc_lc_my_proc_locks(proc)
                    & (ERTS_PROC_LOCK_MAIN
                       | ERTS_PROC_LOCK_MSGQ))
                   == (ERTS_PROC_LOCK_MAIN
                       | ERTS_PROC_LOCK_MSGQ));

    ASSERT(!(proc->sig_qs.flags & FS_FLUSHING_SIGS)
           || ERTS_PROC_IS_EXITING(proc)
           || ERTS_IS_CRASH_DUMPING);

    ASSERT(!(proc->sig_qs.flags & FS_HANDLING_SIGS));

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc, &proc->sig_inq);
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(proc, !0);

    proc->sig_qs.flags &= ~FS_NON_FETCH_CNT_MASK;

    buffers = erts_proc_sig_queue_get_buffers(proc, &need_unget_buffers);

    if (buffers || proc->sig_inq.first)
        erts_proc_sig_fetch__(proc, buffers, need_unget_buffers);
    res = proc->sig_qs.mq_len;

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(proc, !0);

    return res;
}

ERTS_GLB_INLINE Sint
erts_proc_sig_privqs_len(Process *c_p, Sint max_sigs, Sint max_nmsigs)
{
    Sint res = c_p->sig_qs.mq_len;
    int no_nmsigs = 0;
    ErtsMessage **nmsigpp;

    ERTS_HDBG_PRIVQ_LEN(c_p);

    if (max_sigs < 0)
        max_sigs = ERTS_SWORD_MAX; /* Check all... */
    if (res > max_sigs)
        return -res;

    nmsigpp = c_p->sig_qs.nmsigs.next;
    if (max_nmsigs < 0)
        max_nmsigs = ERTS_SWORD_MAX; /* Check all... */
    res += c_p->sig_qs.mlenoffs;
    while (nmsigpp) {
        ErtsNonMsgSignal *nmsigp = (ErtsNonMsgSignal *) *nmsigpp;
        ASSERT(nmsigp);
        res += nmsigp->mlenoffs + 1;
        if (res > max_sigs)
            return -res;
        if (++no_nmsigs > max_nmsigs)
            return -res; /* Abort; to many non-message signals... */
        nmsigpp = nmsigp->specific.next;
    }

    return res;
}

ERTS_GLB_INLINE void
erts_proc_notify_new_sig(Process* rp, erts_aint32_t state,
                         erts_aint32_t enable_flag)
{
    if ((!(state & (ERTS_PSFLG_EXITING
                    | ERTS_PSFLG_ACTIVE_SYS))) | (~state & enable_flag)) {
        /* Schedule process... */
        state = erts_proc_sys_schedule(rp, state, enable_flag);
    }

    if (ERTS_PROC_IN_DIRTY_STATE(state)) {
        erts_ensure_dirty_proc_signals_handled(rp, state, -1, 0);
    }
}



ERTS_GLB_INLINE void
erts_proc_notify_new_message(Process *p, ErtsProcLocks locks)
{
    /* No barrier needed, due to msg lock */
    erts_aint32_t state = erts_atomic32_read_nob(&p->state);
    if (!(state & ERTS_PSFLG_ACTIVE))
	erts_schedule_process(p, state, locks);
    if (ERTS_PROC_NEED_DIRTY_SIG_HANDLING(state)) {
        erts_ensure_dirty_proc_signals_handled(p, state, -1, locks);
    }
}


#undef ERTS_PROC_SIG_RECV_MARK_CLEAR_PENDING_SET_SAVE__
#define ERTS_PROC_SIG_RECV_MARK_CLEAR_PENDING_SET_SAVE__(BLKP) 		\
    do {								\
	if ((BLKP)->pending_set_save_ix >= 0) {				\
	    int clr_ix__ = (BLKP)->pending_set_save_ix;			\
	    ErtsRecvMarker *clr_markp__ = &(BLKP)->marker[clr_ix__];	\
	    ASSERT(!clr_markp__->in_msgq);				\
	    ASSERT(clr_markp__->in_sigq);				\
	    ASSERT(clr_markp__->set_save);				\
	    clr_markp__->set_save = 0;					\
	    (BLKP)->pending_set_save_ix = -1;				\
	}								\
    } while (0)

ERTS_GLB_INLINE int
erts_msgq_eq_recv_mark_id__(Eterm term1, Eterm term2)
{
    int ix, arity;
    Eterm *tp1, *tp2;

    ASSERT(term1 == am_free || term1 == am_undefined || term1 == NIL
	   || is_small(term1) || is_big(term1) || is_internal_ref(term1));
    ASSERT(term2 == am_free || term2 == am_undefined || term2 == NIL
	   || is_small(term2) || is_big(term2) || is_internal_ref(term2));

    if (term1 == term2)
	return !0;

    if (!is_boxed(term1) || !is_boxed(term2))
	return 0;

    tp1 = boxed_val(term1);
    tp2 = boxed_val(term2);

    if (*tp1 != *tp2)
	return 0;

    arity = (int) thing_arityval(*tp1);
    for (ix = 1; ix <= arity; ix++) {
	if (tp1[ix] != tp2[ix])
	    return 0;
    }
    return !0;
}

ERTS_GLB_INLINE void
erts_msgq_recv_marker_set_save__(Process *c_p,
				 ErtsRecvMarkerBlock *blkp,
				 ErtsRecvMarker *markp,
				 int ix)
{
    ERTS_PROC_SIG_RECV_MARK_CLEAR_PENDING_SET_SAVE__(blkp);

    ASSERT(markp->proc == c_p);
    ASSERT(!markp->set_save);
    ASSERT(markp->in_sigq);

    if (markp->in_msgq) {
        ErtsMessage **sigpp = &markp->sig.common.next;
	if (*sigpp && ERTS_SIG_IS_RECV_MARKER(*sigpp))
	    sigpp = erts_msgq_pass_recv_markers(c_p, sigpp);
        c_p->sig_qs.save = sigpp;
    }
    else {
        /*
         * Marker is in the middle queue of signals not
         * processed yet. Trigger handling of signals in loop_rec
         * by setting save pointer to the end of message queue
         * (inner queue). This in order to get the recv marker
         * into the message queue.
         */
        c_p->sig_qs.save = c_p->sig_qs.last;
        ASSERT(!(*c_p->sig_qs.save));
        /*
         * Set save pointer when marker enters message queue...
         */
        markp->set_save = !0;
        ASSERT(blkp->pending_set_save_ix == -1);
	ASSERT(ix == ERTS_RECV_MARKER_IX__(blkp, markp));
        blkp->pending_set_save_ix = ix;
    }
}

ERTS_GLB_INLINE void
erts_msgq_recv_marker_clear(Process *c_p, Eterm id)
{
    ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;
    int ix;
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));

    if (!is_small(id) && !is_big(id) && !is_internal_ref(id))
	return;

    if (!blkp)
	return;

    for (ix = 0; ix < ERTS_RECV_MARKER_BLOCK_SIZE; ix++) {
	if (erts_msgq_eq_recv_mark_id__(blkp->ref[ix], id)) {
	    blkp->unused++;
	    blkp->ref[ix] = am_undefined;
	    blkp->marker[ix].pass = ERTS_RECV_MARKER_PASS_MAX;
	    break;
	}
    }
}

ERTS_GLB_INLINE Eterm
erts_msgq_recv_marker_insert(Process *c_p)
{
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    erts_proc_sig_queue_lock(c_p);
    erts_proc_sig_fetch(c_p);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

    if (c_p->sig_qs.cont || c_p->sig_qs.first)
	return erts_msgq_recv_marker_create_insert(c_p, am_new_uniq);
    return am_undefined;
}

ERTS_GLB_INLINE void erts_msgq_recv_marker_bind(Process *c_p,
						Eterm insert_id,
						Eterm bind_id)
{
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));

    if (is_small(insert_id) || is_big(insert_id)) {
	ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;

	if (blkp) {
	    int ix;
	    for (ix = 0; ix < ERTS_RECV_MARKER_BLOCK_SIZE; ix++) {
		if (erts_msgq_eq_recv_mark_id__(blkp->ref[ix], insert_id)) {
		    if (is_internal_ref(bind_id))
			blkp->ref[ix] = bind_id;
		    else {
			blkp->unused++;
			blkp->ref[ix] = am_undefined;
			blkp->marker[ix].pass = ERTS_RECV_MARKER_PASS_MAX;
		    }
		    break;
		}
	    }
	}
    }
}


ERTS_GLB_INLINE void
erts_msgq_recv_marker_insert_bind(Process *c_p, Eterm id)
{
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    if (is_internal_ref(id)) {
        erts_proc_sig_queue_lock(c_p);
	erts_proc_sig_fetch(c_p);
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

	if (c_p->sig_qs.cont || c_p->sig_qs.first)
	    (void) erts_msgq_recv_marker_create_insert(c_p, id);
    }
}

ERTS_GLB_INLINE void
erts_msgq_recv_marker_set_save(Process *c_p, Eterm id)
{
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    if (is_internal_ref(id)) {
	ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;

	if (blkp) {
	    int ix;
	    for (ix = 0; ix < ERTS_RECV_MARKER_BLOCK_SIZE; ix++) {
		if (erts_msgq_eq_recv_mark_id__(blkp->ref[ix], id)) {
		    ErtsRecvMarker *markp = &blkp->marker[ix];
		    erts_msgq_recv_marker_set_save__(c_p, blkp, markp, ix);
		    break;
		}
	    }
	}

    }
}

ERTS_GLB_INLINE ErtsMessage *
erts_msgq_peek_msg(Process *c_p)
{
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    ASSERT(!(*c_p->sig_qs.save) || ERTS_SIG_IS_MSG(*c_p->sig_qs.save));
    return *c_p->sig_qs.save;
}

ERTS_GLB_INLINE void
erts_msgq_unlink_msg(Process *c_p, ErtsMessage *msgp)
{
    ErtsMessage *sigp = msgp->next;
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(c_p, 0, "before");
    *c_p->sig_qs.save = sigp;
    c_p->sig_qs.mq_len--;
    ASSERT(c_p->sig_qs.mq_len >= 0);
    erts_chk_sys_mon_long_msgq_off(c_p);
    if (sigp && ERTS_SIG_IS_RECV_MARKER(sigp)) {
        ErtsMessage **sigpp = c_p->sig_qs.save;
        ((ErtsRecvMarker *) sigp)->prev_next = sigpp;
        c_p->sig_qs.save = erts_msgq_pass_recv_markers(c_p, sigpp);
	sigp = *c_p->sig_qs.save;
    }
    if (!sigp)
        c_p->sig_qs.last = c_p->sig_qs.save;
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(c_p, 0, "after");
}

ERTS_GLB_INLINE void
erts_msgq_set_save_first(Process *c_p)
{
    ErtsRecvMarkerBlock *blkp = c_p->sig_qs.recv_mrk_blk;
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    if (blkp) {
	ERTS_PROC_SIG_RECV_MARK_CLEAR_PENDING_SET_SAVE__(blkp);
    }    

    /*
     * Remove any receive markers at the front of the
     * message queue, since they don't have any purpose
     * anymore...
     */
    if (c_p->sig_qs.first && ERTS_SIG_IS_RECV_MARKER(c_p->sig_qs.first))
	erts_msgq_remove_leading_recv_markers_set_save_first(c_p);
    else
        c_p->sig_qs.save = &c_p->sig_qs.first;
}

ERTS_GLB_INLINE void
erts_msgq_unlink_msg_set_save_first(Process *c_p, ErtsMessage *msgp)
{
    ErtsMessage *sigp = msgp->next;
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(c_p, 0, "before");
    *c_p->sig_qs.save = sigp;
    c_p->sig_qs.mq_len--;
    ASSERT(c_p->sig_qs.mq_len >= 0);
    erts_chk_sys_mon_long_msgq_off(c_p);
    if (!sigp)
        c_p->sig_qs.last = c_p->sig_qs.save;
    else if (ERTS_SIG_IS_RECV_MARKER(sigp))
        ((ErtsRecvMarker *) sigp)->prev_next = c_p->sig_qs.save;
    erts_msgq_set_save_first(c_p);
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(c_p, 0, "after");
}

ERTS_GLB_INLINE void
erts_msgq_set_save_next(Process *c_p)
{
    ErtsMessage *sigp = (*c_p->sig_qs.save)->next;
    ErtsMessage **sigpp = &(*c_p->sig_qs.save)->next;
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
    if (sigp && ERTS_SIG_IS_RECV_MARKER(sigp))
        sigpp = erts_msgq_pass_recv_markers(c_p, sigpp);
    c_p->sig_qs.save = sigpp;
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(c_p, 0);
}

ERTS_GLB_INLINE void
erts_msgq_set_save_end(Process *c_p)
{
    /* Set save pointer to end of message queue... */
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));

    erts_proc_sig_queue_lock(c_p);
    erts_proc_sig_fetch(c_p);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

    if (!c_p->sig_qs.cont)
        c_p->sig_qs.save = c_p->sig_qs.last;
    else {
        /*
         * Unhandled signals in middle queue; we need to
         * pass a receive marker through it...
         */
	erts_msgq_recv_marker_create_insert_set_save(c_p, NIL);
    }
}

#undef ERTS_PROC_SIG_RECV_MARK_CLEAR_PENDING_SET_SAVE__
#undef ERTS_PROC_SIG_RECV_MARK_CLEAR_OLD_MARK__

ERTS_GLB_INLINE erts_aint32_t
erts_proc_sig_check_wait_dirty_handle_signals(Process *c_p,
                                              erts_aint32_t state_in)
{
    erts_aint32_t state = state_in;
    ASSERT(!!erts_get_scheduler_data());
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    if (c_p->sig_qs.flags & FS_HANDLING_SIGS) {
        erts_proc_sig_do_wait_dirty_handle_signals__(c_p);
        state = erts_atomic32_read_mb(&c_p->state);
    }
    ERTS_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));
    ASSERT(!(c_p->sig_qs.flags & FS_HANDLING_SIGS));
    return state;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERTS_PROC_SIG_QUEUE_H__ */
