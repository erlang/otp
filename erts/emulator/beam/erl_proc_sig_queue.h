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
 *              Currently the following signals are handled:
 *              - Messages
 *              - Exit
 *              - Monitor
 *              - Demonitor
 *              - Monitor down
 *              - Persistent monitor message
 *              - Link
 *              - Unlink
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

struct erl_mesg;
struct erl_dist_external;

typedef struct {
    struct erl_mesg *next;
    union {
        struct erl_mesg **next;
        void *attachment;
    } specific;
    Eterm tag;
} ErtsSignalCommon;

#define ERTS_SIG_HANDLE_REDS_MAX_PREFERED (CONTEXT_REDS/40)

#ifdef ERTS_PROC_SIG_HARD_DEBUG
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P)       \
    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__((P), "")
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P, QL) \
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((P), (QL), "")
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, What)   \
    erts_proc_sig_hdbg_check_in_queue((P), (What), __FILE__, __LINE__)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, QL, What)              \
    erts_proc_sig_hdbg_check_priv_queue((P), (QL), (What), __FILE__, __LINE__)
struct process;
void erts_proc_sig_hdbg_check_priv_queue(struct process *c_p, int qlock,
                                         char *what, char *file, int line);
void erts_proc_sig_hdbg_check_in_queue(struct process *c_p, char *what,
                                       char *file, int line);
#else
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(P)
#  define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(P, QL)
#  define ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__(P, What)
#define ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__(P, QL, What)
#endif

#endif

#if !defined(ERTS_PROC_SIG_QUEUE_H__) && !defined(ERTS_PROC_SIG_QUEUE_TYPE_ONLY)
#define ERTS_PROC_SIG_QUEUE_H__

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


/*
 * ERTS_SIG_Q_OP_MSGQ_LEN_OFFS_MARK is not an actual
 * operation. We keep it at the top of the OP range,
 * larger than ERTS_SIG_Q_OP_MAX.
 */
#define ERTS_SIG_Q_OP_MSGQ_LEN_OFFS_MARK ERTS_SIG_Q_OP_MASK

#define ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK \
    ERTS_PROC_SIG_MAKE_TAG(ERTS_SIG_Q_OP_MSGQ_LEN_OFFS_MARK,0,0)

struct dist_entry_;

/*
 * Send operations of currently supported process signals follow...
 */

/**
 *
 * @brief Send an exit signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
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
erts_proc_sig_send_exit(Process *c_p, Eterm from, Eterm to,
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
 * @brief Send an exit signal due to broken link to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
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
erts_proc_sig_send_link_exit(Process *c_p, Eterm from, ErtsLink *lnk,
                             Eterm reason, Eterm token);

/**
 *
 * @brief Send an link signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
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
erts_proc_sig_send_link(Process *c_p, Eterm to, ErtsLink *lnk);

/**
 *
 * @brief Send an unlink signal to a process.
 *
 *
 * @param[in]     c_p           Pointer to process struct of
 *                              currently executing process.
 *
 * @param[in]     lnk           Pointer to link structure from
 *                              the sending side. It should
 *                              contain information about
 *                              receiver.
 */
void
erts_proc_sig_send_unlink(Process *c_p, ErtsLink *lnk);

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
 * @brief Send an unlink signal to a process.
 *
 * This function is used instead of erts_proc_sig_send_unlink()
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
 */
void
erts_proc_sig_send_dist_unlink(struct dist_entry_ *dep,
                               Eterm from, Eterm to);

/**
 *
 * @brief Send a monitor down signal to a process.
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
erts_proc_sig_send_monitor_down(ErtsMonitor *mon, Eterm reason);

/**
 *
 * @brief Send a demonitor signal to a process.
 *
 * @param[in]     mon           Pointer to origin monitor
 *                              structure from the sending
 *                              side. It should contain
 *                              information about receiver.
 *
 * @param[in]     reason        Exit reason.
 *
 */
void
erts_proc_sig_send_demonitor(ErtsMonitor *mon);

/**
 *
 * @brief Send a monitor signal to a process.
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
erts_proc_sig_send_monitor(ErtsMonitor *mon, Eterm to);

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
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     ref           Reference identifying the monitor.
 *
 */
void
erts_proc_sig_send_dist_demonitor(Eterm to, Eterm ref);

/**
 *
 * @brief Send a persistent monitor triggered signal to a process.
 *
 * Used by monitors that are not auto disabled such as for
 * example 'time_offset' monitors.
 *
 * @param[in]     type          Monitor type.
 *
 * @param[in]     key           Monitor key.
 *
 * @param[in]     from          Identifier of sender.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     msg           Message template.
 *
 * @param[in]     msg_sz        Heap size of message template.
 *
 */
void
erts_proc_sig_send_persistent_monitor_msg(Uint16 type, Eterm key,
                                          Eterm from, Eterm to,
                                          Eterm msg, Uint msg_sz);

/**
 *
 * @brief Send a trace change signal to a process.
 *
 * @param[in]     to            Identifier of receiver.
 *
 * @param[in]     on            Trace flags to enable.
 *
 * @param[in]     off           Trace flags to disable.
 *
 * @param[in]     tracer        Tracer to set. If the non-value,
 *                              tracer will not be changed.
 *
 */
void
erts_proc_sig_send_trace_change(Eterm to, Uint on, Uint off,
                                Eterm tracer);

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
 */
void
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
 * @param[in]     len           Lenght of info index array
 *
 * @param[in]     need_msgq_len Non-zero if message queue
 *                              length is needed; otherwise,
 *                              zero. If non-zero, sig_qs.len
 *                              will be set to correspond
 *                              to the message queue length
 *                              before call to
 *                              erts_process_info()
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
                                        int len,
                                        int need_msgq_len,
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
 * context of the receiving process. A response
 * message '{Ref, Result}' is sent to the sender
 * when 'func' has been called. 'Ref' is the reference
 * returned by this function and 'Result' is the
 * term returned by 'func'. If the return value of
 * 'func' is not an immediate term, 'func' has to
 * allocate a heap fragment where the result is stored
 * and update the the heap fragment pointer pointer
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
 * @returns                     If the request was sent,
 *                              an internal ordinary
 *                              reference; otherwise,
 *                              THE_NON_VALUE (non-existing
 *                              receiver).
 */
Eterm
erts_proc_sig_send_rpc_request(Process *c_p,
                               Eterm to,
                               int reply,
                               Eterm (*func)(Process *, void *, int *, ErlHeapFragment **),
                               void *arg);

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
 * @return                      Returns a non-zero value, when
 *                              no more signals to handle in the
 *                              middle queue remain. A zero
 *                              return value means that there
 *                              remains signals in the middle
 *                              queue.
 */
int
erts_proc_sig_handle_exit(Process *c_p, Sint *redsp);

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
 * @brief Get amount of messages in private queues
 *
 * @param[in]   c_p             Pointer to process struct of
 *                              currently executing process.
 *
 * @returns                     Amount of message signals in
 *                              inner plus middle signal
 *                              queues after fetch completed
 *                              (NOT the message queue
 *                              length).
 */
Sint
erts_proc_sig_privqs_len(Process *c_p);


/**
 * @brief Enqueue list of signals on process.
 *
 * Message queue must be locked on receiving process.
 *
 * @param rp                Receiving process.
 * @param first             First signal in list.
 * @param last              Last signal in list.
 * @param last_next         Pointer to next-pointer to last non-message signal
 *                          or NULL if no non-message signal after 'first'.
 * @param msg_cnt           Number of message signals in list.
 * @param in_state          'state' of rp.
 *
 * @return                  'state' of rp.
 */
erts_aint32_t
erts_enqueue_signals(Process *rp, ErtsMessage *first,
                     ErtsMessage **last, ErtsMessage **last_next,
                     Uint msg_cnt,
                     erts_aint32_t in_state);

/**
 *
 * @brief Flush pending signal.
 *
 */
void
erts_proc_sig_send_pending(ErtsSchedulerData* esdp);

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

void erts_make_dirty_proc_handled(Eterm pid, erts_aint32_t state,
                                  erts_aint32_t prio);


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
 */
Uint erts_proc_sig_prep_msgq_for_inspection(Process *c_p,
                                            Process *rp,
                                            ErtsProcLocks rp_locks,
                                            int info_on_self,
                                            ErtsMessageInfo *mip);

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

ErtsDistExternal *
erts_proc_sig_get_external(ErtsMessage *msgp);

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

void erts_proc_sig_fetch__(Process *proc);
Sint erts_proc_sig_fetch_msgq_len_offs__(Process *proc);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Sint
erts_proc_sig_fetch(Process *proc)
{
    Sint res = 0;
    ErtsSignal *sig;

    ERTS_LC_ASSERT(ERTS_PROC_IS_EXITING(proc)
                   || ((erts_proc_lc_my_proc_locks(proc)
                        & (ERTS_PROC_LOCK_MAIN
                           | ERTS_PROC_LOCK_MSGQ))
                       == (ERTS_PROC_LOCK_MAIN
                           | ERTS_PROC_LOCK_MSGQ)));

    ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE(proc);
    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(proc, !0);

    sig = (ErtsSignal *) proc->sig_inq.first;
    if (sig) {
        if (ERTS_LIKELY(sig->common.tag != ERTS_PROC_SIG_MSGQ_LEN_OFFS_MARK))
            erts_proc_sig_fetch__(proc);
        else
            res = erts_proc_sig_fetch_msgq_len_offs__(proc);
    }

    res += proc->sig_qs.len;

    ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE(proc, !0);

#ifdef ERTS_PROC_SIG_HARD_DEBUG_SIGQ_MSG_LEN
    {
        Sint len = 0;
        ERTS_FOREACH_SIG_PRIVQS(
            proc, mp,
            {
                if (ERTS_SIG_IS_MSG(mp))
                    len++;
            });
        ERTS_ASSERT(res == len);
    }
#endif

    return res;
}

ERTS_GLB_INLINE void
erts_proc_notify_new_sig(Process* rp, erts_aint32_t state,
                         erts_aint32_t enable_flag)
{
    if (~(state & (ERTS_PSFLG_EXITING
                   | ERTS_PSFLG_ACTIVE_SYS
                   | ERTS_PSFLG_SIG_IN_Q))
        | (~state & enable_flag)) {
        /* Schedule process... */
        state = erts_proc_sys_schedule(rp, state, enable_flag);
    }

    if (state & ERTS_PSFLG_DIRTY_RUNNING) {
        /*
         * We ignore ERTS_PSFLG_DIRTY_RUNNING_SYS. For
         * more info see erts_execute_dirty_system_task()
         * in erl_process.c.
         */
        erts_make_dirty_proc_handled(rp->common.id, state, -1);
    }
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERTS_PROC_SIG_QUEUE_H__ */
